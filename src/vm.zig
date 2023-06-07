const std = @import("std");
const types = @import("types.zig");
const util = @import("util.zig");
const bytecode = @import("bytecode.zig");

const Primitive = types.Primitive;

const Opcode = bytecode.Opcode;
const BytecodeStream = bytecode.BytecodeStream;

const Allocator = std.mem.Allocator;

pub const stack_align_bits = 3;
pub const stack_align = 1 << stack_align_bits;

pub const Frame = struct {
    stack: *Stack,
    base: usize,

    pub fn get(self: Frame, comptime T: type, offset: usize) Stack.Error!*T {
        const bytes = try self.getBytes(@sizeOf(T), offset);
        return @ptrCast(*T, bytes);
    }

    pub fn getBytes(self: Frame, comptime len: usize, offset: usize) Stack.Error!*[len]u8 {
        const index = self.base - offset;
        // if (offset + len > self.index) {
        //     return Stack.Error.ReadOutOfRange;
        // }
        return @ptrCast(*[len]u8, &self.stack.buffer[index]);
    }
};

pub const Context = struct {
    stack: Stack,

    pub const Error = error{
        UnimplementedOpcode,
    } || Stack.Error || BytecodeStream.Error;

    pub fn init(stack_buffer: Stack.Buffer) Context {
        return Context{
            .stack = Stack.init(stack_buffer),
        };
    }

    pub fn executeCode(self: *Context, code_buffer: BytecodeStream.Buffer) Error!void {
        var stream = BytecodeStream.init(code_buffer);
        const frame = Frame{
            .stack = &self.stack,
            .base = self.stack.buffer.len,
        };
        try self.executeBytecodeStream(frame, &stream);
    }

    pub fn executeBytecodeStream(self: *Context, frame: Frame, stream: *BytecodeStream) Error!void {
        while (stream.nextOpcode()) |opcode| {
            switch (opcode) {
                inline else => |oc| try self.executeInstruction(frame, stream, oc),
            }
        } else |err| {
            if (err == Error.EndOfStream) {
                return;
            } else {
                return err;
            }
        }
    }

    fn executeInstruction(self: *Context, frame: Frame, stream: *BytecodeStream, comptime opcode: Opcode) Error!void {
        // std.log.info("{s}", .{@tagName(opcode)});
        const opkind = comptime opcode.getKind();
        switch (opkind) {
            .stack => try self.executeStackInstruction(frame, stream, opcode),
            .arithmetic => try self.executeArithmeticInstruction(frame, stream, opcode),
            .control => try self.executeControlInstruction(frame, stream, opcode),
            else => return Error.UnimplementedOpcode,
        }
    }

    fn executeStackInstruction(self: *Context, frame: Frame, stream: *BytecodeStream, comptime opcode: Opcode) Error!void {
        const typ = try stream.nextPrimitiveType();
        switch (typ) {
            inline else => |t| {
                const size = comptime t.getSize();
                const alignment = comptime t.getAlignment();
                switch (opcode) {
                    .push => {
                        const value = try stream.nextBytes(size);
                        try self.stack.pushBytes(size, alignment, value);
                    },
                    .pop => {
                        try self.stack.discard(size);
                    },
                    .dupe => {
                        const offset = try stream.next(bytecode.StackOffset);
                        const bytes = try frame.getBytes(size, offset);
                        try self.stack.pushBytes(size, alignment, bytes);
                    },
                    .copy => {
                        const offset = try stream.next(bytecode.StackOffset);
                        const source = self.stack.peekBytes(size);
                        const dest = try frame.getBytes(size, offset);
                        @memcpy(dest, source);
                    },
                    else => unreachable,
                }
            },
        }
    }

    fn executeArithmeticInstruction(self: *Context, frame: Frame, stream: *BytecodeStream, comptime opcode: Opcode) Error!void {
        _ = frame;
        const typ = try stream.nextPrimitiveType();
        switch (typ) {
            .bool => return Error.IllegalPrimitiveType,
            inline else => |t| {
                const Scalar = comptime t.ZigScalarType();
                const dimensions = comptime t.getDimensions();
                const T = [dimensions]Scalar;
                const rhs = try self.stack.pop(T);
                const lhs = self.stack.peek(T);
                switch (comptime t.getKind()) {
                    .float => {
                        inline for (lhs, rhs) |*a, *b| {
                            switch (opcode) {
                                .add => a.* += b.*,
                                .sub => a.* -= b.*,
                                .mul => a.* *= b.*,
                                .div => a.* /= b.*,
                                else => unreachable,
                            }
                        }
                    },
                    .signed, .unsigned => {
                        inline for (lhs, rhs) |*a, *b| {
                            switch (opcode) {
                                .add => a.* +%= b.*,
                                .sub => a.* -%= b.*,
                                .mul => a.* *%= b.*,
                                .div => a.* = @divFloor(a.*, b.*),
                                else => unreachable,
                            }
                        }
                    },
                    else => unreachable,
                }
            },
        }
    }

    fn executeControlInstruction(self: *Context, frame: Frame, stream: *BytecodeStream, comptime opcode: Opcode) Error!void {
        _ = frame;
        if (opcode == .jump) {
            const address = try stream.nextInstructionAddress();
            stream.jump(address);
            return;
        }
        const typ = try stream.nextPrimitiveType();
        switch (typ) {
            inline else => |t| {
                if (comptime t.getDimensions() != 1) {
                    return Error.IllegalPrimitiveType;
                }
                const address = try stream.nextInstructionAddress();
                const value = self.stack.peek(comptime t.ZigType()).*;
                // if (t != .bool) {
                //     std.log.info("{d}", .{value});
                // }
                switch (opcode) {
                    .jump_zero => {
                        if (t == .bool) {
                            if (!value) stream.jump(address);
                        } else if (value == 0) {
                            stream.jump(address);
                        }
                    },
                    .jump_nz => {
                        if (t == .bool) {
                            if (value) stream.jump(address);
                        } else if (value != 0) {
                            stream.jump(address);
                        }
                    },
                    else => return Error.UnimplementedOpcode,
                }
            },
        }
    }
};

pub const Stack = struct {
    buffer: Buffer,
    index: usize,

    pub const Buffer = []align(stack_align) u8;

    pub const Error = error{
        StackOverflow,
        StackUnderFlow,
        ReadOutOfRange,
    };

    pub fn init(buffer: Buffer) Stack {
        return .{
            .buffer = buffer,
            .index = std.mem.alignBackward(buffer.len, stack_align),
        };
    }

    pub fn push(self: *Stack, value: anytype) Error!void {
        const size = @sizeOf(@TypeOf(value));
        const alignment = @alignOf(@TypeOf(value));
        try self.pushBytes(size, alignment, @ptrCast(*const [size]u8, &value));
    }

    pub fn pushBytes(self: *Stack, comptime len: usize, comptime alignment: u29, bytes: *const [len]u8) Error!void {
        const index = std.mem.alignBackward(self.index -% len, alignment);
        if (index > self.index) {
            return Error.StackOverflow;
        }
        self.index = index;
        const dest = self.buffer[index .. index + len];
        @memcpy(dest, bytes);
    }

    pub fn peek(self: *Stack, comptime T: type) *T {
        const ptr = self.peekBytes(@sizeOf(T));
        return @ptrCast(*T, @alignCast(@alignOf(T), ptr));
    }

    pub fn peekBytes(self: *Stack, comptime len: usize) *[len]u8 {
        return @ptrCast(*[len]u8, &self.buffer[self.index]);
    }

    pub fn pop(self: *Stack, comptime T: type) Error!*T {
        const ptr = try self.popBytes(@sizeOf(T));
        return @ptrCast(*T, @alignCast(@alignOf(T), ptr));
    }

    pub fn popBytes(self: *Stack, comptime len: usize) Error!*[len]u8 {
        const ptr = self.peekBytes(len);
        try self.discard(len);
        return ptr;
    }

    pub fn discard(self: *Stack, comptime len: usize) Error!void {
        const aligned_len = comptime std.mem.alignForward(len, stack_align);
        const index = self.index + aligned_len;
        if (index > self.buffer.len) {
            return Error.StackUnderFlow;
        }
        self.index = index;
    }
};
