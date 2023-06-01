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

pub const Context = struct {
    // allocator: Allocator,
    stack: Stack,
    // owns_stack: bool = false,

    pub const Error = error{
        UnimplementedOpcode,
    } || Stack.Error || BytecodeStream.Error;

    pub fn init(stack_buffer: Stack.Buffer) Context {
        return Context{
            .stack = Stack.init(stack_buffer),
        };
    }

    pub fn execute(self: *Context, code_buffer: BytecodeStream.Buffer) Error!void {
        var stream = BytecodeStream.init(code_buffer);
        while (stream.nextOpcode()) |opcode| {
            switch (opcode) {
                inline else => |oc| try self.executeInstruction(&stream, oc),
            }
        } else |err| {
            if (err == Error.EndOfStream) {
                return;
            } else {
                return err;
            }
        }
    }

    fn executeInstruction(self: *Context, stream: *BytecodeStream, comptime opcode: Opcode) Error!void {
        const opkind = comptime opcode.getKind();
        switch (opkind) {
            .stack => try self.executeStackInstruction(stream, opcode),
            .arithmetic => try self.executeArithmeticInstruction(stream, opcode),
            else => return Error.UnimplementedOpcode,
        }
    }

    fn executeStackInstruction(self: *Context, stream: *BytecodeStream, comptime opcode: Opcode) Error!void {
        const typ = try stream.nextPrimitiveType();
        switch (typ) {
            inline else => |t| {
                const size = comptime t.getSize();
                switch (opcode) {
                    .push => {
                        const value = try stream.nextBytes(size);
                        try self.stack.pushBytes(size, value);
                    },
                    .pop => {
                        try self.stack.discard(size);
                    },
                    .dupe => {
                        const offset = try stream.next(Stack.Offset);
                        const bytes = try self.stack.getBytes(size, offset);
                        try self.stack.pushBytes(size, bytes);
                    },
                    .copy => {
                        const offset = try stream.next(Stack.Offset);
                        const source = try self.stack.popBytes(size);
                        const dest = try self.stack.getBytes(size, offset);
                        @memcpy(dest, source);
                    },
                    else => unreachable,
                }
            },
        }
    }

    fn executeArithmeticInstruction(self: *Context, stream: *BytecodeStream, comptime opcode: Opcode) Error!void {
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
};

pub const Stack = struct {
    buffer: Buffer,
    index: usize,

    pub const Buffer = []align(stack_align) u8;
    pub const Offset = u16;

    pub const Error = error{
        StackOverflow,
        StackUnderFlow,
        ReadOutOfRange,
    };

    pub fn init(buffer: Buffer) Stack {
        return .{
            .buffer = buffer,
            .index = buffer.len,
        };
    }

    pub fn push(self: *Stack, value: anytype) Error!void {
        const size = @sizeOf(@TypeOf(value));
        self.pushBytes(size, @ptrCast(*const [size]u8, &value));
    }

    pub fn pushBytes(self: *Stack, comptime len: usize, bytes: *const [len]u8) Error!void {
        const aligned_len = comptime std.mem.alignForward(len, stack_align);
        if (aligned_len > self.index) {
            return Error.StackOverflow;
        }
        const index = self.index - aligned_len;
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

    pub fn get(self: *Stack, comptime T: type, offset: usize) Error!*T {
        const bytes = try self.getBytes(@sizeOf(T), offset);
        return @ptrCast(*T, bytes);
    }

    pub fn getBytes(self: *Stack, comptime len: usize, offset: usize) Error!*[len]u8 {
        if (offset + len > self.index) {
            return Error.ReadOutOfRange;
        }
        return @ptrCast(*[len]u8, &self.buffer[offset]);
    }
};
