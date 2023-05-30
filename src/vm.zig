const std = @import("std");
const types = @import("types.zig");
const util = @import("util.zig");

const Allocator = std.mem.Allocator;

pub const stack_align_bits = 3;
pub const stack_align = 1 << stack_align_bits;

pub const Context = struct {
    allocator: Allocator,
    stack: Stack,

    pub const Error = error{} | Stack.Error | BytecodeStream.Error;

    pub fn init(allocator: Allocator, stack_size: usize) Allocator.Error!Context {
        const stack_buffer = try allocator.alignedAlloc(u8, stack_align, stack_size);
        errdefer allocator.free(stack_buffer);
        return Context{
            .allocator = allocator,
            .stack = Stack.init(stack_buffer),
        };
    }

    pub fn deinit(self: *Context) void {
        self.allocator.free(self.stack.data);
    }

    pub fn execute(self: *Context, code_buffer: BytecodeStream.Buffer) Error!void {
        const stream = BytecodeStream.init(code_buffer);
        // const stack = &self.stack;
        while (stream.nextOpcode()) |opcode| {
            switch (opcode) {
                inline else => |oc| try self.executeOpcode(&stream, oc),
            }
        } else {
            return;
        }
    }

    fn executeOpcode(self: *Context, stream: *BytecodeStream, comptime opcode: Opcode) Error!void {
        switch (opcode) {
            .push => {
                const typ = try stream.nextPrimitiveType();
                switch (typ) {
                    inline else => |t| {
                        const size = comptime t.getSize();
                        const value = try stream.nextBytes(size);
                        try self.stack.pushBytes(size, value);
                    },
                }
            },
            .pop => {
                const typ = try stream.nextPrimitiveType();
                switch (typ) {
                    inline else => |t| {
                        const size = comptime t.getSize();
                        try self.stack.pop(size);
                    }
                }
            },
            .dupe => {
                const typ = try stream.nextPrimitiveType();
                const offset = try stream.next(Stack.Offset);
                switch (typ) {
                    inline else => |t| {
                        const size = comptime t.getSize();
                        const bytes = try self.stack.getBytes(size, offset);
                        try self.stack.pushBytes(size, bytes);
                    }
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
        return @ptrCast(*T, ptr);
    }

    pub fn peekBytes(self: *Stack, comptime len: usize) *[len] u8 {
        return @ptrCast(*[len]u8, &self.buffer[self.index]);
    }

    pub fn pop(self: *Stack, comptime T: type) Error!*T {
        const ptr = try self.popBytes(@sizeOf(T));
        return @ptrCast(*T, ptr);
    }

    pub fn popBytes(self: *Stack, comptime len: usize) Error!*[len]u8 {
        const ptr = self.popBytes(len);
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

    pub fn getBytes(self: *Stack, comptime len: usize, offset: usize) Error!*[len] u8 {
        if (offset + len > self.index) {
            return Error.ReadOutOfRange;
        }
        return @ptrCast(*[len] u8, &self.buffer[offset]);
    }
};

pub const BytecodeStream = struct {
    buffer: Buffer,
    index: usize = 0,

    pub const Buffer = []const u8;

    pub const Error = error{
        IllegalOpcode,
        IllegalPrimitiveType,
        EndOfStream,
    };

    pub fn init(buffer: Buffer) BytecodeStream {
        return .{
            .buffer = buffer,
        };
    }

    pub fn next(self: *BytecodeStream, comptime T: type) Error!T {
        const size = @sizeOf(T);
        const bytes = try self.nextBytes(size);
        return @ptrCast(*const align(1) T, bytes).*;
    }

    pub fn nextBytes(self: *BytecodeStream, comptime len: usize) Error!*const [len]u8 {
        if (self.index >= self.buffer.len) {
            return Error.EndOfStream;
        }
        const next_index = self.index + len;
        defer self.index = next_index;
        return @ptrCast(*const [len]u8, &self.buffer[self.index]);
    }

    pub fn nextOpcode(self: *BytecodeStream) Error!Opcode {
        return self.nextByteEnum(Opcode, Error.IllegalOpcode);
    }

    pub fn nextPrimitiveType(self: *BytecodeStream) Error!types.Primitive {
        return self.nextByteEnum(type.Primitive, Error.IllegalPrimitiveType);
    }

    fn nextByteEnum(self: *BytecodeStream, comptime E: type, comptime err: Error) Error!E {
        return util.byteToEnum(E, Error, err, try self.next(u8));
    }
};

pub const Opcode = enum(u8) {
    push,
    pop,
    dupe,

};
