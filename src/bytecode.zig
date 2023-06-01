const std = @import("std");
const vm = @import("vm.zig");
const types = @import("types.zig");
const util = @import("util.zig");

const Allocator = std.mem.Allocator;
const Primitive = types.Primitive;

pub const Opcode = enum(u8) {
    /// push an immediate value to the top of the stack
    push = initValue(.stack, 0),
    /// pop a value off the stack and discard it
    pop = initValue(.stack, 1),
    /// read the stack value at [offset] and push it
    dupe = initValue(.stack, 2),
    /// pop the top value and write it to the stack at [offset]
    copy = initValue(.stack, 3),

    add = initValue(.arithmetic, 0),
    sub = initValue(.arithmetic, 1),
    mul = initValue(.arithmetic, 2),
    div = initValue(.arithmetic, 3),

    pub const Kind = enum(u8) {
        stack = 0x0,
        arithmetic = 0x1,
        logical = 0x2,
    };

    fn initValue(kind: Kind, value: u4) u8 {
        return @enumToInt(kind) << 4 | @as(u8, value);
    }

    pub fn getKind(self: Opcode) Kind {
        return @intToEnum(Kind, @enumToInt(self) >> 4);
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
        // assume native endianness (were mainly targetting x86, ARM, and similar, which are all little endian)
        return @ptrCast(*align(1) const T, bytes).*;
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

    pub fn nextPrimitiveType(self: *BytecodeStream) Error!Primitive {
        return self.nextByteEnum(Primitive, Error.IllegalPrimitiveType);
    }

    fn nextByteEnum(self: *BytecodeStream, comptime E: type, comptime err: Error) Error!E {
        return util.byteToEnum(E, Error, err, try self.next(u8));
    }
};

pub const instruction = struct {
    pub fn push(comptime type_arg: Primitive, value: type_arg.ZigType()) [2 + type_arg.getSize()]u8 {
        return generic(.push, type_arg) ++ immediate(type_arg, value);
    }

    pub fn generic(opcode: Opcode, type_arg: Primitive) [2]u8 {
        return .{
            @enumToInt(opcode),
            @enumToInt(type_arg),
        };
    }

    pub fn immediate(comptime typ: Primitive, value: typ.ZigType()) [typ.getSize()]u8 {
        return @bitCast([typ.getSize()]u8, value);
    }

    pub fn stackOffset(offset: vm.Stack.Offset) [@sizeOf(vm.Stack.Offset)]u8 {
        return @bitCast([@sizeOf(vm.Stack.Offset)]u8, offset);
    }
};
