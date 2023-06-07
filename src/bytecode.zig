const std = @import("std");
const vm = @import("vm.zig");
const types = @import("types.zig");
const util = @import("util.zig");

const Allocator = std.mem.Allocator;
const Primitive = types.Primitive;

/// encoding for offsets into the stack, in bytes from the top
/// if you need to access stack slots more than 64KB away, you're doing something very wrong
pub const StackOffset = u16;

/// encoding for instruction addresses (eg jump instructions)
/// if you have a function with more than 4GB of code, you're doing something very wrong
pub const InstructionAddress = u32;

pub const Opcode = enum(u8) {
    /// push an immediate value to the top of the stack
    push = createValue(.stack, 0),
    /// pop a value off the stack and discard it
    pop = createValue(.stack, 1),
    /// read the stack value at [offset] and push it
    dupe = createValue(.stack, 2),
    /// overwrite the stack value at [offset] with the top value
    /// does not pop the top value
    copy = createValue(.stack, 3),

    add = createValue(.arithmetic, 0),
    sub = createValue(.arithmetic, 1),
    mul = createValue(.arithmetic, 2),
    div = createValue(.arithmetic, 3),

    // unconditional jump
    jump = createValue(.control, 0),
    // jump if zero/false
    jump_zero = createValue(.control, 1),
    // jump if nonzero/true
    jump_nz = createValue(.control, 2),
    // // jump if equal
    // jump_eq = createValue(.control, 3),
    // // jump if not equal
    // jump_neq = createValue(.control, 4),
    // // jump if less than
    // jump_lt = createValue(.control, 5),
    // // jump if less than or equal
    // jump_lte = createValue(.control, 6),
    // // jump if greater than
    // jump_gt = createValue(.control, 7),
    // // jump if greater than or equal
    // jump_gte = createValue(.control, 8),

    pub const Kind = enum(u8) {
        stack = 0x0,
        arithmetic = 0x1,
        logical = 0x2,
        control = 0x3,
    };

    fn createValue(kind: Kind, value: u4) u8 {
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

    pub fn jump(self: *BytecodeStream, address: InstructionAddress) void {
        self.index = address;
    }

    pub fn next(self: *BytecodeStream, comptime T: type) Error!T {
        const size = @sizeOf(T);
        const bytes = try self.nextBytes(size);
        // assume native endianness (were mainly targetting x86, ARM, and similar, which are all little endian)
        return @ptrCast(*align(1) const T, bytes).*;
    }

    pub fn nextBytes(self: *BytecodeStream, comptime len: usize) Error!*const [len]u8 {
        defer self.index += len;
        if (self.index + len > self.buffer.len) {
            return Error.EndOfStream;
        }
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

    pub fn nextStackOffset(self: *BytecodeStream) Error!StackOffset {
        return self.next(StackOffset);
    }

    pub fn nextInstructionAddress(self: *BytecodeStream) Error!InstructionAddress {
        return self.next(InstructionAddress);
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

    pub fn stackOffset(offset: StackOffset) [@sizeOf(StackOffset)]u8 {
        return @bitCast([@sizeOf(StackOffset)]u8, offset);
    }

    pub fn instructionAddress(address: usize) [@sizeOf(InstructionAddress)]u8 {
        return @bitCast([@sizeOf(InstructionAddress)]u8, @truncate(InstructionAddress, address));
    }

    pub fn jump(address: usize) [1 + @sizeOf(InstructionAddress)]u8 {
        return [1]u8 { @enumToInt(Opcode.jump)} ++ instructionAddress(address);
    }

};
