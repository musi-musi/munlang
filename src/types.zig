const std = @import("std");

pub const Primitive = enum(u8) {
    pub const Kind = enum(u8) {
        bool = 0x00,
        signed = 0x01,
        unsigned = 0x02,
        float = 0x03,
    };

    bool = createValue(.bool, 8, 1),

    // signed ints and vectors
    i8 = createValue(.signed, 8, 1),
    vec2_i8 = createValue(.signed, 8, 2),
    vec3_i8 = createValue(.signed, 8, 3),
    vec4_i8 = createValue(.signed, 8, 4),

    i16 = createValue(.signed, 16, 1),
    vec2_i16 = createValue(.signed, 16, 2),
    vec3_i16 = createValue(.signed, 16, 3),
    vec4_i16 = createValue(.signed, 16, 4),

    i32 = createValue(.signed, 32, 1),
    vec2_i32 = createValue(.signed, 32, 2),
    vec3_i32 = createValue(.signed, 32, 3),
    vec4_i32 = createValue(.signed, 32, 4),

    i64 = createValue(.signed, 64, 1),
    vec2_i64 = createValue(.signed, 64, 2),
    vec3_i64 = createValue(.signed, 64, 3),
    vec4_i64 = createValue(.signed, 64, 4),

    // unsigned ints and vectors
    u8 = createValue(.unsigned, 8, 1),
    vec2_u8 = createValue(.unsigned, 8, 2),
    vec3_u8 = createValue(.unsigned, 8, 3),
    vec4_u8 = createValue(.unsigned, 8, 4),

    u16 = createValue(.unsigned, 16, 1),
    vec2_u16 = createValue(.unsigned, 16, 2),
    vec3_u16 = createValue(.unsigned, 16, 3),
    vec4_u16 = createValue(.unsigned, 16, 4),

    u32 = createValue(.unsigned, 32, 1),
    vec2_u32 = createValue(.unsigned, 32, 2),
    vec3_u32 = createValue(.unsigned, 32, 3),
    vec4_u32 = createValue(.unsigned, 32, 4),

    u64 = createValue(.unsigned, 64, 1),
    vec2_u64 = createValue(.unsigned, 64, 2),
    vec3_u64 = createValue(.unsigned, 64, 3),
    vec4_u64 = createValue(.unsigned, 64, 4),

    // floats and vectors
    f16 = createValue(.float, 16, 1),
    vec2_f16 = createValue(.float, 16, 2),
    vec3_f16 = createValue(.float, 16, 3),
    vec4_f16 = createValue(.float, 16, 4),

    f32 = createValue(.float, 32, 1),
    vec2_f32 = createValue(.float, 32, 2),
    vec3_f32 = createValue(.float, 32, 3),
    vec4_f32 = createValue(.float, 32, 4),

    f64 = createValue(.float, 64, 1),
    vec2_f64 = createValue(.float, 64, 2),
    vec3_f64 = createValue(.float, 64, 3),
    vec4_f64 = createValue(.float, 64, 4),

    pub fn createValue(
        comptime kind: Kind,
        comptime bits: u8,
        comptime dimensions: u8,
    ) u8 {
        comptime var value = (@enumToInt(kind) << 4);
        value |= switch (bits) {
            8 => @as(u8, 0),
            16 => @as(u8, 1 << 2),
            32 => @as(u8, 2 << 2),
            64 => @as(u8, 3 << 2),
            else => @compileError(std.fmt.comptimePrint("invalid bit count {d}. must be 8, 16, 32, or 64", .{bits})),
        };
        value |= switch (dimensions) {
            1 => @as(u8, 0),
            2 => @as(u8, 1),
            3 => @as(u8, 2),
            4 => @as(u8, 3),
            else => @compileError(std.fmt.comptimePrint("invalid dimension count {d}. must be 1, 2, 3, or 4", .{dimensions})),
        };
        return value;
    }

    pub fn getKind(self: Primitive) Kind {
        return @intToEnum(Kind, @enumToInt(self) >> 4);
    }

    pub fn getBitSize(self: Primitive) u8 {
        return switch ((@enumToInt(self) >> 2) & 3) {
            0 => 8,
            1 => 16,
            2 => 32,
            3 => 64,
            else => unreachable,
        };
    }

    pub fn getDimensions(self: Primitive) u8 {
        return (@enumToInt(self) & 3) + 1;
    }

    pub fn getScalar(self: Primitive) Primitive {
        return @intToEnum(Primitive, @enumToInt(self) & 0b1111_1100);
    }

    pub fn getSize(self: Primitive) usize {
        return (self.getBitSize() >> 3) * self.getDimensions();
    }

    pub fn ZigType(comptime self: Primitive) type {
        const Scalar = self.ZigScalarType();
        const dims = self.getDimensions();
        if (dims == 1) {
            return Scalar;
        } else {
            return [dims]Scalar;
        }
    }

    pub fn ZigScalarType(comptime self: Primitive) type {
        const bits = self.getBitSize();
        return switch (self.getKind()) {
            .bool => bool,
            .signed => std.meta.Int(.signed, bits),
            .unsigned => std.meta.Int(.unsigned, bits),
            .float => std.meta.Float(bits),
        };
    }

    pub fn ZigVectorType(comptime self: Primitive) type {
        return [self.getDimensions()]self.ZigScalarType();
    }

};
