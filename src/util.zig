const std = @import("std");

pub fn byteToEnum(comptime E: type, comptime Error: type, comptime err: Error, byte: u8) Error!E {
    const lut = comptime blk: {
        var value: [256]Error!E = [_]Error!E{ err } ** 256;
        for (std.enums.values(E)) |tag| {
            value[@enumToInt(tag)] = tag;
        }
        break :blk value;
    };
    return lut[byte];
}