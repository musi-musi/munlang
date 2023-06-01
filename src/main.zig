const std = @import("std");
const vm = @import("vm.zig");
const bytecode = @import("bytecode.zig");

const inst = bytecode.instruction;

pub const std_options = struct {

    pub const log_level = std.log.Level.info;

};

const code = 
    inst.push(.i32, 1) ++
    inst.push(.i32, 2) ++
    inst.generic(.add, .i32);

pub fn main() !void {
    var stack_buffer: [1024] u8 align(8) = undefined;
    var context = vm.Context.init(&stack_buffer);
    try context.execute(&code);
    const result = context.stack.peek(i32).*;
    std.log.info("result: {d}", .{result});
}

// export fn execute(ptr: [*]u8, len: usize) void {
//     const code = ptr[0..len];
//     var stack_buffer: [1024] u8 align(8) = undefined;
//     var context = vm.Context.init(&stack_buffer);
//     context.execute(code) catch {};
// }