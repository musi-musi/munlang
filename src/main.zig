const std = @import("std");
const vm = @import("vm.zig");
const bytecode = @import("bytecode.zig");

pub const std_options = struct {

    pub const log_level = std.log.Level.info;

};

const instr = bytecode.instruction;

// const code = 
//     instr.push(.vec2_i32, .{1, 32}) ++
//     instr.push(.vec2_i32, .{2, -12}) ++
//     instr.generic(.add, .vec2_i32);

pub fn main() !void {
    var stack_buffer: [1024] u8 align(8) = undefined;
    var context = vm.Context.init(&stack_buffer);
    // try context.stack.push(@as(i32, 5));
    try context.executeCode(&fib.code);
    const result = context.stack.peek(i32).*;
    std.log.info("result: {d}", .{result});
}

const fib = struct {

    const prelude =
        instr.push(.i32, 0) ++
        instr.push(.i32, 1) ++
        instr.push(.i32, 25 - 1);
    
    const loop_condition = 
        instr.generic(.jump_zero, .i32) ++ instr.instructionAddress(end_address);

    const loop_body = 
        instr.generic(.dupe, .i32) ++ instr.stackOffset(8) ++
        instr.generic(.dupe, .i32) ++ instr.stackOffset(16) ++
        instr.generic(.copy, .i32) ++ instr.stackOffset(8) ++
        instr.generic(.add, .i32) ++
        instr.generic(.copy, .i32) ++ instr.stackOffset(16) ++
        instr.generic(.pop, .i32) ++
        instr.push(.i32, 1) ++
        instr.generic(.sub, .i32) ++
        instr.jump(prelude.len);

    const end_address = prelude.len + 6 + loop_body.len;

    const code = prelude ++ loop_condition ++ loop_body ++
        instr.generic(.pop, .i32) ++
        instr.generic(.copy, .i32) ++ instr.stackOffset(8) ++
        instr.generic(.pop, .i32);

};