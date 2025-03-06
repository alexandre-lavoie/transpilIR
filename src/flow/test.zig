const std = @import("std");

const ast = @import("../ast/lib.zig");
const common = @import("../common.zig");
const cfg = @import("cfg.zig");
const ssa = @import("ssa.zig");
const symbol = @import("../symbol/lib.zig");

const test_lib = @import("../test/lib.zig");
const symbol_test = @import("../symbol/test.zig");

pub const testAST = test_lib.testAST;
pub const test_target = test_lib.test_target;

pub fn testCFG(allocator: std.mem.Allocator, file: []const u8, tree: *ast.AST, symbol_table: *symbol.SymbolTable, target: *const common.Target, graph: *cfg.CFG) !void {
    try symbol_test.testValidate(allocator, file, tree, symbol_table, target);

    try graph.build(tree, symbol_table);
}

pub fn testSSA(allocator: std.mem.Allocator, file: []const u8, tree: *ast.AST, symbol_table: *symbol.SymbolTable, target: *const common.Target, graph: *ssa.SSA) !void {
    try symbol_test.testValidate(allocator, file, tree, symbol_table, target);

    try graph.build();
}
