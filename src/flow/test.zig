const std = @import("std");

const ast = @import("../ast/lib.zig");
const common = @import("../common.zig");
const cfg = @import("cfg.zig");
const symbol = @import("../symbol/lib.zig");

const test_lib = @import("../test/lib.zig");
const symbol_test = @import("../symbol/test.zig");

pub const testAST = test_lib.testAST;
pub const test_target = test_lib.test_target;

pub fn testCFG(allocator: std.mem.Allocator, file: []const u8, tree: *ast.AST, symbol_table: *symbol.SymbolTable, target: *const common.Target, graph: *cfg.CFG) !void {
    try symbol_test.testValidate(allocator, file, tree, symbol_table, target);

    var callback = cfg.CFGWalkCallback.init(
        allocator,
        symbol_table,
        graph,
    );
    defer callback.deinit();

    var walk = ast.ASTWalk.init(allocator, tree);
    defer walk.deinit();

    try walk.start(tree.entrypoint() orelse return error.NotFound);
    while (try walk.next()) |out| {
        try switch (out.enter) {
            true => callback.enter(out.index, out.value),
            false => callback.exit(out.value),
        };
    }
}
