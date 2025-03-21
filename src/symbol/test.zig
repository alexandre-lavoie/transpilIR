const std = @import("std");

const ast = @import("../ast/lib.zig");
const common = @import("../common.zig");
const symbol = @import("lib.zig");

const test_lib = @import("../test/lib.zig");

pub const testAST = test_lib.testAST;
pub const test_target = test_lib.test_target;

pub fn testSource(allocator: std.mem.Allocator, file: []const u8, tree: *ast.AST, symbol_table: *symbol.SymbolTable) !void {
    var file_stream: std.io.StreamSource = .{
        .const_buffer = std.io.fixedBufferStream(file),
    };

    var callback = symbol.SymbolSourceWalkCallback.init(
        allocator,
        symbol_table,
        &file_stream,
    );

    var walk = ast.ASTWalk.init(allocator, tree);
    defer walk.deinit();

    try walk.start(tree.entrypoint() orelse return error.NotFound);
    while (try walk.next()) |out| {
        const stat = tree.getPtr(out.index) orelse return error.NotFound;

        try switch (out.state) {
            .enter => callback.enter(stat),
            .exit => callback.exit(stat),
            else => {},
        };
    }
}

pub fn testMemory(allocator: std.mem.Allocator, file: []const u8, tree: *ast.AST, symbol_table: *symbol.SymbolTable) !void {
    try testSource(allocator, file, tree, symbol_table);

    var callback = symbol.SymbolMemoryWalkCallback.init(
        allocator,
        symbol_table,
    );
    defer callback.deinit();

    var walk = ast.ASTWalk.init(allocator, tree);
    defer walk.deinit();

    try walk.start(tree.entrypoint() orelse return error.NotFound);
    while (try walk.next()) |out| {
        const stat = tree.getPtr(out.index) orelse return error.NotFound;

        try switch (out.state) {
            .enter => callback.enter(stat),
            .exit => callback.exit(stat),
            else => {},
        };
    }
}

pub fn testValidate(allocator: std.mem.Allocator, file: []const u8, tree: *ast.AST, symbol_table: *symbol.SymbolTable, target: *const common.Target) !void {
    try testMemory(allocator, file, tree, symbol_table);

    var callback = symbol.SymbolValidateWalkCallback.init(
        allocator,
        symbol_table,
        target,
    );
    defer callback.deinit();

    var walk = ast.ASTWalk.init(allocator, tree);
    defer walk.deinit();

    try walk.start(tree.entrypoint() orelse return error.NotFound);
    while (try walk.next()) |out| {
        const stat = tree.getPtr(out.index) orelse return error.NotFound;

        try switch (out.state) {
            .enter => callback.enter(stat),
            .exit => callback.exit(stat),
            else => {},
        };
    }
}
