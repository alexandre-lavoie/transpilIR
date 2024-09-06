const std = @import("std");

const ast = @import("../ast/lib.zig");
const common = @import("../common.zig");
const symbol = @import("lib.zig");

const test_lib = @import("../test/lib.zig");

pub const testAST = test_lib.testAST;

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
        try switch (out.enter) {
            true => callback.enter(out.value),
            false => callback.exit(out.value),
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
        try switch (out.enter) {
            true => callback.enter(out.value),
            false => callback.exit(out.value),
        };
    }
}

pub fn testValidate(allocator: std.mem.Allocator, file: []const u8, tree: *ast.AST, symbol_table: *symbol.SymbolTable, target: *const common.Target) !void {
    try testMemory(allocator, file, tree, symbol_table);

    var callback = symbol.SymbolValidateWalkCallback.init(
        allocator,
        target,
        symbol_table,
    );
    defer callback.deinit();

    var walk = ast.ASTWalk.init(allocator, tree);
    defer walk.deinit();

    try walk.start(tree.entrypoint() orelse return error.NotFound);
    while (try walk.next()) |out| {
        try switch (out.enter) {
            true => callback.enter(out.value),
            false => callback.exit(out.value),
        };
    }
}
