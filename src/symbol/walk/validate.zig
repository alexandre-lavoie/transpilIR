const std = @import("std");

const ast = @import("../../ast/lib.zig");
const common = @import("../../common.zig");
const table = @import("../table.zig");
const types = @import("../types.zig");

const source = @import("source.zig");
const memory = @import("memory.zig");

pub const SymbolValidateWalkCallback = struct {
    allocator: std.mem.Allocator,
    symbol_table: *const table.SymbolTable,

    const Self = @This();

    pub fn init(allocator: std.mem.Allocator, symbol_table: *const table.SymbolTable) Self {
        return .{
            .allocator = allocator,
            .symbol_table = symbol_table,
        };
    }

    pub fn deinit(self: *Self) void {
        _ = self;
    }

    pub fn enter(self: *Self, statement: *ast.Statement) !void {
        const instance: types.Instance = .{ .span = statement.span };

        switch (statement.data) {
            .identifier => |identifier| {
                const symbol = self.symbol_table.getSymbolByInstance(&instance) orelse unreachable;

                switch (symbol.memory) {
                    .empty => {
                        switch (identifier.scope) {
                            .label, .local, .type => return error.SymbolNotFound,
                            .global => return {},
                        }
                    },
                    else => {},
                }
            },
            else => {},
        }
    }

    pub fn exit(self: *Self, statement: *ast.Statement) !void {
        _ = self;
        _ = statement;
    }
};

//
// Test Utils
//

const test_lib = @import("../../test.zig");

pub fn testValidate(allocator: std.mem.Allocator, file: []const u8, symbol_table: *const table.SymbolTable) !void {
    var tree = try test_lib.testAST(allocator, file);
    defer tree.deinit();

    var callback = SymbolValidateWalkCallback.init(
        allocator,
        symbol_table,
    );
    defer callback.deinit();

    var walk = ast.ASTWalk.init(allocator, &tree);
    defer walk.deinit();

    try walk.start(tree.entrypoint() orelse return error.NotFound);
    while (try walk.next()) |out| {
        try switch (out.enter) {
            true => callback.enter(out.value),
            false => callback.exit(out.value),
        };
    }
}

//
// Error Tests
//

test "error.SymbolNotFound label" {
    // Arrange
    const allocator = std.testing.allocator;

    const file = "function $f() {@s jmp @dne}";

    var symbol_table = table.SymbolTable.init(allocator);
    defer symbol_table.deinit();

    // Act
    try source.testSource(allocator, file, &symbol_table);
    try memory.testMemory(allocator, file, &symbol_table);
    const res = testValidate(allocator, file, &symbol_table);

    // Assert
    try std.testing.expectError(error.SymbolNotFound, res);
}

test "error.SymbolNotFound local" {
    // Arrange
    const allocator = std.testing.allocator;

    const file = "function $f() {@s %l =w add %dne, 1 ret}";

    var symbol_table = table.SymbolTable.init(allocator);
    defer symbol_table.deinit();

    // Act
    try source.testSource(allocator, file, &symbol_table);
    try memory.testMemory(allocator, file, &symbol_table);
    const res = testValidate(allocator, file, &symbol_table);

    // Assert
    try std.testing.expectError(error.SymbolNotFound, res);
}
