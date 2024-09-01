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
    types: TypeList,

    const Self = @This();
    const TypeList = std.ArrayList(ast.PrimitiveType);

    pub fn init(allocator: std.mem.Allocator, symbol_table: *const table.SymbolTable) Self {
        return .{
            .allocator = allocator,
            .symbol_table = symbol_table,
            .types = TypeList.init(allocator),
        };
    }

    pub fn deinit(self: *Self) void {
        self.types.deinit();
    }

    fn matchType(left: ast.PrimitiveType, right: ast.PrimitiveType) !ast.PrimitiveType {
        if (left == right) return left;

        if (left == .void) {
            switch (right) {
                .single, .double => return error.MismatchTypeError,
                else => return right,
            }
        } else if (right == .void) {
            switch (left) {
                .single, .double => return error.MismatchTypeError,
                else => return left,
            }
        }

        return error.MismatchTypeError;
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
                    .primitive => |primitive| try self.types.append(primitive),
                    .type => try self.types.append(.long),
                    else => {},
                }
            },
            .literal => |literal| {
                try self.types.append(switch (literal.type) {
                    .string => .long,
                    .integer => .void,
                    .single => .single,
                    .double => .double,
                });
            },
            .primitive_type => |primitive| try self.types.append(primitive),
            else => {},
        }
    }

    pub fn exit(self: *Self, statement: *ast.Statement) !void {
        switch (statement.data) {
            .block,
            .data_definition,
            .function_signature,
            .line,
            .type_definition,
            => self.types.clearAndFree(),
            .binary_operation => {
                const right = self.types.pop();
                const left = self.types.pop();
                const data_type = self.types.pop();

                if (try Self.matchType(left, right) != data_type) return error.DataTypeError;

                try self.types.append(data_type);
            },
            .comparison => {
                const right = self.types.pop();
                const left = self.types.pop();
                const comparision_type = self.types.pop();
                const data_type = self.types.pop();

                switch (data_type) {
                    .single, .double => return error.ComparisonTypeError,
                    else => {},
                }

                if (try Self.matchType(left, right) != comparision_type) return error.DataTypeError;

                try self.types.append(data_type);
            },
            else => {},
        }
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

test "error.MismatchTypeError literal" {
    // Arrange
    const allocator = std.testing.allocator;

    const file = "function $f() {@s %r =w add 1, s_1 ret}";

    var symbol_table = table.SymbolTable.init(allocator);
    defer symbol_table.deinit();

    // Act
    try source.testSource(allocator, file, &symbol_table);
    try memory.testMemory(allocator, file, &symbol_table);
    const res = testValidate(allocator, file, &symbol_table);

    // Assert
    try std.testing.expectError(error.MismatchTypeError, res);
}

test "error.MismatchTypeError identifier" {
    // Arrange
    const allocator = std.testing.allocator;

    const file = "function $f() {@s %i =w copy 0 %f =s copy s_0 %r =w add %i, %f ret}";

    var symbol_table = table.SymbolTable.init(allocator);
    defer symbol_table.deinit();

    // Act
    try source.testSource(allocator, file, &symbol_table);
    try memory.testMemory(allocator, file, &symbol_table);
    const res = testValidate(allocator, file, &symbol_table);

    // Assert
    try std.testing.expectError(error.MismatchTypeError, res);
}

test "error.DataTypeError binary_operator" {
    // Arrange
    const allocator = std.testing.allocator;

    const file = "function $f() {@s %r =s add 1, 2 ret}";

    var symbol_table = table.SymbolTable.init(allocator);
    defer symbol_table.deinit();

    // Act
    try source.testSource(allocator, file, &symbol_table);
    try memory.testMemory(allocator, file, &symbol_table);
    const res = testValidate(allocator, file, &symbol_table);

    // Assert
    try std.testing.expectError(error.DataTypeError, res);
}

test "error.MismatchTypeError binary_operator" {
    // Arrange
    const allocator = std.testing.allocator;

    const file = "function $f() {@s %r =w add 0, s_0 ret}";

    var symbol_table = table.SymbolTable.init(allocator);
    defer symbol_table.deinit();

    // Act
    try source.testSource(allocator, file, &symbol_table);
    try memory.testMemory(allocator, file, &symbol_table);
    const res = testValidate(allocator, file, &symbol_table);

    // Assert
    try std.testing.expectError(error.MismatchTypeError, res);
}

test "error.ComparisonTypeError" {
    // Arrange
    const allocator = std.testing.allocator;

    const file = "function $f() {@s %r =s ceqw 0, 0 ret}";

    var symbol_table = table.SymbolTable.init(allocator);
    defer symbol_table.deinit();

    // Act
    try source.testSource(allocator, file, &symbol_table);
    try memory.testMemory(allocator, file, &symbol_table);
    const res = testValidate(allocator, file, &symbol_table);

    // Assert
    try std.testing.expectError(error.ComparisonTypeError, res);
}

test "error.DataTypeError integer comparison" {
    // Arrange
    const allocator = std.testing.allocator;

    const file = "function $f() {@s %r =w ceqw s_0, s_0 ret}";

    var symbol_table = table.SymbolTable.init(allocator);
    defer symbol_table.deinit();

    // Act
    try source.testSource(allocator, file, &symbol_table);
    try memory.testMemory(allocator, file, &symbol_table);
    const res = testValidate(allocator, file, &symbol_table);

    // Assert
    try std.testing.expectError(error.DataTypeError, res);
}

test "error.DataTypeError float comparison" {
    // Arrange
    const allocator = std.testing.allocator;

    const file = "function $f() {@s %r =w cos 0, 0 ret}";

    var symbol_table = table.SymbolTable.init(allocator);
    defer symbol_table.deinit();

    // Act
    try source.testSource(allocator, file, &symbol_table);
    try memory.testMemory(allocator, file, &symbol_table);
    const res = testValidate(allocator, file, &symbol_table);

    // Assert
    try std.testing.expectError(error.DataTypeError, res);
}
