const std = @import("std");

const ast = @import("../ast/lib.zig");
const table = @import("table.zig");

pub const SymbolSourceWalkCallback = struct {
    symbol_table: *table.SymbolTable,
    stream: *std.io.StreamSource,
    create_symbol: bool = false,

    const Self = @This();

    pub fn init(symbol_table: *table.SymbolTable, stream: *std.io.StreamSource) Self {
        return .{
            .symbol_table = symbol_table,
            .stream = stream,
        };
    }

    pub fn enter(self: *Self, statement: *ast.Statement) !void {
        const allocator = self.symbol_table.symbols.allocator;

        switch (statement.data) {
            .assignment,
            .data_definition,
            .type_definition,
            .function_signature,
            .block,
            => {
                self.create_symbol = true;
            },
            .identifier => |identifier| {
                _ = try self.stream.seekTo(statement.span.start);

                const name = try allocator.alloc(u8, statement.span.end - statement.span.start);
                defer allocator.free(name);

                _ = try self.stream.read(name);

                const symbol_identifier: table.SymbolIdentifier = .{
                    .name = name,
                    .scope = identifier.scope,
                };

                if (self.create_symbol) {
                    self.create_symbol = false;

                    _ = try self.symbol_table.addSymbol(&symbol_identifier, statement.span.start);
                }

                _ = try self.symbol_table.addSymbolInstance(&symbol_identifier, &statement.span);
            },
            else => {},
        }
    }
};

//
// Test Utils
//

const test_allocator = std.testing.allocator;
const test_lib = @import("../test.zig");

fn testSource(file: []const u8) !table.SymbolTable {
    var file_stream: std.io.StreamSource = .{
        .const_buffer = std.io.fixedBufferStream(file),
    };

    var symbol_table = table.SymbolTable.init(test_allocator);
    errdefer symbol_table.deinit();

    var tree = try test_lib.testAST(test_allocator, file);
    defer tree.deinit();

    var callback = SymbolSourceWalkCallback.init(
        &symbol_table,
        &file_stream,
    );

    var walk = ast.ASTWalk(@TypeOf(callback)).init(&tree, &callback);
    try walk.walk(test_allocator, tree.entrypoint() orelse return error.NotFound);

    return symbol_table;
}

//
// Valid Tests
//

test "SymbolSourceWalk type" {
    // Arrange
    const file = "type :t = { w } type :t2 = { :t } ";
    const expected = [_]table.Symbol{
        .{
            .identifier = .{
                .name = "t",
                .scope = .type,
            },
            .position = 6,
        },
        .{
            .identifier = .{
                .name = "t2",
                .scope = .type,
            },
            .position = 22,
        },
    };

    // Act
    var symbol_table = try testSource(file);
    defer symbol_table.deinit();

    // Assert
    try std.testing.expectEqualDeep(&expected, symbol_table.symbols.items);
}

test "SymbolSourceWalk function" {
    // Arrange
    const file = "function $test() {@s ret}";
    const expected = [_]table.Symbol{
        .{
            .identifier = .{
                .name = "test",
                .scope = .global,
            },
            .position = 10,
        },
        .{
            .identifier = .{
                .name = "s",
                .scope = .label,
            },
            .position = 19,
        },
    };

    // Act
    var symbol_table = try testSource(file);
    defer symbol_table.deinit();

    // Assert
    try std.testing.expectEqualDeep(&expected, symbol_table.symbols.items);
}

//
// Error Tests
//

test "SymbolSourceWalk error.SymbolNotFound" {
    // Arrange
    const file = "type :t = { :dne }";

    // Act
    const res = testSource(file);

    // Assert
    try std.testing.expectError(error.SymbolNotFound, res);
}
