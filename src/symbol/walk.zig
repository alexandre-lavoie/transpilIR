const std = @import("std");

const ast = @import("../ast/lib.zig");
const table = @import("table.zig");

pub const SymbolSourceWalkCallback = struct {
    symbol_table: *table.SymbolTable,
    stream: *std.io.StreamSource,
    create_symbol: bool = false,
    create_function: bool = false,
    function: ?usize = null,

    const Self = @This();

    pub fn init(symbol_table: *table.SymbolTable, stream: *std.io.StreamSource) Self {
        return .{
            .symbol_table = symbol_table,
            .stream = stream,
        };
    }

    pub fn escapeString(input: []const u8, output: []u8) ![]const u8 {
        var i: usize = 0;
        var o: usize = 0;
        while (i < input.len) {
            output[o] = switch (input[i]) {
                '\\' => scope: {
                    i += 1;

                    break :scope switch (input[i]) {
                        '0' => '\x00',
                        't' => '\t',
                        'n' => '\n',
                        'r' => '\r',
                        'x' => hex: {
                            const vi = i + 1;
                            i += 2;

                            break :hex try std.fmt.parseInt(u8, input[vi .. vi + 2], 16);
                        },
                        else => input[i],
                    };
                },
                else => input[i],
            };

            i += 1;
            o += 1;
        }

        return output[0..o];
    }

    pub fn enter(self: *Self, statement: *ast.Statement) !void {
        const allocator = self.symbol_table.symbols.allocator;

        switch (statement.data) {
            .assignment,
            .data_definition,
            .type_definition,
            .block,
            => {
                self.create_symbol = true;
            },
            .function_signature => {
                self.create_symbol = true;
                self.create_function = true;
            },
            .identifier => |identifier| {
                _ = try self.stream.seekTo(statement.span.start);

                const name = try allocator.alloc(u8, statement.span.end - statement.span.start);
                defer allocator.free(name);

                _ = try self.stream.read(name);

                const symbol_identifier: table.SymbolIdentifier = .{
                    .name = name,
                    .scope = identifier.scope,
                    .function = self.function,
                };

                if (self.create_symbol) {
                    self.create_symbol = false;

                    const index = try self.symbol_table.addSymbol(&symbol_identifier);

                    if (self.create_function) {
                        self.create_function = false;
                        self.function = index;
                    }
                }

                const instance: table.Instance = .{
                    .span = statement.span,
                };

                _ = try self.symbol_table.addSymbolInstance(&symbol_identifier, &instance);
            },
            .literal => |literal| {
                _ = try self.stream.seekTo(statement.span.start);

                const buffer = try allocator.alloc(u8, statement.span.end - statement.span.start);
                defer allocator.free(buffer);

                const output = try allocator.alloc(u8, buffer.len);
                defer allocator.free(output);

                _ = try self.stream.read(buffer);

                const value: table.LiteralValue = switch (literal.type) {
                    .integer => .{ .integer = try std.fmt.parseInt(isize, buffer, 10) },
                    .float => .{ .float = try std.fmt.parseFloat(f64, buffer) },
                    .string => .{ .string = try Self.escapeString(buffer, output) },
                };

                const index = try self.symbol_table.addLiteral(&value);

                const instance: table.Instance = .{
                    .span = statement.span,
                };

                _ = try self.symbol_table.addLiteralInstance(index, &instance);
            },
            else => {},
        }
    }

    pub fn exit(self: *Self, statement: *ast.Statement) !void {
        switch (statement.data) {
            .function => {
                self.function = null;
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

fn testSource(file: []const u8, symbol_table: *table.SymbolTable) !void {
    var file_stream: std.io.StreamSource = .{
        .const_buffer = std.io.fixedBufferStream(file),
    };

    var tree = try test_lib.testAST(test_allocator, file);
    defer tree.deinit();

    var callback = SymbolSourceWalkCallback.init(
        symbol_table,
        &file_stream,
    );

    var walk = ast.ASTWalk(@TypeOf(callback)).init(&tree, &callback);
    try walk.walk(test_allocator, tree.entrypoint() orelse return error.NotFound);
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
        },
        .{
            .identifier = .{
                .name = "t2",
                .scope = .type,
            },
        },
    };

    var symbol_table = table.SymbolTable.init(test_allocator);
    defer symbol_table.deinit();

    // Act
    try testSource(file, &symbol_table);

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
        },
        .{
            .identifier = .{
                .name = "s",
                .scope = .label,
                .function = 0,
            },
        },
    };

    var symbol_table = table.SymbolTable.init(test_allocator);
    defer symbol_table.deinit();

    // Act
    try testSource(file, &symbol_table);

    // Assert
    try std.testing.expectEqualDeep(&expected, symbol_table.symbols.items);
}

test "SymbolSourceWalk function reused symbol" {
    // Arrange
    const file = "function $test() {@s ret} function $test2() {@s ret}";
    const expected = [_]table.Symbol{
        .{
            .identifier = .{
                .name = "test",
                .scope = .global,
            },
        },
        .{
            .identifier = .{
                .name = "s",
                .scope = .label,
                .function = 0,
            },
        },
        .{
            .identifier = .{
                .name = "test2",
                .scope = .global,
            },
        },
        .{
            .identifier = .{
                .name = "s",
                .scope = .label,
                .function = 2,
            },
        },
    };

    var symbol_table = table.SymbolTable.init(test_allocator);
    defer symbol_table.deinit();

    // Act
    try testSource(file, &symbol_table);

    // Assert
    try std.testing.expectEqualDeep(&expected, symbol_table.symbols.items);
}

test "SymbolSourceWalk data" {
    // Arrange
    const file = "data $d = { b \"A \\\"B\\\" \\x43 \\n\", w -1, s s_1, d d_-1.0 }";
    const expected_symbols = [_]table.Symbol{
        .{
            .identifier = .{
                .name = "d",
                .scope = .global,
            },
        },
    };
    const expected_literals = [_]table.Literal{
        .{
            .value = .{
                .string = "A \"B\" C \n",
            },
        },
        .{
            .value = .{
                .integer = -1,
            },
        },
        .{
            .value = .{
                .float = 1,
            },
        },
        .{
            .value = .{
                .float = -1,
            },
        },
    };

    var symbol_table = table.SymbolTable.init(test_allocator);
    defer symbol_table.deinit();

    // Act
    try testSource(file, &symbol_table);

    // Assert
    try std.testing.expectEqualDeep(&expected_symbols, symbol_table.symbols.items);
    try std.testing.expectEqualDeep(&expected_literals, symbol_table.literals.items);
}

//
// Error Tests
//

test "SymbolSourceWalk error.SymbolNotFound" {
    // Arrange
    const file = "type :t = { :dne }";

    var symbol_table = table.SymbolTable.init(test_allocator);
    defer symbol_table.deinit();

    // Act
    const res = testSource(file, &symbol_table);

    // Assert
    try std.testing.expectError(error.SymbolNotFound, res);
}
