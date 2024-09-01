const std = @import("std");

const ast = @import("../../ast/lib.zig");
const common = @import("../../common.zig");
const table = @import("../table.zig");
const types = @import("../types.zig");

const SymbolSourceWalkState = enum {
    default,
    unique,
    function,
    call,
};

pub const SymbolSourceWalkCallback = struct {
    allocator: std.mem.Allocator,
    symbol_table: *table.SymbolTable,
    stream: *std.io.StreamSource,

    state: SymbolSourceWalkState = .default,
    function: ?usize = null,

    const Self = @This();

    pub fn init(allocator: std.mem.Allocator, symbol_table: *table.SymbolTable, stream: *std.io.StreamSource) Self {
        return .{
            .allocator = allocator,
            .symbol_table = symbol_table,
            .stream = stream,
        };
    }

    pub fn enter(self: *Self, statement: *ast.Statement) !void {
        const allocator = self.allocator;

        switch (statement.data) {
            .data_definition,
            .type_definition,
            => self.state = .unique,
            .function_signature => self.state = .function,
            .call => self.state = .call,
            .identifier => |identifier| {
                _ = try self.stream.seekTo(statement.span.start);

                const name = try allocator.alloc(u8, statement.span.end - statement.span.start);
                defer allocator.free(name);

                _ = try self.stream.read(name);

                const symbol_identifier: types.SymbolIdentifier = .{
                    .name = name,
                    .scope = identifier.scope,
                    .function = switch (identifier.scope) {
                        .local,
                        .label,
                        => self.function,
                        else => null,
                    },
                };

                if (self.symbol_table.containsSymbolIdentifier(&symbol_identifier)) {
                    switch (self.state) {
                        .unique => return error.SymbolReuse,
                        .default, .function, .call => {},
                    }
                } else if (identifier.scope != .type or self.state != .default) {
                    _ = try self.symbol_table.addSymbol(&symbol_identifier);
                }

                const instance: types.Instance = .{
                    .span = statement.span,
                };

                _ = try self.symbol_table.addSymbolInstance(&symbol_identifier, &instance);

                if (self.state == .function) {
                    self.function = self.symbol_table.getSymbolIndexByInstance(&instance);
                }

                self.state = .default;
            },
            .literal => |literal| {
                _ = try self.stream.seekTo(statement.span.start);

                const buffer = try allocator.alloc(u8, statement.span.end - statement.span.start);
                defer allocator.free(buffer);

                const output = try allocator.alloc(u8, buffer.len);
                defer allocator.free(output);

                _ = try self.stream.read(buffer);

                const value: types.LiteralValue = switch (literal.type) {
                    .integer => .{ .integer = try std.fmt.parseInt(isize, buffer, 10) },
                    .single => .{ .single = try std.fmt.parseFloat(f32, buffer) },
                    .double => .{ .double = try std.fmt.parseFloat(f64, buffer) },
                    .string => .{ .string = try common.parseString(buffer, output) },
                };

                const index = try self.symbol_table.addLiteral(&value);

                const instance: types.Instance = .{
                    .span = statement.span,
                };

                _ = try self.symbol_table.addLiteralInstance(index, &instance);
            },
            else => {},
        }
    }

    pub fn exit(self: *Self, statement: *ast.Statement) !void {
        switch (statement.data) {
            .function => self.function = null,
            else => {},
        }
    }
};

//
// Test Utils
//

const test_lib = @import("../../test.zig");

pub fn testSource(allocator: std.mem.Allocator, file: []const u8, symbol_table: *table.SymbolTable) !void {
    var file_stream: std.io.StreamSource = .{
        .const_buffer = std.io.fixedBufferStream(file),
    };

    var tree = try test_lib.testAST(allocator, file);
    defer tree.deinit();

    var callback = SymbolSourceWalkCallback.init(
        allocator,
        symbol_table,
        &file_stream,
    );

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
// Valid Tests
//

test "type" {
    // Arrange
    const allocator = std.testing.allocator;

    const file = "type :t = { w } type :t2 = { :t } ";
    const expected = [_]types.Symbol{
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

    var symbol_table = table.SymbolTable.init(allocator);
    defer symbol_table.deinit();

    // Act
    try testSource(allocator, file, &symbol_table);

    // Assert
    try std.testing.expectEqualDeep(&expected, symbol_table.symbols.items);
}

test "function" {
    // Arrange
    const allocator = std.testing.allocator;

    const file = "function $test() {@s ret}";
    const expected = [_]types.Symbol{
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

    var symbol_table = table.SymbolTable.init(allocator);
    defer symbol_table.deinit();

    // Act
    try testSource(allocator, file, &symbol_table);

    // Assert
    try std.testing.expectEqualDeep(&expected, symbol_table.symbols.items);
}

test "function type" {
    // Arrange
    const allocator = std.testing.allocator;

    const file = "type :t = { w } function :t $test() {@s ret}";
    const expected = [_]types.Symbol{
        .{
            .identifier = .{
                .name = "t",
                .scope = .type,
            },
        },
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
                .function = 1,
            },
        },
    };

    var symbol_table = table.SymbolTable.init(allocator);
    defer symbol_table.deinit();

    // Act
    try testSource(allocator, file, &symbol_table);

    // Assert
    try std.testing.expectEqualDeep(&expected, symbol_table.symbols.items);
}

test "reused local" {
    // Arrange
    const allocator = std.testing.allocator;

    const file = "function $test() {@s ret} function $test2() {@s ret}";
    const expected = [_]types.Symbol{
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

    var symbol_table = table.SymbolTable.init(allocator);
    defer symbol_table.deinit();

    // Act
    try testSource(allocator, file, &symbol_table);

    // Assert
    try std.testing.expectEqualDeep(&expected, symbol_table.symbols.items);
}

test "reassigned local" {
    // Arrange
    const allocator = std.testing.allocator;

    const file = "function $fun() {@s %p =w add 0, 0 %p =w add 0, 0 ret}";
    const expected = [_]types.Symbol{
        .{
            .identifier = .{
                .name = "fun",
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
                .name = "p",
                .scope = .local,
                .function = 0,
            },
        },
    };

    var symbol_table = table.SymbolTable.init(allocator);
    defer symbol_table.deinit();

    // Act
    try testSource(allocator, file, &symbol_table);

    // Assert
    try std.testing.expectEqualDeep(&expected, symbol_table.symbols.items);
}

test "call" {
    // Arrange
    const allocator = std.testing.allocator;

    const file = "type :t = { w } function $fun() {@s %v =:t call $other(w 0) %v =l call $fun() ret}";
    const expected = [_]types.Symbol{
        .{
            .identifier = .{
                .name = "t",
                .scope = .type,
            },
        },
        .{
            .identifier = .{
                .name = "fun",
                .scope = .global,
            },
        },
        .{
            .identifier = .{
                .name = "s",
                .scope = .label,
                .function = 1,
            },
        },
        .{
            .identifier = .{
                .name = "v",
                .scope = .local,
                .function = 1,
            },
        },
        .{
            .identifier = .{
                .name = "other",
                .scope = .global,
            },
        },
    };

    var symbol_table = table.SymbolTable.init(allocator);
    defer symbol_table.deinit();

    // Act
    try testSource(allocator, file, &symbol_table);

    // Assert
    try std.testing.expectEqualDeep(&expected, symbol_table.symbols.items);
}

test "data" {
    // Arrange
    const allocator = std.testing.allocator;

    const file = "data $d = { b \"A \\\"B\\\" \\x43 \\n\", w -1, s s_1, d d_-1.0 }";
    const expected_symbols = [_]types.Symbol{
        .{
            .identifier = .{
                .name = "d",
                .scope = .global,
            },
        },
    };
    const expected_literals = [_]types.Literal{
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
                .single = 1,
            },
        },
        .{
            .value = .{
                .double = -1,
            },
        },
    };

    var symbol_table = table.SymbolTable.init(allocator);
    defer symbol_table.deinit();

    // Act
    try testSource(allocator, file, &symbol_table);

    // Assert
    try std.testing.expectEqualDeep(&expected_symbols, symbol_table.symbols.items);
    try std.testing.expectEqualDeep(&expected_literals, symbol_table.literals.items);
}

//
// Error Tests
//

test "error.SymbolNotFound type" {
    // Arrange
    const allocator = std.testing.allocator;

    const file = "type :t = { :dne }";

    var symbol_table = table.SymbolTable.init(allocator);
    defer symbol_table.deinit();

    // Act
    const res = testSource(allocator, file, &symbol_table);

    // Assert
    try std.testing.expectError(error.SymbolNotFound, res);
}

test "error.SymbolNotFound local type" {
    // Arrange
    const allocator = std.testing.allocator;

    const file = "function $fun(:dne %a) {@s ret}";

    var symbol_table = table.SymbolTable.init(allocator);
    defer symbol_table.deinit();

    // Act
    const res = testSource(allocator, file, &symbol_table);

    // Assert
    try std.testing.expectError(error.SymbolNotFound, res);
}

test "error.SymbolReuse type" {
    // Arrange
    const allocator = std.testing.allocator;

    const file = "type :t = { w } type :t = { b }";

    var symbol_table = table.SymbolTable.init(allocator);
    defer symbol_table.deinit();

    // Act
    const res = testSource(allocator, file, &symbol_table);

    // Assert
    try std.testing.expectError(error.SymbolReuse, res);
}
