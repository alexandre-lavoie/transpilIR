const std = @import("std");

const ast = @import("../ast/lib.zig");
const common = @import("../common.zig");
const table = @import("table.zig");
const types = @import("types.zig");

const test_lib = @import("test.zig");

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
    unique_set: UniqueSet,
    function: ?usize = null,

    const Self = @This();
    const UniqueSet = std.AutoArrayHashMap(usize, void);

    pub fn init(allocator: std.mem.Allocator, symbol_table: *table.SymbolTable, stream: *std.io.StreamSource) Self {
        return .{
            .allocator = allocator,
            .symbol_table = symbol_table,
            .stream = stream,
            .unique_set = UniqueSet.init(allocator),
        };
    }

    pub fn deinit(self: *Self) void {
        self.unique_set.deinit();
    }

    pub fn enter(self: *Self, statement: ast.Statement) !void {
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
                        .unique => {
                            if (self.unique_set.contains(try symbol_identifier.key(self.allocator))) {
                                return error.SymbolReuse;
                            }
                        },
                        .default, .function, .call => {},
                    }
                } else if (identifier.scope != .type or self.state != .default) {
                    _ = try self.symbol_table.addSymbol(&symbol_identifier);
                }

                if (self.state == .unique) {
                    try self.unique_set.put(
                        try symbol_identifier.key(self.allocator),
                        undefined,
                    );
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
                    .integer => b: {
                        const is_negative = switch (buffer.len > 0) {
                            true => buffer[0] == '-',
                            false => false,
                        };

                        const value: usize = switch (is_negative) {
                            true => @bitCast(try std.fmt.parseInt(isize, buffer, 10)),
                            false => try std.fmt.parseInt(usize, buffer, 10),
                        };

                        break :b .{ .integer = value };
                    },
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

    pub fn exit(self: *Self, statement: ast.Statement) !void {
        switch (statement.data) {
            .function => self.function = null,
            else => {},
        }
    }
};

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

    var tree = try test_lib.testAST(allocator, file);
    defer tree.deinit();

    var symbol_table = table.SymbolTable.init(allocator);
    defer symbol_table.deinit();

    // Act
    try test_lib.testSource(allocator, file, &tree, &symbol_table);

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

    var tree = try test_lib.testAST(allocator, file);
    defer tree.deinit();

    var symbol_table = table.SymbolTable.init(allocator);
    defer symbol_table.deinit();

    // Act
    try test_lib.testSource(allocator, file, &tree, &symbol_table);

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

    var tree = try test_lib.testAST(allocator, file);
    defer tree.deinit();

    var symbol_table = table.SymbolTable.init(allocator);
    defer symbol_table.deinit();

    // Act
    try test_lib.testSource(allocator, file, &tree, &symbol_table);

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

    var tree = try test_lib.testAST(allocator, file);
    defer tree.deinit();

    var symbol_table = table.SymbolTable.init(allocator);
    defer symbol_table.deinit();

    // Act
    try test_lib.testSource(allocator, file, &tree, &symbol_table);

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

    var tree = try test_lib.testAST(allocator, file);
    defer tree.deinit();

    var symbol_table = table.SymbolTable.init(allocator);
    defer symbol_table.deinit();

    // Act
    try test_lib.testSource(allocator, file, &tree, &symbol_table);

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

    var tree = try test_lib.testAST(allocator, file);
    defer tree.deinit();

    var symbol_table = table.SymbolTable.init(allocator);
    defer symbol_table.deinit();

    // Act
    try test_lib.testSource(allocator, file, &tree, &symbol_table);

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
                .integer = @bitCast(@as(isize, -1)),
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

    var tree = try test_lib.testAST(allocator, file);
    defer tree.deinit();

    var symbol_table = table.SymbolTable.init(allocator);
    defer symbol_table.deinit();

    // Act
    try test_lib.testSource(allocator, file, &tree, &symbol_table);

    // Assert
    try std.testing.expectEqualDeep(&expected_symbols, symbol_table.symbols.items);
    try std.testing.expectEqualDeep(&expected_literals, symbol_table.literals.items);
}

test "data use before declaration" {
    // Arrange
    const allocator = std.testing.allocator;

    const file = "data $a = { l $b } data $b = { b 0 }";
    const expected = [_]types.Symbol{
        .{
            .identifier = .{
                .name = "a",
                .scope = .global,
            },
        },
        .{
            .identifier = .{
                .name = "b",
                .scope = .global,
            },
        },
    };

    var tree = try test_lib.testAST(allocator, file);
    defer tree.deinit();

    var symbol_table = table.SymbolTable.init(allocator);
    defer symbol_table.deinit();

    // Act
    try test_lib.testSource(allocator, file, &tree, &symbol_table);

    // Assert
    try std.testing.expectEqualDeep(&expected, symbol_table.symbols.items);
}

//
// Error Tests
//

test "error.SymbolNotFound type" {
    // Arrange
    const allocator = std.testing.allocator;

    const file = "type :t = { :dne }";

    var tree = try test_lib.testAST(allocator, file);
    defer tree.deinit();

    var symbol_table = table.SymbolTable.init(allocator);
    defer symbol_table.deinit();

    // Act
    const res = test_lib.testSource(allocator, file, &tree, &symbol_table);

    // Assert
    try std.testing.expectError(error.SymbolNotFound, res);
}

test "error.SymbolNotFound local type" {
    // Arrange
    const allocator = std.testing.allocator;

    const file = "function $fun(:dne %a) {@s ret}";

    var tree = try test_lib.testAST(allocator, file);
    defer tree.deinit();

    var symbol_table = table.SymbolTable.init(allocator);
    defer symbol_table.deinit();

    // Act
    const res = test_lib.testSource(allocator, file, &tree, &symbol_table);

    // Assert
    try std.testing.expectError(error.SymbolNotFound, res);
}

test "error.SymbolReuse type" {
    // Arrange
    const allocator = std.testing.allocator;

    const file = "type :t = { w } type :t = { b }";

    var tree = try test_lib.testAST(allocator, file);
    defer tree.deinit();

    var symbol_table = table.SymbolTable.init(allocator);
    defer symbol_table.deinit();

    // Act
    const res = test_lib.testSource(allocator, file, &tree, &symbol_table);

    // Assert
    try std.testing.expectError(error.SymbolReuse, res);
}
