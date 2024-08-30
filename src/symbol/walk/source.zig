const std = @import("std");

const ast = @import("../../ast/lib.zig");
const common = @import("../../common.zig");
const table = @import("../table.zig");
const types = @import("../types.zig");

const SymbolSourceWalkState = enum {
    null,
    unique,
    symbol,
    function,
    parameter,
    parameter_type,
};

pub const SymbolSourceWalkCallback = struct {
    symbol_table: *table.SymbolTable,
    stream: *std.io.StreamSource,

    state: SymbolSourceWalkState = .null,
    function: ?usize = null,

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
            => {
                self.state = .symbol;
            },
            .data_definition,
            .type_definition,
            .block,
            => {
                self.state = .unique;
            },
            .function_signature => {
                self.state = .function;
            },
            .type_parameter => {
                if (self.state == .null) {
                    self.state = .symbol;
                }
            },
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

                const symbol_exists: bool = self.symbol_table.containsSymbolIdentifier(&symbol_identifier);

                const check_unique: bool = switch (self.state) {
                    .unique,
                    .function,
                    .parameter,
                    => true,
                    .null,
                    .symbol,
                    .parameter_type,
                    => false,
                };

                if (symbol_exists and check_unique) {
                    return error.SymbolReuse;
                }

                self.state = switch (self.state) {
                    .symbol, .unique => scope: {
                        if (!symbol_exists) _ = try self.symbol_table.addSymbol(&symbol_identifier);

                        break :scope .null;
                    },
                    .function => scope: {
                        self.function = try self.symbol_table.addSymbol(&symbol_identifier);

                        break :scope .parameter;
                    },
                    .parameter => scope: {
                        _ = try self.symbol_table.addSymbol(&symbol_identifier);

                        break :scope .parameter_type;
                    },
                    .null,
                    .parameter_type,
                    => self.state,
                };

                const instance: types.Instance = .{
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

                const value: types.LiteralValue = switch (literal.type) {
                    .integer => .{ .integer = try std.fmt.parseInt(isize, buffer, 10) },
                    .float => .{ .float = try std.fmt.parseFloat(f64, buffer) },
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
            .function => {
                self.function = null;
            },
            .function_signature => {
                self.state = .null;
            },
            .node => {
                self.state = switch (self.state) {
                    .parameter_type => .parameter,
                    else => self.state,
                };
            },
            else => {},
        }
    }
};

//
// Test Utils
//

const test_allocator = std.testing.allocator;
const test_lib = @import("../../test.zig");

pub fn testSource(allocator: std.mem.Allocator, file: []const u8, symbol_table: *table.SymbolTable) !void {
    var file_stream: std.io.StreamSource = .{
        .const_buffer = std.io.fixedBufferStream(file),
    };

    var tree = try test_lib.testAST(allocator, file);
    defer tree.deinit();

    var callback = SymbolSourceWalkCallback.init(
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

test "SymbolSourceWalk type" {
    // Arrange
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

    var symbol_table = table.SymbolTable.init(test_allocator);
    defer symbol_table.deinit();

    // Act
    try testSource(test_allocator, file, &symbol_table);

    // Assert
    try std.testing.expectEqualDeep(&expected, symbol_table.symbols.items);
}

test "SymbolSourceWalk function" {
    // Arrange
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

    var symbol_table = table.SymbolTable.init(test_allocator);
    defer symbol_table.deinit();

    // Act
    try testSource(test_allocator, file, &symbol_table);

    // Assert
    try std.testing.expectEqualDeep(&expected, symbol_table.symbols.items);
}

test "SymbolSourceWalk reused local" {
    // Arrange
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

    var symbol_table = table.SymbolTable.init(test_allocator);
    defer symbol_table.deinit();

    // Act
    try testSource(test_allocator, file, &symbol_table);

    // Assert
    try std.testing.expectEqualDeep(&expected, symbol_table.symbols.items);
}

test "SymbolSourceWalk reassigned local" {
    // Arrange
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

    var symbol_table = table.SymbolTable.init(test_allocator);
    defer symbol_table.deinit();

    // Act
    try testSource(test_allocator, file, &symbol_table);

    // Assert
    try std.testing.expectEqualDeep(&expected, symbol_table.symbols.items);
}

test "SymbolSourceWalk data" {
    // Arrange
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
    try testSource(test_allocator, file, &symbol_table);

    // Assert
    try std.testing.expectEqualDeep(&expected_symbols, symbol_table.symbols.items);
    try std.testing.expectEqualDeep(&expected_literals, symbol_table.literals.items);
}

//
// Error Tests
//

test "SymbolSourceWalk type error.SymbolNotFound" {
    // Arrange
    const file = "type :t = { :dne }";

    var symbol_table = table.SymbolTable.init(test_allocator);
    defer symbol_table.deinit();

    // Act
    const res = testSource(test_allocator, file, &symbol_table);

    // Assert
    try std.testing.expectError(error.SymbolNotFound, res);
}

test "SymbolSourceWalk local type error.SymbolNotFound" {
    // Arrange
    const file = "function $fun(:dne %a) {@s ret}";

    var symbol_table = table.SymbolTable.init(test_allocator);
    defer symbol_table.deinit();

    // Act
    const res = testSource(test_allocator, file, &symbol_table);

    // Assert
    try std.testing.expectError(error.SymbolNotFound, res);
}

test "SymbolSourceWalk global error.SymbolNotFound" {
    // Arrange
    const file = "function $fun() {@s %t =w add $dne, 0 ret}";

    var symbol_table = table.SymbolTable.init(test_allocator);
    defer symbol_table.deinit();

    // Act
    const res = testSource(test_allocator, file, &symbol_table);

    // Assert
    try std.testing.expectError(error.SymbolNotFound, res);
}

test "SymbolSourceWalk label error.SymbolNotFound" {
    // Arrange
    const file = "function $fun() {@s jmp @dne}";

    var symbol_table = table.SymbolTable.init(test_allocator);
    defer symbol_table.deinit();

    // Act
    const res = testSource(test_allocator, file, &symbol_table);

    // Assert
    try std.testing.expectError(error.SymbolNotFound, res);
}

test "SymbolSourceWalk type error.SymbolReuse" {
    // Arrange
    const file = "type :t = { w } type :t = { b }";

    var symbol_table = table.SymbolTable.init(test_allocator);
    defer symbol_table.deinit();

    // Act
    const res = testSource(test_allocator, file, &symbol_table);

    // Assert
    try std.testing.expectError(error.SymbolReuse, res);
}

test "SymbolSourceWalk global error.SymbolReuse" {
    // Arrange
    const file = "data $d = { w 0 } function $d() {@s ret}";

    var symbol_table = table.SymbolTable.init(test_allocator);
    defer symbol_table.deinit();

    // Act
    const res = testSource(test_allocator, file, &symbol_table);

    // Assert
    try std.testing.expectError(error.SymbolReuse, res);
}
