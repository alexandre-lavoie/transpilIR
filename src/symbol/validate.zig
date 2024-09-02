const std = @import("std");

const ast = @import("../ast/lib.zig");
const common = @import("../common.zig");
const test_lib = @import("../test.zig");
const table = @import("table.zig");
const types = @import("types.zig");
const source = @import("source.zig");
const memory = @import("memory.zig");

pub const SymbolValidateWalkCallback = struct {
    allocator: std.mem.Allocator,
    symbol_table: *const table.SymbolTable,
    types: TypeList,
    return_type: ?ast.PrimitiveType = null,
    phi_type: ?ast.PrimitiveType = null,

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
            return right;
        } else if (right == .void) {
            return left;
        }

        return error.MismatchType;
    }

    fn castType(primitive: ast.PrimitiveType) ast.PrimitiveType {
        return switch (primitive) {
            .void => .void,
            .byte_unsigned,
            .byte,
            => .byte,
            .double => .double,
            .half_word_unsigned,
            .half_word,
            => .half_word,
            .long_unsigned,
            .long,
            => .long,
            .single => .single,
            .word_unsigned,
            .word,
            => .word,
        };
    }

    fn validateType(target: ast.PrimitiveType, from: ast.PrimitiveType) bool {
        return switch (target) {
            .void => true,
            .byte_unsigned,
            .byte,
            => switch (from) {
                .single, .double => false,
                else => true,
            },
            .double => switch (from) {
                .void, .double => true,
                else => false,
            },
            .half_word_unsigned,
            .half_word,
            => switch (from) {
                .single, .double, .byte, .byte_unsigned => false,
                else => true,
            },
            .long_unsigned,
            .long,
            => switch (from) {
                .void, .long, .long_unsigned => true,
                else => false,
            },
            .single => switch (from) {
                .void, .single => true,
                else => false,
            },
            .word_unsigned,
            .word,
            => switch (from) {
                .void, .word, .word_unsigned, .long, .long_unsigned => true,
                else => false,
            },
        };
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
                            .global => try self.types.append(.long),
                        }
                    },
                    .primitive => |primitive| try self.types.append(Self.castType(primitive)),
                    .type => try self.types.append(.long),
                    .data => try self.types.append(.long),
                    .function => |function| {
                        if (self.return_type == null) {
                            self.return_type = switch (function.@"return") {
                                .type => .long,
                                .primitive => |primitive| primitive,
                            };
                        }

                        try self.types.append(.long);
                    },
                    else => {},
                }
            },
            .literal => |literal| {
                try self.types.append(switch (literal.type) {
                    .string => .long,
                    .integer,
                    .single,
                    .double,
                    => .void,
                });
            },
            .primitive_type => |primitive| try self.types.append(Self.castType(primitive)),
            .phi => self.phi_type = self.types.pop(),
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
            .function => self.return_type = null,
            .binary_operation => {
                const right = self.types.pop();
                const left = self.types.pop();
                const data_type = self.types.pop();

                if (!Self.validateType(data_type, try Self.matchType(left, right))) return error.DataType;
            },
            .comparison => {
                const right = self.types.pop();
                const left = self.types.pop();
                const comparision_type = self.types.pop();
                const data_type = self.types.pop();

                switch (data_type) {
                    .single, .double => return error.ComparisonType,
                    else => {},
                }

                if (!Self.validateType(comparision_type, try Self.matchType(left, right))) return error.DataType;
            },
            .negate => {
                const value = self.types.pop();
                const data_type = self.types.pop();

                if (!Self.validateType(data_type, value)) return error.DataType;
            },
            .@"return" => {
                const @"type": ast.PrimitiveType = switch (self.types.items.len) {
                    0 => .void,
                    else => self.types.pop(),
                };

                if (!Self.validateType(self.return_type orelse .void, @"type")) return error.DataType;
            },
            .vastart => {
                const value = self.types.pop();

                if (!Self.validateType(.long, value)) return error.DataType;
            },
            .vaarg => {
                const value = self.types.pop();
                _ = self.types.pop();

                if (!Self.validateType(.long, value)) return error.DataType;
            },
            .phi_parameter => {
                const @"type" = self.types.pop();

                if (!Self.validateType(self.phi_type orelse .void, @"type")) return error.DataType;
            },
            .branch => {
                const condition = self.types.pop();

                if (!Self.validateType(.byte, condition)) return error.DataType;
            },
            .blit => {
                const size = self.types.pop();
                const to = self.types.pop();
                const from = self.types.pop();

                if (size != .void) return error.DataType;
                if (!Self.validateType(.long, to)) return error.DataType;
                if (!Self.validateType(.long, from)) return error.DataType;
            },
            .convert => {
                const value = self.types.pop();
                const from_type = self.types.pop();
                const to_type = self.types.pop();
                const data_type = self.types.pop();

                if (!Self.validateType(from_type, value)) return error.DataType;
                if (!Self.validateType(data_type, to_type)) return error.DataType;
            },
            .copy => {
                const value = self.types.pop();
                const data_type = self.types.pop();

                if (!Self.validateType(data_type, value)) return error.DataType;
            },
            .cast => {
                const value = self.types.pop();
                const data_type = self.types.pop();

                const from_type: ast.PrimitiveType = switch (data_type) {
                    .long => .double,
                    .single => .word,
                    .double => .long,
                    else => .single,
                };

                if (!Self.validateType(from_type, value)) return error.DataType;
            },
            .load => {
                const address = self.types.pop();
                const memory_type = self.types.pop();
                const data_type = self.types.pop();

                const source_type = switch (data_type) {
                    .word => switch (memory_type) {
                        .byte, .half_word => .word,
                        else => memory_type,
                    },
                    .long => switch (memory_type) {
                        .byte, .half_word, .word => .long,
                        else => memory_type,
                    },
                    else => memory_type,
                };

                if (!Self.validateType(.long, address)) return error.DataType;
                if (!Self.validateType(data_type, source_type)) return error.DataType;
            },
            .store => {
                const address = self.types.pop();
                const value = self.types.pop();
                const memory_type = self.types.pop();

                if (!Self.validateType(.long, address)) return error.DataType;
                if (!Self.validateType(memory_type, value)) return error.DataType;
            },
            else => {},
        }
    }
};

//
// Test Utils
//

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
// Valid Tests

test "load global" {
    // Arrange
    const allocator = std.testing.allocator;

    const file = "function $f() {@s %w =w loadw $ptr ret}";

    var symbol_table = table.SymbolTable.init(allocator);
    defer symbol_table.deinit();

    // Act + Assert
    try source.testSource(allocator, file, &symbol_table);
    try memory.testMemory(allocator, file, &symbol_table);
    try testValidate(allocator, file, &symbol_table);
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

test "error.MismatchType" {
    // Arrange
    const allocator = std.testing.allocator;

    const file = "function $f() {@s %l =w copy 0 %r =s copy s_0 %r =w add %l, %r ret}";

    var symbol_table = table.SymbolTable.init(allocator);
    defer symbol_table.deinit();

    // Act
    try source.testSource(allocator, file, &symbol_table);
    try memory.testMemory(allocator, file, &symbol_table);
    const res = testValidate(allocator, file, &symbol_table);

    // Assert
    try std.testing.expectError(error.MismatchType, res);
}

test "error.DataType binary_operator" {
    // Arrange
    const allocator = std.testing.allocator;

    const file = "function $f() {@s %l =w copy 0 %r =w copy 0 %r =s add %l, %r ret}";

    var symbol_table = table.SymbolTable.init(allocator);
    defer symbol_table.deinit();

    // Act
    try source.testSource(allocator, file, &symbol_table);
    try memory.testMemory(allocator, file, &symbol_table);
    const res = testValidate(allocator, file, &symbol_table);

    // Assert
    try std.testing.expectError(error.DataType, res);
}

test "error.MismatchType binary_operator" {
    // Arrange
    const allocator = std.testing.allocator;

    const file = "function $f() {@s %l =w copy 0 %r =s copy s_0 %r =w add %l, %r ret}";

    var symbol_table = table.SymbolTable.init(allocator);
    defer symbol_table.deinit();

    // Act
    try source.testSource(allocator, file, &symbol_table);
    try memory.testMemory(allocator, file, &symbol_table);
    const res = testValidate(allocator, file, &symbol_table);

    // Assert
    try std.testing.expectError(error.MismatchType, res);
}

test "error.ComparisonType" {
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
    try std.testing.expectError(error.ComparisonType, res);
}

test "error.DataType integer comparison" {
    // Arrange
    const allocator = std.testing.allocator;

    const file = "function $f() {@s %l =s copy s_0 %r =s copy s_0 %r =w ceqw %l, %r ret}";

    var symbol_table = table.SymbolTable.init(allocator);
    defer symbol_table.deinit();

    // Act
    try source.testSource(allocator, file, &symbol_table);
    try memory.testMemory(allocator, file, &symbol_table);
    const res = testValidate(allocator, file, &symbol_table);

    // Assert
    try std.testing.expectError(error.DataType, res);
}

test "error.DataType float comparison" {
    // Arrange
    const allocator = std.testing.allocator;

    const file = "function $f() {@s %l =w copy 0 %r =w copy 0 %r =w cos %l, %r ret}";

    var symbol_table = table.SymbolTable.init(allocator);
    defer symbol_table.deinit();

    // Act
    try source.testSource(allocator, file, &symbol_table);
    try memory.testMemory(allocator, file, &symbol_table);
    const res = testValidate(allocator, file, &symbol_table);

    // Assert
    try std.testing.expectError(error.DataType, res);
}

test "error.DataType negate" {
    // Arrange
    const allocator = std.testing.allocator;

    const file = "function $f() {@s %v =w copy 0 %r =s neg %v ret}";

    var symbol_table = table.SymbolTable.init(allocator);
    defer symbol_table.deinit();

    // Act
    try source.testSource(allocator, file, &symbol_table);
    try memory.testMemory(allocator, file, &symbol_table);
    const res = testValidate(allocator, file, &symbol_table);

    // Assert
    try std.testing.expectError(error.DataType, res);
}

test "error.DataType return" {
    // Arrange
    const allocator = std.testing.allocator;

    const file = "function s $f() {@s %v =w copy 0 ret %v}";

    var symbol_table = table.SymbolTable.init(allocator);
    defer symbol_table.deinit();

    // Act
    try source.testSource(allocator, file, &symbol_table);
    try memory.testMemory(allocator, file, &symbol_table);
    const res = testValidate(allocator, file, &symbol_table);

    // Assert
    try std.testing.expectError(error.DataType, res);
}

test "error.DataType vastart" {
    // Arrange
    const allocator = std.testing.allocator;

    const file = "function $f() {@s %v =s copy 0 vastart %v ret}";

    var symbol_table = table.SymbolTable.init(allocator);
    defer symbol_table.deinit();

    // Act
    try source.testSource(allocator, file, &symbol_table);
    try memory.testMemory(allocator, file, &symbol_table);
    const res = testValidate(allocator, file, &symbol_table);

    // Assert
    try std.testing.expectError(error.DataType, res);
}

test "error.DataType vaarg" {
    // Arrange
    const allocator = std.testing.allocator;

    const file = "function $f() {@s %v =s copy 0 %a =w vaarg %v ret}";

    var symbol_table = table.SymbolTable.init(allocator);
    defer symbol_table.deinit();

    // Act
    try source.testSource(allocator, file, &symbol_table);
    try memory.testMemory(allocator, file, &symbol_table);
    const res = testValidate(allocator, file, &symbol_table);

    // Assert
    try std.testing.expectError(error.DataType, res);
}

test "error.DataType phi" {
    // Arrange
    const allocator = std.testing.allocator;

    const file = "function $f() {@a %a =w copy 0 @b %b =s copy s_0 @c %c =w phi @a %a, @b %b ret}";

    var symbol_table = table.SymbolTable.init(allocator);
    defer symbol_table.deinit();

    // Act
    try source.testSource(allocator, file, &symbol_table);
    try memory.testMemory(allocator, file, &symbol_table);
    const res = testValidate(allocator, file, &symbol_table);

    // Assert
    try std.testing.expectError(error.DataType, res);
}

test "error.DataType branch" {
    // Arrange
    const allocator = std.testing.allocator;

    const file = "function $f() {@a %a =s copy s_0 jnz %a, @t, @f @t @f ret}";

    var symbol_table = table.SymbolTable.init(allocator);
    defer symbol_table.deinit();

    // Act
    try source.testSource(allocator, file, &symbol_table);
    try memory.testMemory(allocator, file, &symbol_table);
    const res = testValidate(allocator, file, &symbol_table);

    // Assert
    try std.testing.expectError(error.DataType, res);
}

test "error.DataType blit target" {
    // Arrange
    const allocator = std.testing.allocator;

    const file = "function $f() {@s %to =b copy 0 %from =l copy 0 blit %to, %from, 32 ret}";

    var symbol_table = table.SymbolTable.init(allocator);
    defer symbol_table.deinit();

    // Act
    try source.testSource(allocator, file, &symbol_table);
    try memory.testMemory(allocator, file, &symbol_table);
    const res = testValidate(allocator, file, &symbol_table);

    // Assert
    try std.testing.expectError(error.DataType, res);
}

test "error.DataType blit source" {
    // Arrange
    const allocator = std.testing.allocator;

    const file = "function $f() {@s %to =l copy 0 %from =b copy 0 blit %to, %from, 32 ret}";

    var symbol_table = table.SymbolTable.init(allocator);
    defer symbol_table.deinit();

    // Act
    try source.testSource(allocator, file, &symbol_table);
    try memory.testMemory(allocator, file, &symbol_table);
    const res = testValidate(allocator, file, &symbol_table);

    // Assert
    try std.testing.expectError(error.DataType, res);
}

test "error.DataType blit size" {
    // Arrange
    const allocator = std.testing.allocator;

    const file = "function $f() {@s %to =l copy 0 %from =l copy 0 blit %to, %from, %to ret}";

    var symbol_table = table.SymbolTable.init(allocator);
    defer symbol_table.deinit();

    // Act
    try source.testSource(allocator, file, &symbol_table);
    try memory.testMemory(allocator, file, &symbol_table);
    const res = testValidate(allocator, file, &symbol_table);

    // Assert
    try std.testing.expectError(error.DataType, res);
}

test "error.DataType copy" {
    // Arrange
    const allocator = std.testing.allocator;

    const file = "function $f() {@s %a =s copy s_0 %b =l copy %a ret}";

    var symbol_table = table.SymbolTable.init(allocator);
    defer symbol_table.deinit();

    // Act
    try source.testSource(allocator, file, &symbol_table);
    try memory.testMemory(allocator, file, &symbol_table);
    const res = testValidate(allocator, file, &symbol_table);

    // Assert
    try std.testing.expectError(error.DataType, res);
}

test "error.DataType cast" {
    // Arrange
    const allocator = std.testing.allocator;

    const file = "function $f() {@s %s =s copy 0 %rt =d cast %s ret}";

    var symbol_table = table.SymbolTable.init(allocator);
    defer symbol_table.deinit();

    // Act
    try source.testSource(allocator, file, &symbol_table);
    try memory.testMemory(allocator, file, &symbol_table);
    const res = testValidate(allocator, file, &symbol_table);

    // Assert
    try std.testing.expectError(error.DataType, res);
}

test "error.DataType conversion" {
    // Arrange
    const allocator = std.testing.allocator;

    const file = "function $f() {@s %s =s copy 0 %rt =d swtof %s ret}";

    var symbol_table = table.SymbolTable.init(allocator);
    defer symbol_table.deinit();

    // Act
    try source.testSource(allocator, file, &symbol_table);
    try memory.testMemory(allocator, file, &symbol_table);
    const res = testValidate(allocator, file, &symbol_table);

    // Assert
    try std.testing.expectError(error.DataType, res);
}

test "error.DataType load pointer" {
    // Arrange
    const allocator = std.testing.allocator;

    const file = "function $f() {@s %ptr =b copy 0 %l =l loadl %ptr ret}";

    var symbol_table = table.SymbolTable.init(allocator);
    defer symbol_table.deinit();

    // Act
    try source.testSource(allocator, file, &symbol_table);
    try memory.testMemory(allocator, file, &symbol_table);
    const res = testValidate(allocator, file, &symbol_table);

    // Assert
    try std.testing.expectError(error.DataType, res);
}

test "error.DataType load type" {
    // Arrange
    const allocator = std.testing.allocator;

    const file = "function $f() {@s %ptr =l copy 0 %s =s loadl %ptr ret}";

    var symbol_table = table.SymbolTable.init(allocator);
    defer symbol_table.deinit();

    // Act
    try source.testSource(allocator, file, &symbol_table);
    try memory.testMemory(allocator, file, &symbol_table);
    const res = testValidate(allocator, file, &symbol_table);

    // Assert
    try std.testing.expectError(error.DataType, res);
}

test "error.DataType store pointer" {
    // Arrange
    const allocator = std.testing.allocator;

    const file = "function $f() {@s %ptr =s copy 0 storew 1, %ptr ret}";

    var symbol_table = table.SymbolTable.init(allocator);
    defer symbol_table.deinit();

    // Act
    try source.testSource(allocator, file, &symbol_table);
    try memory.testMemory(allocator, file, &symbol_table);
    const res = testValidate(allocator, file, &symbol_table);

    // Assert
    try std.testing.expectError(error.DataType, res);
}

test "error.DataType store type" {
    // Arrange
    const allocator = std.testing.allocator;

    const file = "function $f() {@s %val =s copy 0 storew %val, 0 ret}";

    var symbol_table = table.SymbolTable.init(allocator);
    defer symbol_table.deinit();

    // Act
    try source.testSource(allocator, file, &symbol_table);
    try memory.testMemory(allocator, file, &symbol_table);
    const res = testValidate(allocator, file, &symbol_table);

    // Assert
    try std.testing.expectError(error.DataType, res);
}
