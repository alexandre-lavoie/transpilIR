const std = @import("std");

const ast = @import("../ast/lib.zig");
const common = @import("../common.zig");
const table = @import("table.zig");
const types = @import("types.zig");

const test_lib = @import("test.zig");

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
            .linkage,
            .node,
            .module,
            .data_definition,
            .typed_data,
            .offset,
            .array_type,
            .opaque_type,
            .struct_type,
            .type_definition,
            .union_type,
            .zero_type,
            .block,
            .line,
            .call,
            .call_parameter,
            .function,
            .function_signature,
            .function_parameter,
            .variadic_parameter,
            .vaarg,
            .vastart,
            .allocate,
            .assignment,
            .blit,
            .copy,
            .cast,
            .convert,
            .load,
            .store,
            .branch,
            .halt,
            .jump,
            .phi_parameter,
            .@"return",
            .binary_operation,
            .comparison,
            .negate,
            => {},
            .identifier => |identifier| {
                const symbol = self.symbol_table.getSymbolByInstance(&instance) orelse unreachable;

                switch (symbol.memory) {
                    .empty => {
                        switch (identifier.scope) {
                            .label, .local, .type => return error.SymbolNotFound,
                            .global => try self.types.append(.long),
                        }
                    },
                    .type,
                    .data,
                    .@"struct",
                    .@"opaque",
                    .@"union",
                    .env,
                    => try self.types.append(.long),
                    .primitive => |primitive| try self.types.append(primitive),
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
            .primitive_type => |primitive| try self.types.append(primitive),
            .env_type => try self.types.append(.long),
            .phi => self.phi_type = self.types.pop(),
        }
    }

    pub fn exit(self: *Self, statement: *ast.Statement) !void {
        switch (statement.data) {
            .identifier,
            .literal,
            .linkage,
            .node,
            .module,
            .typed_data,
            .offset,
            .array_type,
            .env_type,
            .opaque_type,
            .primitive_type,
            .struct_type,
            .union_type,
            .zero_type,
            .function_parameter,
            .variadic_parameter,
            .allocate,
            .halt,
            .jump,
            => {},
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
                const data_type = self.types.popOrNull() orelse return error.DataType;

                try self.types.append(data_type);

                if (!Self.validateType(data_type, try Self.matchType(left, right))) return error.DataType;
            },
            .comparison => {
                const right = self.types.pop();
                const left = self.types.pop();
                const comparision_type = self.types.popOrNull() orelse return error.DataType;
                const data_type = self.types.popOrNull() orelse return error.DataType;

                switch (data_type) {
                    .single, .double => return error.ComparisonType,
                    else => {},
                }

                try self.types.append(data_type);

                if (!Self.validateType(comparision_type, try Self.matchType(left, right))) return error.DataType;
            },
            .negate => {
                const value = self.types.pop();
                const data_type = self.types.popOrNull() orelse return error.DataType;

                try self.types.append(data_type);

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
                const value = self.types.popOrNull() orelse return error.DataType;

                if (!Self.validateType(.long, value)) return error.DataType;
            },
            .vaarg => {
                const value = self.types.pop();
                const data_type = self.types.popOrNull() orelse return error.DataType;

                try self.types.append(data_type);

                if (!Self.validateType(.long, value)) return error.DataType;
            },
            .phi_parameter => {
                const @"type" = self.types.popOrNull() orelse return error.DataType;

                if (!Self.validateType(self.phi_type orelse .void, @"type")) return error.DataType;
            },
            .phi => {
                try self.types.append(self.phi_type orelse unreachable);
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
                const from_type = self.types.popOrNull() orelse return error.DataType;
                const to_type = self.types.popOrNull() orelse return error.DataType;
                const data_type = self.types.popOrNull() orelse return error.DataType;

                try self.types.append(data_type);

                if (!Self.validateType(from_type, value)) return error.DataType;
                if (!Self.validateType(data_type, to_type)) return error.DataType;
            },
            .copy => {
                const value = self.types.pop();
                const data_type = self.types.popOrNull() orelse return error.DataType;

                try self.types.append(data_type);

                if (!Self.validateType(data_type, value)) return error.DataType;
            },
            .cast => {
                const value = self.types.pop();
                const data_type = self.types.popOrNull() orelse return error.DataType;

                const from_type: ast.PrimitiveType = switch (data_type) {
                    .long => .double,
                    .single => .word,
                    .double => .long,
                    else => .single,
                };

                try self.types.append(data_type);

                if (!Self.validateType(from_type, value)) return error.DataType;
            },
            .load => {
                const address = self.types.pop();
                const memory_type = self.types.popOrNull() orelse return error.DataType;
                const data_type = self.types.popOrNull() orelse return error.DataType;

                const source_type = switch (data_type) {
                    .word, .word_unsigned => switch (memory_type) {
                        .byte,
                        .byte_unsigned,
                        .half_word,
                        .half_word_unsigned,
                        => .word,
                        else => memory_type,
                    },
                    .long, .long_unsigned => switch (memory_type) {
                        .byte,
                        .byte_unsigned,
                        .half_word,
                        .half_word_unsigned,
                        .word,
                        .word_unsigned,
                        => .long,
                        else => memory_type,
                    },
                    else => memory_type,
                };

                try self.types.append(data_type);

                if (!Self.validateType(.long, address)) return error.DataType;
                if (!Self.validateType(data_type, source_type)) return error.DataType;
            },
            .store => {
                const address = self.types.pop();
                const value = self.types.pop();
                const memory_type = self.types.popOrNull() orelse return error.DataType;

                if (!Self.validateType(.long, address)) return error.DataType;
                if (!Self.validateType(memory_type, value)) return error.DataType;
            },
            .call_parameter => {
                const @"type" = self.types.pop();
                const value = self.types.pop();

                if (!Self.validateType(@"type", value)) return error.DataType;
            },
            .call => {
                const return_type = self.types.pop();
                const address = self.types.popOrNull() orelse return error.DataType;

                try self.types.append(return_type);

                if (!Self.validateType(.long, address)) return error.DataType;
            },
            .assignment => {
                const value = self.types.pop();
                const @"type" = self.types.popOrNull() orelse return error.DataType;

                if (!Self.validateType(@"type", value)) return error.DataType;
            },
        }
    }
};

//
// Valid Tests

test "load global" {
    // Arrange
    const allocator = std.testing.allocator;

    const file = "function $f() {@s %w =w loadw $ptr ret}";

    var tree = try test_lib.testAST(allocator, file);
    defer tree.deinit();

    var symbol_table = table.SymbolTable.init(allocator);
    defer symbol_table.deinit();

    // Act + Assert
    try test_lib.testValidate(allocator, file, &tree, &symbol_table);
}

test "call no arguments" {
    // Arrange
    const allocator = std.testing.allocator;

    const file = "function $f() {@s %v =w call $no_arg() ret}";

    var tree = try test_lib.testAST(allocator, file);
    defer tree.deinit();

    var symbol_table = table.SymbolTable.init(allocator);
    defer symbol_table.deinit();

    // Act + Assert
    try test_lib.testValidate(allocator, file, &tree, &symbol_table);
}

//
// Error Tests
//

test "error.SymbolNotFound label" {
    // Arrange
    const allocator = std.testing.allocator;

    const file = "function $f() {@s jmp @dne}";

    var tree = try test_lib.testAST(allocator, file);
    defer tree.deinit();

    var symbol_table = table.SymbolTable.init(allocator);
    defer symbol_table.deinit();

    // Act
    const res = test_lib.testValidate(allocator, file, &tree, &symbol_table);

    // Assert
    try std.testing.expectError(error.SymbolNotFound, res);
}

test "error.SymbolNotFound local" {
    // Arrange
    const allocator = std.testing.allocator;

    const file = "function $f() {@s %l =w add %dne, 1 ret}";

    var tree = try test_lib.testAST(allocator, file);
    defer tree.deinit();

    var symbol_table = table.SymbolTable.init(allocator);
    defer symbol_table.deinit();

    // Act
    const res = test_lib.testValidate(allocator, file, &tree, &symbol_table);

    // Assert
    try std.testing.expectError(error.SymbolNotFound, res);
}

test "error.MismatchType" {
    // Arrange
    const allocator = std.testing.allocator;

    const file = "function $f() {@s %l =w copy 0 %r =s copy s_0 %r =w add %l, %r ret}";

    var tree = try test_lib.testAST(allocator, file);
    defer tree.deinit();

    var symbol_table = table.SymbolTable.init(allocator);
    defer symbol_table.deinit();

    // Act
    const res = test_lib.testValidate(allocator, file, &tree, &symbol_table);

    // Assert
    try std.testing.expectError(error.MismatchType, res);
}

test "error.DataType binary_operator" {
    // Arrange
    const allocator = std.testing.allocator;

    const file = "function $f() {@s %l =w copy 0 %r =w copy 0 %r =s add %l, %r ret}";

    var tree = try test_lib.testAST(allocator, file);
    defer tree.deinit();

    var symbol_table = table.SymbolTable.init(allocator);
    defer symbol_table.deinit();

    // Act
    const res = test_lib.testValidate(allocator, file, &tree, &symbol_table);

    // Assert
    try std.testing.expectError(error.DataType, res);
}

test "error.MismatchType binary_operator" {
    // Arrange
    const allocator = std.testing.allocator;

    const file = "function $f() {@s %l =w copy 0 %r =s copy s_0 %r =w add %l, %r ret}";

    var tree = try test_lib.testAST(allocator, file);
    defer tree.deinit();

    var symbol_table = table.SymbolTable.init(allocator);
    defer symbol_table.deinit();

    // Act
    const res = test_lib.testValidate(allocator, file, &tree, &symbol_table);

    // Assert
    try std.testing.expectError(error.MismatchType, res);
}

test "error.ComparisonType" {
    // Arrange
    const allocator = std.testing.allocator;

    const file = "function $f() {@s %r =s ceqw 0, 0 ret}";

    var tree = try test_lib.testAST(allocator, file);
    defer tree.deinit();

    var symbol_table = table.SymbolTable.init(allocator);
    defer symbol_table.deinit();

    // Act
    const res = test_lib.testValidate(allocator, file, &tree, &symbol_table);

    // Assert
    try std.testing.expectError(error.ComparisonType, res);
}

test "error.DataType integer comparison" {
    // Arrange
    const allocator = std.testing.allocator;

    const file = "function $f() {@s %l =s copy s_0 %r =s copy s_0 %r =w ceqw %l, %r ret}";

    var tree = try test_lib.testAST(allocator, file);
    defer tree.deinit();

    var symbol_table = table.SymbolTable.init(allocator);
    defer symbol_table.deinit();

    // Act
    const res = test_lib.testValidate(allocator, file, &tree, &symbol_table);

    // Assert
    try std.testing.expectError(error.DataType, res);
}

test "error.DataType float comparison" {
    // Arrange
    const allocator = std.testing.allocator;

    const file = "function $f() {@s %l =w copy 0 %r =w copy 0 %r =w cos %l, %r ret}";

    var tree = try test_lib.testAST(allocator, file);
    defer tree.deinit();

    var symbol_table = table.SymbolTable.init(allocator);
    defer symbol_table.deinit();

    // Act
    const res = test_lib.testValidate(allocator, file, &tree, &symbol_table);

    // Assert
    try std.testing.expectError(error.DataType, res);
}

test "error.DataType negate" {
    // Arrange
    const allocator = std.testing.allocator;

    const file = "function $f() {@s %v =w copy 0 %r =s neg %v ret}";

    var tree = try test_lib.testAST(allocator, file);
    defer tree.deinit();

    var symbol_table = table.SymbolTable.init(allocator);
    defer symbol_table.deinit();

    // Act
    const res = test_lib.testValidate(allocator, file, &tree, &symbol_table);

    // Assert
    try std.testing.expectError(error.DataType, res);
}

test "error.DataType return" {
    // Arrange
    const allocator = std.testing.allocator;

    const file = "function s $f() {@s %v =w copy 0 ret %v}";

    var tree = try test_lib.testAST(allocator, file);
    defer tree.deinit();

    var symbol_table = table.SymbolTable.init(allocator);
    defer symbol_table.deinit();

    // Act
    const res = test_lib.testValidate(allocator, file, &tree, &symbol_table);

    // Assert
    try std.testing.expectError(error.DataType, res);
}

test "error.DataType vastart" {
    // Arrange
    const allocator = std.testing.allocator;

    const file = "function $f() {@s %v =s copy 0 vastart %v ret}";

    var tree = try test_lib.testAST(allocator, file);
    defer tree.deinit();

    var symbol_table = table.SymbolTable.init(allocator);
    defer symbol_table.deinit();

    // Act
    const res = test_lib.testValidate(allocator, file, &tree, &symbol_table);

    // Assert
    try std.testing.expectError(error.DataType, res);
}

test "error.DataType vaarg" {
    // Arrange
    const allocator = std.testing.allocator;

    const file = "function $f() {@s %v =s copy 0 %a =w vaarg %v ret}";

    var tree = try test_lib.testAST(allocator, file);
    defer tree.deinit();

    var symbol_table = table.SymbolTable.init(allocator);
    defer symbol_table.deinit();

    // Act
    const res = test_lib.testValidate(allocator, file, &tree, &symbol_table);

    // Assert
    try std.testing.expectError(error.DataType, res);
}

test "error.DataType phi" {
    // Arrange
    const allocator = std.testing.allocator;

    const file = "function $f() {@a %a =w copy 0 @b %b =s copy s_0 @c %c =w phi @a %a, @b %b ret}";

    var tree = try test_lib.testAST(allocator, file);
    defer tree.deinit();

    var symbol_table = table.SymbolTable.init(allocator);
    defer symbol_table.deinit();

    // Act
    const res = test_lib.testValidate(allocator, file, &tree, &symbol_table);

    // Assert
    try std.testing.expectError(error.DataType, res);
}

test "error.DataType branch" {
    // Arrange
    const allocator = std.testing.allocator;

    const file = "function $f() {@a %a =s copy s_0 jnz %a, @t, @f @t @f ret}";

    var tree = try test_lib.testAST(allocator, file);
    defer tree.deinit();

    var symbol_table = table.SymbolTable.init(allocator);
    defer symbol_table.deinit();

    // Act
    const res = test_lib.testValidate(allocator, file, &tree, &symbol_table);

    // Assert
    try std.testing.expectError(error.DataType, res);
}

test "error.DataType blit target" {
    // Arrange
    const allocator = std.testing.allocator;

    const file = "function $f() {@s %to =b copy 0 %from =l copy 0 blit %to, %from, 32 ret}";

    var tree = try test_lib.testAST(allocator, file);
    defer tree.deinit();

    var symbol_table = table.SymbolTable.init(allocator);
    defer symbol_table.deinit();

    // Act
    const res = test_lib.testValidate(allocator, file, &tree, &symbol_table);

    // Assert
    try std.testing.expectError(error.DataType, res);
}

test "error.DataType blit source" {
    // Arrange
    const allocator = std.testing.allocator;

    const file = "function $f() {@s %to =l copy 0 %from =b copy 0 blit %to, %from, 32 ret}";

    var tree = try test_lib.testAST(allocator, file);
    defer tree.deinit();

    var symbol_table = table.SymbolTable.init(allocator);
    defer symbol_table.deinit();

    // Act
    const res = test_lib.testValidate(allocator, file, &tree, &symbol_table);

    // Assert
    try std.testing.expectError(error.DataType, res);
}

test "error.DataType blit size" {
    // Arrange
    const allocator = std.testing.allocator;

    const file = "function $f() {@s %to =l copy 0 %from =l copy 0 blit %to, %from, %to ret}";

    var tree = try test_lib.testAST(allocator, file);
    defer tree.deinit();

    var symbol_table = table.SymbolTable.init(allocator);
    defer symbol_table.deinit();

    // Act
    const res = test_lib.testValidate(allocator, file, &tree, &symbol_table);

    // Assert
    try std.testing.expectError(error.DataType, res);
}

test "error.DataType copy" {
    // Arrange
    const allocator = std.testing.allocator;

    const file = "function $f() {@s %a =s copy s_0 %b =l copy %a ret}";

    var tree = try test_lib.testAST(allocator, file);
    defer tree.deinit();

    var symbol_table = table.SymbolTable.init(allocator);
    defer symbol_table.deinit();

    // Act
    const res = test_lib.testValidate(allocator, file, &tree, &symbol_table);

    // Assert
    try std.testing.expectError(error.DataType, res);
}

test "error.DataType cast" {
    // Arrange
    const allocator = std.testing.allocator;

    const file = "function $f() {@s %s =s copy 0 %rt =d cast %s ret}";

    var tree = try test_lib.testAST(allocator, file);
    defer tree.deinit();

    var symbol_table = table.SymbolTable.init(allocator);
    defer symbol_table.deinit();

    // Act
    const res = test_lib.testValidate(allocator, file, &tree, &symbol_table);

    // Assert
    try std.testing.expectError(error.DataType, res);
}

test "error.DataType conversion" {
    // Arrange
    const allocator = std.testing.allocator;

    const file = "function $f() {@s %s =s copy 0 %rt =d swtof %s ret}";

    var tree = try test_lib.testAST(allocator, file);
    defer tree.deinit();

    var symbol_table = table.SymbolTable.init(allocator);
    defer symbol_table.deinit();

    // Act
    const res = test_lib.testValidate(allocator, file, &tree, &symbol_table);

    // Assert
    try std.testing.expectError(error.DataType, res);
}

test "error.DataType load pointer" {
    // Arrange
    const allocator = std.testing.allocator;

    const file = "function $f() {@s %ptr =b copy 0 %l =l loadl %ptr ret}";

    var tree = try test_lib.testAST(allocator, file);
    defer tree.deinit();

    var symbol_table = table.SymbolTable.init(allocator);
    defer symbol_table.deinit();

    // Act
    const res = test_lib.testValidate(allocator, file, &tree, &symbol_table);

    // Assert
    try std.testing.expectError(error.DataType, res);
}

test "error.DataType load type" {
    // Arrange
    const allocator = std.testing.allocator;

    const file = "function $f() {@s %ptr =l copy 0 %s =s loadl %ptr ret}";

    var tree = try test_lib.testAST(allocator, file);
    defer tree.deinit();

    var symbol_table = table.SymbolTable.init(allocator);
    defer symbol_table.deinit();

    // Act
    const res = test_lib.testValidate(allocator, file, &tree, &symbol_table);

    // Assert
    try std.testing.expectError(error.DataType, res);
}

test "error.DataType store pointer" {
    // Arrange
    const allocator = std.testing.allocator;

    const file = "function $f() {@s %ptr =s copy 0 storew 1, %ptr ret}";

    var tree = try test_lib.testAST(allocator, file);
    defer tree.deinit();

    var symbol_table = table.SymbolTable.init(allocator);
    defer symbol_table.deinit();

    // Act
    const res = test_lib.testValidate(allocator, file, &tree, &symbol_table);

    // Assert
    try std.testing.expectError(error.DataType, res);
}

test "error.DataType store type" {
    // Arrange
    const allocator = std.testing.allocator;

    const file = "function $f() {@s %val =s copy 0 storew %val, 0 ret}";

    var tree = try test_lib.testAST(allocator, file);
    defer tree.deinit();

    var symbol_table = table.SymbolTable.init(allocator);
    defer symbol_table.deinit();

    // Act
    const res = test_lib.testValidate(allocator, file, &tree, &symbol_table);

    // Assert
    try std.testing.expectError(error.DataType, res);
}

test "error.DataType assignment" {
    // Arrange
    const allocator = std.testing.allocator;

    const file = "function $f() {@s %v =s copy 0 %v =w copy 0 ret}";

    var tree = try test_lib.testAST(allocator, file);
    defer tree.deinit();

    var symbol_table = table.SymbolTable.init(allocator);
    defer symbol_table.deinit();

    // Act
    const res = test_lib.testValidate(allocator, file, &tree, &symbol_table);

    // Assert
    try std.testing.expectError(error.DataType, res);
}

test "error.DataType call local" {
    // Arrange
    const allocator = std.testing.allocator;

    const file = "function $f() {@s %f =s copy 0 call %f() ret}";

    var tree = try test_lib.testAST(allocator, file);
    defer tree.deinit();

    var symbol_table = table.SymbolTable.init(allocator);
    defer symbol_table.deinit();

    // Act
    const res = test_lib.testValidate(allocator, file, &tree, &symbol_table);

    // Assert
    try std.testing.expectError(error.DataType, res);
}
