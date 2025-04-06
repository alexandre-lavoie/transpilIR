const std = @import("std");

const ast = @import("../ast/lib.zig");
const common = @import("../common.zig");
const table = @import("table.zig");
const types = @import("types.zig");

const test_lib = @import("test.zig");

pub const SymbolValidateWalkCallback = struct {
    allocator: std.mem.Allocator,
    symbol_table: *const table.SymbolTable,
    target: *const common.Target,
    types: TypeList,
    return_type: ?ast.PrimitiveType = null,
    phi_type: ?ast.PrimitiveType = null,

    const Self = @This();
    const TypeList = std.ArrayList(ast.PrimitiveType);

    pub fn init(allocator: std.mem.Allocator, symbol_table: *const table.SymbolTable, target: *const common.Target) Self {
        return .{
            .allocator = allocator,
            .symbol_table = symbol_table,
            .target = target,
            .types = TypeList.init(allocator),
        };
    }

    pub fn deinit(self: *Self) void {
        self.types.deinit();
    }

    fn matchType(self: *const Self, left: ast.PrimitiveType, right: ast.PrimitiveType) !ast.PrimitiveType {
        if (left == right) return left;

        if (left == .void) {
            return right;
        } else if (right == .void) {
            return left;
        }

        if (left == .ptr or right == .ptr) {
            if (Self.unsignedType(self.valueOrPointer(left)) != Self.unsignedType(self.valueOrPointer(right))) {
                return error.MismatchType;
            }

            return .ptr;
        }

        return error.MismatchType;
    }

    fn valueOrPointer(self: *const Self, value: ast.PrimitiveType) ast.PrimitiveType {
        return switch (value) {
            .ptr => switch (self.target.arch) {
                .a8 => .u8,
                .a16 => .u16,
                .a32 => .u32,
                .a64 => .u64,
            },
            else => value,
        };
    }

    fn unsignedType(value: ast.PrimitiveType) ast.PrimitiveType {
        return switch (value) {
            .i8 => .u8,
            .i16 => .u16,
            .i32 => .u32,
            .i64 => .u64,
            else => value,
        };
    }

    fn validatePointer(self: *Self, t: ast.PrimitiveType) bool {
        if (t == .void) {
            return true;
        }

        return self.valueOrPointer(.ptr) == Self.unsignedType(self.valueOrPointer(t));
    }

    fn validateType(self: *Self, target: ast.PrimitiveType, from: ast.PrimitiveType) bool {
        const t = self.valueOrPointer(target);
        const f = self.valueOrPointer(from);

        return switch (t) {
            .void => true,
            .bool,
            .u8,
            .i8,
            => switch (f) {
                .f32,
                .f64,
                => false,
                else => true,
            },
            .u16,
            .i16,
            => switch (f) {
                .i8,
                .u8,
                .f32,
                .f64,
                => false,
                else => true,
            },
            .u32,
            .i32,
            => switch (f) {
                .void,
                .bool,
                .i32,
                .u32,
                .i64,
                .u64,
                => true,
                else => false,
            },
            .u64,
            .i64,
            => switch (f) {
                .void,
                .bool,
                .i64,
                .u64,
                => true,
                else => false,
            },
            .f32 => switch (f) {
                .void,
                .f32,
                => true,
                else => false,
            },
            .f64 => switch (f) {
                .void,
                .f64,
                => true,
                else => false,
            },
            .ptr => unreachable,
        };
    }

    pub fn enter(self: *Self, statement: *const ast.Statement) !void {
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
                const symbol = self.symbol_table.getSymbolByInstance(&instance) orelse return error.IdentifierNotFound;

                switch (symbol.memory) {
                    .empty => {
                        switch (identifier.scope) {
                            .label, .local, .type => return error.SymbolNotFound,
                            .global => try self.types.append(.ptr),
                        }
                    },
                    .type,
                    .data,
                    .@"struct",
                    .@"opaque",
                    .@"union",
                    .env,
                    => try self.types.append(.ptr),
                    .primitive => |primitive| try self.types.append(primitive),
                    .function => |function| {
                        if (self.return_type == null) {
                            self.return_type = switch (function.@"return") {
                                .type => .ptr,
                                .primitive => |primitive| primitive,
                            };
                        }

                        try self.types.append(.ptr);
                    },
                    .function_pointer,
                    .stack_allocation,
                    => try self.types.append(.ptr),
                    else => {},
                }
            },
            .literal => |literal| {
                try self.types.append(switch (literal.type) {
                    .string => .ptr,
                    .integer,
                    .single,
                    .double,
                    => .void,
                });
            },
            .primitive_type => |primitive| try self.types.append(primitive),
            .env_type => try self.types.append(.ptr),
            .phi => self.phi_type = self.types.popOrNull() orelse return error.DataType,
        }
    }

    pub fn exit(self: *Self, statement: *const ast.Statement) !void {
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
                const right = self.types.popOrNull() orelse return error.MissingBinaryOpRight;
                const left = self.types.popOrNull() orelse return error.MissingBinaryOpLeft;
                const data_type = self.types.popOrNull() orelse return error.MissingBinaryOpType;

                try self.types.append(data_type);

                if (!self.validateType(data_type, try self.matchType(left, right))) return error.DataType;
            },
            .comparison => {
                const right = self.types.popOrNull() orelse return error.MissingComparisonRight;
                const left = self.types.popOrNull() orelse return error.MissingComparisonLeft;
                const comparision_type = self.types.popOrNull() orelse return error.MissingComparisonComparisonType;
                const data_type = self.types.popOrNull() orelse return error.MissingComparisonDataType;

                try self.types.append(data_type);

                if (!self.validateType(comparision_type, try self.matchType(left, right))) return error.DataType;
            },
            .negate => {
                const value = self.types.popOrNull() orelse return error.MissingNegateValue;
                const data_type = self.types.popOrNull() orelse return error.MissingNegateType;

                try self.types.append(data_type);

                if (!self.validateType(data_type, value)) return error.DataType;
            },
            .@"return" => {
                const @"type": ast.PrimitiveType = switch (self.types.items.len) {
                    0 => .void,
                    else => self.types.pop(),
                };

                if (!self.validateType(self.return_type orelse .void, @"type")) return error.DataType;
            },
            .vastart => {
                const value = self.types.popOrNull() orelse return error.MissingVaStartValue;

                if (!self.validatePointer(value)) return error.DataType;
            },
            .vaarg => {
                const value = self.types.popOrNull() orelse return error.MissingVaArgValue;
                const data_type = self.types.popOrNull() orelse return error.MissingVaArgType;

                try self.types.append(data_type);

                if (!self.validatePointer(value)) return error.DataType;
            },
            .phi_parameter => {
                const @"type" = self.types.popOrNull() orelse return error.MissingPhiParameterType;

                if (!self.validateType(self.phi_type orelse .void, @"type")) return error.DataType;
            },
            .phi => {
                try self.types.append(self.phi_type orelse return error.DataType);
            },
            .branch => {
                const condition = self.types.popOrNull() orelse return error.MissingBranchCondition;

                if (!self.validateType(.bool, condition)) return error.DataType;
            },
            .blit => {
                const size = self.types.popOrNull() orelse return error.MissingBlitSize;
                const to = self.types.popOrNull() orelse return error.MissingBlitTo;
                const from = self.types.popOrNull() orelse return error.MissingBlitFrom;

                if (size != .void) return error.DataType;
                if (!self.validatePointer(to)) return error.DataType;
                if (!self.validatePointer(from)) return error.DataType;
            },
            .convert => {
                const value = self.types.popOrNull() orelse return error.MissingConvertValue;
                const from_type = self.types.popOrNull() orelse return error.MissingConvertFromType;
                const to_type = self.types.popOrNull() orelse return error.MissingConvertToType;
                const data_type = self.types.popOrNull() orelse return error.MissingConvertDataType;

                try self.types.append(data_type);

                if (!self.validateType(from_type, value)) return error.DataType;
                if (!self.validateType(data_type, to_type)) return error.DataType;
            },
            .copy => {
                const value = self.types.popOrNull() orelse return error.MissingCopyValue;
                const data_type = self.types.popOrNull() orelse return error.MissingCopyType;

                try self.types.append(data_type);

                if (!self.validateType(data_type, value)) return error.DataType;
            },
            .cast => {
                const value = self.types.popOrNull() orelse return error.MissingCastValue;
                const data_type = self.types.popOrNull() orelse return error.MissingCastType;

                const from_type: ast.PrimitiveType = switch (data_type) {
                    .i64 => .f64,
                    .f32 => .i32,
                    .f64 => .i64,
                    else => .f32,
                };

                try self.types.append(data_type);

                if (!self.validateType(from_type, value)) return error.DataType;
            },
            .load => {
                const address = self.types.popOrNull() orelse return error.MissingLoadAddress;
                const memory_type = self.types.popOrNull() orelse return error.MissingLoadMemoryType;
                const data_type = self.types.popOrNull() orelse return error.MissingLoadDataType;

                const source_type = switch (data_type) {
                    .i32,
                    .u32,
                    => switch (memory_type) {
                        .i8,
                        .u8,
                        .i16,
                        .u16,
                        => .i32,
                        else => memory_type,
                    },
                    .i64,
                    .u64,
                    => switch (memory_type) {
                        .i8,
                        .u8,
                        .i16,
                        .u16,
                        .i32,
                        .u32,
                        => .i64,
                        else => memory_type,
                    },
                    else => memory_type,
                };

                try self.types.append(data_type);

                if (!self.validatePointer(address)) return error.DataType;
                if (!self.validateType(data_type, source_type)) return error.DataType;
            },
            .store => {
                const address = self.types.popOrNull() orelse return error.MissingStoreAddress;
                const value = self.types.popOrNull() orelse return error.MissingStoreValue;
                const memory_type = self.types.popOrNull() orelse return error.MissingStoreMemoryType;

                if (!self.validatePointer(address)) return error.DataType;
                if (!self.validateType(memory_type, value)) return error.DataType;
            },
            .call_parameter => {
                const @"type" = self.types.popOrNull() orelse return error.MissingCallParameterType;
                const value = self.types.popOrNull() orelse return error.MissingCallParameterValue;

                if (!self.validateType(@"type", value)) return error.DataType;
            },
            .call => {
                const return_type = self.types.popOrNull() orelse return error.MissingCallReturnType;
                const address = self.types.popOrNull() orelse return error.MissingCallAddress;

                try self.types.append(return_type);

                if (!self.validateType(.ptr, address)) return error.DataType;
            },
            .assignment => {
                const value = self.types.popOrNull() orelse return error.MissingAssignmentValue;
                const @"type" = self.types.popOrNull() orelse return error.MissingAssignmentType;

                if (!self.validateType(@"type", value)) return error.DataType;
            },
        }
    }
};

//
// Valid Tests
//

test "load global" {
    // Arrange
    const allocator = std.testing.allocator;

    const file = "function $f() {@s %w =w loadw $ptr ret}";

    var tree = try test_lib.testAST(allocator, file);
    defer tree.deinit();

    var symbol_table = table.SymbolTable.init(allocator);
    defer symbol_table.deinit();

    // Act + Assert
    try test_lib.testValidate(allocator, file, &tree, &symbol_table, &test_lib.test_target);
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
    try test_lib.testValidate(allocator, file, &tree, &symbol_table, &test_lib.test_target);
}

test "call pointer" {
    // Arrange
    const allocator = std.testing.allocator;

    const file = "function $f() {@s %f =l load 0 call %f() ret}";

    var tree = try test_lib.testAST(allocator, file);
    defer tree.deinit();

    var symbol_table = table.SymbolTable.init(allocator);
    defer symbol_table.deinit();

    // Act + Assert
    try test_lib.testValidate(allocator, file, &tree, &symbol_table, &test_lib.test_target);
}

test "downcast integer" {
    // Arrange
    const allocator = std.testing.allocator;

    const file = "function $f() {@s %b =w copy 0 %w =b copy %b ret}";

    var tree = try test_lib.testAST(allocator, file);
    defer tree.deinit();

    var symbol_table = table.SymbolTable.init(allocator);
    defer symbol_table.deinit();

    // Act + Assert
    try test_lib.testValidate(allocator, file, &tree, &symbol_table, &test_lib.test_target);
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
    const res = test_lib.testValidate(allocator, file, &tree, &symbol_table, &test_lib.test_target);

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
    const res = test_lib.testValidate(allocator, file, &tree, &symbol_table, &test_lib.test_target);

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
    const res = test_lib.testValidate(allocator, file, &tree, &symbol_table, &test_lib.test_target);

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
    const res = test_lib.testValidate(allocator, file, &tree, &symbol_table, &test_lib.test_target);

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
    const res = test_lib.testValidate(allocator, file, &tree, &symbol_table, &test_lib.test_target);

    // Assert
    try std.testing.expectError(error.MismatchType, res);
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
    const res = test_lib.testValidate(allocator, file, &tree, &symbol_table, &test_lib.test_target);

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
    const res = test_lib.testValidate(allocator, file, &tree, &symbol_table, &test_lib.test_target);

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
    const res = test_lib.testValidate(allocator, file, &tree, &symbol_table, &test_lib.test_target);

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
    const res = test_lib.testValidate(allocator, file, &tree, &symbol_table, &test_lib.test_target);

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
    const res = test_lib.testValidate(allocator, file, &tree, &symbol_table, &test_lib.test_target);

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
    const res = test_lib.testValidate(allocator, file, &tree, &symbol_table, &test_lib.test_target);

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
    const res = test_lib.testValidate(allocator, file, &tree, &symbol_table, &test_lib.test_target);

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
    const res = test_lib.testValidate(allocator, file, &tree, &symbol_table, &test_lib.test_target);

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
    const res = test_lib.testValidate(allocator, file, &tree, &symbol_table, &test_lib.test_target);

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
    const res = test_lib.testValidate(allocator, file, &tree, &symbol_table, &test_lib.test_target);

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
    const res = test_lib.testValidate(allocator, file, &tree, &symbol_table, &test_lib.test_target);

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
    const res = test_lib.testValidate(allocator, file, &tree, &symbol_table, &test_lib.test_target);

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
    const res = test_lib.testValidate(allocator, file, &tree, &symbol_table, &test_lib.test_target);

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
    const res = test_lib.testValidate(allocator, file, &tree, &symbol_table, &test_lib.test_target);

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
    const res = test_lib.testValidate(allocator, file, &tree, &symbol_table, &test_lib.test_target);

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
    const res = test_lib.testValidate(allocator, file, &tree, &symbol_table, &test_lib.test_target);

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
    const res = test_lib.testValidate(allocator, file, &tree, &symbol_table, &test_lib.test_target);

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
    const res = test_lib.testValidate(allocator, file, &tree, &symbol_table, &test_lib.test_target);

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
    const res = test_lib.testValidate(allocator, file, &tree, &symbol_table, &test_lib.test_target);

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
    const res = test_lib.testValidate(allocator, file, &tree, &symbol_table, &test_lib.test_target);

    // Assert
    try std.testing.expectError(error.DataType, res);
}
