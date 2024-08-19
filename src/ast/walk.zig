const std = @import("std");

const ast = @import("ast.zig");
const statement = @import("statement.zig");

pub fn ASTWalk(comptime Callback: type) type {
    return struct {
        tree: *ast.AST,
        callback: *Callback,

        const Self = @This();

        const has_enter: bool = std.meta.hasFn(Callback, "enter");
        fn callback_enter(self: *Self, s: *statement.Statement) !void {
            if (has_enter) {
                try self.callback.enter(s);
            }
        }

        const has_exit: bool = std.meta.hasFn(Callback, "exit");
        fn callback_exit(self: *Self, s: *statement.Statement) !void {
            if (has_exit) {
                try self.callback.exit(s);
            }
        }

        pub fn init(tree: *ast.AST, callback: *Callback) Self {
            return Self{
                .tree = tree,
                .callback = callback,
            };
        }

        pub fn walk(self: *Self, optional_index: ?statement.StatementIndex) !void {
            const index = optional_index orelse return;

            const s = self.tree.getPtr(index) orelse return error.NotFound;

            try self.callback_enter(s);

            switch (s.data) {
                .identifier => {},
                .literal => {},
                .linkage => |*d| {
                    try self.walk(d.section);
                    try self.walk(d.flags);
                },
                .node => |*d| {
                    try self.walk(d.value);
                    try self.walk(d.next);
                },
                .module => |*d| {
                    try self.walk(d.types);
                    try self.walk(d.data);
                    try self.walk(d.functions);
                },
                .data_definition => |*d| {
                    try self.walk(d.identifier);
                    try self.walk(d.linkage);
                    try self.walk(d.values);
                },
                .typed_data => |*d| {
                    try self.walk(d.type);
                    try self.walk(d.value);
                },
                .offset => |*d| {
                    try self.walk(d.identifier);
                    try self.walk(d.value);
                },
                .array_type => |*d| {
                    try self.walk(d.item);
                    try self.walk(d.count);
                },
                .env_type => {},
                .opaque_type => |*d| {
                    try self.walk(d.alignment);
                    try self.walk(d.size);
                },
                .primitive_type => {},
                .struct_type => |*d| {
                    try self.walk(d.alignment);
                    try self.walk(d.members);
                },
                .type_definition => |*d| {
                    try self.walk(d.identifier);
                    try self.walk(d.type);
                },
                .union_type => |*d| {
                    try self.walk(d.alignment);
                    try self.walk(d.types);
                },
                .zero_type => {},
                .block => |*d| {
                    try self.walk(d.label);
                    try self.walk(d.phi_statements);
                    try self.walk(d.statements);
                    try self.walk(d.flow_statement);
                },
                .call => |*d| {
                    try self.walk(d.target);
                    try self.walk(d.return_type);
                    try self.walk(d.parameters);
                },
                .function => |*d| {
                    try self.walk(d.signature);
                    try self.walk(d.body);
                },
                .function_signature => |*d| {
                    try self.walk(d.name);
                    try self.walk(d.linkage);
                    try self.walk(d.return_type);
                    try self.walk(d.parameters);
                },
                .type_parameter => |*d| {
                    try self.walk(d.value);
                    try self.walk(d.type_statement);
                },
                .variadic_parameter => {},
                .vaarg => |*d| {
                    try self.walk(d.data_type);
                    try self.walk(d.parameter);
                },
                .vastart => |*d| {
                    try self.walk(d.parameter);
                },
                .allocate => |*d| {
                    try self.walk(d.data_type);
                    try self.walk(d.alignment);
                    try self.walk(d.size);
                },
                .assignment => |*d| {
                    try self.walk(d.identifier);
                    try self.walk(d.statement);
                },
                .blit => |*d| {
                    try self.walk(d.source);
                    try self.walk(d.target);
                    try self.walk(d.size);
                },
                .copy => |*d| {
                    try self.walk(d.data_type);
                    try self.walk(d.to_type);
                    try self.walk(d.from_type);
                    try self.walk(d.value);
                },
                .load => |*d| {
                    try self.walk(d.data_type);
                    try self.walk(d.memory_type);
                    try self.walk(d.source);
                },
                .store => |*d| {
                    try self.walk(d.memory_type);
                    try self.walk(d.source);
                    try self.walk(d.target);
                },
                .branch => |*d| {
                    try self.walk(d.condition);
                    try self.walk(d.true);
                    try self.walk(d.false);
                },
                .halt => {},
                .jump => |*d| {
                    try self.walk(d.identifier);
                },
                .phi => |*d| {
                    try self.walk(d.data_type);
                    try self.walk(d.parameters);
                },
                .phi_parameter => |*d| {
                    try self.walk(d.identifier);
                    try self.walk(d.value);
                },
                .@"return" => |*d| {
                    try self.walk(d.value);
                },
                .binary_operation => |*d| {
                    try self.walk(d.data_type);
                    try self.walk(d.left);
                    try self.walk(d.right);
                },
                .comparison => |*d| {
                    try self.walk(d.data_type);
                    try self.walk(d.comparison_type);
                    try self.walk(d.left);
                    try self.walk(d.right);
                },
                .negate => |*d| {
                    try self.walk(d.data_type);
                    try self.walk(d.value);
                },
            }

            try self.callback_exit(s);
        }
    };
}

//
// Test Utils
//

const test_allocator = std.testing.allocator;

const test_lib = @import("../test.zig");

const TestASTWalkCallback = struct {
    enter_types: Collection,
    exit_types: Collection,

    const Self = @This();
    const Collection = std.ArrayList(statement.Statement);

    pub fn init(allocator: std.mem.Allocator) Self {
        return Self{
            .enter_types = Collection.init(allocator),
            .exit_types = Collection.init(allocator),
        };
    }

    pub fn deinit(self: *Self) void {
        self.enter_types.deinit();
        self.exit_types.deinit();
    }

    pub fn enter(self: *Self, s: *statement.Statement) !void {
        try self.enter_types.append(s.*);
    }

    pub fn exit(self: *Self, s: *statement.Statement) !void {
        try self.exit_types.append(s.*);
    }
};

fn testEnter(buffer: anytype) ![]statement.Statement {
    var tree = try test_lib.testAST(test_allocator, buffer);
    defer tree.deinit();

    var callback = TestASTWalkCallback.init(test_allocator);
    defer callback.deinit();

    var walk = ASTWalk(@TypeOf(callback)).init(&tree, &callback);
    try walk.walk(tree.entrypoint() orelse return error.NotFound);

    return try callback.enter_types.toOwnedSlice();
}

fn assertEnter(buffer: anytype, expected: []const statement.StatementType) !void {
    const statements = try testEnter(buffer);
    defer test_allocator.free(statements);

    try test_lib.assertStatementTypes(expected, statements);
}

fn printStatements(file: []const u8) !void {
    const statements = try testEnter(file);
    defer test_allocator.free(statements);

    for (statements) |s| {
        std.log.err("{any}", .{@as(statement.StatementType, s.data)});
    }
}

//
// Valid Test
//

test "type" {
    // Arrange
    const file = "type :t1 = { w, s 100 } type :t2 = { {w} {s} }";
    const expected = [_]statement.StatementType{
        .module,
        .node,
        .type_definition,
        .identifier,
        .struct_type,
        .node,
        .primitive_type,
        .node,
        .array_type,
        .primitive_type,
        .literal,
        .node,
        .type_definition,
        .identifier,
        .union_type,
        .node,
        .struct_type,
        .node,
        .primitive_type,
        .node,
        .struct_type,
        .node,
        .primitive_type,
    };

    // Act + Assert
    try assertEnter(file, &expected);
}

test "data" {
    // Arrange
    const file = "data $d = { w 1 2, b 3 } data $d2 = { z 100 }";
    const expected = [_]statement.StatementType{
        .module,
        .node,
        .data_definition,
        .identifier,
        .node,
        .typed_data,
        .primitive_type,
        .literal,
        .node,
        .typed_data,
        .primitive_type,
        .literal,
        .node,
        .typed_data,
        .primitive_type,
        .literal,
        .node,
        .data_definition,
        .identifier,
        .node,
        .typed_data,
        .zero_type,
        .literal,
    };

    // Act + Assert
    try assertEnter(file, &expected);
}

test "function" {
    // Arrange
    const file = "function $fun(w %a, :tag %b) {@s %t =w add 1, %a %t2 =w ceqs 1, 1 @e ret}";
    const expected = [_]statement.StatementType{
        .module,
        .node,
        .function,
        .function_signature,
        .identifier,
        .node,
        .type_parameter,
        .identifier,
        .primitive_type,
        .node,
        .type_parameter,
        .identifier,
        .identifier,
        .node,
        .block,
        .identifier,
        .node,
        .assignment,
        .identifier,
        .binary_operation,
        .primitive_type,
        .literal,
        .identifier,
        .node,
        .assignment,
        .identifier,
        .comparison,
        .primitive_type,
        .primitive_type,
        .literal,
        .literal,
        .jump,
        .identifier,
        .node,
        .block,
        .identifier,
        .@"return",
    };

    // Act + Assert
    try assertEnter(file, &expected);
}
