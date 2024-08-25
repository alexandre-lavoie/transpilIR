const std = @import("std");

const ast = @import("ast.zig");
const statement = @import("statement.zig");

const ASTWalkEntry = struct {
    index: statement.StatementIndex,
    enter: bool = true,
};

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

        pub fn walk(self: *Self, allocator: std.mem.Allocator, start_index: statement.StatementIndex) !void {
            var queue = std.ArrayList(ASTWalkEntry).init(allocator);
            defer queue.deinit();

            var stack = std.ArrayList(ASTWalkEntry).init(allocator);
            defer stack.deinit();

            try queue.append(.{ .index = start_index });

            while (true) {
                const entry = queue.popOrNull() orelse break;

                const stat = self.tree.getPtr(entry.index) orelse return error.NotFound;

                if (!entry.enter) {
                    try self.callback_exit(stat);

                    continue;
                }

                try self.callback_enter(stat);

                var exit_after = true;
                switch (stat.data) {
                    .identifier => {},
                    .literal => {},
                    .linkage => |*d| {
                        if (d.section) |section| try stack.append(.{ .index = section });
                        if (d.flags) |flags| try stack.append(.{ .index = flags });
                    },
                    .node => |*d| {
                        try stack.append(.{ .index = d.value });

                        exit_after = false;
                        try stack.append(.{ .index = entry.index, .enter = false });

                        if (d.next) |next| try stack.append(.{ .index = next });
                    },
                    .module => |*d| {
                        if (d.types) |types| try stack.append(.{ .index = types });
                        if (d.data) |data| try stack.append(.{ .index = data });
                        if (d.functions) |functions| try stack.append(.{ .index = functions });
                    },
                    .data_definition => |*d| {
                        try stack.append(.{ .index = d.identifier });
                        try stack.append(.{ .index = d.linkage });
                        try stack.append(.{ .index = d.values });
                    },
                    .typed_data => |*d| {
                        try stack.append(.{ .index = d.type });
                        try stack.append(.{ .index = d.value });
                    },
                    .offset => |*d| {
                        try stack.append(.{ .index = d.identifier });
                        try stack.append(.{ .index = d.value });
                    },
                    .array_type => |*d| {
                        try stack.append(.{ .index = d.item });
                        try stack.append(.{ .index = d.count });
                    },
                    .env_type => {},
                    .opaque_type => |*d| {
                        if (d.alignment) |alignment| try stack.append(.{ .index = alignment });
                        try stack.append(.{ .index = d.size });
                    },
                    .primitive_type => {},
                    .struct_type => |*d| {
                        if (d.alignment) |alignment| try stack.append(.{ .index = alignment });
                        try stack.append(.{ .index = d.members });
                    },
                    .type_definition => |*d| {
                        try stack.append(.{ .index = d.identifier });
                        try stack.append(.{ .index = d.type });
                    },
                    .union_type => |*d| {
                        if (d.alignment) |alignment| try stack.append(.{ .index = alignment });
                        try stack.append(.{ .index = d.types });
                    },
                    .zero_type => {},
                    .block => |*d| {
                        try stack.append(.{ .index = d.label });
                        if (d.phi_statements) |phi_statements| try stack.append(.{ .index = phi_statements });
                        if (d.statements) |statements| try stack.append(.{ .index = statements });
                        try stack.append(.{ .index = d.flow_statement });
                    },
                    .call => |*d| {
                        try stack.append(.{ .index = d.target });
                        try stack.append(.{ .index = d.return_type });
                        if (d.parameters) |parameters| try stack.append(.{ .index = parameters });
                    },
                    .function => |*d| {
                        try stack.append(.{ .index = d.signature });
                        try stack.append(.{ .index = d.body });
                    },
                    .function_signature => |*d| {
                        try stack.append(.{ .index = d.name });
                        try stack.append(.{ .index = d.linkage });
                        try stack.append(.{ .index = d.return_type });
                        if (d.parameters) |parameters| try stack.append(.{ .index = parameters });
                    },
                    .type_parameter => |*d| {
                        try stack.append(.{ .index = d.value });
                        try stack.append(.{ .index = d.type_statement });
                    },
                    .variadic_parameter => {},
                    .vaarg => |*d| {
                        try stack.append(.{ .index = d.data_type });
                        try stack.append(.{ .index = d.parameter });
                    },
                    .vastart => |*d| {
                        try stack.append(.{ .index = d.parameter });
                    },
                    .allocate => |*d| {
                        try stack.append(.{ .index = d.data_type });
                        try stack.append(.{ .index = d.alignment });
                        try stack.append(.{ .index = d.size });
                    },
                    .assignment => |*d| {
                        try stack.append(.{ .index = d.identifier });
                        try stack.append(.{ .index = d.statement });
                    },
                    .blit => |*d| {
                        try stack.append(.{ .index = d.source });
                        try stack.append(.{ .index = d.target });
                        try stack.append(.{ .index = d.size });
                    },
                    .copy => |*d| {
                        try stack.append(.{ .index = d.data_type });
                        try stack.append(.{ .index = d.to_type });
                        if (d.from_type) |from_type| try stack.append(.{ .index = from_type });
                        try stack.append(.{ .index = d.value });
                    },
                    .load => |*d| {
                        try stack.append(.{ .index = d.data_type });
                        try stack.append(.{ .index = d.memory_type });
                        try stack.append(.{ .index = d.source });
                    },
                    .store => |*d| {
                        try stack.append(.{ .index = d.memory_type });
                        try stack.append(.{ .index = d.source });
                        try stack.append(.{ .index = d.target });
                    },
                    .branch => |*d| {
                        try stack.append(.{ .index = d.condition });
                        try stack.append(.{ .index = d.true });
                        try stack.append(.{ .index = d.false });
                    },
                    .halt => {},
                    .jump => |*d| {
                        try stack.append(.{ .index = d.identifier });
                    },
                    .phi => |*d| {
                        try stack.append(.{ .index = d.data_type });
                        try stack.append(.{ .index = d.parameters });
                    },
                    .phi_parameter => |*d| {
                        try stack.append(.{ .index = d.identifier });
                        try stack.append(.{ .index = d.value });
                    },
                    .@"return" => |*d| {
                        if (d.value) |value| try stack.append(.{ .index = value });
                    },
                    .binary_operation => |*d| {
                        try stack.append(.{ .index = d.data_type });
                        try stack.append(.{ .index = d.left });
                        try stack.append(.{ .index = d.right });
                    },
                    .comparison => |*d| {
                        try stack.append(.{ .index = d.data_type });
                        try stack.append(.{ .index = d.comparison_type });
                        try stack.append(.{ .index = d.left });
                        try stack.append(.{ .index = d.right });
                    },
                    .negate => |*d| {
                        try stack.append(.{ .index = d.data_type });
                        try stack.append(.{ .index = d.value });
                    },
                }

                if (exit_after) {
                    try stack.append(.{ .index = entry.index, .enter = false });
                }

                while (true) {
                    try queue.append(stack.popOrNull() orelse break);
                }
            }
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
    try walk.walk(test_allocator, tree.entrypoint() orelse return error.NotFound);

    return try callback.enter_types.toOwnedSlice();
}

fn assertEnter(buffer: anytype, expected: []const statement.StatementType) !void {
    const statements = try testEnter(buffer);
    defer test_allocator.free(statements);

    try test_lib.assertStatementTypes(test_allocator, expected, statements);
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
        .linkage,
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
        .linkage,
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
        .linkage,
        .primitive_type,
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
