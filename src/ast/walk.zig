const std = @import("std");

const ast = @import("ast.zig");
const statement = @import("statement.zig");

const test_lib = @import("../test/lib.zig");

const ASTWalkEntry = struct {
    index: statement.StatementIndex,
    enter: bool = true,
};

pub const ASTWalkOutputState = enum {
    enter,
    middle,
    exit,
};

pub const ASTWalkOutput = struct {
    state: ASTWalkOutputState,
    index: statement.StatementIndex,
    previous: ?statement.StatementIndex = null,
    next: ?statement.StatementIndex = null,
};

pub const ASTWalk = struct {
    tree: *const ast.AST,
    queue: EntryList,
    stack: EntryList,
    scope: ScopeStack,

    previous: ?statement.StatementIndex = null,

    const Self = @This();
    const EntryList = std.ArrayList(ASTWalkEntry);
    const ScopeStack = std.ArrayList(usize);

    pub fn init(allocator: std.mem.Allocator, tree: *const ast.AST) Self {
        return Self{
            .tree = tree,
            .queue = EntryList.init(allocator),
            .stack = EntryList.init(allocator),
            .scope = ScopeStack.init(allocator),
        };
    }

    pub fn deinit(self: *Self) void {
        self.queue.deinit();
        self.stack.deinit();
        self.scope.deinit();
    }

    pub fn start(self: *Self, index: statement.StatementIndex) !void {
        try self.queue.append(.{ .index = index });
    }

    pub fn next(self: *Self) !?ASTWalkOutput {
        const peek = self.queue.getLastOrNull() orelse return null;

        if (!peek.enter) {
            const entry = self.queue.pop();

            self.previous = entry.index;

            _ = self.scope.popOrNull() orelse return error.EmptyScope;

            return .{
                .state = .exit,
                .index = entry.index,
            };
        }

        if (self.previous) |p| {
            self.previous = null;

            const index = self.scope.getLastOrNull() orelse return null;

            return .{
                .state = .middle,
                .index = index,
                .previous = p,
                .next = peek.index,
            };
        }

        const entry = self.queue.pop();

        try self.scope.append(entry.index);

        const stat = self.tree.getPtr(entry.index) orelse return error.NotFound;

        var exit_after = true;
        switch (stat.data) {
            .identifier,
            .literal,
            => {},
            .linkage => |*d| {
                if (d.section) |section| try self.stack.append(.{ .index = section });
                if (d.flags) |flags| try self.stack.append(.{ .index = flags });
            },
            .node => |*d| {
                try self.stack.append(.{ .index = d.value });

                exit_after = false;
                try self.stack.append(.{ .index = entry.index, .enter = false });

                if (d.next) |index| try self.stack.append(.{ .index = index });
            },
            .module => |*d| {
                if (d.types) |types| try self.stack.append(.{ .index = types });
                if (d.data) |data| try self.stack.append(.{ .index = data });
                if (d.functions) |functions| try self.stack.append(.{ .index = functions });
            },
            .data_definition => |*d| {
                try self.stack.append(.{ .index = d.identifier });
                try self.stack.append(.{ .index = d.linkage });
                if (d.alignment) |a| try self.stack.append(.{ .index = a });
                try self.stack.append(.{ .index = d.values });
            },
            .typed_data => |*d| {
                try self.stack.append(.{ .index = d.type });
                try self.stack.append(.{ .index = d.value });
            },
            .offset => |*d| {
                try self.stack.append(.{ .index = d.identifier });
                try self.stack.append(.{ .index = d.value });
            },
            .array_type => |*d| {
                try self.stack.append(.{ .index = d.item });
                try self.stack.append(.{ .index = d.count });
            },
            .env_type => {},
            .opaque_type => |*d| {
                if (d.alignment) |alignment| try self.stack.append(.{ .index = alignment });
                try self.stack.append(.{ .index = d.size });
            },
            .primitive_type => {},
            .struct_type => |*d| {
                if (d.alignment) |alignment| try self.stack.append(.{ .index = alignment });
                if (d.members) |members| try self.stack.append(.{ .index = members });
            },
            .type_definition => |*d| {
                try self.stack.append(.{ .index = d.identifier });
                try self.stack.append(.{ .index = d.type });
            },
            .union_type => |*d| {
                if (d.alignment) |alignment| try self.stack.append(.{ .index = alignment });
                try self.stack.append(.{ .index = d.types });
            },
            .zero_type => {},
            .block => |*d| {
                try self.stack.append(.{ .index = d.label });
                if (d.phis) |phis| try self.stack.append(.{ .index = phis });
                if (d.lines) |lines| try self.stack.append(.{ .index = lines });
                try self.stack.append(.{ .index = d.flow });
            },
            .debug_file => |*d| {
                try self.stack.append(.{ .index = d.path });
            },
            .debug_location => |*d| {
                try self.stack.append(.{ .index = d.line });
                try self.stack.append(.{ .index = d.column });
            },
            .line => |d| {
                try self.stack.append(.{ .index = d });
            },
            .call => |*d| {
                try self.stack.append(.{ .index = d.target });
                try self.stack.append(.{ .index = d.return_type });
                if (d.parameters) |parameters| try self.stack.append(.{ .index = parameters });
            },
            .function => |*d| {
                try self.stack.append(.{ .index = d.signature });
                try self.stack.append(.{ .index = d.body });
            },
            .function_signature => |*d| {
                try self.stack.append(.{ .index = d.name });
                try self.stack.append(.{ .index = d.linkage });
                try self.stack.append(.{ .index = d.return_type });
                if (d.parameters) |parameters| try self.stack.append(.{ .index = parameters });
            },
            .function_parameter => |*d| {
                try self.stack.append(.{ .index = d.value });
                try self.stack.append(.{ .index = d.type });
            },
            .call_parameter => |*d| {
                try self.stack.append(.{ .index = d.value });
                try self.stack.append(.{ .index = d.type });
            },
            .variadic_parameter => {},
            .vaarg => |*d| {
                try self.stack.append(.{ .index = d.data_type });
                try self.stack.append(.{ .index = d.parameter });
            },
            .vastart => |*d| {
                try self.stack.append(.{ .index = d.parameter });
            },
            .allocate => |*d| {
                try self.stack.append(.{ .index = d.data_type });
                try self.stack.append(.{ .index = d.alignment });
                try self.stack.append(.{ .index = d.size });
            },
            .assignment => |*d| {
                try self.stack.append(.{ .index = d.identifier });
                try self.stack.append(.{ .index = d.statement });
            },
            .blit => |*d| {
                try self.stack.append(.{ .index = d.source });
                try self.stack.append(.{ .index = d.target });
                try self.stack.append(.{ .index = d.size });
            },
            .convert => |*d| {
                try self.stack.append(.{ .index = d.data_type });
                try self.stack.append(.{ .index = d.to_type });
                try self.stack.append(.{ .index = d.from_type });
                try self.stack.append(.{ .index = d.value });
            },
            .copy => |*d| {
                try self.stack.append(.{ .index = d.data_type });
                try self.stack.append(.{ .index = d.value });
            },
            .cast => |*d| {
                try self.stack.append(.{ .index = d.data_type });
                try self.stack.append(.{ .index = d.value });
            },
            .load => |*d| {
                try self.stack.append(.{ .index = d.data_type });
                try self.stack.append(.{ .index = d.memory_type });
                try self.stack.append(.{ .index = d.address });
            },
            .store => |*d| {
                try self.stack.append(.{ .index = d.memory_type });
                try self.stack.append(.{ .index = d.value });
                try self.stack.append(.{ .index = d.address });
            },
            .branch => |*d| {
                try self.stack.append(.{ .index = d.condition });
                try self.stack.append(.{ .index = d.true });
                try self.stack.append(.{ .index = d.false });
            },
            .halt => {},
            .jump => |*d| {
                try self.stack.append(.{ .index = d.identifier });
            },
            .phi => |*d| {
                try self.stack.append(.{ .index = d.data_type });
                if (d.parameters) |p| try self.stack.append(.{ .index = p });
            },
            .phi_parameter => |*d| {
                try self.stack.append(.{ .index = d.identifier });
                try self.stack.append(.{ .index = d.value });
            },
            .@"return" => |*d| {
                if (d.value) |value| try self.stack.append(.{ .index = value });
            },
            .binary_operation => |*d| {
                try self.stack.append(.{ .index = d.data_type });
                try self.stack.append(.{ .index = d.left });
                try self.stack.append(.{ .index = d.right });
            },
            .comparison => |*d| {
                try self.stack.append(.{ .index = d.data_type });
                try self.stack.append(.{ .index = d.comparison_type });
                try self.stack.append(.{ .index = d.left });
                try self.stack.append(.{ .index = d.right });
            },
            .negate => |*d| {
                try self.stack.append(.{ .index = d.data_type });
                try self.stack.append(.{ .index = d.value });
            },
        }

        if (exit_after) {
            try self.stack.append(.{ .index = entry.index, .enter = false });
        }

        while (self.stack.popOrNull()) |e| {
            try self.queue.append(e);
        }

        return .{
            .state = .enter,
            .index = entry.index,
        };
    }
};

//
// Test Utils
//

fn testEnter(allocator: std.mem.Allocator, buffer: anytype) ![]statement.Statement {
    var tree = try test_lib.testAST(allocator, buffer);
    defer tree.deinit();

    var statements = std.ArrayList(statement.Statement).init(allocator);
    defer statements.deinit();

    var walk = ASTWalk.init(allocator, &tree);
    defer walk.deinit();

    try walk.start(tree.entrypoint() orelse return error.NotFound);
    while (try walk.next()) |out| {
        const stat = tree.get(out.index) orelse return error.NotFound;

        try switch (out.state) {
            .enter => statements.append(stat),
            else => {},
        };
    }

    return try statements.toOwnedSlice();
}

fn assertEnter(allocator: std.mem.Allocator, buffer: anytype, expected: []const statement.StatementType) !void {
    const statements = try testEnter(allocator, buffer);
    defer allocator.free(statements);

    try test_lib.assertStatementTypes(allocator, expected, statements);
}

fn testMiddle(allocator: std.mem.Allocator, buffer: anytype) ![]statement.Statement {
    var tree = try test_lib.testAST(allocator, buffer);
    defer tree.deinit();

    var statements = std.ArrayList(statement.Statement).init(allocator);
    defer statements.deinit();

    var walk = ASTWalk.init(allocator, &tree);
    defer walk.deinit();

    try walk.start(tree.entrypoint() orelse return error.NotFound);
    while (try walk.next()) |out| {
        const stat = tree.get(out.index) orelse return error.NotFound;

        try switch (out.state) {
            .middle => statements.append(stat),
            else => {},
        };
    }

    return try statements.toOwnedSlice();
}

fn assertMiddle(allocator: std.mem.Allocator, buffer: anytype, expected: []const statement.StatementType) !void {
    const statements = try testMiddle(allocator, buffer);
    defer allocator.free(statements);

    try test_lib.assertStatementTypes(allocator, expected, statements);
}

//
// Valid Test
//

test "type" {
    // Arrange
    const allocator = std.testing.allocator;

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
    try assertEnter(allocator, file, &expected);
}

test "data" {
    // Arrange
    const allocator = std.testing.allocator;

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
    try assertEnter(allocator, file, &expected);
}

test "function" {
    // Arrange
    const allocator = std.testing.allocator;

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
        .function_parameter,
        .identifier,
        .primitive_type,
        .node,
        .function_parameter,
        .identifier,
        .identifier,
        .node,
        .block,
        .identifier,
        .node,
        .line,
        .assignment,
        .identifier,
        .binary_operation,
        .primitive_type,
        .literal,
        .identifier,
        .node,
        .line,
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
    try assertEnter(allocator, file, &expected);
}

test "middle" {
    // Arrange
    const allocator = std.testing.allocator;

    const file = "function $fun(w %a, :tag %b) {@s %t =w add 1, %a %t2 =w ceqs 1, 1 @e ret}";
    const expected = [_]statement.StatementType{
        .function_signature,
        .function_signature,
        .function_signature,
        .function_parameter,
        .function_signature,
        .function_parameter,
        .function,
        .block,
        .assignment,
        .binary_operation,
        .binary_operation,
        .block,
        .assignment,
        .comparison,
        .comparison,
        .comparison,
        .block,
        .function,
        .block,
    };

    // Act + Assert
    try assertMiddle(allocator, file, &expected);
}
