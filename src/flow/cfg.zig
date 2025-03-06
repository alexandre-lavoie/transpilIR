const std = @import("std");

const ast = @import("../ast/lib.zig");
const common = @import("../common.zig");
const symbol = @import("../symbol/lib.zig");

const test_lib = @import("test.zig");

const ArrayHashSet = std.AutoArrayHashMap(usize, void);

pub const CFGNode = union(enum) {
    enter: usize,
    exit,
    jump: usize,
    branch: struct {
        left: usize,
        right: usize,
    },
};

pub const CFG = struct {
    allocator: std.mem.Allocator,
    nodes: NodeCollection,
    entrypoints: EntrypointCollection,
    node_predecessors: PredecessorCollection,

    const Self = @This();
    const NodeCollection = std.AutoArrayHashMap(usize, CFGNode);
    const PredecessorCollection = std.AutoArrayHashMap(usize, ArrayHashSet);
    const EntrypointCollection = std.ArrayList(usize);

    pub fn init(allocator: std.mem.Allocator) Self {
        return Self{
            .allocator = allocator,
            .nodes = NodeCollection.init(allocator),
            .entrypoints = EntrypointCollection.init(allocator),
            .node_predecessors = PredecessorCollection.init(allocator),
        };
    }

    pub fn deinit(self: *Self) void {
        self.nodes.deinit();
        self.entrypoints.deinit();

        for (self.node_predecessors.values()) |*p| p.deinit();
        self.node_predecessors.deinit();
    }

    pub fn put(self: *Self, idx: usize, node: CFGNode) !void {
        try self.nodes.put(idx, node);

        switch (node) {
            .enter => |n| {
                try self.entrypoints.append(idx);
                try self.addPredecessor(n, idx);
            },
            .jump => |n| {
                try self.addPredecessor(n, idx);
            },
            .branch => |n| {
                try self.addPredecessor(n.left, idx);
                try self.addPredecessor(n.right, idx);
            },
            .exit => {},
        }
    }

    pub fn predecessors(self: *const Self, idx: usize) ?*const ArrayHashSet {
        return self.node_predecessors.getPtr(idx);
    }

    fn addPredecessor(self: *Self, idx: usize, p: usize) !void {
        if (!self.node_predecessors.contains(idx)) {
            try self.node_predecessors.put(idx, ArrayHashSet.init(self.allocator));
        }

        try self.node_predecessors.getPtr(idx).?.put(p, undefined);
    }

    pub fn build(self: *Self, tree: *const ast.AST, symbol_table: *const symbol.SymbolTable) !void {
        var callback = CFGWalkCallback.init(
            self.allocator,
            symbol_table,
            self,
        );
        defer callback.deinit();

        var walk = ast.ASTWalk.init(self.allocator, tree);
        defer walk.deinit();

        try walk.start(tree.entrypoint() orelse return error.NotFound);
        while (try walk.next()) |out| {
            try switch (out.enter) {
                true => callback.enter(out.index, out.value),
                false => callback.exit(out.value),
            };
        }
    }
};

const CFGWalkState = enum {
    // Default/null state
    default,
    // When a function is entered
    function_enter,
    // When the first block is entered
    block_entry,
    // When subsequent blocks are entered
    block_enter,
    // When inside a block's body
    block_body,
};

pub const CFGWalkCallback = struct {
    symbol_table: *const symbol.SymbolTable,
    ast_cfg: *CFG,

    state: CFGWalkState = .default,
    scope: usize = 0,
    function: usize = 0,
    block: usize = 0,

    symbol_cfg: CFG,
    symbol_statement_map: SymbolStatementMap,
    labels: LabelCollection,

    const Self = @This();
    const LabelCollection = std.ArrayList(usize);
    const SymbolStatementMap = std.AutoArrayHashMap(usize, usize);

    pub fn init(allocator: std.mem.Allocator, symbol_table: *const symbol.SymbolTable, ast_cfg: *CFG) Self {
        return .{
            .symbol_table = symbol_table,
            .ast_cfg = ast_cfg,
            .symbol_cfg = CFG.init(allocator),
            .symbol_statement_map = SymbolStatementMap.init(allocator),
            .labels = LabelCollection.init(allocator),
        };
    }

    pub fn deinit(self: *Self) void {
        self.labels.deinit();
        self.symbol_cfg.deinit();
        self.symbol_statement_map.deinit();
    }

    pub fn enter(self: *Self, index: ast.StatementIndex, statement: *const ast.Statement) !void {
        const instance: symbol.Instance = .{ .span = statement.span };

        switch (statement.data) {
            .function_signature => {
                self.state = .function_enter;
            },
            .identifier => |identifier| switch (identifier.scope) {
                .global => switch (self.state) {
                    .function_enter => {
                        const sym = self.symbol_table.getSymbolIndexByInstance(&instance) orelse return error.NotFound;

                        try self.symbol_statement_map.put(sym, self.function);

                        self.scope = sym;
                        self.state = .block_entry;
                    },
                    else => {},
                },
                .label => {
                    const sym = self.symbol_table.getSymbolIndexByInstance(&instance) orelse return error.NotFound;

                    switch (self.state) {
                        .block_entry => {
                            try self.symbol_cfg.put(self.scope, .{ .enter = sym });

                            try self.symbol_statement_map.put(sym, self.block);

                            self.scope = sym;
                            self.state = .block_body;
                        },
                        .block_enter => {
                            try self.symbol_statement_map.put(sym, self.block);

                            self.scope = sym;
                            self.state = .block_body;
                        },
                        .block_body => try self.labels.append(sym),
                        else => unreachable,
                    }
                },
                else => {},
            },
            .function => {
                self.function = index;
            },
            .block => {
                self.block = index;

                switch (self.state) {
                    .block_entry => {},
                    else => self.state = .block_enter,
                }
            },
            else => {},
        }
    }

    pub fn exit(self: *Self, statement: *const ast.Statement) !void {
        switch (statement.data) {
            .phi => self.labels.clearAndFree(),
            .halt, .@"return" => try self.symbol_cfg.put(self.scope, .exit),
            .jump => {
                try self.symbol_cfg.put(self.scope, .{ .jump = self.labels.items[0] });

                self.labels.clearAndFree();
            },
            .branch => {
                const left = self.labels.items[0];
                const right = self.labels.items[1];

                const node: CFGNode = switch (left == right) {
                    true => .{ .jump = left },
                    false => .{ .branch = .{
                        .left = left,
                        .right = right,
                    } },
                };
                try self.symbol_cfg.put(self.scope, node);

                self.labels.clearAndFree();
            },
            .module => try self.buildASTCFG(),
            else => {},
        }
    }

    fn buildASTCFG(self: *Self) !void {
        var iter = self.symbol_cfg.nodes.iterator();

        while (iter.next()) |entry| {
            const pidx: usize = entry.key_ptr.*;
            const pnode: CFGNode = entry.value_ptr.*;

            const idx = self.symbol_statement_map.get(pidx).?;

            const node: CFGNode = switch (pnode) {
                .enter => |n| .{ .enter = self.symbol_statement_map.get(n).? },
                .jump => |n| .{ .jump = self.symbol_statement_map.get(n).? },
                .branch => |n| .{ .branch = .{
                    .left = self.symbol_statement_map.get(n.left).?,
                    .right = self.symbol_statement_map.get(n.right).?,
                } },
                .exit => .exit,
            };

            try self.ast_cfg.put(idx, node);
        }
    }
};

//
// Test
//

test "enter" {
    // Arrange
    const allocator = std.testing.allocator;

    const file = "function $fun() {@s ret}";

    var tree = try test_lib.testAST(allocator, file);
    defer tree.deinit();

    var symbol_table = symbol.SymbolTable.init(allocator);
    defer symbol_table.deinit();

    var cfg = CFG.init(allocator);
    defer cfg.deinit();

    // Act
    try test_lib.testCFG(allocator, file, &tree, &symbol_table, &test_lib.test_target, &cfg);

    // Assert

    // fun
    try std.testing.expectEqualDeep(
        CFGNode{ .enter = 6 },
        cfg.nodes.get(8),
    );

    // s
    try std.testing.expectEqualDeep(
        .exit,
        cfg.nodes.get(6),
    );
}

test "entrypoints" {
    // Arrange
    const allocator = std.testing.allocator;

    const file = "function $f0() {@s ret} function $f1() {@s ret}";

    var tree = try test_lib.testAST(allocator, file);
    defer tree.deinit();

    var symbol_table = symbol.SymbolTable.init(allocator);
    defer symbol_table.deinit();

    var cfg = CFG.init(allocator);
    defer cfg.deinit();

    // Act
    try test_lib.testCFG(allocator, file, &tree, &symbol_table, &test_lib.test_target, &cfg);

    // Assert

    try std.testing.expectEqualSlices(
        usize,
        &[_]usize{ 8, 18 },
        cfg.entrypoints.items,
    );
}

test "jump" {
    // Arrange
    const allocator = std.testing.allocator;

    const file = "function $fun() {@s jmp @e @e ret}";

    var tree = try test_lib.testAST(allocator, file);
    defer tree.deinit();

    var symbol_table = symbol.SymbolTable.init(allocator);
    defer symbol_table.deinit();

    var cfg = CFG.init(allocator);
    defer cfg.deinit();

    // Act
    try test_lib.testCFG(allocator, file, &tree, &symbol_table, &test_lib.test_target, &cfg);

    // Assert

    // fun
    try std.testing.expectEqualDeep(
        CFGNode{ .enter = 7 },
        cfg.nodes.get(13),
    );

    // s
    try std.testing.expectEqualDeep(
        CFGNode{ .jump = 11 },
        cfg.nodes.get(7),
    );

    // e
    try std.testing.expectEqualDeep(
        .exit,
        cfg.nodes.get(11),
    );
}

test "branch" {
    // Arrange
    const allocator = std.testing.allocator;

    const file = "function $fun() {@s jnz 0, @t, @f @t @f ret}";

    var tree = try test_lib.testAST(allocator, file);
    defer tree.deinit();

    var symbol_table = symbol.SymbolTable.init(allocator);
    defer symbol_table.deinit();

    var cfg = CFG.init(allocator);
    defer cfg.deinit();

    // Act
    try test_lib.testCFG(allocator, file, &tree, &symbol_table, &test_lib.test_target, &cfg);

    // Assert

    // fun
    try std.testing.expectEqualDeep(
        CFGNode{ .enter = 9 },
        cfg.nodes.get(20),
    );

    // s
    try std.testing.expectEqualDeep(
        CFGNode{
            .branch = .{
                .left = 14,
                .right = 18,
            },
        },
        cfg.nodes.get(9),
    );

    // t
    try std.testing.expectEqualDeep(
        CFGNode{
            .jump = 18,
        },
        cfg.nodes.get(14),
    );

    // f
    try std.testing.expectEqualDeep(
        .exit,
        cfg.nodes.get(18),
    );
}

test "jump loop" {
    // Arrange
    const allocator = std.testing.allocator;

    const file = "function $fun() {@s jmp @s}";

    var tree = try test_lib.testAST(allocator, file);
    defer tree.deinit();

    var symbol_table = symbol.SymbolTable.init(allocator);
    defer symbol_table.deinit();

    var cfg = CFG.init(allocator);
    defer cfg.deinit();

    // Act
    try test_lib.testCFG(allocator, file, &tree, &symbol_table, &test_lib.test_target, &cfg);

    // Assert

    // fun
    try std.testing.expectEqualDeep(
        CFGNode{ .enter = 7 },
        cfg.nodes.get(9),
    );

    // s
    try std.testing.expectEqualDeep(
        CFGNode{ .jump = 7 },
        cfg.nodes.get(7),
    );
}

test "branch label reused" {
    // Arrange
    const allocator = std.testing.allocator;

    const file = "function $fun() {@s jnz 0, @e, @e @e ret}";

    var tree = try test_lib.testAST(allocator, file);
    defer tree.deinit();

    var symbol_table = symbol.SymbolTable.init(allocator);
    defer symbol_table.deinit();

    var cfg = CFG.init(allocator);
    defer cfg.deinit();

    // Act
    try test_lib.testCFG(allocator, file, &tree, &symbol_table, &test_lib.test_target, &cfg);

    // Assert

    // fun
    try std.testing.expectEqualDeep(
        CFGNode{ .enter = 9 },
        cfg.nodes.get(15),
    );

    // s
    try std.testing.expectEqualDeep(
        CFGNode{ .jump = 13 },
        cfg.nodes.get(9),
    );

    // e
    try std.testing.expectEqualDeep(
        .exit,
        cfg.nodes.get(13),
    );
}

test "multiple jumps" {
    // Arrange
    const allocator = std.testing.allocator;

    const file = "function $fun() {@s jmp @l @l jmp @e @e ret}";

    var tree = try test_lib.testAST(allocator, file);
    defer tree.deinit();

    var symbol_table = symbol.SymbolTable.init(allocator);
    defer symbol_table.deinit();

    var cfg = CFG.init(allocator);
    defer cfg.deinit();

    // Act
    try test_lib.testCFG(allocator, file, &tree, &symbol_table, &test_lib.test_target, &cfg);

    // Assert

    // fun
    try std.testing.expectEqualDeep(
        CFGNode{ .enter = 7 },
        cfg.nodes.get(18),
    );

    // s
    try std.testing.expectEqualDeep(
        CFGNode{ .jump = 12 },
        cfg.nodes.get(7),
    );

    // l
    try std.testing.expectEqualDeep(
        CFGNode{ .jump = 16 },
        cfg.nodes.get(12),
    );

    // e
    try std.testing.expectEqualDeep(
        .exit,
        cfg.nodes.get(16),
    );
}
