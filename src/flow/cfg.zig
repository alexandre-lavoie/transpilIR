const std = @import("std");

const ast = @import("../ast/lib.zig");
const common = @import("../common.zig");
const symbol = @import("../symbol/lib.zig");

const test_lib = @import("test.zig");

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
    nodes: NodeCollection,
    entrypoints: EntrypointCollection,

    const Self = @This();
    const NodeCollection = std.AutoArrayHashMap(usize, CFGNode);
    const EntrypointCollection = std.ArrayList(usize);

    pub fn init(allocator: std.mem.Allocator) Self {
        return Self{
            .nodes = NodeCollection.init(allocator),
            .entrypoints = EntrypointCollection.init(allocator),
        };
    }

    pub fn deinit(self: *Self) void {
        self.nodes.deinit();
        self.entrypoints.deinit();
    }

    pub fn put(self: *Self, sym: usize, node: CFGNode) !void {
        try self.nodes.put(sym, node);
        if (node == .enter) {
            try self.entrypoints.append(sym);
        }
    }
};

const CFGWalkState = enum {
    default,
    function_enter,
    block_entry,
    block_enter,
    block_body,
};

pub const CFGWalkCallback = struct {
    symbol_table: *const symbol.SymbolTable,
    cfg: *CFG,
    labels: LabelCollection,

    state: CFGWalkState = .default,
    scope: usize = 0,

    const Self = @This();
    const LabelCollection = std.ArrayList(usize);

    pub fn init(allocator: std.mem.Allocator, symbol_table: *const symbol.SymbolTable, cfg: *CFG) Self {
        return .{
            .symbol_table = symbol_table,
            .cfg = cfg,
            .labels = LabelCollection.init(allocator),
        };
    }

    pub fn deinit(self: *Self) void {
        self.labels.deinit();
    }

    pub fn enter(self: *Self, statement: *ast.Statement) !void {
        const instance: symbol.Instance = .{ .span = statement.span };

        switch (statement.data) {
            .function_signature => {
                self.state = .function_enter;
            },
            .identifier => |identifier| switch (identifier.scope) {
                .global => switch (self.state) {
                    .function_enter => {
                        self.scope = self.symbol_table.getSymbolIndexByInstance(&instance) orelse return error.NotFound;
                        self.state = .block_entry;
                    },
                    else => {},
                },
                .label => {
                    const sym = self.symbol_table.getSymbolIndexByInstance(&instance) orelse return error.NotFound;

                    switch (self.state) {
                        .block_entry => {
                            try self.cfg.put(self.scope, .{ .enter = sym });
                            self.scope = sym;
                            self.state = .block_body;
                        },
                        .block_enter => {
                            self.scope = sym;
                            self.state = .block_body;
                        },
                        .block_body => try self.labels.append(sym),
                        else => unreachable,
                    }
                },
                else => {},
            },
            .block => switch (self.state) {
                .block_entry => {},
                else => self.state = .block_enter,
            },
            else => {},
        }
    }

    pub fn exit(self: *Self, statement: *ast.Statement) !void {
        switch (statement.data) {
            .phi => self.labels.clearAndFree(),
            .halt, .@"return" => try self.cfg.put(self.scope, .exit),
            .jump => {
                try self.cfg.put(self.scope, .{ .jump = self.labels.items[0] });
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
                try self.cfg.put(self.scope, node);

                self.labels.clearAndFree();
            },
            else => {},
        }
    }
};

//
// Test Utils
//

const test_target: common.Target = .{
    .arch = .a64,
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
    try test_lib.testCFG(allocator, file, &tree, &symbol_table, &test_target, &cfg);

    // Assert

    // fun
    try std.testing.expectEqualDeep(
        CFGNode{ .enter = 1 },
        cfg.nodes.get(0),
    );

    // s
    try std.testing.expectEqualDeep(
        .exit,
        cfg.nodes.get(1),
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
    try test_lib.testCFG(allocator, file, &tree, &symbol_table, &test_target, &cfg);

    // Assert
    try std.testing.expectEqualSlices(
        usize,
        &[_]usize{ 0, 2 },
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
    try test_lib.testCFG(allocator, file, &tree, &symbol_table, &test_target, &cfg);

    // Assert

    // fun
    try std.testing.expectEqualDeep(
        CFGNode{ .enter = 1 },
        cfg.nodes.get(0),
    );

    // s
    try std.testing.expectEqualDeep(
        CFGNode{ .jump = 2 },
        cfg.nodes.get(1),
    );

    // e
    try std.testing.expectEqualDeep(
        .exit,
        cfg.nodes.get(2),
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
    try test_lib.testCFG(allocator, file, &tree, &symbol_table, &test_target, &cfg);

    // Assert

    // fun
    try std.testing.expectEqualDeep(
        CFGNode{ .enter = 1 },
        cfg.nodes.get(0),
    );

    // s
    try std.testing.expectEqualDeep(
        CFGNode{
            .branch = .{
                .left = 2,
                .right = 3,
            },
        },
        cfg.nodes.get(1),
    );

    // t
    try std.testing.expectEqualDeep(
        CFGNode{
            .jump = 3,
        },
        cfg.nodes.get(2),
    );

    // f
    try std.testing.expectEqualDeep(
        .exit,
        cfg.nodes.get(3),
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
    try test_lib.testCFG(allocator, file, &tree, &symbol_table, &test_target, &cfg);

    // Assert

    // fun
    try std.testing.expectEqualDeep(
        CFGNode{ .enter = 1 },
        cfg.nodes.get(0),
    );

    // s
    try std.testing.expectEqualDeep(
        CFGNode{ .jump = 1 },
        cfg.nodes.get(1),
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
    try test_lib.testCFG(allocator, file, &tree, &symbol_table, &test_target, &cfg);

    // Assert

    // fun
    try std.testing.expectEqualDeep(
        CFGNode{ .enter = 1 },
        cfg.nodes.get(0),
    );

    // s
    try std.testing.expectEqualDeep(
        CFGNode{ .jump = 2 },
        cfg.nodes.get(1),
    );

    // e
    try std.testing.expectEqualDeep(
        .exit,
        cfg.nodes.get(2),
    );
}
