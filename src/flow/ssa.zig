const std = @import("std");

const ast = @import("../ast/lib.zig");
const cfg = @import("cfg.zig");
const dom = @import("dominance.zig");
const editor = @import("../editor.zig");
const symbol = @import("../symbol/lib.zig");

const test_lib = @import("test.zig");

const ArrayHashSet = std.AutoArrayHashMap(usize, void);

const BlockSet = std.AutoArrayHashMap(usize, usize);
const BlockSets = std.AutoArrayHashMap(usize, BlockSet);

const IdentifierBlocks = std.AutoArrayHashMap(usize, usize);

const SymbolCounter = std.AutoArrayHashMap(usize, usize);

pub const SSA = struct {
    allocator: std.mem.Allocator,
    ast: *ast.AST,
    symbol_table: *symbol.SymbolTable,
    editor: editor.DataEditor,

    const Self = @This();

    pub fn init(allocator: std.mem.Allocator, tree: *ast.AST, symbol_table: *symbol.SymbolTable) Self {
        return .{
            .allocator = allocator,
            .ast = tree,
            .symbol_table = symbol_table,
            .editor = editor.DataEditor.init(tree, symbol_table),
        };
    }

    pub fn deinit(self: *Self) void {
        _ = self;
    }

    pub fn build(self: *Self) !void {
        var ast_walk = ast.ASTWalk.init(self.allocator, self.ast);
        defer ast_walk.deinit();

        var graph = cfg.CFG.init(self.allocator);
        defer graph.deinit();

        var graph_walk = cfg.CFGWalkCallback.init(
            self.allocator,
            self.symbol_table,
            &graph,
        );
        defer graph_walk.deinit();

        try ast_walk.start(self.ast.entrypoint() orelse return error.NotFound);
        while (try ast_walk.next()) |out| {
            try switch (out.enter) {
                true => graph_walk.enter(out.index, out.value),
                false => graph_walk.exit(out.value),
            };
        }

        var dom_sets = dom.DomSets.init(self.allocator);
        defer dom_sets.deinit();
        try dom_sets.build(&graph);

        var dom_trees = dom.DomTrees.init(self.allocator);
        defer dom_trees.deinit();
        try dom_trees.build(&dom_sets);

        var identifier_blocks = IdentifierBlocks.init(self.allocator);
        defer identifier_blocks.deinit();

        for (graph.nodes.keys()) |block| {
            const ident = try ast.utils.getStatementIdentifierByIndex(
                block,
                self.ast,
                self.symbol_table,
            );

            try identifier_blocks.put(ident, block);
        }

        var block_assignment_sets = BlockSets.init(self.allocator);
        defer {
            for (block_assignment_sets.values()) |*v| v.deinit();
            block_assignment_sets.deinit();
        }

        var block_phi_sets = BlockSets.init(self.allocator);
        defer {
            for (block_phi_sets.values()) |*v| v.deinit();
            block_phi_sets.deinit();
        }

        for (graph.nodes.keys()) |block| {
            try block_assignment_sets.put(block, BlockSet.init(self.allocator));
            try block_phi_sets.put(block, BlockSet.init(self.allocator));
        }

        var sym_counter = SymbolCounter.init(self.allocator);
        defer sym_counter.deinit();

        try self.updateSingleAssignment(
            &graph,
            &block_assignment_sets,
            &block_phi_sets,
            &sym_counter,
        );

        try self.updateAutoPhis(
            &graph,
            &dom_trees,
            &block_assignment_sets,
            &block_phi_sets,
            &sym_counter,
        );

        try self.updateUserPhis(
            &dom_trees,
            &identifier_blocks,
            &block_assignment_sets,
            &block_phi_sets,
        );

        try self.updateDependencies(
            &graph,
            &dom_trees,
            &block_assignment_sets,
            &block_phi_sets,
        );
    }

    fn incrementCount(counter: *SymbolCounter, sidx: usize) !usize {
        if (!counter.contains(sidx)) try counter.put(sidx, 0);

        const count = counter.getPtr(sidx).?;
        const index = count.*;
        count.* += 1;

        return index;
    }

    fn updateSingleAssignment(
        self: *Self,
        graph: *const cfg.CFG,
        block_assignment_sets: *BlockSets,
        block_phi_sets: *BlockSets,
        sym_counter: *SymbolCounter,
    ) !void {
        var ast_walk = ast.ASTWalk.init(self.allocator, self.ast);
        defer ast_walk.deinit();

        for (graph.nodes.keys()) |block| {
            const body = switch (self.ast.get(block).?.data) {
                .block => block,
                .function => |n| n.signature,
                else => unreachable,
            };

            var block_assignment_set: *BlockSet = block_assignment_sets.getPtr(block).?;
            var block_phi_set: *BlockSet = block_phi_sets.getPtr(block).?;

            var current_assign: ?usize = null;
            var current_phi: ?usize = null;

            try ast_walk.start(body);
            while (try ast_walk.next()) |out| {
                // Find assignment statements
                const assign: ?usize = switch (out.value.data) {
                    .phi => l: {
                        if (out.enter) {
                            current_phi = out.index;
                        }

                        break :l null;
                    },
                    .assignment => |n| switch (out.enter) {
                        true => l: {
                            current_assign = n.identifier;

                            break :l null;
                        },
                        false => n.identifier,
                    },
                    .function_parameter => |n| switch (out.enter) {
                        true => l: {
                            current_assign = n.value;

                            break :l n.value;
                        },
                        false => null,
                    },
                    .line => l: {
                        if (!out.enter) {
                            current_phi = null;
                        }

                        break :l null;
                    },
                    else => null,
                };

                // If there is an assignment set
                if (assign) |idx| {
                    // Get symbol
                    const ident = self.ast.get(idx) orelse return error.NotFound;
                    const inst: symbol.Instance = .{ .span = ident.span };

                    const sidx = self.symbol_table.getSymbolIndexByInstance(&inst) orelse return error.NotFound;

                    // Create next symbol
                    const index = try Self.incrementCount(sym_counter, sidx);

                    const nidx = try self.editor.childSymbol(sidx, index);
                    _ = try self.symbol_table.updateSymbolInstanceByIndex(nidx, &inst);

                    if (current_phi == null) {
                        // Add to normal assignment when not phi
                        try block_assignment_set.put(sidx, nidx);
                    } else {
                        // Otherwise, add to phi
                        try block_phi_set.put(sidx, nidx);
                    }
                }

                // Find dependency statements
                const depend: ?usize = switch (out.value.data) {
                    .identifier => |n| l: {
                        if (current_phi != null) break :l null;
                        if (current_assign == out.index) break :l null;

                        break :l switch (out.enter) {
                            true => switch (n.scope) {
                                .local => out.index,
                                else => null,
                            },
                            false => null,
                        };
                    },
                    else => null,
                };

                // Check if there is a dependency
                if (depend) |idx| {
                    // Get symbol
                    const ident = self.ast.get(idx) orelse return error.NotFound;
                    const inst: symbol.Instance = .{ .span = ident.span };

                    const sidx = self.symbol_table.getSymbolIndexByInstance(&inst) orelse return error.NotFound;

                    // If the value is already set, add node to latest value
                    if (block_assignment_set.get(sidx)) |nidx| {
                        _ = try self.symbol_table.updateSymbolInstanceByIndex(nidx, &inst);
                    }
                }
            }
        }
    }

    fn updateAutoPhis(
        self: *Self,
        graph: *const cfg.CFG,
        dom_trees: *const dom.DomTrees,
        block_assignment_sets: *const BlockSets,
        block_phi_sets: *BlockSets,
        sym_counter: *SymbolCounter,
    ) !void {
        for (graph.nodes.keys()) |block| {
            // Check if block is an intersection
            const predecessors: *const ArrayHashSet = graph.predecessors(block) orelse continue;
            if (predecessors.count() < 2) continue;

            // Skip any nodes that are immediate children of start
            var is_child = false;

            for (predecessors.keys()) |imm| {
                const node = graph.nodes.get(imm).?;

                if (node == .enter) {
                    is_child = true;
                    break;
                }
            }

            if (is_child) continue;

            // Get previous intersection
            const idom = dom_trees.predecessor(block) orelse return error.NotFound;

            // Find all assignments per predecessor branch
            // Follows a Domain Frontier-ish approach
            var branch_sets = BlockSets.init(self.allocator);
            defer {
                for (branch_sets.values()) |*v| v.deinit();
                branch_sets.deinit();
            }

            // For each predecessor branch
            for (predecessors.keys()) |branch| {
                try branch_sets.put(branch, BlockSet.init(self.allocator));
                var branch_set: *BlockSet = branch_sets.getPtr(branch).?;

                var seen = ArrayHashSet.init(self.allocator);
                defer seen.deinit();

                // Get all branch assignments
                var prev = branch;
                while (true) {
                    // Iterate all assignments
                    if (block_assignment_sets.getPtr(prev)) |block_assignment_set| {
                        var iter = block_assignment_set.iterator();

                        while (iter.next()) |entry| {
                            const sym: usize = entry.key_ptr.*;
                            const csym: usize = entry.value_ptr.*;

                            if (!branch_set.contains(sym)) {
                                try branch_set.put(sym, csym);
                            }
                        }
                    }

                    // Stop when an intersection is reached
                    if (prev == idom) break;

                    // Get predecessor, skipping over intersections
                    if (dom_trees.predecessor(prev)) |p| {
                        prev = p;
                    } else {
                        break;
                    }
                }
            }

            // Get symbols that are relevant to this node
            var sym_filter = ArrayHashSet.init(self.allocator);
            defer sym_filter.deinit();

            var fprev: ?usize = block;
            while (fprev != null) {
                const block_assignment_set = block_assignment_sets.get(fprev.?).?;

                for (block_assignment_set.keys()) |v| {
                    try sym_filter.put(v, undefined);
                }

                fprev = dom_trees.predecessor(fprev.?);
            }

            // Iterate over each predecessor branch data for this block
            const block_phi_set: *BlockSet = block_phi_sets.getPtr(block).?;

            var block_phis = BlockSet.init(self.allocator);
            defer block_phis.deinit();

            var iter = branch_sets.iterator();
            while (iter.next()) |entry| {
                const branch: usize = entry.key_ptr.*;
                const branch_label: usize = try ast.utils.getStatementIdentifierByIndex(
                    branch,
                    self.ast,
                    self.symbol_table,
                );

                const assignments: BlockSet = entry.value_ptr.*;

                // Iterate each assignments for this branch
                var aiter = assignments.iterator();
                while (aiter.next()) |aentry| {
                    // Conversion from old to new symbol
                    const sym: usize = aentry.key_ptr.*;
                    const csym: usize = aentry.value_ptr.*;

                    // Check if symbol is used
                    if (!sym_filter.contains(sym)) continue;

                    // Find phi statement index
                    const phi_idx = l: {
                        // If already computed, return early
                        if (block_phis.get(sym)) |pidx| {
                            break :l pidx;
                        }

                        // Create new left-hand symbol
                        const index = try Self.incrementCount(sym_counter, sym);
                        const nidx = try self.editor.childSymbol(sym, index);

                        // Add symbol to phi assignment set if not already set
                        if (!block_phi_set.contains(sym)) {
                            try block_phi_set.put(sym, nidx);
                        }

                        // Create a new phi
                        const pidx = try self.editor.phiLine(block, nidx);
                        try block_phis.put(sym, pidx);

                        break :l pidx;
                    };

                    // Add a parameter to the phi statement
                    _ = try self.editor.phiParameter(
                        phi_idx,
                        branch_label,
                        csym,
                    );
                }
            }
        }
    }

    fn updateUserPhis(
        self: *Self,
        dom_trees: *const dom.DomTrees,
        identifier_blocks: *const IdentifierBlocks,
        block_assignment_sets: *const BlockSets,
        block_phi_sets: *const BlockSets,
    ) !void {
        var ast_walk = ast.ASTWalk.init(self.allocator, self.ast);
        defer ast_walk.deinit();

        try ast_walk.start(self.ast.entrypoint().?);
        while (try ast_walk.next()) |out| {
            if (!out.enter) continue;

            switch (out.value.data) {
                .phi_parameter => |n| {
                    const value_ident = try ast.utils.getStatementIdentifierByIndex(
                        n.value,
                        self.ast,
                        self.symbol_table,
                    );
                    const value_sym = self.symbol_table.getSymbolPtr(value_ident).?;

                    // Skip if already converted
                    if (value_sym.memory == .child) continue;

                    const value_stat = self.ast.getPtr(n.value).?;

                    const ident = try ast.utils.getStatementIdentifierByIndex(
                        n.identifier,
                        self.ast,
                        self.symbol_table,
                    );

                    var block = identifier_blocks.get(ident) orelse return error.NotFound;
                    while (true) {
                        const block_assignment_set: *const BlockSet = block_assignment_sets.getPtr(block).?;
                        const block_phi_set: *const BlockSet = block_phi_sets.getPtr(block).?;

                        const o: ?usize = l: {
                            if (block_assignment_set.get(value_ident)) |v| {
                                break :l v;
                            } else if (block_phi_set.get(value_ident)) |v| {
                                break :l v;
                            } else break :l null;
                        };

                        if (o) |last_assign| {
                            const instance: symbol.Instance = .{ .span = value_stat.span };

                            _ = try self.symbol_table.updateSymbolInstanceByIndex(last_assign, &instance);

                            break;
                        }

                        block = dom_trees.predecessor(block) orelse return error.NotFound;
                    }
                },
                else => {},
            }
        }
    }

    fn updateDependencies(
        self: *Self,
        graph: *const cfg.CFG,
        dom_trees: *const dom.DomTrees,
        block_assignment_sets: *const BlockSets,
        block_phi_sets: *const BlockSets,
    ) !void {
        var ast_walk = ast.ASTWalk.init(self.allocator, self.ast);
        defer ast_walk.deinit();

        for (graph.nodes.keys()) |block| {
            try ast_walk.start(block);

            while (try ast_walk.next()) |out| {
                if (!out.enter) continue;

                switch (out.value.data) {
                    .identifier => |n| {
                        // Only modify local variables
                        if (n.scope != .local) continue;

                        const instance: symbol.Instance = .{ .span = out.value.span };
                        const sidx = self.symbol_table.getSymbolIndexByInstance(&instance) orelse continue;
                        const sym = self.symbol_table.getSymbolPtr(sidx).?;

                        // Skip already updated values
                        if (sym.memory == .child) continue;

                        // Loop parents until an assignment is found
                        var prev = block;
                        while (true) {
                            const block_assignment_set: *const BlockSet = block_assignment_sets.getPtr(prev).?;
                            const block_phi_set: *const BlockSet = block_phi_sets.getPtr(prev).?;

                            const o: ?usize = l: {
                                if (prev != block) {
                                    if (block_assignment_set.get(sidx)) |v| {
                                        break :l v;
                                    }
                                }

                                if (block_phi_set.get(sidx)) |v| {
                                    break :l v;
                                }

                                break :l null;
                            };

                            if (o) |last_assign| {
                                _ = try self.symbol_table.updateSymbolInstanceByIndex(last_assign, &instance);

                                break;
                            }

                            prev = dom_trees.predecessor(prev) orelse break;
                        }
                    },
                    else => {},
                }
            }
        }
    }
};

//
// Test
//

test "basic SSA conversion" {
    // Arrange
    const allocator = std.testing.allocator;

    const file = "function $fun(w %p) {@s %r =w copy %p jmp @l @l %r =w add %r, 1 jnz %r, @l, @e @e hlt}";

    var tree = try test_lib.testAST(allocator, file);
    defer tree.deinit();

    var symbol_table = symbol.SymbolTable.init(allocator);
    defer symbol_table.deinit();

    var ssa = SSA.init(allocator, &tree, &symbol_table);
    defer ssa.deinit();

    // Act
    try test_lib.testSSA(allocator, file, &tree, &symbol_table, &test_lib.test_target, &ssa);
}
