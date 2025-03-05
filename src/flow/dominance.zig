const std = @import("std");

const cfg = @import("cfg.zig");

const test_lib = @import("test.zig");

const ArrayHashSet = std.AutoArrayHashMap(usize, void);

// Dominance Sets
pub const DomSets = struct {
    allocator: std.mem.Allocator,
    collection: Collection,

    const Self = @This();

    // Key is node index and value is set of node indices that dominates this node.
    const Collection = std.AutoArrayHashMap(usize, ArrayHashSet);

    pub fn init(allocator: std.mem.Allocator) Self {
        return .{
            .allocator = allocator,
            .collection = Collection.init(allocator),
        };
    }

    pub fn deinit(self: *Self) void {
        for (self.collection.values()) |*v| v.deinit();
        self.collection.deinit();
    }

    pub fn build(self: *Self, graph: *const cfg.CFG) !void {
        var queue = std.ArrayList(struct { previous: ?usize, value: usize }).init(self.allocator);
        defer queue.deinit();

        for (graph.entrypoints.items) |entrypoint| {
            try queue.append(.{ .previous = null, .value = entrypoint });
        }

        while (queue.popOrNull()) |current| {
            const is_new_dom_set = !self.collection.contains(current.value);
            if (is_new_dom_set) {
                var dom_set = ArrayHashSet.init(self.allocator);
                try dom_set.put(current.value, undefined);
                try self.collection.put(current.value, dom_set);
            }
            var dom_set: *ArrayHashSet = self.collection.getPtr(current.value).?;

            if (current.previous) |previous| {
                const previous_set = self.collection.get(previous).?;

                if (is_new_dom_set) {
                    for (previous_set.keys()) |key| {
                        try dom_set.put(key, undefined);
                    }
                } else {
                    for (dom_set.keys()) |key| {
                        if (key == current.value) continue;
                        if (!previous_set.contains(key)) _ = dom_set.swapRemove(key);
                    }
                }
            }

            switch (graph.nodes.get(current.value).?) {
                .enter => |n| if (!dom_set.contains(n)) {
                    try queue.append(.{ .previous = current.value, .value = n });
                },
                .jump => |n| if (!dom_set.contains(n)) {
                    try queue.append(.{ .previous = current.value, .value = n });
                },
                .branch => |n| {
                    if (!dom_set.contains(n.right)) try queue.append(.{ .previous = current.value, .value = n.right });
                    if (!dom_set.contains(n.left)) try queue.append(.{ .previous = current.value, .value = n.left });
                },
                .exit => {},
            }
        }
    }
};

// Immediate Dominator Trees
pub const DomTrees = struct {
    allocator: std.mem.Allocator,
    collection: Collection,

    const Self = @This();

    // Key is child index and value is parent index
    const Collection = std.AutoArrayHashMap(usize, usize);

    pub fn init(allocator: std.mem.Allocator) Self {
        return .{
            .allocator = allocator,
            .collection = Collection.init(allocator),
        };
    }

    pub fn deinit(self: *Self) void {
        self.collection.deinit();
    }

    pub fn build(self: *Self, sets: *const DomSets) !void {
        var idom_set = ArrayHashSet.init(self.allocator);
        defer idom_set.deinit();

        var iter = sets.collection.iterator();
        while (iter.next()) |entry| {
            idom_set.clearAndFree();

            const entry_index: usize = entry.key_ptr.*;
            const entry_set: *ArrayHashSet = entry.value_ptr;

            try idom_set.put(entry_index, undefined);

            for (entry_set.keys()) |index| {
                if (entry_index == index) continue;

                const set = sets.collection.getPtr(index).?;
                for (set.keys()) |dom| {
                    if (dom == index) continue;
                    try idom_set.put(dom, undefined);
                }
            }

            for (entry_set.keys()) |index| {
                if (idom_set.contains(index)) continue;

                try self.collection.put(entry_index, index);
                break;
            }
        }
    }
};

// Dominance Frontier Sets
pub const DomFSets = struct {
    allocator: std.mem.Allocator,
    collection: Collection,

    const Self = @This();

    // Key is node index and value is set of node indices that are in the dominance frontier of this node.
    const Collection = std.AutoArrayHashMap(usize, ArrayHashSet);

    pub fn init(allocator: std.mem.Allocator) Self {
        return .{
            .allocator = allocator,
            .collection = Collection.init(allocator),
        };
    }

    pub fn deinit(self: *Self) void {
        for (self.collection.values()) |*v| v.deinit();
        self.collection.deinit();
    }

    pub fn build(self: *Self, graph: *const cfg.CFG, trees: *const DomTrees) !void {
        // Set of immediately previous nodes for each node
        var previous_sets = Collection.init(self.allocator);
        defer {
            for (previous_sets.values()) |*v| v.deinit();
            previous_sets.deinit();
        }

        // Initialize sets
        for (graph.nodes.keys()) |i| {
            try self.collection.put(i, ArrayHashSet.init(self.allocator));
            try previous_sets.put(i, ArrayHashSet.init(self.allocator));
        }

        // Populate immediately previous nodes
        var iter = graph.nodes.iterator();
        while (iter.next()) |entry| {
            const idx = entry.key_ptr.*;

            switch (entry.value_ptr.*) {
                .enter => |n| try previous_sets.getPtr(n).?.put(idx, undefined),
                .jump => |n| try previous_sets.getPtr(n).?.put(idx, undefined),
                .branch => |n| {
                    try previous_sets.getPtr(n.left).?.put(idx, undefined);
                    try previous_sets.getPtr(n.right).?.put(idx, undefined);
                },
                .exit => {},
            }
        }

        // Populate DF
        iter = graph.nodes.iterator();
        while (iter.next()) |entry| {
            const nidx = entry.key_ptr.*;

            // Only DF if there is at least 2 previous nodes
            const nprevious: ArrayHashSet = previous_sets.get(nidx).?;
            if (nprevious.count() < 2) continue;

            // Get previous intersection
            const idom_o = trees.collection.get(nidx);
            if (idom_o == null) continue;
            const idom = idom_o.?;

            // Initialize queue
            var queue = std.ArrayList(usize).init(self.allocator);
            defer queue.deinit();

            for (nprevious.keys()) |prev| {
                try queue.append(prev);
            }

            // Keep track of seen nodes
            var seen = ArrayHashSet.init(self.allocator);
            defer seen.deinit();

            while (queue.popOrNull()) |pidx| {
                // Ignore seen nodes
                if (seen.get(pidx) != null) continue;
                try seen.put(pidx, undefined);

                // Stop when reached intersection
                if (pidx == idom) continue;

                // Add current to DF of previous
                try self.collection.getPtr(pidx).?.put(nidx, undefined);

                // Continue upward propagation
                try queue.append(trees.collection.get(pidx).?);
            }
        }
    }
};

//
// Test
//

test "dom_tree base" {
    // 0
    // |
    // 1

    // Arrange
    const allocator = std.testing.allocator;

    var c = cfg.CFG.init(allocator);
    defer c.deinit();

    try c.put(0, .{ .enter = 1 });
    try c.put(1, .exit);

    // Act
    var sets = DomSets.init(allocator);
    defer sets.deinit();
    try sets.build(&c);

    var trees = DomTrees.init(allocator);
    defer trees.deinit();
    try trees.build(&sets);

    // Assert
    try std.testing.expectEqual(null, trees.collection.get(0));
    try std.testing.expectEqual(0, trees.collection.get(1));
}

test "dom_tree multiple" {
    // 0 2
    // | |
    // 1 3

    // Arrange
    const allocator = std.testing.allocator;

    var c = cfg.CFG.init(allocator);
    defer c.deinit();

    try c.put(0, .{ .enter = 1 });
    try c.put(1, .exit);

    try c.put(2, .{ .enter = 3 });
    try c.put(3, .exit);

    // Act
    var sets = DomSets.init(allocator);
    defer sets.deinit();
    try sets.build(&c);

    var trees = DomTrees.init(allocator);
    defer trees.deinit();
    try trees.build(&sets);

    // Assert
    try std.testing.expectEqual(null, trees.collection.get(0));
    try std.testing.expectEqual(0, trees.collection.get(1));

    try std.testing.expectEqual(null, trees.collection.get(2));
    try std.testing.expectEqual(2, trees.collection.get(3));
}

test "dom_tree loop" {
    // 0
    // |
    // 1*
    // |
    // 2

    // Arrange
    const allocator = std.testing.allocator;

    var c = cfg.CFG.init(allocator);
    defer c.deinit();

    try c.put(0, .{ .enter = 1 });
    try c.put(1, .{ .branch = .{ .left = 2, .right = 1 } });
    try c.put(2, .exit);

    // Act
    var sets = DomSets.init(allocator);
    defer sets.deinit();
    try sets.build(&c);

    var trees = DomTrees.init(allocator);
    defer trees.deinit();
    try trees.build(&sets);

    // Assert
    try std.testing.expectEqual(null, trees.collection.get(0));
    try std.testing.expectEqual(0, trees.collection.get(1));
    try std.testing.expectEqual(1, trees.collection.get(2));
}

test "dom_tree split" {
    //   0
    //   |
    //   1
    //  / \
    // 2   3

    // Arrange
    const allocator = std.testing.allocator;

    var c = cfg.CFG.init(allocator);
    defer c.deinit();

    try c.put(0, .{ .enter = 1 });
    try c.put(1, .{ .branch = .{ .left = 2, .right = 3 } });
    try c.put(2, .exit);
    try c.put(3, .exit);

    // Act
    var sets = DomSets.init(allocator);
    defer sets.deinit();
    try sets.build(&c);

    var trees = DomTrees.init(allocator);
    defer trees.deinit();
    try trees.build(&sets);

    // Assert
    try std.testing.expectEqual(null, trees.collection.get(0));
    try std.testing.expectEqual(0, trees.collection.get(1));
    try std.testing.expectEqual(1, trees.collection.get(2));
    try std.testing.expectEqual(1, trees.collection.get(3));
}

test "dom_tree intersection" {
    //   0
    //   |
    //   1
    //  / \
    // 2   3
    //  \ /
    //   4

    // Arrange
    const allocator = std.testing.allocator;

    var c = cfg.CFG.init(allocator);
    defer c.deinit();

    try c.put(0, .{ .enter = 1 });
    try c.put(1, .{ .branch = .{ .left = 2, .right = 3 } });
    try c.put(2, .{ .jump = 4 });
    try c.put(3, .{ .jump = 4 });
    try c.put(4, .exit);

    // Act
    var sets = DomSets.init(allocator);
    defer sets.deinit();
    try sets.build(&c);

    var trees = DomTrees.init(allocator);
    defer trees.deinit();
    try trees.build(&sets);

    // Assert
    try std.testing.expectEqual(null, trees.collection.get(0));
    try std.testing.expectEqual(0, trees.collection.get(1));
    try std.testing.expectEqual(1, trees.collection.get(2));
    try std.testing.expectEqual(1, trees.collection.get(3));
    try std.testing.expectEqual(1, trees.collection.get(4));
}

test "dom_tree complex" {
    //   0
    //   |
    //   1<----.
    //  / \    |
    // |   2   |
    // |  / \  |
    // | 3   4-'
    // |  \ /
    // |   5
    //  \ /
    //   6

    // Arrange
    const allocator = std.testing.allocator;

    var c = cfg.CFG.init(allocator);
    defer c.deinit();

    try c.put(0, .{ .enter = 1 });
    try c.put(1, .{ .branch = .{ .left = 6, .right = 2 } });
    try c.put(2, .{ .branch = .{ .left = 3, .right = 4 } });
    try c.put(3, .{ .jump = 5 });
    try c.put(4, .{ .branch = .{ .left = 5, .right = 1 } });
    try c.put(5, .{ .jump = 6 });
    try c.put(6, .exit);

    // Act
    var sets = DomSets.init(allocator);
    defer sets.deinit();
    try sets.build(&c);

    var trees = DomTrees.init(allocator);
    defer trees.deinit();
    try trees.build(&sets);

    // Assert
    try std.testing.expectEqual(null, trees.collection.get(0));
    try std.testing.expectEqual(0, trees.collection.get(1));
    try std.testing.expectEqual(1, trees.collection.get(2));
    try std.testing.expectEqual(2, trees.collection.get(3));
    try std.testing.expectEqual(2, trees.collection.get(4));
    try std.testing.expectEqual(2, trees.collection.get(5));
    try std.testing.expectEqual(1, trees.collection.get(6));
}

test "domf_set" {
    //     0
    //     |
    //     1
    //    / \
    // .>2   3
    // | |   |
    // .-4   5*
    //    \ /
    //     6

    // Arrange
    const allocator = std.testing.allocator;

    var c = cfg.CFG.init(allocator);
    defer c.deinit();

    try c.put(0, .{ .enter = 1 });
    try c.put(1, .{ .branch = .{ .left = 2, .right = 3 } });
    try c.put(2, .{ .jump = 4 });
    try c.put(3, .{ .jump = 5 });
    try c.put(4, .{ .branch = .{ .left = 2, .right = 6 } });
    try c.put(5, .{ .branch = .{ .left = 6, .right = 5 } });
    try c.put(6, .exit);

    var sets = DomSets.init(allocator);
    defer sets.deinit();
    try sets.build(&c);

    var trees = DomTrees.init(allocator);
    defer trees.deinit();
    try trees.build(&sets);

    // Act
    var df = DomFSets.init(allocator);
    defer df.deinit();
    try df.build(&c, &trees);

    // Assert
    try std.testing.expectEqual(0, df.collection.get(0).?.count());
    try std.testing.expectEqual(0, df.collection.get(1).?.count());
    try std.testing.expectEqual(2, df.collection.get(2).?.count());
    try std.testing.expectEqual(1, df.collection.get(3).?.count());
    try std.testing.expectEqual(2, df.collection.get(4).?.count());
    try std.testing.expectEqual(2, df.collection.get(5).?.count());
    try std.testing.expectEqual(0, df.collection.get(6).?.count());
}
