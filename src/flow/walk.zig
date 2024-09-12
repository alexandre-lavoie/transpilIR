const std = @import("std");

const cfg = @import("cfg.zig");

pub const CFGWalk = struct {
    graph: *const cfg.CFG,
    queue: QueueList,
    seen: SeenSet,

    const Self = @This();
    const QueueList = std.ArrayList(usize);
    const SeenSet = std.AutoArrayHashMap(usize, void);

    pub fn init(allocator: std.mem.Allocator, graph: *const cfg.CFG) Self {
        return .{
            .graph = graph,
            .queue = QueueList.init(allocator),
            .seen = SeenSet.init(allocator),
        };
    }

    pub fn deinit(self: *Self) void {
        self.queue.deinit();
        self.seen.deinit();
    }

    pub fn start(self: *Self, entry: usize) !void {
        self.queue.clearAndFree();
        self.seen.clearAndFree();

        try self.queue.append(entry);
    }

    pub fn next(self: *Self) !?usize {
        const index: usize = scope: {
            while (self.queue.popOrNull()) |key| {
                if (self.seen.contains(key)) continue;
                break :scope key;
            }

            return null;
        };
        try self.seen.put(index, undefined);

        const node = self.graph.nodes.getPtr(index) orelse unreachable;

        switch (node.*) {
            .enter, .jump => |n| try self.queue.append(n),
            .branch => |n| {
                try self.queue.append(n.right);
                try self.queue.append(n.left);
            },
            .exit => {},
        }

        return index;
    }
};

test "walk" {
    //   0
    //   |
    //   1
    //  / \
    // |   2
    // |  / \
    // | 3   4*
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
    try c.put(4, .{ .branch = .{ .left = 5, .right = 4 } });
    try c.put(5, .{ .jump = 6 });
    try c.put(6, .exit);

    var walk = CFGWalk.init(allocator, &c);
    defer walk.deinit();

    var output = std.ArrayList(usize).init(allocator);
    defer output.deinit();

    // Act
    try walk.start(0);
    while (try walk.next()) |index| {
        try output.append(index);
    }

    // Assert
    try std.testing.expectEqualSlices(
        usize,
        &[_]usize{ 0, 1, 6, 2, 3, 5, 4 },
        output.items,
    );
}
