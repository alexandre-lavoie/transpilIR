const std = @import("std");

const common = @import("../common.zig");
const statement = @import("statement.zig");

pub const AST = struct {
    collection: Collection,

    span: usize = 0,

    const Self = @This();
    const Collection = std.ArrayList(statement.Statement);

    pub fn init(allocator: std.mem.Allocator) Self {
        return Self{
            .collection = Collection.init(allocator),
        };
    }

    pub fn deinit(self: *Self) void {
        self.collection.deinit();
    }

    pub fn entrypoint(self: *const Self) ?statement.StatementIndex {
        if (self.collection.items.len == 0) return null;

        var i = self.collection.items.len - 1;
        while (true) : (i -= 1) {
            const s = self.getPtr(i) orelse return null;

            if (@as(statement.StatementType, s.data) == .module) return i;

            if (i == 0) return null;
        }
    }

    pub fn get(self: *const Self, index: statement.StatementIndex) ?statement.Statement {
        if (index >= self.collection.items.len) return null;

        return self.collection.items[index];
    }

    pub fn getPtr(self: *const Self, index: statement.StatementIndex) ?*const statement.Statement {
        if (index >= self.collection.items.len) return null;

        return &self.collection.items[index];
    }

    pub fn getPtrMut(self: *Self, index: statement.StatementIndex) ?*statement.Statement {
        if (index >= self.collection.items.len) return null;

        return &self.collection.items[index];
    }

    pub fn append(self: *Self, next_statement: statement.Statement) !usize {
        self.span = @max(self.span, next_statement.span.end + 1);

        const index = self.collection.items.len;

        try self.collection.append(next_statement);

        return index;
    }

    pub fn nextSpan(self: *Self) common.SourceSpan {
        self.span += 1;

        return .{ .start = self.span, .end = self.span };
    }

    pub fn toSlice(self: *const Self, allocator: std.mem.Allocator) ![]statement.Statement {
        const statements = try allocator.alloc(statement.Statement, self.collection.items.len);
        @memcpy(statements, self.collection.items);

        return statements;
    }
};
