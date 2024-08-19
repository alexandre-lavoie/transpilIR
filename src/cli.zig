const std = @import("std");

const lib = @import("lib.zig");

pub fn main() !void {
    var gp = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gp.deinit();
    const allocator = gp.allocator();

    var args = std.process.args();
    _ = args.skip();

    var files = std.ArrayList([]const u8).init(allocator);
    defer files.deinit();

    while (args.next()) |file_arg| {
        try files.append(file_arg);
    }

    for (files.items) |file_arg| {
        var buffer: [4096]u8 = undefined;
        const file_path = try std.fs.cwd().realpath(file_arg, &buffer);

        std.log.info("=== File ===", .{});
        std.log.info("{s}", .{file_path});

        const file = try std.fs.openFileAbsolute(file_path, .{});
        defer file.close();

        var file_reader = file.reader();

        std.log.info("=== Lexer ===", .{});
        var tokens = std.ArrayList(lib.ssa.Token).init(allocator);
        defer tokens.deinit();

        var lexer = lib.ssa.Lexer(@TypeOf(file_reader), @TypeOf(tokens)).init(&file_reader, &tokens);
        try lexer.lex();

        for (tokens.items) |token| {
            std.log.info("{s} {}:{}", .{ @tagName(token.token_type), token.span.start, token.span.end });
        }

        const token_slice = try tokens.toOwnedSlice();
        defer tokens.allocator.free(token_slice);

        var token_reader = lib.ssa.TokenReader(@TypeOf(token_slice)).init(token_slice);

        std.log.info("=== Parser ===", .{});
        var ast = lib.ast.AST.init(allocator);
        defer ast.deinit();

        var parser = lib.ssa.Parser(@TypeOf(token_reader)).init(&token_reader, &ast);
        _ = try parser.parse();

        var callback = LogASTWalkCallback.init();
        var walk = lib.ast.ASTWalk(@TypeOf(callback)).init(&ast, &callback);
        try walk.walk(ast.entrypoint() orelse return error.NotFound);
    }
}

const LogASTWalkCallback = struct {
    depth: usize,

    const Self = @This();

    pub fn init() Self {
        return Self{ .depth = 0 };
    }

    pub fn enter(self: *Self, statement: *lib.ast.Statement) !void {
        switch (statement.data) {
            .node => return,
            else => {},
        }

        var temp_buffer: [128]u8 = undefined;
        var depth_buffer = temp_buffer[0 .. self.depth * 2];
        @memset(depth_buffer[0..], ' ');

        std.log.info("{s}{s} {}:{}", .{ depth_buffer, @tagName(statement.data), statement.span.start, statement.span.end });

        self.depth += 1;
    }

    pub fn exit(self: *Self, statement: *lib.ast.Statement) !void {
        switch (statement.data) {
            .node => return,
            else => {},
        }

        self.depth -= 1;
    }
};
