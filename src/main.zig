const std = @import("std");

const lib = @import("lib.zig");

pub fn main() !void {
    var gp = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gp.deinit();
    const allocator = gp.allocator();

    const file_path = "third-party/qbe/test/_alt.ssa";

    const file = try std.fs.cwd().openFile(file_path, .{});
    defer file.close();

    var file_reader = file.reader();

    var tokens = std.ArrayList(lib.ssa.Token).init(allocator);
    defer tokens.deinit();

    var lexer = lib.ssa.Lexer(@TypeOf(file_reader), @TypeOf(tokens)).init(&file_reader, &tokens);
    try lexer.lex();

    for (tokens.items) |token| {
        std.log.info("{any}", .{token});
    }
}
