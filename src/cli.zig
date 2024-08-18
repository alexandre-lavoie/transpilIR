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

    const token_slice = try tokens.toOwnedSlice();
    defer tokens.allocator.free(token_slice);

    var token_reader = lib.ssa.TokenReader(@TypeOf(token_slice)).init(token_slice);

    var statements = std.ArrayList(lib.ast.Statement).init(allocator);
    defer statements.deinit();

    var parser = lib.ssa.Parser(@TypeOf(token_reader), @TypeOf(statements)).init(&token_reader, &statements);
    _ = try parser.parse();

    for (statements.items) |statement| {
        std.log.info("{any}", .{statement});
    }
}
