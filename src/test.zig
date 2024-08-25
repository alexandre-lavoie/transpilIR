const std = @import("std");

const lib = @import("lib.zig");

pub fn testAST(allocator: std.mem.Allocator, buffer: anytype) !lib.ast.AST {
    var file_stream = std.io.fixedBufferStream(buffer);

    var file_reader = file_stream.reader();

    var tokens = std.ArrayList(lib.ssa.Token).init(allocator);
    defer tokens.deinit();

    var lex = lib.ssa.Lexer(@TypeOf(file_reader), @TypeOf(tokens)).init(&file_reader, &tokens);
    try lex.lex();

    const token_slice = try tokens.toOwnedSlice();
    defer tokens.allocator.free(token_slice);

    var token_reader = lib.ssa.TokenReader(@TypeOf(token_slice)).init(token_slice);

    var tree = lib.ast.AST.init(allocator);
    errdefer tree.deinit();

    var parser = lib.ssa.Parser(@TypeOf(token_reader)).init(&token_reader, &tree);
    _ = try parser.parse();

    return tree;
}

pub fn assertStatementTypes(allocator: std.mem.Allocator, types: []const lib.ast.StatementType, statements: []const lib.ast.Statement) !void {
    const expected = types;

    var actual = try std.ArrayList(lib.ast.StatementType).initCapacity(allocator, statements.len);
    defer actual.deinit();

    for (statements) |statement| {
        try actual.append(@as(lib.ast.StatementType, statement.data));
    }

    try std.testing.expectEqualSlices(lib.ast.StatementType, expected, actual.items);
}
