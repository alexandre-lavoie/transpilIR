const std = @import("std");

const ast = @import("../ast/lib.zig");
const qbe = @import("../qbe/lib.zig");

pub fn testAST(allocator: std.mem.Allocator, buffer: anytype) !ast.AST {
    var file_stream = std.io.fixedBufferStream(buffer);

    var file_reader = file_stream.reader();

    var tokens = std.ArrayList(qbe.Token).init(allocator);
    defer tokens.deinit();

    var lex = qbe.Lexer(@TypeOf(file_reader), @TypeOf(tokens)).init(&file_reader, &tokens);
    try lex.lex();

    const token_slice = try tokens.toOwnedSlice();
    defer tokens.allocator.free(token_slice);

    var token_reader = qbe.TokenReader.init(token_slice);

    var tree = ast.AST.init(allocator);
    errdefer tree.deinit();

    var parser = qbe.Parser(@TypeOf(token_reader)).init(&token_reader, &tree);
    _ = try parser.parse();

    return tree;
}

pub fn assertStatementTypes(allocator: std.mem.Allocator, types: []const ast.StatementType, statements: []const ast.Statement) !void {
    const expected = types;

    var actual = try std.ArrayList(ast.StatementType).initCapacity(allocator, statements.len);
    defer actual.deinit();

    for (statements) |statement| {
        try actual.append(@as(ast.StatementType, statement.data));
    }

    try std.testing.expectEqualSlices(ast.StatementType, expected, actual.items);
}
