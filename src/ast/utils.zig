const std = @import("std");

const ast = @import("ast.zig");
const statement = @import("statement.zig");
const symbol = @import("../symbol/lib.zig");

pub fn getStatementIdentifierByIndex(idx: statement.StatementIndex, tree: *const ast.AST, symbol_table: *const symbol.SymbolTable) !usize {
    const stat = tree.collection.items[idx];

    return switch (stat.data) {
        .identifier => {
            const instance: symbol.Instance = .{ .span = stat.span };

            return symbol_table.getSymbolIndexByInstance(&instance) orelse error.NotFound;
        },
        .function => |d| getStatementIdentifierByIndex(d.signature, tree, symbol_table),
        .function_signature => |d| getStatementIdentifierByIndex(d.name, tree, symbol_table),
        .block => |d| getStatementIdentifierByIndex(d.label, tree, symbol_table),
        else => unreachable,
    };
}
