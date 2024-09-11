const std = @import("std");

const ast = @import("ast/lib.zig");
pub const AST = ast.AST;
pub const ASTWalk = ast.ASTWalk;

const common = @import("common.zig");
pub const Color = common.Color;
pub const CollectionIterator = common.CollectionIterator;
pub const EmitWriterConfig = common.EmitWriterConfig;
pub const SourceSpan = common.SourceSpan;
pub const Target = common.Target;
pub const fileNewLines = common.fileNewLines;
pub const printError = common.printError;
pub const printSpan = common.printSpan;

const qbe = @import("qbe/lib.zig");
pub const QBEEmitWalkCallback = qbe.QBEEmitWalkCallback;
pub const QBEEmitWriter = qbe.QBEEmitWriter;
pub const QBELexer = qbe.QBELexer;
pub const QBEParser = qbe.QBEParser;
pub const QBEToken = qbe.QBEToken;

const symbol = @import("symbol/lib.zig");
pub const SymbolTable = symbol.SymbolTable;
pub const Symbol = symbol.Symbol;
pub const SymbolMemory = symbol.SymbolMemory;
pub const Instance = symbol.Instance;
pub const SymbolSourceWalkCallback = symbol.SymbolSourceWalkCallback;
pub const SymbolMemoryWalkCallback = symbol.SymbolMemoryWalkCallback;
pub const SymbolValidateWalkCallback = symbol.SymbolValidateWalkCallback;

test {
    std.testing.refAllDeclsRecursive(@This());
}
