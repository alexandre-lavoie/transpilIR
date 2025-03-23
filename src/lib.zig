const std = @import("std");

const ast = @import("ast/lib.zig");
pub const AST = ast.AST;
pub const ASTWalk = ast.ASTWalk;
pub const Statement = ast.Statement;
pub const ast_utils = ast.utils;

const c = @import("c/lib.zig");
pub const CEmitWalkCallback = c.CEmitWalkCallback;
pub const CEmitWriter = c.CEmitWriter;
pub const CToken = c.CToken;

const common = @import("common.zig");
pub const Assembly = common.Assembly;
pub const Color = common.Color;
pub const CollectionIterator = common.CollectionIterator;
pub const EmitWriterConfig = common.EmitWriterConfig;
pub const IR = common.IR;
pub const Optimization = common.Optimization;
pub const SourceSpan = common.SourceSpan;
pub const Target = common.Target;
pub const fileNewLines = common.fileNewLines;
pub const printError = common.printError;
pub const printSpan = common.printSpan;
pub const readerToWriter = common.readerToWriter;

const compiler = @import("compiler/lib.zig");
pub const GCC = compiler.GCC;
pub const IRC = compiler.IRC;
pub const LLVM = compiler.LLVM;
pub const QBE = compiler.QBE;

const flow = @import("flow/lib.zig");
pub const CFG = flow.CFG;
pub const CFGNode = flow.CFGNode;
pub const CFGWalkCallback = flow.CFGWalkCallback;
pub const CFGWalk = flow.CFGWalk;
pub const DomSets = flow.DomSets;
pub const DomTrees = flow.DomTrees;
pub const SSA = flow.SSA;

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
