const ast = @import("ast.zig");
pub const AST = ast.AST;

const statement = @import("statement.zig");
pub const Statement = statement.Statement;
pub const StatementIndex = statement.StatementIndex;
pub const StatementType = statement.StatementType;
pub const StatementData = statement.StatementData;
pub const Scope = statement.Scope;
pub const PrimitiveType = statement.PrimitiveType;
pub const BinaryOperationtype = statement.BinaryOperationType;
pub const ComparisonOperationType = statement.ComparisonOperationType;

const walk = @import("walk.zig");
pub const ASTWalk = walk.ASTWalk;
