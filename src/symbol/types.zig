const std = @import("std");

const ast = @import("../ast/lib.zig");
const common = @import("../common.zig");

pub const Instance = struct {
    span: common.SourceSpan,
};

pub const SymbolIdentifier = struct {
    name: []const u8,
    scope: ast.Scope,
    function: ?usize = null,

    const Self = @This();

    pub fn key(self: *const Self, allocator: std.mem.Allocator) ![]const u8 {
        const scope_value = @intFromEnum(self.scope);

        if (self.function) |f| {
            return try std.fmt.allocPrint(
                allocator,
                "{s}:{}:{}",
                .{
                    self.name,
                    scope_value,
                    f,
                },
            );
        } else {
            return try std.fmt.allocPrint(
                allocator,
                "{s}:{}",
                .{
                    self.name,
                    scope_value,
                },
            );
        }
    }
};

pub const Symbol = struct {
    identifier: SymbolIdentifier,
    memory: SymbolMemory = .{ .empty = undefined },
};

pub const LiteralValueType = enum {
    integer,
    float,
    string,
};

pub const LiteralValue = union(LiteralValueType) {
    integer: isize,
    float: f64,
    string: []const u8,
};

pub const Literal = struct {
    value: LiteralValue,
};

pub const SymbolType = union(enum) {
    primitive: ast.PrimitiveType,
    type: usize,
};

pub const SymbolMemoryOpaque = struct {
    alignment: ?usize = null,
    size: usize,
};

pub const SymbolMemoryStructEntry = struct {
    base: SymbolType,
    count: usize,
};

pub const SymbolMemoryStruct = struct {
    alignment: ?usize = null,
    members: []const SymbolMemoryStructEntry,
};

pub const SymbolMemoryUnion = struct {
    alignment: ?usize = null,
    structs: []const SymbolMemoryStruct,
};

pub const SymbolMemoryDataOffset = struct {
    index: usize,
    offset: isize,
};

pub const SymbolMemoryDataValue = union(enum) {
    integer: isize,
    float: f64,
    string: []const u8,
    symbol: SymbolMemoryDataOffset,
};

pub const SymbolMemoryDataEntry = struct {
    type: ast.PrimitiveType,
    value: SymbolMemoryDataValue,
};

pub const SymbolMemoryLinkage = struct {
    thread: bool = false,
    @"export": bool = false,
    section: ?[]const u8 = null,
    flags: ?[]const u8 = null,
};

pub const SymbolMemoryData = struct {
    linkage: SymbolMemoryLinkage,
    entries: []const SymbolMemoryDataEntry,
};

pub const SymbolMemoryParameterType = union(enum) {
    primitive: ast.PrimitiveType,
    type: usize,
    env: void,
};

pub const SymbolMemoryFunction = struct {
    linkage: SymbolMemoryLinkage,
    @"return": ast.PrimitiveType,
    parameters: []const SymbolMemoryParameterType,
    vararg: bool = false,
};

pub const SymbolMemory = union(enum) {
    empty: void,
    primitive: ast.PrimitiveType,
    type: usize,
    env: void,
    @"opaque": SymbolMemoryOpaque,
    @"struct": SymbolMemoryStruct,
    @"union": SymbolMemoryUnion,
    data: SymbolMemoryData,
    function: SymbolMemoryFunction,
};
