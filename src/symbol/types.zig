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

pub const SymbolMemoryOpaque = struct {
    alignment: ?usize = null,
    size: usize,
};

pub const SymbolMemoryStructEntry = struct {
    base: union(enum) {
        primitive: ast.PrimitiveType,
        type: usize,
    },
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

pub const SymbolMemory = union(enum) {
    empty: void,
    primitive: ast.PrimitiveType,
    type: usize,
    @"opaque": SymbolMemoryOpaque,
    @"struct": SymbolMemoryStruct,
    @"union": SymbolMemoryUnion,
};
