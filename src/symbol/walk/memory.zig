const std = @import("std");

const ast = @import("../../ast/lib.zig");
const common = @import("../../common.zig");
const table = @import("../table.zig");
const types = @import("../types.zig");

const source = @import("source.zig");

const SymbolMemoryEntry = union(enum) {
    local: usize,
    global: usize,
    literal: *types.Literal,
    primitive: ast.PrimitiveType,
    type: usize,
    env: void,
    @"struct": void,
    @"union": void,
    @"opaque": void,
    variadic: void,
    linkage: types.SymbolMemoryLinkage,
    data_offset: types.SymbolMemoryDataOffset,
    data_entry: struct {
        primitive: ast.PrimitiveType,
        value: union(enum) {
            symbol: types.SymbolMemoryDataOffset,
            literal: *types.Literal,
        },
    },
    array: struct {
        base: union(enum) {
            primitive: ast.PrimitiveType,
            type: usize,
        },
        count: usize,
    },
};

pub const SymbolMemoryWalkCallback = struct {
    symbol_table: *table.SymbolTable,

    entries: EntryList,

    const Self = @This();
    const EntryList = std.ArrayList(SymbolMemoryEntry);

    pub fn init(symbol_table: *table.SymbolTable) Self {
        return .{
            .symbol_table = symbol_table,
            .entries = EntryList.init(symbol_table.symbols.allocator),
        };
    }

    pub fn enter(self: *Self, statement: *ast.Statement) !void {
        const instance: types.Instance = .{ .span = statement.span };

        switch (statement.data) {
            .module => {
                self.entries = EntryList.init(self.symbol_table.symbols.allocator);
            },
            .struct_type => {
                try self.entries.append(.{
                    .@"struct" = undefined,
                });
            },
            .union_type => {
                try self.entries.append(.{
                    .@"union" = undefined,
                });
            },
            .opaque_type => {
                try self.entries.append(.{
                    .@"opaque" = undefined,
                });
            },
            .env_type => {
                try self.entries.append(.{
                    .env = undefined,
                });
            },
            .variadic_parameter => {
                try self.entries.append(.{
                    .variadic = undefined,
                });
            },
            .identifier => |*identifier| {
                switch (identifier.scope) {
                    .local,
                    => {
                        try self.entries.append(.{
                            .local = self.symbol_table.getSymbolIndexByInstance(&instance) orelse unreachable,
                        });
                    },
                    .global,
                    => {
                        try self.entries.append(.{
                            .global = self.symbol_table.getSymbolIndexByInstance(&instance) orelse unreachable,
                        });
                    },
                    .type => {
                        try self.entries.append(.{
                            .type = self.symbol_table.getSymbolIndexByInstance(&instance) orelse unreachable,
                        });
                    },
                    else => {},
                }
            },
            .literal => {
                const literal = self.symbol_table.getLiteralByInstance(&instance) orelse unreachable;

                try self.entries.append(.{
                    .literal = literal,
                });
            },
            .primitive_type => |primitive| {
                try self.entries.append(.{
                    .primitive = primitive,
                });
            },
            else => {},
        }
    }

    pub fn exit(self: *Self, statement: *ast.Statement) !void {
        const allocator = self.symbol_table.symbols.allocator;

        switch (statement.data) {
            .module => {
                self.entries.deinit();
            },
            .identifier,
            .literal,
            .primitive_type,
            .node,
            .struct_type,
            .union_type,
            .opaque_type,
            .env_type,
            .variadic_parameter,
            .binary_operation,
            => {},
            .array_type => {
                const count: usize = switch (self.entries.pop()) {
                    .literal => |literal| switch (literal.value) {
                        .integer => |integer| @as(usize, @intCast(integer)),
                        else => unreachable,
                    },
                    else => unreachable,
                };

                try self.entries.append(switch (self.entries.pop()) {
                    .primitive => |primitive| .{
                        .array = .{
                            .base = .{
                                .primitive = primitive,
                            },
                            .count = count,
                        },
                    },
                    .type => |t| .{
                        .array = .{
                            .base = .{
                                .type = t,
                            },
                            .count = count,
                        },
                    },
                    else => unreachable,
                });
            },
            .type_parameter => {
                const @"type" = self.entries.pop();
                const value = self.entries.pop();

                switch (value) {
                    .local => |local| {
                        const symbol = &self.symbol_table.symbols.items[local];

                        const memory: types.SymbolMemory = switch (@"type") {
                            .primitive => |primitive| .{
                                .primitive = primitive,
                            },
                            .type => |t| .{
                                .type = t,
                            },
                            .env => .{
                                .env = undefined,
                            },
                            else => unreachable,
                        };

                        // TODO: If set, check memory equals previous.
                        symbol.memory = memory;
                    },
                    else => unreachable,
                }

                try self.entries.append(@"type");
            },
            .assignment => {
                const symbol: *types.Symbol = switch (self.entries.items[0]) {
                    .local => |index| &self.symbol_table.symbols.items[index],
                    else => unreachable,
                };

                symbol.memory = switch (self.entries.items[1]) {
                    .primitive => |v| .{ .primitive = v },
                    .type => |v| .{ .type = v },
                    else => unreachable,
                };

                self.entries.clearAndFree();
            },
            .type_definition => {
                var i: usize = 0;

                const symbol: *types.Symbol = switch (self.entries.items[i]) {
                    .type => |index| &self.symbol_table.symbols.items[index],
                    else => unreachable,
                };
                i += 1;

                const type_entry = self.entries.items[i];
                i += 1;

                const alignment: ?usize = switch (self.entries.items.len) {
                    1, 2, 3 => null,
                    else => switch (self.entries.items[i]) {
                        .literal => |literal| scope: {
                            switch (literal.value) {
                                .integer => |integer| {
                                    i += 1;

                                    break :scope @as(usize, @intCast(integer));
                                },
                                else => unreachable,
                            }
                        },
                        else => null,
                    },
                };

                symbol.memory = switch (type_entry) {
                    .@"opaque" => scope: {
                        const literal: *types.Literal = switch (self.entries.items[i]) {
                            .literal => |v| v,
                            else => unreachable,
                        };

                        const size: usize = switch (literal.value) {
                            .integer => |s_size| @as(usize, @intCast(s_size)),
                            else => unreachable,
                        };

                        break :scope .{
                            .@"opaque" = .{
                                .alignment = alignment,
                                .size = size,
                            },
                        };
                    },
                    .@"struct",
                    .@"union",
                    => scope: {
                        var structs = std.ArrayList([]types.SymbolMemoryStructEntry).init(allocator);
                        defer structs.deinit();

                        var members = std.ArrayList(types.SymbolMemoryStructEntry).init(allocator);
                        defer members.deinit();

                        while (i < self.entries.items.len) : (i += 1) {
                            const entry = self.entries.items[i];

                            switch (entry) {
                                .@"struct" => {
                                    if (members.items.len > 0) {
                                        try structs.append(try members.toOwnedSlice());

                                        members.clearAndFree();
                                    }

                                    continue;
                                },
                                else => {},
                            }

                            const next: types.SymbolMemoryStructEntry = switch (entry) {
                                .primitive => |primitive| .{
                                    .base = .{
                                        .primitive = primitive,
                                    },
                                    .count = 1,
                                },
                                .type => |t| .{
                                    .base = .{
                                        .type = t,
                                    },
                                    .count = 1,
                                },
                                .array => |array| .{
                                    .base = switch (array.base) {
                                        .primitive => |v| .{ .primitive = v },
                                        .type => |v| .{ .type = v },
                                    },
                                    .count = array.count,
                                },
                                else => undefined,
                            };

                            try members.append(next);
                        }

                        if (members.items.len > 0) {
                            try structs.append(try members.toOwnedSlice());

                            members.clearAndFree();
                        }

                        if (structs.items.len == 1) {
                            break :scope .{
                                .@"struct" = .{
                                    .alignment = alignment,
                                    .members = structs.items[0],
                                },
                            };
                        } else {
                            const union_structs = try allocator.alloc(types.SymbolMemoryStruct, structs.items.len);

                            for (0..structs.items.len) |j| {
                                union_structs[j] = .{
                                    .alignment = alignment,
                                    .members = structs.items[j],
                                };
                            }

                            break :scope .{
                                .@"union" = .{
                                    .alignment = alignment,
                                    .structs = union_structs,
                                },
                            };
                        }
                    },
                    else => unreachable,
                };

                self.entries.clearAndFree();
            },
            .linkage => |*linkage| {
                const flags: ?[]const u8 = switch (self.entries.items.len) {
                    1, 2 => null,
                    3 => switch (self.entries.pop()) {
                        .literal => |literal| switch (literal.value) {
                            .string => |v| v,
                            else => unreachable,
                        },
                        else => unreachable,
                    },
                    else => unreachable,
                };

                const section: ?[]const u8 = switch (self.entries.items.len) {
                    1 => null,
                    2 => switch (self.entries.pop()) {
                        .literal => |literal| switch (literal.value) {
                            .string => |v| v,
                            else => unreachable,
                        },
                        else => unreachable,
                    },
                    else => unreachable,
                };

                try self.entries.append(.{
                    .linkage = .{
                        .@"export" = linkage.@"export",
                        .thread = linkage.thread,
                        .section = section,
                        .flags = flags,
                    },
                });
            },
            .offset => {
                const offset = self.entries.pop();
                const index = self.entries.pop();

                try self.entries.append(.{
                    .data_offset = .{
                        .index = switch (index) {
                            .global => |v| v,
                            else => unreachable,
                        },
                        .offset = switch (offset) {
                            .literal => |literal| switch (literal.value) {
                                .integer => |v| v,
                                else => unreachable,
                            },
                            else => unreachable,
                        },
                    },
                });
            },
            .typed_data => {
                const value = self.entries.pop();
                const primitive = self.entries.pop();

                try self.entries.append(.{
                    .data_entry = .{
                        .primitive = switch (primitive) {
                            .primitive => |v| v,
                            else => unreachable,
                        },
                        .value = switch (value) {
                            .literal => |v| .{ .literal = v },
                            .data_offset => |v| .{ .symbol = v },
                            else => unreachable,
                        },
                    },
                });
            },
            .data_definition => {
                var i: usize = 0;

                var symbol: *types.Symbol = switch (self.entries.items[i]) {
                    .global => |g| &self.symbol_table.symbols.items[g],
                    else => unreachable,
                };
                i += 1;

                const linkage: types.SymbolMemoryLinkage = switch (self.entries.items[i]) {
                    .linkage => |l| l,
                    else => unreachable,
                };
                i += 1;

                var entries = try allocator.alloc(types.SymbolMemoryDataEntry, self.entries.items.len - i);

                var j: usize = 0;
                while (i < self.entries.items.len) {
                    entries[j] = switch (self.entries.items[i]) {
                        .data_entry => |data_entry| .{
                            .type = data_entry.primitive,
                            .value = switch (data_entry.value) {
                                .literal => |literal| switch (literal.value) {
                                    .integer => |v| .{ .integer = v },
                                    .float => |v| .{ .float = v },
                                    .string => |v| .{ .string = v },
                                },
                                .symbol => |v| .{ .symbol = v },
                            },
                        },
                        else => unreachable,
                    };

                    i += 1;
                    j += 1;
                }

                symbol.memory = .{
                    .data = .{
                        .linkage = linkage,
                        .entries = entries,
                    },
                };

                self.entries.clearAndFree();
            },
            .function_signature => {
                const symbol: *types.Symbol = switch (self.entries.items[0]) {
                    .global => |g| &self.symbol_table.symbols.items[g],
                    else => unreachable,
                };

                const linkage: types.SymbolMemoryLinkage = switch (self.entries.items[1]) {
                    .linkage => |l| l,
                    else => unreachable,
                };

                const @"return": ast.PrimitiveType = switch (self.entries.items[2]) {
                    .primitive => |p| p,
                    else => unreachable,
                };

                const vararg: bool = switch (self.entries.items[self.entries.items.len - 1]) {
                    .variadic => scope: {
                        _ = self.entries.pop();

                        break :scope true;
                    },
                    else => false,
                };

                const offset = 3;
                var parameters = try allocator.alloc(types.SymbolMemoryParameterType, self.entries.items.len - offset);

                var i: usize = 0;
                while (i < parameters.len) : (i += 1) {
                    parameters[i] = switch (self.entries.items[i + offset]) {
                        .primitive => |p| .{ .primitive = p },
                        .type => |t| .{ .type = t },
                        .env => .{ .env = undefined },
                        else => unreachable,
                    };
                }

                symbol.memory = .{
                    .function = .{
                        .linkage = linkage,
                        .@"return" = @"return",
                        .vararg = vararg,
                        .parameters = parameters,
                    },
                };

                self.entries.clearAndFree();
            },
            else => {
                // TODO: Handle all statements.
                self.entries.clearAndFree();
            },
        }
    }
};

//
// Test Utils
//

const test_allocator = std.testing.allocator;
const test_lib = @import("../../test.zig");

fn testMemory(allocator: std.mem.Allocator, file: []const u8, symbol_table: *table.SymbolTable) !void {
    var tree = try test_lib.testAST(allocator, file);
    defer tree.deinit();

    var callback = SymbolMemoryWalkCallback.init(
        symbol_table,
    );

    var walk = ast.ASTWalk.init(allocator, &tree);
    defer walk.deinit();

    try walk.start(tree.entrypoint() orelse return error.NotFound);
    while (try walk.next()) |out| {
        try switch (out.enter) {
            true => callback.enter(out.value),
            false => callback.exit(out.value),
        };
    }
}

//
// Valid Tests
//

test "type_definition" {
    // Arrange
    const file = "type :o = align 8 { 32 } type :s = align 16 { w 100, :o } type :u = align 32 { { s 2 } { w } }";
    const expected = [_]types.Symbol{
        .{
            .identifier = .{
                .name = "o",
                .scope = .type,
            },
            .memory = .{
                .@"opaque" = .{
                    .alignment = 8,
                    .size = 32,
                },
            },
        },
        .{
            .identifier = .{
                .name = "s",
                .scope = .type,
            },
            .memory = .{
                .@"struct" = .{
                    .alignment = 16,
                    .members = &[_]types.SymbolMemoryStructEntry{
                        .{
                            .base = .{
                                .primitive = .word,
                            },
                            .count = 100,
                        },
                        .{
                            .base = .{
                                .type = 0,
                            },
                            .count = 1,
                        },
                    },
                },
            },
        },
        .{
            .identifier = .{
                .name = "u",
                .scope = .type,
            },
            .memory = .{
                .@"union" = .{
                    .alignment = 32,
                    .structs = &[_]types.SymbolMemoryStruct{
                        .{
                            .alignment = 32,
                            .members = &[_]types.SymbolMemoryStructEntry{
                                .{
                                    .base = .{
                                        .primitive = .single,
                                    },
                                    .count = 2,
                                },
                            },
                        },
                        .{
                            .alignment = 32,
                            .members = &[_]types.SymbolMemoryStructEntry{
                                .{
                                    .base = .{
                                        .primitive = .word,
                                    },
                                    .count = 1,
                                },
                            },
                        },
                    },
                },
            },
        },
    };

    var symbol_table = table.SymbolTable.init(test_allocator);
    defer symbol_table.deinit();

    // Act
    try source.testSource(test_allocator, file, &symbol_table);
    try testMemory(test_allocator, file, &symbol_table);

    // Assert
    try std.testing.expectEqualDeep(&expected, symbol_table.symbols.items);
}

test "data_definition" {
    // Arrange
    const file = "export thread section \"data\" \"flags\" data $d = { b -1 1, w -1 1, s s_-1 s_1, d d_-1 d_1 } data $o = { l $d+32 }";
    const expected = [_]types.Symbol{
        .{
            .identifier = .{
                .name = "d",
                .scope = .global,
            },
            .memory = .{
                .data = .{
                    .linkage = .{
                        .@"export" = true,
                        .thread = true,
                        .section = "data",
                        .flags = "flags",
                    },
                    .entries = &[_]types.SymbolMemoryDataEntry{
                        .{
                            .type = .byte,
                            .value = .{
                                .integer = -1,
                            },
                        },
                        .{
                            .type = .byte,
                            .value = .{
                                .integer = 1,
                            },
                        },
                        .{
                            .type = .word,
                            .value = .{
                                .integer = -1,
                            },
                        },
                        .{
                            .type = .word,
                            .value = .{
                                .integer = 1,
                            },
                        },
                        .{
                            .type = .single,
                            .value = .{
                                .float = -1,
                            },
                        },
                        .{
                            .type = .single,
                            .value = .{
                                .float = 1,
                            },
                        },
                        .{
                            .type = .double,
                            .value = .{
                                .float = -1,
                            },
                        },
                        .{
                            .type = .double,
                            .value = .{
                                .float = 1,
                            },
                        },
                    },
                },
            },
        },
        .{
            .identifier = .{
                .name = "o",
                .scope = .global,
            },
            .memory = .{
                .data = .{
                    .linkage = .{
                        .@"export" = false,
                        .thread = false,
                        .section = null,
                        .flags = null,
                    },
                    .entries = &[_]types.SymbolMemoryDataEntry{
                        .{
                            .type = .long,
                            .value = .{
                                .symbol = .{
                                    .index = 0,
                                    .offset = 32,
                                },
                            },
                        },
                    },
                },
            },
        },
    };

    var symbol_table = table.SymbolTable.init(test_allocator);
    defer symbol_table.deinit();

    // Act
    try source.testSource(test_allocator, file, &symbol_table);
    try testMemory(test_allocator, file, &symbol_table);

    // Assert
    try std.testing.expectEqualDeep(&expected, symbol_table.symbols.items);
}

test "function" {
    // Arrange
    const file = "type :t = { 32 } export thread section \"function\" \"flags\" function $test(env %e, w %w, :t %s, ...) {@s ret}";
    const expected = [_]types.Symbol{
        .{
            .identifier = .{
                .name = "t",
                .scope = .type,
            },
            .memory = .{
                .@"opaque" = .{
                    .size = 32,
                },
            },
        },
        .{
            .identifier = .{
                .name = "test",
                .scope = .global,
            },
            .memory = .{
                .function = .{
                    .linkage = .{
                        .@"export" = true,
                        .thread = true,
                        .section = "function",
                        .flags = "flags",
                    },
                    .@"return" = .void,
                    .vararg = true,
                    .parameters = &[_]types.SymbolMemoryParameterType{
                        .{
                            .env = undefined,
                        },
                        .{
                            .primitive = .word,
                        },
                        .{
                            .type = 0,
                        },
                    },
                },
            },
        },
        .{
            .identifier = .{
                .name = "e",
                .scope = .local,
                .function = 1,
            },
            .memory = .{
                .env = undefined,
            },
        },
        .{
            .identifier = .{
                .name = "w",
                .scope = .local,
                .function = 1,
            },
            .memory = .{
                .primitive = .word,
            },
        },
        .{
            .identifier = .{
                .name = "s",
                .scope = .local,
                .function = 1,
            },
            .memory = .{
                .type = 0,
            },
        },
        .{
            .identifier = .{
                .name = "s",
                .scope = .label,
                .function = 1,
            },
        },
    };

    var symbol_table = table.SymbolTable.init(test_allocator);
    defer symbol_table.deinit();

    // Act
    try source.testSource(test_allocator, file, &symbol_table);
    try testMemory(test_allocator, file, &symbol_table);

    // Assert
    try std.testing.expectEqualDeep(&expected, symbol_table.symbols.items);
}

test "assignment" {
    // Arrange
    const file = "type :t = { 32 } function $test() {@s %f =w add 0, 0 %s =:t add 0, 0 ret}";
    const expected = [_]types.Symbol{
        .{
            .identifier = .{
                .name = "t",
                .scope = .type,
            },
            .memory = .{
                .@"opaque" = .{
                    .size = 32,
                },
            },
        },
        .{
            .identifier = .{
                .name = "test",
                .scope = .global,
            },
            .memory = .{
                .function = .{
                    .linkage = .{},
                    .@"return" = .void,
                    .vararg = false,
                    .parameters = &[_]types.SymbolMemoryParameterType{},
                },
            },
        },
        .{
            .identifier = .{
                .name = "s",
                .scope = .label,
                .function = 1,
            },
        },
        .{
            .identifier = .{
                .name = "f",
                .scope = .local,
                .function = 1,
            },
            .memory = .{
                .primitive = .word,
            },
        },
        .{
            .identifier = .{
                .name = "s",
                .scope = .local,
                .function = 1,
            },
            .memory = .{
                .type = 0,
            },
        },
    };

    var symbol_table = table.SymbolTable.init(test_allocator);
    defer symbol_table.deinit();

    // Act
    try source.testSource(test_allocator, file, &symbol_table);
    try testMemory(test_allocator, file, &symbol_table);

    // Assert
    try std.testing.expectEqualDeep(&expected, symbol_table.symbols.items);
}

//
// Error Tests
//
