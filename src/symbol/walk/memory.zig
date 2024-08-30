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
    zero: void,
    variadic: void,
    linkage: types.SymbolMemoryLinkage,
    data_offset: types.SymbolMemoryDataOffset,
    data_entry: union(enum) {
        init: struct {
            primitive: ast.PrimitiveType,
            value: union(enum) {
                symbol: types.SymbolMemoryDataOffset,
                literal: *types.Literal,
            },
        },
        zero: usize,
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
    function: ?usize = null,

    const Self = @This();
    const EntryList = std.ArrayList(SymbolMemoryEntry);

    pub fn init(symbol_table: *table.SymbolTable) Self {
        return .{
            .symbol_table = symbol_table,
            .entries = EntryList.init(symbol_table.symbols.allocator),
        };
    }

    pub fn deinit(self: *Self) void {
        self.entries.deinit();
    }

    fn entryToType(self: *Self, entry: *const SymbolMemoryEntry) ast.PrimitiveType {
        return switch (entry.*) {
            .local => |i| switch (self.symbol_table.symbols.items[i].memory) {
                .primitive => |v| v,
                .type => .long,
                else => unreachable,
            },
            .global => .long,
            .literal => |v| switch (v.value) {
                .integer => .void,
                .single => .single,
                .double => .double,
                .string => .long,
            },
            else => unreachable,
        };
    }

    fn matchType(left: ast.PrimitiveType, right: ast.PrimitiveType) !ast.PrimitiveType {
        if (left == right) return left;

        if (left == .void) {
            return right;
        } else if (right == .void) {
            return left;
        }

        return error.TypeError;
    }

    pub fn enter(self: *Self, statement: *ast.Statement) !void {
        const instance: types.Instance = .{ .span = statement.span };

        switch (statement.data) {
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
            .zero_type => {
                try self.entries.append(.{
                    .zero = undefined,
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
            // TODO: Handle all statements.
            .vaarg,
            .vastart,
            .allocate,
            .blit,
            .copy,
            .load,
            .store,
            .branch,
            .halt,
            .jump,
            .phi,
            .phi_parameter,
            .negate,
            => unreachable,
            // ----------------------------
            .block,
            .env_type,
            .identifier,
            .literal,
            .module,
            .node,
            .opaque_type,
            .primitive_type,
            .struct_type,
            .union_type,
            .variadic_parameter,
            .zero_type,
            => {},
            .line => {
                self.entries.clearAndFree();
            },
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
            .function_parameter => {
                const @"type" = self.entries.pop();
                const value = self.entries.pop();

                switch (value) {
                    .local => |local| {
                        const symbol = &self.symbol_table.symbols.items[local];

                        symbol.memory = switch (@"type") {
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
                    },
                    else => unreachable,
                }

                try self.entries.append(@"type");
            },
            .binary_operation, .comparison => {
                const right = self.entries.pop();
                const left = self.entries.pop();

                const @"type" = try Self.matchType(self.entryToType(&left), self.entryToType(&right));

                try self.entries.append(.{ .primitive = @"type" });
            },
            .call_parameter => {
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

                        if (!std.meta.eql(symbol.memory, memory)) {
                            return error.InvalidType;
                        }
                    },
                    .global,
                    .literal,
                    => {},
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
                const value_type = self.entries.pop();

                try self.entries.append(switch (value_type) {
                    .primitive => |primitive| .{
                        .data_entry = .{
                            .init = .{
                                .primitive = primitive,
                                .value = switch (value) {
                                    .literal => |v| .{ .literal = v },
                                    .data_offset => |v| .{ .symbol = v },
                                    else => unreachable,
                                },
                            },
                        },
                    },
                    .zero => .{
                        .data_entry = .{
                            .zero = switch (value) {
                                .literal => |literal| switch (literal.value) {
                                    .integer => |v| @intCast(v),
                                    else => return error.InvalidSize,
                                },
                                else => return error.InvalidSize,
                            },
                        },
                    },
                    else => unreachable,
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
                        .data_entry => |data_entry| switch (data_entry) {
                            .init => |*vi| .{
                                .init = .{
                                    .type = vi.primitive,
                                    .value = switch (vi.value) {
                                        .literal => |literal| switch (literal.value) {
                                            .integer => |v| .{ .integer = v },
                                            .single => |v| .{ .single = v },
                                            .double => |v| .{ .double = v },
                                            .string => |v| .{ .string = v },
                                        },
                                        .symbol => |v| .{ .symbol = v },
                                    },
                                },
                            },
                            .zero => |size| .{
                                .zero = size,
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
            .function => {
                self.function = null;
            },
            .function_signature => {
                const symbol_index: usize = switch (self.entries.items[0]) {
                    .global => |g| g,
                    else => unreachable,
                };
                self.function = symbol_index;

                const symbol: *types.Symbol = &self.symbol_table.symbols.items[symbol_index];

                const linkage: types.SymbolMemoryLinkage = switch (self.entries.items[1]) {
                    .linkage => |l| l,
                    else => unreachable,
                };

                const @"return": types.SymbolType = switch (self.entries.items[2]) {
                    .primitive => |p| .{ .primitive = p },
                    .type => |t| .{ .type = t },
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

                const memory: types.SymbolMemory = .{
                    .function = .{
                        .linkage = linkage,
                        .@"return" = @"return",
                        .vararg = vararg,
                        .parameters = parameters,
                    },
                };

                // TODO: Check definition matches.
                switch (symbol.memory) {
                    .empty => {
                        symbol.memory = memory;
                    },
                    .function => |f| {
                        allocator.free(f.parameters);

                        symbol.memory = memory;
                    },
                    else => unreachable,
                }

                self.entries.clearAndFree();
            },
            .call => {
                const symbol: *types.Symbol = switch (self.entries.items[0]) {
                    .global => |g| &self.symbol_table.symbols.items[g],
                    else => unreachable,
                };

                const return_entry = self.entries.items[1];
                const @"return": types.SymbolType = switch (return_entry) {
                    .primitive => |p| .{ .primitive = p },
                    .type => |t| .{ .type = t },
                    else => unreachable,
                };

                const offset = 2;
                const length = self.entries.items.len - offset;
                var parameters = try allocator.alloc(types.SymbolMemoryParameterType, length);

                var vararg: bool = false;
                var i: usize = 0;
                while (i < parameters.len) : (i += 1) {
                    parameters[i] = switch (self.entries.items[i + offset]) {
                        .primitive => |p| .{ .primitive = p },
                        .type => |t| .{ .type = t },
                        .env => .{ .env = undefined },
                        .variadic => {
                            vararg = true;
                            break;
                        },
                        else => unreachable,
                    };
                }

                if (i < length) {
                    parameters = try allocator.realloc(parameters, i);
                }

                const memory: types.SymbolMemory = .{
                    .function = .{
                        .linkage = .{},
                        .@"return" = @"return",
                        .vararg = vararg,
                        .parameters = parameters,
                    },
                };

                // TODO: Check definition matches.
                switch (symbol.memory) {
                    .empty => {
                        symbol.memory = memory;
                    },
                    .function => {
                        allocator.free(parameters);
                    },
                    else => unreachable,
                }

                self.entries.clearAndFree();
                try self.entries.append(return_entry);
            },
            .@"return" => {
                if (self.entries.items.len == 0) return;

                const expected: ast.PrimitiveType = switch (self.symbol_table.symbols.items[self.function orelse unreachable].memory) {
                    .function => |function| switch (function.@"return") {
                        .primitive => |p| p,
                        .type => .long,
                    },
                    else => unreachable,
                };
                const actual = self.entryToType(&self.entries.pop());

                if (actual != .void and expected != actual) return error.TypeError;
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
    defer callback.deinit();

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
    const file = "export thread section \"data\" \"flags\" data $d = { b -1 1, w -1 1, s s_-1 s_1, d d_-1 d_1, z 10 } data $o = { l $d+32 }";
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
                            .init = .{
                                .type = .byte,
                                .value = .{
                                    .integer = -1,
                                },
                            },
                        },
                        .{
                            .init = .{
                                .type = .byte,
                                .value = .{
                                    .integer = 1,
                                },
                            },
                        },
                        .{
                            .init = .{
                                .type = .word,
                                .value = .{
                                    .integer = -1,
                                },
                            },
                        },
                        .{
                            .init = .{
                                .type = .word,
                                .value = .{
                                    .integer = 1,
                                },
                            },
                        },
                        .{
                            .init = .{
                                .type = .single,
                                .value = .{
                                    .single = -1,
                                },
                            },
                        },
                        .{
                            .init = .{
                                .type = .single,
                                .value = .{
                                    .single = 1,
                                },
                            },
                        },
                        .{
                            .init = .{
                                .type = .double,
                                .value = .{
                                    .double = -1,
                                },
                            },
                        },
                        .{
                            .init = .{
                                .type = .double,
                                .value = .{
                                    .double = 1,
                                },
                            },
                        },
                        .{ .zero = 10 },
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
                            .init = .{
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
    const file = "type :t = { 32 } export thread section \"function\" \"flags\" function :t $test(env %e, w %w, :t %s, ...) {@s ret}";
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
                    .@"return" = .{
                        .type = 0,
                    },
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

test "call" {
    // Arrange
    const file = "type :t = { 32 } function $test(env %e, l %l, :t %s, ...) {@s call $other(env %e, w 0, :t %s, ..., l %l) call $test(env %e, l $test, :t %s, ..., l %l) ret}";
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
                    .@"return" = .{
                        .primitive = .void,
                    },
                    .vararg = true,
                    .parameters = &[_]types.SymbolMemoryParameterType{
                        .{
                            .env = undefined,
                        },
                        .{
                            .primitive = .long,
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
                .name = "l",
                .scope = .local,
                .function = 1,
            },
            .memory = .{
                .primitive = .long,
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
        .{
            .identifier = .{
                .name = "other",
                .scope = .global,
            },
            .memory = .{
                .function = .{
                    .linkage = .{},
                    .@"return" = .{
                        .primitive = .void,
                    },
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
    const file = "type :t = { 32 } data $d = { w 0 } function $test() {@s %f =l add $d, 1 %s =:t add $d, %f ret}";
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
                .name = "d",
                .scope = .global,
            },
            .memory = .{
                .data = .{
                    .linkage = .{},
                    .entries = &[_]types.SymbolMemoryDataEntry{.{
                        .init = .{
                            .type = .word,
                            .value = .{
                                .integer = 0,
                            },
                        },
                    }},
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
                    .@"return" = .{
                        .primitive = .void,
                    },
                    .vararg = false,
                    .parameters = &[_]types.SymbolMemoryParameterType{},
                },
            },
        },
        .{
            .identifier = .{
                .name = "s",
                .scope = .label,
                .function = 2,
            },
        },
        .{
            .identifier = .{
                .name = "f",
                .scope = .local,
                .function = 2,
            },
            .memory = .{
                .primitive = .long,
            },
        },
        .{
            .identifier = .{
                .name = "s",
                .scope = .local,
                .function = 2,
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

test "return" {
    // Arrange
    const file = "type :t = { 32 } data $d = { w 0 } function w $ret1(w %p) {@s ret %p} function :t $ret2() {@s ret $d} function l $ret3() {@s ret 0}";
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
                .name = "d",
                .scope = .global,
            },
            .memory = .{
                .data = .{
                    .linkage = .{},
                    .entries = &[_]types.SymbolMemoryDataEntry{.{
                        .init = .{
                            .type = .word,
                            .value = .{
                                .integer = 0,
                            },
                        },
                    }},
                },
            },
        },
        .{
            .identifier = .{
                .name = "ret1",
                .scope = .global,
            },
            .memory = .{
                .function = .{
                    .linkage = .{},
                    .@"return" = .{
                        .primitive = .word,
                    },
                    .vararg = false,
                    .parameters = &[_]types.SymbolMemoryParameterType{
                        .{
                            .primitive = .word,
                        },
                    },
                },
            },
        },
        .{
            .identifier = .{
                .name = "p",
                .scope = .local,
                .function = 2,
            },
            .memory = .{
                .primitive = .word,
            },
        },
        .{
            .identifier = .{
                .name = "s",
                .scope = .label,
                .function = 2,
            },
        },
        .{
            .identifier = .{
                .name = "ret2",
                .scope = .global,
            },
            .memory = .{
                .function = .{
                    .linkage = .{},
                    .@"return" = .{
                        .type = 0,
                    },
                    .vararg = false,
                    .parameters = &[_]types.SymbolMemoryParameterType{},
                },
            },
        },
        .{
            .identifier = .{
                .name = "s",
                .scope = .label,
                .function = 5,
            },
        },
        .{
            .identifier = .{
                .name = "ret3",
                .scope = .global,
            },
            .memory = .{
                .function = .{
                    .linkage = .{},
                    .@"return" = .{
                        .primitive = .long,
                    },
                    .vararg = false,
                    .parameters = &[_]types.SymbolMemoryParameterType{},
                },
            },
        },
        .{
            .identifier = .{
                .name = "s",
                .scope = .label,
                .function = 7,
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

test "error.TypeError binary_operation" {
    // Arrange
    const file = "function $test(w %a, s %b) {@s %c =w add %a, %b ret}";

    var symbol_table = table.SymbolTable.init(test_allocator);
    defer symbol_table.deinit();

    // Act
    try source.testSource(test_allocator, file, &symbol_table);
    const res = testMemory(test_allocator, file, &symbol_table);

    // Assert
    try std.testing.expectError(error.TypeError, res);
}

test "error.InvalidSize literal" {
    // Arrange
    const file = "data $d = { z s_1 }";

    var symbol_table = table.SymbolTable.init(test_allocator);
    defer symbol_table.deinit();

    // Act
    try source.testSource(test_allocator, file, &symbol_table);
    const res = testMemory(test_allocator, file, &symbol_table);

    // Assert
    try std.testing.expectError(error.InvalidSize, res);
}

test "error.InvalidSize global" {
    // Arrange
    const file = "data $d = { z $d }";

    var symbol_table = table.SymbolTable.init(test_allocator);
    defer symbol_table.deinit();

    // Act
    try source.testSource(test_allocator, file, &symbol_table);
    const res = testMemory(test_allocator, file, &symbol_table);

    // Assert
    try std.testing.expectError(error.InvalidSize, res);
}
