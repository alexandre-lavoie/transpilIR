const std = @import("std");

const ast = @import("../ast/lib.zig");
const common = @import("../common.zig");
const types = @import("./types.zig");
const utils = @import("utils.zig");

pub const SymbolTable = struct {
    allocator: std.mem.Allocator,

    symbols: SymbolList,
    symbol_identifier_map: IndexMap,
    symbol_instance_map: IndexMap,

    literals: LiteralList,
    literal_instance_map: IndexMap,

    types: IndexList,
    globals: IndexList,
    function_locals: IndexMapList,

    span: usize = 0,

    const Self = @This();

    const SymbolList = std.ArrayList(types.Symbol);
    const LiteralList = std.ArrayList(types.Literal);

    const IndexList = std.ArrayList(usize);
    const IndexMap = std.AutoArrayHashMap(usize, usize);
    const IndexMapList = std.AutoArrayHashMap(usize, IndexList);

    // Function should always have some locals
    // Specifying a large enough size to reduce re-allocations
    const DEFAULT_FUNCTION_LOCAL_CAPACITY = 32;

    pub fn init(allocator: std.mem.Allocator) Self {
        return .{
            .allocator = allocator,
            .symbols = SymbolList.init(allocator),
            .symbol_identifier_map = IndexMap.init(allocator),
            .symbol_instance_map = IndexMap.init(allocator),
            .literals = LiteralList.init(allocator),
            .literal_instance_map = IndexMap.init(allocator),
            .types = IndexList.init(allocator),
            .globals = IndexList.init(allocator),
            .function_locals = IndexMapList.init(allocator),
        };
    }

    pub fn deinit(self: *Self) void {
        self.types.deinit();
        self.globals.deinit();

        for (self.function_locals.values()) |*list| {
            list.deinit();
        }

        self.function_locals.deinit();

        for (self.symbols.items) |*symbol| {
            self.allocator.free(symbol.identifier.name);

            utils.freeSymbolMemory(self.allocator, symbol.memory);
        }

        self.symbols.deinit();

        self.symbol_identifier_map.deinit();

        self.symbol_instance_map.deinit();

        for (self.literals.items) |*literal| {
            switch (literal.value) {
                .string => |s| self.literals.allocator.free(s),
                else => {},
            }
        }
        self.literals.deinit();

        self.literal_instance_map.deinit();
    }

    pub fn nextSpan(self: *Self) common.SourceSpan {
        self.span += 1;

        return .{ .start = self.span, .end = self.span };
    }

    pub fn getSymbolPtr(self: *const Self, idx: usize) ?*const types.Symbol {
        if (idx >= self.symbols.items.len) return null;
        return &self.symbols.items[idx];
    }

    pub fn getSymbolPtrMut(self: *Self, idx: usize) ?*types.Symbol {
        if (idx >= self.symbols.items.len) return null;
        return &self.symbols.items[idx];
    }

    pub fn addSymbol(self: *Self, identifier: *const types.SymbolIdentifier) !usize {
        const name = try self.allocator.alloc(u8, identifier.name.len);
        errdefer self.allocator.free(name);
        @memcpy(name, identifier.name);

        const key = try identifier.key(self.allocator);

        const identifier_copy: types.SymbolIdentifier = .{
            .name = name,
            .scope = identifier.scope,
            .function = identifier.function,
        };

        const index = self.symbols.items.len;
        try self.symbols.append(.{
            .identifier = identifier_copy,
        });

        switch (identifier.scope) {
            .global => try self.globals.append(index),
            .type => try self.types.append(index),
            .local => {
                if (identifier.function) |func| {
                    if (!self.function_locals.contains(func)) {
                        const list = try IndexList.initCapacity(
                            self.allocator,
                            DEFAULT_FUNCTION_LOCAL_CAPACITY,
                        );

                        try self.function_locals.put(func, list);
                    }

                    const list = self.function_locals.getPtr(func).?;
                    try list.append(index);
                }
            },
            .label => {},
        }

        try self.symbol_identifier_map.put(key, index);

        return index;
    }

    pub fn getSymbolIndexByIdentifier(self: *const Self, identifier: *const types.SymbolIdentifier) ?usize {
        const key = identifier.key(self.allocator) catch return null;

        return self.symbol_identifier_map.get(key) orelse return null;
    }

    pub fn getSymbolByIdentifier(self: *const Self, identifier: *const types.SymbolIdentifier) ?*types.Symbol {
        const index = self.getSymbolIndexByIdentifier(identifier) orelse return null;

        return &self.symbols.items[index];
    }

    pub fn containsSymbolIdentifier(self: *const Self, identifier: *const types.SymbolIdentifier) bool {
        const key = identifier.key(self.allocator) catch return false;

        if (self.symbol_identifier_map.get(key)) |_| {
            return true;
        } else {
            return false;
        }
    }

    pub fn addSymbolInstanceByIndex(self: *Self, sym: usize, instance: *const types.Instance) !usize {
        const key = try instance.key(self.allocator);
        try self.symbol_instance_map.put(key, sym);

        self.span = @max(self.span, instance.span.end + 1);

        return sym;
    }

    pub fn addSymbolInstance(self: *Self, identifier: *const types.SymbolIdentifier, instance: *const types.Instance) !usize {
        const idkey = try identifier.key(self.allocator);
        const index = self.symbol_identifier_map.get(idkey) orelse return error.SymbolNotFound;

        return self.addSymbolInstanceByIndex(index, instance);
    }

    pub fn updateSymbolInstanceByIndex(self: *Self, sym: usize, instance: *const types.Instance) !usize {
        const key = try instance.key(self.allocator);
        try self.symbol_instance_map.put(key, sym);

        self.span = @max(self.span, instance.span.end + 1);

        return sym;
    }

    pub fn updateSymbolInstance(self: *Self, identifier: *const types.SymbolIdentifier, instance: *const types.Instance) !usize {
        const idkey = try identifier.key(self.allocator);
        const index = self.symbol_identifier_map.get(idkey) orelse return error.SymbolNotFound;

        return self.updateSymbolInstanceByIndex(index, instance);
    }

    pub fn getSymbolIndexByInstance(self: *const Self, instance: *const types.Instance) ?usize {
        const key = instance.key(self.allocator) catch return null;

        return self.symbol_instance_map.get(key) orelse return null;
    }

    pub fn getSymbolByInstance(self: *const Self, instance: *const types.Instance) ?*types.Symbol {
        const index = self.getSymbolIndexByInstance(instance) orelse return null;

        return &self.symbols.items[index];
    }

    pub fn addLiteral(self: *Self, value: *const types.LiteralValue) !usize {
        const index = self.literals.items.len;

        const value_copy: types.LiteralValue = switch (value.*) {
            .integer => |i| .{ .integer = i },
            .single => |f| .{ .single = f },
            .double => |f| .{ .double = f },
            .string => |s| scope: {
                const t = try self.allocator.alloc(u8, s.len);
                errdefer self.allocator.free(t);

                @memcpy(t, s);

                break :scope .{ .string = t };
            },
        };

        try self.literals.append(.{ .value = value_copy });

        return index;
    }

    pub fn addLiteralInstance(self: *Self, index: usize, instance: *const types.Instance) !usize {
        const key = try instance.key(self.allocator);

        try self.literal_instance_map.put(key, index);

        self.span = @max(self.span, instance.span.end + 1);

        return index;
    }

    pub fn getLiteralByInstance(self: *const Self, instance: *const types.Instance) ?*types.Literal {
        const key = instance.key(self.allocator) catch return null;

        const index = self.literal_instance_map.get(key) orelse return null;

        return &self.literals.items[index];
    }
};

//
// Valid Tests
//

test "symbol" {
    // Arrange
    const allocator = std.testing.allocator;

    var symbol_table = SymbolTable.init(allocator);
    defer symbol_table.deinit();

    const identifier: types.SymbolIdentifier = .{
        .name = "symbol",
        .scope = .local,
    };
    _ = try symbol_table.addSymbol(&identifier);

    const instance: types.Instance = .{ .span = .{} };

    // Act
    _ = try symbol_table.addSymbolInstance(&identifier, &instance);

    // Assert
    const actual = symbol_table.getSymbolByInstance(&instance) orelse return error.NotFound;

    try std.testing.expectEqualDeep(identifier, actual.identifier);
}

test "symbol with same name but different scope" {
    // Arrange
    const allocator = std.testing.allocator;

    var symbol_table = SymbolTable.init(allocator);
    defer symbol_table.deinit();

    const local_identifier: types.SymbolIdentifier = .{
        .name = "symbol",
        .scope = .local,
    };

    const global_identifier: types.SymbolIdentifier = .{
        .name = "symbol",
        .scope = .global,
    };

    // Act
    _ = try symbol_table.addSymbol(&local_identifier);
    _ = try symbol_table.addSymbol(&global_identifier);

    // Assert
    try std.testing.expectEqualDeep(local_identifier, symbol_table.getSymbolByIdentifier(&local_identifier).?.identifier);
    try std.testing.expectEqualDeep(global_identifier, symbol_table.getSymbolByIdentifier(&global_identifier).?.identifier);
}

test "literal" {
    // Arrange
    const allocator = std.testing.allocator;

    var symbol_table = SymbolTable.init(allocator);
    defer symbol_table.deinit();

    const value: types.LiteralValue = .{
        .string = "Test",
    };
    const index = try symbol_table.addLiteral(&value);

    const instance: types.Instance = .{ .span = .{} };

    // Act
    _ = try symbol_table.addLiteralInstance(index, &instance);

    // Assert
    const actual = symbol_table.getLiteralByInstance(&instance) orelse return error.NotFound;

    try std.testing.expectEqualDeep(value, actual.value);
}

//
// Error Tests
//

test "error.SymbolNotFound" {
    // Arrange
    const allocator = std.testing.allocator;

    var symbol_table = SymbolTable.init(allocator);
    defer symbol_table.deinit();

    const identifier: types.SymbolIdentifier = .{
        .name = "symbol",
        .scope = .local,
    };
    _ = try symbol_table.addSymbol(&identifier);

    const invalid_identifier: types.SymbolIdentifier = .{
        .name = "invalid",
        .scope = .local,
    };

    const instance: types.Instance = .{ .span = .{} };

    // Act + Assert
    try std.testing.expectError(
        error.SymbolNotFound,
        symbol_table.addSymbolInstance(&invalid_identifier, &instance),
    );
}
