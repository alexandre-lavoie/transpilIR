const std = @import("std");

const ast = @import("../ast/lib.zig");
const types = @import("./types.zig");
const common = @import("../common.zig");

pub const SymbolTable = struct {
    symbols: SymbolList,
    symbol_identifier_map: SymbolIdentifierMap,
    symbol_instance_map: SymbolInstanceMap,

    literals: LiteralList,
    literal_instance_map: LiteralInstanceMap,

    const Self = @This();

    const SymbolList = std.ArrayList(types.Symbol);
    const SymbolIdentifierMap = std.StringArrayHashMap(usize);
    const SymbolInstanceMap = std.AutoArrayHashMap(types.Instance, usize);

    const LiteralList = std.ArrayList(types.Literal);
    const LiteralInstanceMap = std.AutoArrayHashMap(types.Instance, usize);

    pub fn init(allocator: std.mem.Allocator) Self {
        return .{
            .symbols = SymbolList.init(allocator),
            .symbol_identifier_map = SymbolIdentifierMap.init(allocator),
            .symbol_instance_map = SymbolInstanceMap.init(allocator),
            .literals = LiteralList.init(allocator),
            .literal_instance_map = LiteralInstanceMap.init(allocator),
        };
    }

    pub fn deinit(self: *Self) void {
        for (self.symbols.items) |*symbol| {
            const allocator = self.symbols.allocator;

            switch (symbol.memory) {
                .@"struct" => |s| {
                    allocator.free(s.members);
                },
                .@"union" => |u| {
                    for (u.structs) |s| {
                        allocator.free(s.members);
                    }

                    allocator.free(u.structs);
                },
                .data => |d| {
                    allocator.free(d.entries);
                },
                .function => |f| {
                    allocator.free(f.parameters);
                },
                else => {},
            }

            allocator.free(symbol.identifier.name);
        }
        self.symbols.deinit();

        for (self.symbol_identifier_map.keys()) |*key| {
            self.symbol_identifier_map.allocator.free(key.*);
        }
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

    pub fn addSymbol(self: *Self, identifier: *const types.SymbolIdentifier) !usize {
        const allocator = self.symbols.allocator;

        const name = try allocator.alloc(u8, identifier.name.len);
        errdefer allocator.free(name);
        @memcpy(name, identifier.name);

        const key = try identifier.key(allocator);
        errdefer allocator.free(key);

        const identifier_copy: types.SymbolIdentifier = .{
            .name = name,
            .scope = identifier.scope,
            .function = identifier.function,
        };

        const index = self.symbols.items.len;
        try self.symbols.append(.{
            .identifier = identifier_copy,
        });

        try self.symbol_identifier_map.put(key, index);

        return index;
    }

    pub fn getSymbolByIdentifier(self: *const Self, identifier: *const types.SymbolIdentifier) ?*types.Symbol {
        const allocator = self.symbols.allocator;
        const key = identifier.key(allocator) catch return null;
        defer allocator.free(key);

        const index = self.symbol_identifier_map.get(key) orelse return null;

        return &self.symbols.items[index];
    }

    pub fn containsSymbolIdentifier(self: *const Self, identifier: *const types.SymbolIdentifier) bool {
        const allocator = self.symbols.allocator;
        const key = identifier.key(allocator) catch return false;
        defer allocator.free(key);

        if (self.symbol_identifier_map.get(key)) |_| {
            return true;
        } else {
            return false;
        }
    }

    pub fn addSymbolInstance(self: *Self, identifier: *const types.SymbolIdentifier, instance: *const types.Instance) !usize {
        const allocator = self.symbols.allocator;
        const key = try identifier.key(allocator);
        defer allocator.free(key);

        const index = self.symbol_identifier_map.get(key) orelse return error.SymbolNotFound;

        try self.symbol_instance_map.put(instance.*, index);

        return index;
    }

    pub fn getSymbolByInstance(self: *const Self, instance: *const types.Instance) ?*types.Symbol {
        const index = self.symbol_instance_map.get(instance.*) orelse return null;

        return &self.symbols.items[index];
    }

    pub fn getSymbolIndexByInstance(self: *const Self, instance: *const types.Instance) ?usize {
        return self.symbol_instance_map.get(instance.*) orelse return null;
    }

    pub fn addLiteral(self: *Self, value: *const types.LiteralValue) !usize {
        const allocator = self.literals.allocator;

        const index = self.literals.items.len;

        const value_copy: types.LiteralValue = switch (value.*) {
            .integer => |i| .{ .integer = i },
            .single => |f| .{ .single = f },
            .double => |f| .{ .double = f },
            .string => |s| scope: {
                const t = try allocator.alloc(u8, s.len);
                errdefer allocator.free(t);

                @memcpy(t, s);

                break :scope .{ .string = t };
            },
        };

        try self.literals.append(.{ .value = value_copy });

        return index;
    }

    pub fn addLiteralInstance(self: *Self, index: usize, instance: *const types.Instance) !usize {
        try self.literal_instance_map.put(instance.*, index);

        return index;
    }

    pub fn getLiteralByInstance(self: *const Self, instance: *const types.Instance) ?*types.Literal {
        const index = self.literal_instance_map.get(instance.*) orelse return null;

        return &self.literals.items[index];
    }
};

//
// Test Utils
//

const test_allocator = std.testing.allocator;

//
// Valid Tests
//

test "symbol" {
    // Arrange
    var symbol_table = SymbolTable.init(test_allocator);
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
    var symbol_table = SymbolTable.init(test_allocator);
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
    var symbol_table = SymbolTable.init(test_allocator);
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
    var symbol_table = SymbolTable.init(test_allocator);
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
