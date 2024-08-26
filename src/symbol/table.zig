const std = @import("std");

const ast = @import("../ast/lib.zig");
const common = @import("../common.zig");

pub const Instance = struct {
    span: common.SourceSpan,
};

pub const SymbolIdentifier = struct {
    name: []const u8,
    scope: ast.Scope,

    const Self = @This();

    pub fn key(self: *const Self, allocator: std.mem.Allocator) ![]const u8 {
        const scope_value = @intFromEnum(self.scope);

        return try std.fmt.allocPrint(allocator, "{s}:{}", .{ self.name, scope_value });
    }
};

pub const Symbol = struct {
    identifier: SymbolIdentifier,
    position: usize,
};

pub const SymbolTable = struct {
    symbols: SymbolList,
    symbol_identifier_map: SymbolIdentifierMap,
    symbol_instance_map: SymbolInstanceMap,

    const Self = @This();

    const SymbolList = std.ArrayList(Symbol);
    const SymbolIdentifierMap = std.StringArrayHashMap(usize);
    const SymbolInstanceMap = std.AutoArrayHashMap(Instance, usize);

    pub fn init(allocator: std.mem.Allocator) Self {
        return .{
            .symbols = SymbolList.init(allocator),
            .symbol_identifier_map = SymbolIdentifierMap.init(allocator),
            .symbol_instance_map = SymbolInstanceMap.init(allocator),
        };
    }

    pub fn deinit(self: *Self) void {
        for (self.symbols.items) |*symbol| {
            self.symbols.allocator.free(symbol.identifier.name);
        }
        self.symbols.deinit();

        for (self.symbol_identifier_map.keys()) |*key| {
            self.symbol_identifier_map.allocator.free(key.*);
        }
        self.symbol_identifier_map.deinit();

        self.symbol_instance_map.deinit();
    }

    pub fn addSymbol(self: *Self, identifier: *const SymbolIdentifier, position: usize) !usize {
        const allocator = self.symbols.allocator;

        const name = try allocator.alloc(u8, identifier.name.len);
        errdefer allocator.free(name);
        @memcpy(name, identifier.name);

        const key = try identifier.key(allocator);
        errdefer allocator.free(key);

        const identifier_copy: SymbolIdentifier = .{
            .name = name,
            .scope = identifier.scope,
        };

        const index = self.symbols.items.len;
        try self.symbols.append(.{
            .identifier = identifier_copy,
            .position = position,
        });

        try self.symbol_identifier_map.put(key, index);

        return index;
    }

    pub fn getSymbolByIdentifier(self: *const Self, identifier: *const SymbolIdentifier) ?*Symbol {
        const allocator = self.symbols.allocator;
        const key = identifier.key(allocator) catch return null;
        defer allocator.free(key);

        const index = self.symbol_identifier_map.get(key) orelse return null;

        return &self.symbols.items[index];
    }

    pub fn containsSymbolIdentifier(self: *const Self, identifier: *const SymbolIdentifier) bool {
        const allocator = self.symbols.allocator;
        const key = identifier.key(allocator) catch return false;
        defer allocator.free(key);

        if (self.symbol_identifier_map.get(key)) |_| {
            return true;
        } else {
            return false;
        }
    }

    pub fn addSymbolInstance(self: *Self, identifier: *const SymbolIdentifier, span: *const common.SourceSpan) !usize {
        const allocator = self.symbols.allocator;
        const key = try identifier.key(allocator);
        defer allocator.free(key);

        const index = self.symbol_identifier_map.get(key) orelse return error.SymbolNotFound;

        try self.symbol_instance_map.put(.{ .span = span.* }, index);

        return index;
    }

    pub fn getSymbolByInstance(self: *const Self, instance: *const Instance) ?*Symbol {
        const index = self.symbol_instance_map.get(instance.*) orelse return null;

        return &self.symbols.items[index];
    }
};

//
// Test Utils
//

const test_allocator = std.testing.allocator;

//
// Valid Tests
//

test "same name but different scope" {
    // Arrange
    var symbol_table = SymbolTable.init(test_allocator);
    defer symbol_table.deinit();

    const local_identifier: SymbolIdentifier = .{
        .name = "symbol",
        .scope = .local,
    };

    const global_identifier: SymbolIdentifier = .{
        .name = "symbol",
        .scope = .global,
    };

    // Act
    _ = try symbol_table.addSymbol(&local_identifier, 0);
    _ = try symbol_table.addSymbol(&global_identifier, 1);

    // Assert
    try std.testing.expectEqualDeep(local_identifier, symbol_table.getSymbolByIdentifier(&local_identifier).?.identifier);
    try std.testing.expectEqualDeep(global_identifier, symbol_table.getSymbolByIdentifier(&global_identifier).?.identifier);
}

test "instance" {
    // Arrange
    var symbol_table = SymbolTable.init(test_allocator);
    defer symbol_table.deinit();

    const identifier: SymbolIdentifier = .{
        .name = "symbol",
        .scope = .local,
    };
    _ = try symbol_table.addSymbol(&identifier, 0);

    const instance: Instance = .{ .span = .{} };

    // Act
    _ = try symbol_table.addSymbolInstance(&identifier, &instance.span);

    // Assert
    const actual = symbol_table.getSymbolByInstance(&instance) orelse return error.NotFound;

    try std.testing.expectEqualDeep(identifier, actual.identifier);
}

//
// Error Tests
//

test "error.SymbolNotFound" {
    // Arrange
    var symbol_table = SymbolTable.init(test_allocator);
    defer symbol_table.deinit();

    const identifier: SymbolIdentifier = .{
        .name = "symbol",
        .scope = .local,
    };
    _ = try symbol_table.addSymbol(&identifier, 0);

    const invalid_identifier: SymbolIdentifier = .{
        .name = "invalid",
        .scope = .local,
    };

    const span: common.SourceSpan = .{};

    // Act + Assert
    try std.testing.expectError(
        error.SymbolNotFound,
        symbol_table.addSymbolInstance(&invalid_identifier, &span),
    );
}
