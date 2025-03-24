const std = @import("std");

const ast = @import("../ast/lib.zig");
const common = @import("../common.zig");
const symbol = @import("../symbol/lib.zig");
const token = @import("token.zig");

const symbol_test = @import("../symbol/test.zig");

pub fn emit(allocator: std.mem.Allocator, tree: *ast.AST, symbol_table: *symbol.SymbolTable, target: *const common.Target) ![]token.CToken {
    var walk = ast.ASTWalk.init(allocator, tree);
    defer walk.deinit();

    var emit_callback = CEmitWalkCallback.init(
        allocator,
        target,
        symbol_table,
    );
    defer emit_callback.deinit();

    try walk.start(tree.entrypoint() orelse return error.NotFound);
    while (try walk.next()) |out| {
        const stat = tree.getPtr(out.index) orelse return error.NotFound;

        try switch (out.state) {
            .enter => emit_callback.enter(stat),
            .middle => emit_callback.middle(stat, out.previous.?, out.next.?),
            .exit => emit_callback.exit(stat),
        };
    }

    return try emit_callback.tokens.toOwnedSlice();
}

pub const CEmitWalkCallback = struct {
    allocator: std.mem.Allocator,
    target: *const common.Target,
    symbol_table: *symbol.SymbolTable,
    tokens: TokenList,

    field_index: usize = 0,
    token_stack: TokenList,

    forward_declarations: bool = false,
    function_sym_idx: ?usize = null,
    function_last_param_token: ?token.CToken = null,
    block_sym_idx: ?usize = null,
    phi_token: ?token.CToken = null,
    phi_type: ?token.CToken = null,

    const Self = @This();
    const TokenList = std.ArrayList(token.CToken);

    const opaque_identifier = symbol.SymbolIndentifier{ .name = "opaque", .scope = .global };

    const link_export_identifier = symbol.SymbolIndentifier{ .name = "EXPORT", .scope = .global };
    const link_thread_identifier = symbol.SymbolIndentifier{ .name = "THREAD", .scope = .global };
    const link_identifier = symbol.SymbolIndentifier{ .name = "LINK", .scope = .global };
    const link_flags_identifier = symbol.SymbolIndentifier{ .name = "LINK_FLAGS", .scope = .global };

    const align_identifier = symbol.SymbolIndentifier{ .name = "ALIGN", .scope = .global };
    const align_default_identifier = symbol.SymbolIndentifier{ .name = "ALIGN_DEFAULT", .scope = .global };
    const allocate_identifier = symbol.SymbolIndentifier{ .name = "ALLOCATE", .scope = .global };
    const blit_identifier = symbol.SymbolIndentifier{ .name = "BLIT", .scope = .global };

    const all_nan_identifier = symbol.SymbolIndentifier{ .name = "ALL_NAN", .scope = .global };
    const any_nan_identifier = symbol.SymbolIndentifier{ .name = "ANY_NAN", .scope = .global };

    const vastart_identifier = symbol.SymbolIndentifier{ .name = "VA_START", .scope = .global };
    const vaarg_identifier = symbol.SymbolIndentifier{ .name = "VA_ARG", .scope = .global };

    const emit_identifiers = [_]symbol.SymbolIndentifier{
        opaque_identifier,
        link_export_identifier,
        link_thread_identifier,
        link_identifier,
        link_flags_identifier,
        align_identifier,
        align_default_identifier,
        allocate_identifier,
        blit_identifier,
        all_nan_identifier,
        any_nan_identifier,
        vastart_identifier,
        vaarg_identifier,
    };

    const block_variable = "__block__";
    const BlockVariableType = u8; // TODO: Possibly could collide?

    const va_variable = "__va_param__";

    pub fn init(allocator: std.mem.Allocator, target: *const common.Target, symbol_table: *symbol.SymbolTable) Self {
        return .{
            .allocator = allocator,
            .target = target,
            .symbol_table = symbol_table,
            .tokens = TokenList.init(allocator),
            .token_stack = TokenList.init(allocator),
        };
    }

    pub fn deinit(self: *Self) void {
        self.tokens.deinit();
        self.token_stack.deinit();
    }

    fn push(self: *Self, token_type: token.CTokenType, span: ?common.SourceSpan) !void {
        try self.tokens.append(.{
            .token_type = token_type,
            .span = span orelse .{},
        });
    }

    fn append(self: *Self, tok: token.CToken) !void {
        try self.tokens.append(tok);
    }

    fn pop(self: *Self) !token.CToken {
        return self.tokens.popOrNull() orelse return error.NotFound;
    }

    fn peek(self: *Self) !token.CTokenType {
        if (self.tokens.getLastOrNull()) |t| {
            return t.token_type;
        } else {
            return error.EmptyStack;
        }
    }

    fn dupe(self: *Self) !void {
        if (self.tokens.getLastOrNull()) |t| {
            try self.push(t.token_type, t.span);
        } else {
            return error.EmptyStack;
        }
    }

    fn rot2(self: *Self) !void {
        const t = try self.pop();
        const b = try self.pop();

        try self.push(t.token_type, t.span);
        try self.push(b.token_type, b.span);
    }

    fn newline(self: *Self) !void {
        try self.push(.newline, null);
    }

    fn pushField(self: *Self) !void {
        const idx = self.field_index;
        self.field_index += 1;

        _ = try self.pushSymbol(
            .global_identifier,
            null,
            "F_{}",
            .{idx},
        );
    }

    fn pushSymbol(self: *Self, token_type: token.CTokenType, function: ?usize, comptime fmt: []const u8, args: anytype) !symbol.Instance {
        const name = try std.fmt.allocPrint(self.allocator, fmt, args);
        defer self.allocator.free(name);

        var sym = symbol.SymbolIndentifier{
            .name = name,
            .scope = switch (token_type) {
                .local_identifier => .local,
                .global_identifier => .global,
                .type_identifier => .type,
                else => unreachable,
            },
            .function = function,
        };
        const sym_idx = try self.symbol_table.addSymbol(&sym);

        return self.pushSymbolInstance(token_type, sym_idx);
    }

    fn pushSymbolInstance(self: *Self, token_type: token.CTokenType, sym_idx: usize) !symbol.Instance {
        const span = self.symbol_table.nextSpan();
        var instance = symbol.Instance{ .span = span };
        _ = try self.symbol_table.addSymbolInstanceByIndex(sym_idx, &instance);

        try self.push(token_type, span);

        return instance;
    }

    fn pushSymbolIdentifier(self: *Self, token_type: token.CTokenType, identifier: *const symbol.SymbolIndentifier) !symbol.Instance {
        const sym_idx = self.symbol_table.getSymbolIndexByIdentifier(identifier) orelse return error.NotFound;
        return try self.pushSymbolInstance(token_type, sym_idx);
    }

    fn pushSymbolType(self: *Self, sym: *const symbol.Symbol) !void {
        switch (sym.memory) {
            .primitive => |p| {
                try self.pushPrimitiveType(p);
            },
            .env => {
                try self.pushPrimitiveType(.ptr);
            },
            .type => |t| {
                try self.pushSymbolTypePrefix(self.symbol_table.getSymbolPtr(t) orelse return error.NotFound);

                _ = try self.pushSymbolInstance(.type_identifier, t);

                try self.push(.pointer, null);
            },
            .@"struct",
            .@"union",
            => {
                try self.pushSymbolTypePrefix(sym);

                _ = try self.pushSymbolIdentifier(.type_identifier, &sym.identifier);
            },
            else => unreachable,
        }
    }

    fn literalValueToToken(t: symbol.LiteralValue) token.CTokenType {
        return switch (t) {
            .integer => .integer_literal,
            .single => .single_literal,
            .double => .double_literal,
            .string => .string_literal,
        };
    }

    fn literalTypeToToken(t: ast.LiteralType) token.CTokenType {
        return switch (t) {
            .integer => .integer_literal,
            .single => .single_literal,
            .double => .double_literal,
            .string => .string_literal,
        };
    }

    fn pushPrimitiveType(self: *Self, primitive: ast.PrimitiveType) !void {
        const token_type: token.CTokenType = switch (primitive) {
            .void => .void,
            .ptr => switch (self.target.arch) {
                .a8 => .u8,
                .a16 => .u16,
                .a32 => .u32,
                .a64 => .u64,
            },
            .i8 => .i8,
            .bool,
            .u8,
            => .u8,
            .i16 => .i16,
            .u16 => .u16,
            .i32 => .i32,
            .u32 => .u32,
            .i64 => .i64,
            .u64 => .u64,
            .f32 => .f32,
            .f64 => .f64,
        };
        try self.push(token_type, null);
    }

    fn pushLiteral(self: *Self, val: *const symbol.LiteralValue) !void {
        const lit = try self.symbol_table.addLiteral(val);

        const span = self.symbol_table.nextSpan();
        const instance = symbol.Instance{ .span = span };
        _ = try self.symbol_table.addLiteralInstance(lit, &instance);

        try self.push(Self.literalValueToToken(val.*), span);
    }

    fn pushArraySize(self: *Self, count: usize) !void {
        try self.push(.open_bracket, null);

        const val = symbol.LiteralValue{ .integer = @intCast(count) };
        try self.pushLiteral(&val);

        try self.push(.close_bracket, null);
    }

    fn pushCast(self: *Self, primitive: ast.PrimitiveType, pointer: bool) !void {
        try self.push(.open_parenthesis, null);

        try self.pushPrimitiveType(primitive);

        if (pointer) {
            try self.push(.pointer, null);
        }

        try self.push(.close_parenthesis, null);
    }

    fn pushTypePrefix(self: *Self, instance: *const symbol.Instance) !void {
        const sym = self.symbol_table.getSymbolByInstance(instance) orelse return error.NotFound;

        return self.pushSymbolTypePrefix(sym);
    }

    fn pushSymbolTypePrefix(self: *Self, sym: *const symbol.Symbol) !void {
        switch (sym.memory) {
            .@"opaque",
            .@"struct",
            => try self.push(.@"struct", null),
            .@"union" => try self.push(.@"union", null),
            else => {},
        }
    }

    fn pushDataType(self: *Self, instance: *const symbol.Instance) !void {
        const sym = self.symbol_table.getSymbolByInstance(instance) orelse return error.NotFound;

        switch (sym.memory) {
            .data => |data| {
                try self.push(.@"struct", null);
                _ = try self.pushSymbol(
                    .type_identifier,
                    null,
                    "{s}_D",
                    .{sym.identifier.name},
                );

                try self.push(.open_curly_brace, null);

                for (data.entries) |entry| {
                    switch (entry) {
                        .init => |d| {
                            try self.pushPrimitiveType(d.type);
                            try self.pushField();

                            switch (d.value) {
                                .string => |v| {
                                    try self.pushArraySize(v.len);
                                },
                                else => {},
                            }

                            try self.push(.semi_colon, null);
                        },
                        .zero => |count| {
                            try self.pushPrimitiveType(.u8);
                            try self.pushField();
                            try self.pushArraySize(count);
                            try self.push(.semi_colon, null);
                        },
                    }
                }

                try self.push(.close_curly_brace, null);

                if (data.alignment) |a| {
                    _ = try self.pushSymbolIdentifier(.global_identifier, &align_identifier);

                    try self.push(.open_parenthesis, null);

                    var lit = symbol.LiteralValue{ .integer = @intCast(a) };
                    try self.pushLiteral(&lit);

                    try self.push(.close_parenthesis, null);
                } else {
                    _ = try self.pushSymbolIdentifier(.global_identifier, &align_default_identifier);
                }

                try self.push(.semi_colon, null);
                try self.push(.newline, null);
            },
            else => {},
        }
    }

    fn storeToken(self: *Self) !void {
        try self.token_stack.append(try self.pop());
    }

    fn loadToken(self: *Self) !void {
        if (self.token_stack.popOrNull()) |t| {
            try self.push(t.token_type, t.span);
        } else {
            return error.NoToken;
        }
    }

    fn clearToken(self: *Self) !void {
        if (self.token_stack.items.len > 0) {
            self.token_stack.clearRetainingCapacity();
        } else {
            return error.NoToken;
        }
    }

    fn dropUntil(self: *Self, token_type: token.CTokenType) !void {
        while ((try self.peek()) != token_type) {
            _ = try self.pop();
        }
    }

    fn binaryOpToToken(op: ast.BinaryOperationType) token.CTokenType {
        return switch (op) {
            .addition => .plus,
            .divide => .divide,
            .divide_unsigned => .divide,
            .multiply => .multiply,
            .remainder => .remainder,
            .remainder_unsigned => .remainder,
            .subtract => .minus,
            .arthimetic_shift_right => .shift_right,
            .@"and" => .bitwise_and,
            .@"or" => .bitwise_or,
            .logical_shift_right => .shift_right,
            .shift_left => .shift_left,
            .xor => .bitwise_xor,
        };
    }

    fn comparisonOpToToken(op: ast.ComparisonOperationType) token.CTokenType {
        return switch (op) {
            .equal => .equal,
            .greater_than => .greater_than,
            .greater_than_equal => .greater_than_equal,
            .less_than => .less_than,
            .less_than_equal => .less_than_equal,
            .not_equal => .not_equal,
            else => .comma,
        };
    }

    fn getBlockLabelValue(sym_idx: ?usize) isize {
        if (sym_idx) |v| {
            const m: usize = std.math.maxInt(BlockVariableType);
            const o: u8 = @intCast(v % m);

            return @intCast(o);
        } else {
            return 0;
        }
    }

    fn pushBlockAssign(self: *Self) !void {
        try self.push(.tab, null);

        const identitifer = symbol.SymbolIndentifier{
            .scope = .global,
            .name = block_variable,
            .function = self.function_sym_idx.?,
        };
        _ = try self.pushSymbolIdentifier(.global_identifier, &identitifer);

        try self.push(.assign, null);

        const lit = symbol.LiteralValue{ .integer = Self.getBlockLabelValue(self.block_sym_idx) };
        try self.pushLiteral(&lit);

        try self.push(.semi_colon, null);
        try self.push(.newline, null);
    }

    fn pushForwardDeclarations(self: *Self) !void {
        for (self.symbol_table.symbols.items) |*sym| {
            switch (sym.memory) {
                .function => try self.pushFunctionForwardDeclaration(sym),
                .empty => {
                    if (sym.identifier.scope == .global) {
                        // Forward declare external globals

                        try self.push(.@"extern", null);

                        try self.pushPrimitiveType(.ptr);

                        _ = try self.pushSymbolIdentifier(.global_identifier, &sym.identifier);

                        try self.push(.semi_colon, null);
                        try self.push(.newline, null);
                    }
                },
                else => {},
            }
        }

        try self.push(.newline, null);
    }

    fn pushFunctionForwardDeclaration(self: *Self, sym: *const symbol.Symbol) !void {
        switch (sym.memory) {
            .function => |*f| {
                if (!f.external and !f.linkage.@"export") {
                    try self.push(.static, null);
                }

                switch (f.@"return") {
                    .primitive => |v| {
                        const t: ast.PrimitiveType = switch (v) {
                            .void => .i8, // Force to always have a return type
                            else => v,
                        };

                        try self.pushPrimitiveType(t);
                    },
                    .type => |v| {
                        const instance = try self.pushSymbolInstance(.type_identifier, v);

                        try self.storeToken();
                        try self.pushTypePrefix(&instance);
                        try self.loadToken();
                    },
                }

                _ = try self.pushSymbolIdentifier(.global_identifier, &sym.identifier);

                try self.push(.open_parenthesis, null);

                for (f.parameters) |p| {
                    switch (p) {
                        .primitive => |v| try self.pushPrimitiveType(v),
                        .env => try self.pushPrimitiveType(.ptr),
                        .type => |v| {
                            const instance = try self.pushSymbolInstance(.type_identifier, v);

                            try self.storeToken();
                            try self.pushTypePrefix(&instance);
                            try self.loadToken();
                        },
                    }

                    try self.push(.comma, null);
                }

                if ((try self.peek()) == .comma) {
                    _ = try self.pop();
                }

                if (f.vararg) {
                    if (f.parameters.len > 0) {
                        try self.push(.comma, null);
                    }

                    try self.push(.variable_arguments, null);
                }

                try self.push(.close_parenthesis, null);
                try self.push(.semi_colon, null);
                try self.push(.newline, null);
            },
            else => {},
        }
    }

    fn pushLocalVariables(self: *Self) !void {
        const function_sym = self.symbol_table.getSymbolPtr(self.function_sym_idx.?) orelse return error.NotFound;
        const is_vararg: bool = switch (function_sym.memory) {
            .function => |f| f.vararg,
            else => false,
        };

        const param_count: usize = switch (function_sym.memory) {
            .function => |f| f.parameters.len,
            else => 0,
        };

        var idx: usize = 0;
        var last_param: ?*const symbol.Symbol = null;

        // TODO: This algo should be possible with less duplicate checks
        for (self.symbol_table.symbols.items) |*sym| {
            if (sym.identifier.function != self.function_sym_idx) {
                continue;
            }

            if (sym.identifier.scope != .local) {
                continue;
            }

            idx += 1;

            if (idx <= param_count) {
                last_param = sym;
                continue;
            }

            try self.push(.tab, null);

            try self.pushSymbolType(sym);
            if ((try self.peek()) == .pointer) {
                _ = try self.pop();
            }

            _ = try self.pushSymbolIdentifier(.local_identifier, &sym.identifier);

            try self.push(.semi_colon, null);
            try self.push(.newline, null);
        }

        if (is_vararg) {
            if (last_param) |p| {
                _ = try self.pushSymbolIdentifier(.local_identifier, &p.identifier);

                self.function_last_param_token = try self.pop();
            }
        }

        try self.push(.tab, null);
        try self.push(.u8, null); // TODO: Make sure that it matches BlockVariableType

        _ = try self.pushSymbol(
            .global_identifier,
            self.function_sym_idx,
            block_variable,
            .{},
        );

        try self.push(.semi_colon, null);
        try self.push(.newline, null);
    }

    fn pushOpaqueOpen(self: *Self) !void {
        try self.push(.open_curly_brace, null);
        try self.push(.u8, null);

        _ = try self.pushSymbolIdentifier(.global_identifier, &opaque_identifier);

        try self.push(.open_bracket, null);
    }

    fn wrapDereference(self: *Self) !void {
        try self.pushCast(.u8, true);

        try self.push(.dereference, null);
    }

    fn unwrapDereference(self: *Self) !bool {
        try self.storeToken();

        var deref = false;

        if ((try self.peek()) == .dereference) {
            for (0..5) |_| {
                _ = try self.pop();
            }

            deref = true;
        }

        try self.loadToken();

        return deref;
    }

    fn pushCallOpen(self: *Self) !void {
        if ((try self.peek()) == .void) {
            _ = try self.pop();
        } else {
            try self.push(.close_parenthesis, null);
        }

        try self.loadToken();

        try self.push(.open_parenthesis, null);
    }

    fn pushFunctionOpen(self: *Self) !void {
        if ((try self.peek()) == .void) {
            // Force to always have a return type

            _ = try self.pop();

            try self.pushPrimitiveType(.i8);
        }

        try self.loadToken();

        try self.push(.open_parenthesis, null);
    }

    pub fn enter(self: *Self, statement: *const ast.Statement) !void {
        switch (statement.data) {
            .module => {
                self.field_index = 0;
                self.forward_declarations = false;

                for (Self.emit_identifiers) |identifier| {
                    const sym_idx = try self.symbol_table.addSymbol(&identifier);
                    var sym = self.symbol_table.getSymbolPtrMut(sym_idx).?;

                    sym.memory = .{ .primitive = .void };
                }

                try self.push(.module_start, null);
            },
            .identifier => |identifier| {
                const instance = symbol.Instance{ .span = statement.span };

                const token_type: token.CTokenType = switch (identifier.scope) {
                    .type => l: {
                        try self.pushTypePrefix(&instance);

                        break :l .type_identifier;
                    },
                    .global => l: {
                        if (self.function_sym_idx == null) {
                            self.function_sym_idx = self.symbol_table.getSymbolIndexByInstance(&instance);
                        }

                        try self.wrapDereference();

                        break :l .global_identifier;
                    },
                    .label => l: {
                        if (self.block_sym_idx == null) {
                            self.block_sym_idx = self.symbol_table.getSymbolIndexByInstance(&instance);
                        }

                        break :l .label_identifier;
                    },
                    .local => l: {
                        const sym = self.symbol_table.getSymbolByInstance(&instance) orelse return error.NotFound;

                        if (sym.memory == .type) {
                            try self.wrapDereference();
                        }

                        break :l .local_identifier;
                    },
                };

                try self.push(
                    token_type,
                    statement.span,
                );
            },
            .literal => |literal| {
                try self.push(
                    Self.literalTypeToToken(literal.type),
                    statement.span,
                );
            },
            .primitive_type => |primitive| {
                try self.pushPrimitiveType(primitive);
            },
            .opaque_type => {
                try self.pushOpaqueOpen();
            },
            .union_type => {
                try self.push(.open_curly_brace, null);

                try self.push(.@"struct", null);
            },
            .struct_type => {
                try self.push(.open_curly_brace, null);
            },
            .zero_type => {
                try self.push(.colon, null);
            },
            .env_type => {
                try self.pushPrimitiveType(.ptr);
            },
            .variadic_parameter => {
                try self.push(.variable_arguments, null);
            },
            .linkage => |l| {
                if (l.section != null) {
                    if (l.flags != null) {
                        _ = try self.pushSymbolIdentifier(.global_identifier, &link_flags_identifier);
                    } else {
                        _ = try self.pushSymbolIdentifier(.global_identifier, &link_identifier);
                    }

                    try self.push(.open_parenthesis, null);
                }
            },
            .offset => {
                try self.pushCast(.u8, true);
            },
            .function => {
                self.function_sym_idx = null;

                if (!self.forward_declarations) {
                    self.forward_declarations = true;

                    try self.pushForwardDeclarations();
                }
            },
            .block => {
                self.block_sym_idx = null;
            },
            .line => {
                try self.push(.tab, null);
            },
            .@"return" => {
                try self.push(.tab, null);
                try self.push(.@"return", null);
            },
            .jump => {
                try self.pushBlockAssign();

                try self.push(.tab, null);
                try self.push(.goto, null);
            },
            .branch => {
                try self.pushBlockAssign();

                try self.push(.tab, null);
                try self.push(.@"if", null);
                try self.push(.open_parenthesis, null);
            },
            .halt => {
                try self.push(.tab, null);
                try self.push(.@"while", null);
                try self.push(.open_parenthesis, null);

                const val = symbol.LiteralValue{ .integer = 1 };
                try self.pushLiteral(&val);

                try self.push(.close_parenthesis, null);
            },
            .blit => {
                _ = try self.pushSymbolIdentifier(.global_identifier, &blit_identifier);
                try self.push(.open_parenthesis, null);
            },
            .allocate,
            .binary_operation,
            .call_parameter,
            .cast,
            .convert,
            .copy,
            .negate,
            .load,
            .vaarg,
            => {
                try self.push(.open_parenthesis, null);
            },
            .store,
            => {
                try self.push(.pointer, null);
                try self.push(.open_parenthesis, null);
            },
            .comparison => {
                try self.push(.open_parenthesis, null);
            },
            .vastart => {
                _ = try self.pushSymbolIdentifier(.global_identifier, &vastart_identifier);
                try self.push(.open_parenthesis, null);
            },
            .call => {
                if ((try self.peek()) == .assign) {
                    try self.push(.open_parenthesis, null);
                }
            },
            .phi_parameter => {
                try self.push(.tab, null);
                try self.push(.tab, null);
                try self.push(.case, null);
            },
            else => {},
        }
    }

    pub fn middle(self: *Self, statement: *const ast.Statement, previous: usize, next: usize) !void {
        _ = next;

        switch (statement.data) {
            .union_type => |u| {
                if (u.alignment == previous) {
                    try self.storeToken();
                } else {
                    try self.pushField();
                    try self.push(.semi_colon, null);
                    try self.push(.@"struct", null);
                }
            },
            .struct_type => |s| {
                if (s.alignment == previous) {
                    try self.storeToken();
                } else {
                    if ((try self.peek()) != .close_bracket) {
                        try self.pushField();
                    }

                    try self.push(.semi_colon, null);
                }
            },
            .opaque_type => |o| {
                if (o.alignment == previous) {
                    try self.storeToken();
                }
            },
            .array_type => {
                try self.pushField();
                try self.push(.open_bracket, null);
            },
            .linkage => |l| {
                if (l.section == previous) {
                    try self.push(.comma, null);
                }
            },
            .data_definition => |d| {
                if (d.identifier == previous) {
                    _ = try self.unwrapDereference();

                    const identifier = try self.pop();

                    const instance = symbol.Instance{ .span = identifier.span };
                    try self.pushDataType(&instance);

                    try self.push(identifier.token_type, identifier.span);

                    try self.storeToken();
                } else if (d.alignment == previous) {
                    _ = try self.pop();
                } else if (d.linkage == previous) {
                    // Add type to data

                    // Identifier should be stored on stack at this point

                    const stored_instance = symbol.Instance{ .span = self.token_stack.getLast().span };
                    const stored_sym = self.symbol_table.getSymbolByInstance(&stored_instance) orelse return error.NotFound;

                    const type_name = try std.fmt.allocPrint(self.allocator, "{s}_D", .{stored_sym.identifier.name});
                    defer self.allocator.free(type_name);

                    const type_instance = symbol.SymbolIndentifier{ .name = type_name, .scope = .type };
                    const type_sym_idx = self.symbol_table.getSymbolIndexByIdentifier(&type_instance) orelse return error.NotFound;

                    try self.push(.@"struct", null);
                    _ = try self.pushSymbolInstance(.type_identifier, type_sym_idx);

                    try self.loadToken();

                    // Open assign
                    try self.push(.assign, null);
                    try self.push(.open_curly_brace, null);
                } else {
                    try self.push(.comma, null);
                }
            },
            .typed_data => |d| {
                if (d.type == previous) {
                    if ((try self.peek()) != .colon) {
                        _ = try self.pop(); // TODO: Remove struct/union?
                    }
                }
            },
            .offset => {
                try self.push(.plus, null);
            },
            .function_signature => |fs| {
                if (fs.name == previous) {
                    _ = try self.unwrapDereference();

                    try self.storeToken();
                } else if (fs.return_type == previous) {
                    try self.pushFunctionOpen();
                } else if (fs.linkage != previous and (try self.peek()) != .open_parenthesis) {
                    try self.push(.comma, null);
                }
            },
            .function_parameter => |fp| {
                if (fp.value == previous) {
                    _ = try self.unwrapDereference();

                    try self.storeToken();
                }
            },
            .block => |b| {
                if (b.label == previous) {
                    try self.push(.colon, null);
                    try self.push(.newline, null);
                }
            },
            .branch => |b| {
                if (b.condition == previous) {
                    try self.push(.close_parenthesis, null);
                    try self.push(.goto, null);
                } else if (b.true == previous) {
                    try self.push(.semi_colon, null);
                    try self.push(.@"else", null);
                    try self.push(.goto, null);
                }
            },
            .assignment => {
                _ = try self.unwrapDereference();

                try self.push(.assign, null);
            },
            .cast => |c| {
                if (c.data_type == previous) {
                    try self.push(.close_parenthesis, null);
                }
            },
            .copy => |c| {
                if (c.data_type == previous) {
                    try self.push(.close_parenthesis, null);
                }
            },
            .convert => |c| {
                if (c.data_type == previous) {
                    try self.push(.close_parenthesis, null);
                    try self.push(.open_parenthesis, null);
                } else if (c.to_type == previous) {
                    try self.push(.close_parenthesis, null);
                    try self.push(.open_parenthesis, null);
                } else if (c.from_type == previous) {
                    try self.push(.close_parenthesis, null);
                }
            },
            .negate => |n| {
                if (n.data_type == previous) {
                    try self.push(.close_parenthesis, null);
                    try self.push(.open_parenthesis, null);
                    try self.push(.bitwise_not, null);
                }
            },
            .binary_operation => |op| {
                if (op.data_type == previous) {
                    try self.push(.close_parenthesis, null);
                    try self.push(.open_parenthesis, null);
                } else if (op.left == previous) {
                    try self.push(Self.binaryOpToToken(op.operation_type), null);
                }
            },
            .allocate => |a| {
                if (a.data_type == previous) {
                    try self.push(.close_parenthesis, null);
                    try self.push(.open_parenthesis, null);

                    _ = try self.pushSymbolIdentifier(.global_identifier, &allocate_identifier);
                    try self.push(.open_parenthesis, null);
                } else if (a.alignment == previous) {
                    try self.push(.comma, null);
                }
            },
            .blit => {
                try self.push(.comma, null);
            },
            .load => |l| {
                if (l.data_type == previous) {
                    try self.push(.close_parenthesis, null);

                    try self.push(.pointer, null);

                    try self.push(.open_parenthesis, null);
                } else if (l.memory_type == previous) {
                    if ((try self.peek()) == .void) {
                        // If void, use return type

                        // Skip to and remove parenthesis
                        _ = try self.dropUntil(.close_parenthesis);
                        _ = try self.pop();

                        if ((try self.peek()) == .pointer) {
                            _ = try self.pop();
                        }

                        const t = try self.pop(); // Pop type

                        // Pop struct / union
                        const s: ?token.CToken = switch (t.token_type == .type_identifier) {
                            true => try self.pop(),
                            false => null,
                        };

                        // Add pointer in front
                        try self.push(.pointer, null);
                        try self.rot2();

                        if (s) |v| {
                            try self.push(v.token_type, v.span);
                        }

                        try self.push(t.token_type, t.span);
                    }

                    try self.push(.pointer, null);
                    try self.push(.close_parenthesis, null);
                }
            },
            .store => |s| {
                if (s.memory_type == previous) {
                    try self.push(.pointer, null);
                    try self.push(.close_parenthesis, null);
                } else if (s.value == previous) {
                    _ = try self.unwrapDereference();

                    try self.storeToken();
                }
            },
            .comparison => |c| {
                if (c.data_type == previous) {
                    try self.push(.close_parenthesis, null);
                    try self.push(.open_parenthesis, null);

                    switch (c.operation_type) {
                        .any_nan,
                        .all_nan,
                        => {
                            const identifier: *const symbol.SymbolIndentifier = switch (c.operation_type) {
                                .any_nan => &any_nan_identifier,
                                .all_nan => &all_nan_identifier,
                                else => unreachable,
                            };

                            _ = try self.pushSymbolIdentifier(.global_identifier, identifier);
                            try self.push(.open_parenthesis, null);
                        },
                        else => {},
                    }

                    try self.push(.open_parenthesis, null);
                } else if (c.comparison_type == previous) {
                    try self.dupe();
                    try self.push(.close_parenthesis, null);
                    try self.rot2();
                } else if (c.left == previous) {
                    try self.rot2();

                    const typ = try self.pop();

                    try self.push(Self.comparisonOpToToken(c.operation_type), null);

                    try self.push(.open_parenthesis, null);
                    try self.push(typ.token_type, typ.span);
                    try self.push(.close_parenthesis, null);
                }
            },
            .vaarg => {
                try self.dupe();

                try self.storeToken(); // TODO: struct/union type?

                try self.push(.close_parenthesis, null);

                _ = try self.pushSymbolIdentifier(.global_identifier, &vaarg_identifier);
                try self.push(.open_parenthesis, null);

                try self.loadToken(); // TODO: struct/union type?

                try self.push(.comma, null);
            },
            .call => |c| {
                if (c.target == previous) {
                    _ = try self.unwrapDereference();

                    try self.storeToken();
                } else if (c.return_type == previous) {
                    try self.pushCallOpen();
                } else {
                    if ((try self.peek()) == .variable_arguments) {
                        _ = try self.pop();
                    } else {
                        _ = try self.push(.comma, null);
                    }
                }
            },
            .call_parameter => |p| {
                if (p.value == previous) {
                    _ = try self.unwrapDereference();

                    try self.storeToken();
                }
            },
            .phi => |p| {
                if (p.data_type == previous) {
                    self.phi_type = try self.pop();
                    _ = try self.pop();
                    self.phi_token = try self.pop();

                    try self.push(.@"switch", null);

                    try self.push(.open_parenthesis, null);

                    const identitifer = symbol.SymbolIndentifier{
                        .scope = .global,
                        .name = block_variable,
                        .function = self.function_sym_idx.?,
                    };
                    _ = try self.pushSymbolIdentifier(.global_identifier, &identitifer);

                    try self.push(.close_parenthesis, null);

                    try self.push(.open_curly_brace, null);
                    try self.push(.newline, null);
                }
            },
            .phi_parameter => |p| {
                if (p.identifier == previous) {
                    const tok = try self.pop();

                    const instance = symbol.Instance{ .span = tok.span };
                    const sym_idx = self.symbol_table.getSymbolIndexByInstance(&instance) orelse return error.NotFound;

                    const lit = symbol.LiteralValue{ .integer = Self.getBlockLabelValue(sym_idx) };
                    try self.pushLiteral(&lit);

                    try self.push(.colon, null);

                    try self.push(self.phi_token.?.token_type, self.phi_token.?.span);
                    try self.push(.assign, null);
                }
            },
            else => {},
        }
    }

    pub fn exit(self: *Self, statement: *const ast.Statement) !void {
        switch (statement.data) {
            .module => try self.push(.module_end, null),
            .opaque_type => |o| {
                try self.push(.close_bracket, null);

                try self.push(.semi_colon, null);
                try self.push(.close_curly_brace, null);

                if (o.alignment == null) {
                    _ = try self.pushSymbolIdentifier(.global_identifier, &align_identifier);

                    try self.push(.open_parenthesis, null);

                    try self.loadToken();

                    try self.push(.close_parenthesis, null);
                } else {
                    _ = try self.pushSymbolIdentifier(.global_identifier, &align_default_identifier);
                }
            },
            .union_type => |u| {
                try self.pushField();
                try self.push(.semi_colon, null);

                try self.push(.close_curly_brace, null);

                if (u.alignment != null) {
                    _ = try self.pushSymbolIdentifier(.global_identifier, &align_identifier);

                    try self.push(.open_parenthesis, null);

                    try self.loadToken();

                    try self.push(.close_parenthesis, null);
                } else {
                    _ = try self.pushSymbolIdentifier(.global_identifier, &align_default_identifier);
                }
            },
            .struct_type => |s| {
                if ((try self.peek()) != .close_bracket) {
                    try self.pushField();
                }

                try self.push(.semi_colon, null);

                try self.push(.close_curly_brace, null);

                if (s.alignment != null) {
                    _ = try self.pushSymbolIdentifier(.global_identifier, &align_identifier);

                    try self.push(.open_parenthesis, null);

                    try self.loadToken();

                    try self.push(.close_parenthesis, null);
                } else {
                    _ = try self.pushSymbolIdentifier(.global_identifier, &align_default_identifier);
                }
            },
            .array_type => {
                try self.push(.close_bracket, null);
            },
            .data_definition => {
                try self.push(.close_curly_brace, null);
                try self.push(.semi_colon, null);
                try self.push(.newline, null);
                try self.push(.newline, null);
            },
            .typed_data => {
                const val = try self.pop();

                if ((try self.peek()) == .colon) {
                    // Zero data

                    _ = try self.pop();

                    try self.push(.open_curly_brace, null);

                    var lit = symbol.LiteralValue{ .integer = 0 };
                    try self.pushLiteral(&lit);

                    try self.push(.close_curly_brace, null);
                } else {
                    try self.push(val.token_type, val.span);
                }
            },
            .type_definition,
            => {
                try self.push(.semi_colon, null);
                try self.push(.newline, null);
                try self.push(.newline, null);
            },
            .linkage => |l| {
                if (l.section != null) {
                    try self.push(.close_parenthesis, null);
                }

                if (l.thread) {
                    _ = try self.pushSymbolIdentifier(.global_identifier, &link_thread_identifier);
                }

                if (l.@"export") {
                    _ = try self.pushSymbolIdentifier(.global_identifier, &link_export_identifier);
                } else {
                    _ = try self.push(.static, null);
                }
            },
            .function => {
                try self.push(.close_curly_brace, null);
                try self.push(.newline, null);
                try self.push(.newline, null);
            },
            .function_signature => |fs| {
                if (fs.parameters == null) {
                    try self.pushFunctionOpen();
                }

                try self.push(.close_parenthesis, null);
                try self.push(.open_curly_brace, null);
                try self.push(.newline, null);

                try self.pushLocalVariables();
            },
            .function_parameter => {
                if ((try self.peek()) == .colon) {
                    _ = try self.pop();
                    try self.clearToken();
                } else {
                    try self.loadToken();
                }
            },
            .line,
            .jump,
            .branch,
            .halt,
            => {
                try self.push(.semi_colon, null);
                try self.push(.newline, null);
            },
            .@"return" => |ret| {
                if (ret.value == null) {
                    const lit = symbol.LiteralValue{ .integer = 0 };
                    _ = try self.pushLiteral(&lit);
                } else {
                    const fn_sym = self.symbol_table.getSymbolPtr(self.function_sym_idx.?) orelse return error.NotFound;

                    switch (fn_sym.memory) {
                        .function => |f| {
                            switch (f.@"return") {
                                .type => |t| {
                                    _ = try self.unwrapDereference();

                                    // Handle struct/union return
                                    try self.storeToken();

                                    try self.push(.pointer, null);

                                    try self.push(.open_parenthesis, null);

                                    const ret_type = self.symbol_table.getSymbolPtr(t) orelse return error.NotFound;
                                    try self.pushSymbolType(ret_type);

                                    try self.push(.pointer, null);
                                    try self.push(.close_parenthesis, null);

                                    try self.loadToken();

                                    // TODO: Globals might need to always be dereferenced?
                                    if ((try self.peek()) == .global_identifier) {
                                        try self.push(.dereference, null);
                                        try self.rot2();
                                    }
                                },
                                else => {},
                            }
                        },
                        else => unreachable,
                    }
                }

                try self.push(.semi_colon, null);
                try self.push(.newline, null);
            },
            .allocate => {
                try self.push(.close_parenthesis, null);
                try self.push(.close_parenthesis, null);
            },
            .binary_operation,
            .blit,
            .negate,
            .vaarg,
            => {
                try self.push(.close_parenthesis, null);
            },
            .vastart => {
                try self.push(.comma, null);

                try self.push(self.function_last_param_token.?.token_type, self.function_last_param_token.?.span);

                try self.push(.close_parenthesis, null);
            },
            .store => {
                try self.push(.assign, null);

                if (self.token_stack.getLastOrNull()) |v| {
                    if (v.token_type == .global_identifier) {
                        _ = try self.wrapDereference();
                    }
                }

                try self.loadToken();
            },
            .comparison => |c| {
                switch (c.operation_type) {
                    .any_nan,
                    .all_nan,
                    => {
                        try self.push(.close_parenthesis, null);
                    },
                    else => {},
                }

                try self.push(.close_parenthesis, null);
            },
            .call => |c| {
                if (c.parameters == null) {
                    try self.pushCallOpen();
                }

                try self.push(.close_parenthesis, null);
            },
            .call_parameter => {
                const val_sym: ?*const symbol.Symbol = l: {
                    const tok = self.token_stack.getLastOrNull() orelse return error.NotFound;

                    if (tok.token_type != .local_identifier) {
                        break :l null;
                    }

                    const inst = symbol.Instance{ .span = tok.span };

                    break :l self.symbol_table.getSymbolByInstance(&inst) orelse return error.NotFound;
                };

                switch (try self.peek()) {
                    .type_identifier => l: {
                        // Handle struct param type

                        const t = try self.pop(); // Pop type

                        const p = try self.pop(); // Pop struct/union
                        _ = try self.pop(); // Pop open parenthesis

                        // Skip if value is type
                        if (val_sym) |sym| {
                            if (sym.memory == .type) break :l;
                        }

                        try self.push(.pointer, null);

                        try self.push(.open_parenthesis, null);

                        try self.push(p.token_type, p.span);
                        try self.push(t.token_type, t.span);

                        try self.push(.pointer, null);

                        try self.push(.close_parenthesis, null);
                    },
                    else => {
                        // Handle base param type
                        try self.push(.close_parenthesis, null);

                        // Dereference if struct
                        if (val_sym) |sym| {
                            if (sym.memory == .type) try self.push(.dereference, null);
                        }
                    },
                }

                try self.loadToken();

                if ((try self.peek()) == .global_identifier) {
                    try self.storeToken();

                    try self.wrapDereference();

                    try self.loadToken();
                }
            },
            .phi => {
                try self.push(.tab, null);
                try self.push(.close_curly_brace, null);
            },
            .phi_parameter => {
                try self.push(.semi_colon, null);

                try self.push(.@"break", null);
                try self.push(.semi_colon, null);

                try self.push(.newline, null);
            },
            else => {},
        }
    }
};

const EmitWriterState = enum {
    default,
    space,
    done,
};

pub fn CEmitWriter(comptime Reader: type, comptime Writer: type) type {
    return struct {
        reader: *Reader,
        writer: *Writer,
        config: *const common.EmitWriterConfig,
        symbol_table: *const symbol.SymbolTable,

        state: EmitWriterState = .default,

        const Self = @This();

        const include_prefix = @embedFile("./include.h");

        fn reader_next(self: *Self) *const token.CToken {
            return self.reader.next();
        }

        fn writer_writeByte(self: *Self, byte: u8) !void {
            try self.writer.writeByte(byte);
        }

        fn writer_write(self: *Self, bytes: []const u8) !void {
            _ = try self.writer.write(bytes);
        }

        fn writer_print(self: *Self, comptime format: []const u8, args: anytype) !void {
            _ = try self.writer.print(format, args);
        }

        fn write(self: *Self, color: std.io.tty.Color, buffer: []const u8) !void {
            try self.config.tty.setColor(self.writer, color);
            try self.writer_write(buffer);
            try self.config.tty.setColor(self.writer, .reset);
        }

        fn print(self: *Self, color: std.io.tty.Color, comptime format: []const u8, args: anytype) !void {
            try self.config.tty.setColor(self.writer, color);
            try self.writer_print(format, args);
            try self.config.tty.setColor(self.writer, .reset);
        }

        pub fn init(reader: *Reader, writer: *Writer, config: *const common.EmitWriterConfig, symbol_table: *const symbol.SymbolTable) Self {
            return .{
                .reader = reader,
                .writer = writer,
                .config = config,
                .symbol_table = symbol_table,
            };
        }

        pub fn sanitizeName(allocator: std.mem.Allocator, name: []const u8) ![]const u8 {
            const out = try allocator.alloc(u8, name.len);

            _ = std.mem.replace(u8, name, ".", "_", out);

            return out;
        }

        pub fn next(self: *Self) !bool {
            if (self.state == .done) return false;

            const tok = self.reader_next();

            return try self.parse(tok);
        }

        fn parse(self: *Self, tok: *const token.CToken) !bool {
            switch (tok.token_type) {
                .module_start => {
                    try self.print(common.Color.identifier, Self.include_prefix, .{});

                    return true;
                },
                .module_end => {
                    self.state = .done;

                    return false;
                },
                else => {},
            }

            switch (self.state) {
                .space => switch (tok.token_type) {
                    .newline,
                    .tab,
                    .semi_colon,
                    .open_bracket,
                    .close_bracket,
                    .close_parenthesis,
                    .comma,
                    .colon,
                    => {},
                    else => try self.writer_writeByte(' '),
                },
                else => {},
            }

            const color = token.tokenColor(tok.token_type);

            const word: []const u8 = token.tokenString(tok.token_type);
            if (word.len > 0) {
                try self.write(color, word);
            } else {
                const instance: symbol.Instance = .{ .span = tok.span };
                switch (tok.token_type) {
                    .local_identifier,
                    .global_identifier,
                    .type_identifier,
                    .label_identifier,
                    => {
                        if (self.symbol_table.getSymbolByInstance(&instance)) |sym| {
                            const allocator = self.symbol_table.allocator;

                            const name = try Self.sanitizeName(allocator, sym.identifier.name);
                            defer allocator.free(name);

                            try switch (sym.identifier.scope) {
                                .local => self.print(color, "R_{s}", .{name}),
                                .global => self.print(color, "{s}", .{name}),
                                .type => self.print(color, "T_{s}", .{name}),
                                .label => self.print(color, "L_{s}", .{name}),
                            };
                        } else {
                            return error.NotFound;
                        }
                    },
                    .string_literal,
                    .double_literal,
                    .single_literal,
                    .integer_literal,
                    => {
                        const literal = self.symbol_table.getLiteralByInstance(&instance) orelse return error.NotFound;

                        try switch (literal.value) {
                            .integer => |v| self.print(color, "{}", .{v}),
                            .single => |v| self.print(color, "{d:.}f", .{v}),
                            .double => |v| self.print(color, "{d:.}", .{v}),
                            .string => |v| {
                                const allocator = self.symbol_table.allocator;

                                const s = try common.escapeString(allocator, v);
                                defer allocator.free(s);

                                try self.print(color, "{s}", .{s});
                            },
                        };
                    },
                    else => unreachable,
                }
            }

            self.state = switch (tok.token_type) {
                .newline,
                .tab,
                .open_parenthesis,
                .open_bracket,
                .dereference,
                .bitwise_not,
                => .default,
                else => .space,
            };

            return true;
        }
    };
}

test "Emit" {
    // Arrange
    const allocator = std.testing.allocator;

    const file = @embedFile("../test/qbe.ssa");

    const target: common.Target = .{
        .arch = .a64,
    };

    var tree = try symbol_test.testAST(allocator, file);
    defer tree.deinit();

    var symbol_table = symbol.SymbolTable.init(allocator);
    defer symbol_table.deinit();

    var output_array = std.ArrayList(u8).init(allocator);
    defer output_array.deinit();

    var output_writer = output_array.writer();

    // Act
    try symbol_test.testValidate(allocator, file, &tree, &symbol_table, &target);

    const tokens = try emit(allocator, &tree, &symbol_table, &target);
    defer allocator.free(tokens);
    var token_reader = common.CollectionIterator(token.CToken).init(tokens);

    const emit_config: common.EmitWriterConfig = .{
        .tty = .no_color,
    };

    var emit_writer = CEmitWriter(
        @TypeOf(token_reader),
        @TypeOf(output_writer),
    ).init(
        &token_reader,
        &output_writer,
        &emit_config,
        &symbol_table,
    );

    while (try emit_writer.next()) {}

    const actual = try output_array.toOwnedSlice();
    defer allocator.free(actual);
}
