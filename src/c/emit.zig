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
    stored_token: ?token.CToken = null,

    const Self = @This();
    const TokenList = std.ArrayList(token.CToken);

    const opaque_identifier = symbol.SymbolIndentifier{ .name = "opaque", .scope = .local };

    const link_export_identifier = symbol.SymbolIndentifier{ .name = "LINK_EXPORT", .scope = .local };
    const link_thread_identifier = symbol.SymbolIndentifier{ .name = "LINK_THREAD", .scope = .local };
    const link_section_identifier = symbol.SymbolIndentifier{ .name = "LINK_SECTION", .scope = .local };
    const link_flags_identifier = symbol.SymbolIndentifier{ .name = "LINK_FLAGS", .scope = .local };

    pub fn init(allocator: std.mem.Allocator, target: *const common.Target, symbol_table: *symbol.SymbolTable) Self {
        return .{
            .allocator = allocator,
            .target = target,
            .symbol_table = symbol_table,
            .tokens = TokenList.init(allocator),
        };
    }

    pub fn deinit(self: *Self) void {
        self.tokens.deinit();
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
        if (self.tokens.items.len == 0) return error.EmptyStack;

        return self.tokens.items[self.tokens.items.len - 1].token_type;
    }

    fn newline(self: *Self) !void {
        try self.push(.newline, null);
    }

    fn pushField(self: *Self) !void {
        const idx = self.field_index;
        self.field_index += 1;

        _ = try self.pushSymbol(.local_identifier, "f{}", .{idx});
    }

    fn pushSymbol(self: *Self, token_type: token.CTokenType, comptime fmt: []const u8, args: anytype) !symbol.Instance {
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
        // Sign
        switch (primitive) {
            .ptr,
            .bool,
            .u8,
            .u16,
            .u32,
            .u64,
            => try self.push(.unsigned, null),
            .void,
            .i8,
            .i16,
            .i32,
            .i64,
            .f32,
            .f64,
            => {},
        }

        const token_type: token.CTokenType = switch (primitive) {
            .void,
            .ptr,
            => .void,
            .bool,
            .u8,
            .i8,
            => .char,
            .u16,
            .i16,
            => .short,
            .u32,
            .i32,
            => .int,
            .u64,
            .i64,
            => .long,
            .f32 => .float,
            .f64 => .double,
        };
        try self.push(token_type, null);

        if (primitive == .ptr) {
            try self.push(.pointer, null);
        }
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
                _ = try self.pushSymbol(.type_identifier, "{s}", .{sym.identifier.name});

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

                try self.push(.semi_colon, null);
                try self.push(.newline, null);
            },
            else => {},
        }
    }

    fn storeToken(self: *Self) !void {
        self.stored_token = try self.pop();
    }

    fn loadToken(self: *Self) !void {
        if (self.stored_token) |t| {
            try self.push(t.token_type, t.span);
        } else {
            return error.NoToken;
        }
    }

    fn clearToken(self: *Self) !void {
        if (self.stored_token != null) {
            self.stored_token = null;
        } else {
            return error.NoToken;
        }
    }

    fn swapFunctionReturn(self: *Self) !void {
        const ret = try self.pop();

        var queue = try std.ArrayList(token.CToken).initCapacity(self.allocator, 16);
        defer queue.deinit();

        while (true) {
            const prev = try self.pop();
            try queue.append(prev);

            if (prev.token_type == .global_identifier) {
                break;
            }
        }

        try self.push(ret.token_type, ret.span);

        while (queue.popOrNull()) |v| {
            try self.push(v.token_type, v.span);
        }
    }

    pub fn enter(self: *Self, statement: *const ast.Statement) !void {
        switch (statement.data) {
            .module => {
                self.field_index = 0;

                _ = try self.symbol_table.addSymbol(&opaque_identifier);

                _ = try self.symbol_table.addSymbol(&link_export_identifier);
                _ = try self.symbol_table.addSymbol(&link_thread_identifier);
                _ = try self.symbol_table.addSymbol(&link_section_identifier);
                _ = try self.symbol_table.addSymbol(&link_flags_identifier);

                try self.push(.module_start, null);
            },
            .identifier => |identifier| {
                const token_type: token.CTokenType = switch (identifier.scope) {
                    .type => l: {
                        const instance = symbol.Instance{ .span = statement.span };

                        try self.pushTypePrefix(&instance);

                        break :l .type_identifier;
                    },
                    .global => l: {
                        break :l .global_identifier;
                    },
                    .local => .local_identifier,
                    .label => .label_identifier,
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
                try self.push(.open_curly_brace, null);
                try self.push(.unsigned, null);
                try self.push(.char, null);

                const sym_idx = self.symbol_table.getSymbolIndexByIdentifier(&opaque_identifier) orelse return error.NotFound;
                _ = try self.pushSymbolInstance(.local_identifier, sym_idx);

                try self.push(.open_bracket, null);
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
                try self.push(.colon, null);
            },
            .variadic_parameter => {
                try self.push(.variable_arguments, null);
            },
            .linkage => |l| {
                try self.storeToken();

                if (l.@"export") {
                    const sym_idx = self.symbol_table.getSymbolIndexByIdentifier(&link_export_identifier) orelse return error.NotFound;

                    _ = try self.pushSymbolInstance(.local_identifier, sym_idx);
                }

                if (l.thread) {
                    const sym_idx = self.symbol_table.getSymbolIndexByIdentifier(&link_thread_identifier) orelse return error.NotFound;

                    _ = try self.pushSymbolInstance(.local_identifier, sym_idx);
                }

                if (l.section != null) {
                    const sym_idx = self.symbol_table.getSymbolIndexByIdentifier(&link_section_identifier) orelse return error.NotFound;

                    _ = try self.pushSymbolInstance(.local_identifier, sym_idx);
                    try self.push(.open_parenthesis, null);
                }
            },
            .offset => {
                try self.pushCast(.u8, true);
                try self.push(.dereference, null);
            },
            .line => {
                try self.push(.tab, null);
            },
            .@"return" => {
                try self.push(.tab, null);
                try self.push(.@"return", null);
            },
            .jump => {
                try self.push(.tab, null);
                try self.push(.goto, null);
            },
            .branch => {
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
            else => {},
        }
    }

    pub fn middle(self: *Self, statement: *const ast.Statement, previous: usize, next: usize) !void {
        switch (statement.data) {
            .union_type => {
                try self.pushField();
                try self.push(.semi_colon, null);
                try self.push(.@"struct", null);
            },
            .struct_type => {
                if (self.tokens.getLast().token_type != .close_bracket) {
                    try self.pushField();
                }

                try self.push(.semi_colon, null);
            },
            .array_type => {
                try self.pushField();
                try self.push(.open_bracket, null);
            },
            .linkage => |l| {
                if (l.section == previous) {
                    try self.push(.close_parenthesis, null);
                }

                if (l.flags == next) {
                    const sym_idx = self.symbol_table.getSymbolIndexByIdentifier(&link_flags_identifier) orelse return error.NotFound;

                    _ = try self.pushSymbolInstance(.local_identifier, sym_idx);
                    try self.push(.open_parenthesis, null);
                }
            },
            .data_definition => |d| {
                if (d.identifier == previous) {
                    const identifier = try self.pop();

                    const instance = symbol.Instance{ .span = identifier.span };
                    try self.pushDataType(&instance);

                    try self.push(identifier.token_type, identifier.span);
                } else if (d.linkage == previous) {
                    // Add type to data
                    try self.storeToken();

                    const stored_instance = symbol.Instance{ .span = self.stored_token.?.span };
                    const stored_sym = self.symbol_table.getSymbolByInstance(&stored_instance) orelse return error.NotFound;

                    const type_instance = symbol.SymbolIndentifier{ .name = stored_sym.identifier.name, .scope = .type };
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
                    if (self.tokens.getLast().token_type != .colon) {
                        _ = try self.pop(); // TODO: Remove struct/union?
                    }
                }
            },
            .offset => {
                try self.push(.plus, null);
            },
            .function_signature => |fs| {
                if (fs.return_type == previous) {
                    try self.swapFunctionReturn();
                    try self.push(.open_parenthesis, null);
                } else if (fs.name != previous and fs.linkage != previous and self.tokens.getLast().token_type != .open_parenthesis) {
                    try self.push(.comma, null);
                }
            },
            .function_parameter => |fp| {
                if (fp.value == previous) {
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
                try self.push(.assign, null);
            },
            else => {},
        }
    }

    pub fn exit(self: *Self, statement: *const ast.Statement) !void {
        switch (statement.data) {
            .module => try self.push(.module_end, null),
            .opaque_type => {
                try self.push(.close_bracket, null);
                try self.push(.semi_colon, null);
                try self.push(.close_curly_brace, null);
            },
            .union_type => {
                try self.pushField();
                try self.push(.semi_colon, null);

                try self.push(.close_curly_brace, null);
            },
            .struct_type => {
                if (self.tokens.getLast().token_type != .close_bracket) {
                    try self.pushField();
                }

                try self.push(.semi_colon, null);

                try self.push(.close_curly_brace, null);
            },
            .array_type => {
                try self.push(.close_bracket, null);
            },
            .data_definition => {
                try self.push(.close_curly_brace, null);
                try self.push(.semi_colon, null);
                try self.push(.newline, null);
            },
            .typed_data => {
                const val = try self.pop();

                if (self.tokens.getLast().token_type == .colon) {
                    // Zero data

                    _ = try self.pop(); // TODO: Free data?

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
            },
            .linkage => |l| {
                if (l.flags != null) {
                    try self.push(.close_parenthesis, null);
                }

                try self.loadToken();
            },
            .function => {
                try self.push(.close_curly_brace, null);
                try self.push(.newline, null);
            },
            .function_signature => |fs| {
                if (fs.parameters == null) {
                    try self.swapFunctionReturn();
                    try self.push(.open_parenthesis, null);
                }

                try self.push(.close_parenthesis, null);
                try self.push(.open_curly_brace, null);
                try self.push(.newline, null);
            },
            .function_parameter => {
                if (self.tokens.getLast().token_type == .colon) {
                    _ = try self.pop();
                    try self.clearToken();
                } else {
                    try self.loadToken();
                }
            },
            .line,
            .@"return",
            .jump,
            .branch,
            .halt,
            => {
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

        const includes = "";

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

        pub fn next(self: *Self) !bool {
            if (self.state == .done) return false;

            const tok = self.reader_next();

            return try self.parse(tok);
        }

        fn parse(self: *Self, tok: *const token.CToken) !bool {
            switch (tok.token_type) {
                .module_start => {
                    try self.print(common.Color.identifier, Self.includes, .{});

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
                            const name = sym.identifier.name;

                            try switch (sym.identifier.scope) {
                                .local => self.print(color, "{s}", .{name}),
                                .global => self.print(color, "{s}", .{name}),
                                .type => self.print(color, "{s}_T", .{name}),
                                .label => self.print(color, "{s}_L", .{name}),
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
                            .string => |v| self.print(color, "\"{s}\"", .{v}), // TODO: Escape string symbols
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
                .colon,
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

    // std.debug.print("{s}", .{actual});
}
