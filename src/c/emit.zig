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
        try switch (out.enter) {
            true => emit_callback.enter(out.value),
            false => emit_callback.exit(out.value),
        };
    }

    return try emit_callback.tokens.toOwnedSlice();
}

const EmitWalkState = enum {
    default,
    array,
    @"struct",
    @"union",
};

pub const CEmitWalkCallback = struct {
    allocator: std.mem.Allocator,
    target: *const common.Target,
    symbol_table: *symbol.SymbolTable,
    state_stack: StateStack,
    tokens: TokenList,

    state: EmitWalkState = .default,
    field_index: usize = 0,

    const Self = @This();
    const TokenList = std.ArrayList(token.CToken);
    const StateStack = std.ArrayList(EmitWalkState);

    pub fn init(allocator: std.mem.Allocator, target: *const common.Target, symbol_table: *symbol.SymbolTable) Self {
        return .{
            .allocator = allocator,
            .target = target,
            .symbol_table = symbol_table,
            .state_stack = StateStack.init(allocator),
            .tokens = TokenList.init(allocator),
        };
    }

    pub fn deinit(self: *Self) void {
        self.state_stack.deinit();
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

    fn pushSymbol(self: *Self, token_type: token.CTokenType, comptime fmt: []const u8, args: anytype) !void {
        const name = try std.fmt.allocPrint(self.allocator, fmt, args);
        defer self.allocator.free(name);

        var sym = symbol.SymbolIndentifier{
            .name = name,
            .scope = switch (token_type) {
                .local_identifier => .local,
                .global_identifier => .global,
                else => unreachable,
            },
        };
        const sym_idx = try self.symbol_table.addSymbol(&sym);

        const span = self.symbol_table.nextSpan();
        var instance = symbol.Instance{ .span = span };
        _ = try self.symbol_table.addSymbolInstanceByIndex(sym_idx, &instance);

        try self.push(token_type, span);
    }

    fn pushField(self: *Self) !void {
        const idx = self.field_index;
        self.field_index += 1;

        try self.pushSymbol(.local_identifier, "f{}", .{idx});
    }

    fn pushState(self: *Self, state: EmitWalkState) !void {
        try self.state_stack.append(self.state);
        self.state = state;
    }

    fn popState(self: *Self) !void {
        self.state = self.state_stack.popOrNull() orelse return error.EmptyStack;
    }

    pub fn enter(self: *Self, statement: *const ast.Statement) !void {
        switch (statement.data) {
            .module => try self.push(.module_start, null),
            .identifier => |identifier| {
                const token_type: token.CTokenType = switch (identifier.scope) {
                    .global => .global_identifier,
                    .local => .local_identifier,
                    .type => .type_identifier,
                    .label => .label_identifier,
                };

                if (token_type == .type_identifier) {
                    const instance = symbol.Instance{ .span = statement.span };
                    const sym = self.symbol_table.getSymbolByInstance(&instance) orelse return error.NotFound;

                    switch (sym.memory) {
                        .@"opaque",
                        .@"struct",
                        => try self.push(.@"struct", null),
                        .@"union" => try self.push(.@"union", null),
                        else => {},
                    }
                }

                try self.push(
                    token_type,
                    statement.span,
                );

                if (self.state == .@"struct") {
                    try self.pushField();
                    try self.push(.semi_colon, null);
                }
            },
            .literal => |literal| {
                if (self.state == .array) {
                    try self.pushField();
                    try self.push(.open_bracket, null);
                }

                try self.push(
                    switch (literal.type) {
                        .integer => .integer_literal,
                        .single => .single_literal,
                        .double => .double_literal,
                        .string => .string_literal,
                    },
                    statement.span,
                );
            },
            .primitive_type => |primitive| {
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

                if (self.state == .@"struct") {
                    try self.pushField();
                    try self.push(.semi_colon, null);
                }
            },
            .opaque_type => {
                try self.push(.open_curly_brace, null);
                try self.push(.unsigned, null);
                try self.push(.char, null);
                try self.pushField();
                try self.push(.open_bracket, null);
            },
            .union_type => {
                try self.pushState(.@"union");

                try self.push(.open_curly_brace, null);
            },
            .struct_type => {
                if (self.state == .@"union") {
                    try self.push(.@"struct", null);
                }

                try self.pushState(.@"struct");

                try self.push(.open_curly_brace, null);
            },
            .array_type => {
                try self.pushState(.array);
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
                try self.popState();

                try self.push(.close_curly_brace, null);
            },
            .struct_type => {
                try self.popState();

                try self.push(.close_curly_brace, null);

                if (self.state == .@"union") {
                    try self.pushField();
                    try self.push(.semi_colon, null);
                }
            },
            .array_type => {
                try self.popState();

                try self.push(.close_bracket, null);
                try self.push(.semi_colon, null);
            },
            .type_definition => {
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
                            .double => |v| self.print(color, "{d:.}d", .{v}),
                            .string => |v| self.print(color, "\"{s}\"", .{v}), // TODO: Escape string symbols
                        };
                    },
                    else => unreachable,
                }
            }

            self.state = switch (tok.token_type) {
                .newline,
                .tab,
                .open_bracket,
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
