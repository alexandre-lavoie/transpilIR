const std = @import("std");

const ast = @import("../ast/lib.zig");
const common = @import("../common.zig");
const symbol = @import("../symbol/lib.zig");
const token = @import("token.zig");

const symbol_test = @import("../symbol/test.zig");

pub fn emit(allocator: std.mem.Allocator, tree: *ast.AST) ![]token.Token {
    var walk = ast.ASTWalk.init(allocator, tree);
    defer walk.deinit();

    var emit_callback = EmitWalkCallback.init(
        allocator,
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
    assignment_enter,
    function_enter,
    data_enter,
    block_enter,
    type_enter,
    type_scope,
    type_exit,
    op_enter,
    call_enter,
    phi_enter,
};

pub const EmitWalkCallback = struct {
    allocator: std.mem.Allocator,
    tokens: TokenList,

    state: EmitWalkState = .default,

    const Self = @This();
    const TokenList = std.ArrayList(token.Token);

    pub fn init(allocator: std.mem.Allocator) Self {
        return .{
            .allocator = allocator,
            .tokens = TokenList.init(allocator),
        };
    }

    pub fn deinit(self: *Self) void {
        self.tokens.deinit();
    }

    fn push(self: *Self, token_type: token.TokenType, span: ?common.SourceSpan) !void {
        try self.tokens.append(.{
            .token_type = token_type,
            .span = span orelse .{},
        });
    }

    fn append(self: *Self, tok: token.Token) !void {
        try self.tokens.append(tok);
    }

    fn pop(self: *Self) !token.Token {
        return self.tokens.popOrNull() orelse return error.NotFound;
    }

    fn rot2(self: *Self) !void {
        const v0 = try self.pop();
        const v1 = try self.pop();

        try self.append(v0);
        try self.append(v1);
    }

    fn newline(self: *Self) !void {
        try self.push(.newline, null);
    }

    fn functionOpen(self: *Self) !void {
        self.state = .default;

        const return_type = try self.pop();
        const label = try self.pop();

        if (return_type.token_type != .zero) try self.append(return_type);
        try self.append(label);

        try self.push(.open_parenthesis, null);
    }

    fn callOpen(self: *Self) !void {
        self.state = .default;

        const return_type = try self.pop();
        const label = try self.pop();
        const call = try self.pop();

        if (return_type.token_type != .zero) try self.append(return_type);
        try self.append(call);
        try self.append(label);

        try self.push(.open_parenthesis, null);
    }

    pub fn enter(self: *Self, statement: *ast.Statement) !void {
        switch (statement.data) {
            .node,
            .linkage,
            .offset,
            .array_type,
            .function_signature,
            .allocate,
            .blit,
            .convert,
            .load,
            .store,
            .comparison,
            => {},
            .assignment => self.state = .assignment_enter,
            .module => try self.push(.module_start, null),
            .line => try self.push(.tab, null),
            .negate => try self.push(.negate, null),
            .env_type => try self.push(.env, null),
            .zero_type => try self.push(.zero, null),
            .vastart => try self.push(.vastart, null),
            .vaarg => try self.push(.vaarg, null),
            .copy => try self.push(.copy, null),
            .cast => try self.push(.cast, null),
            .opaque_type,
            .struct_type,
            .union_type,
            => try self.push(.open_curly_brace, null),
            .phi => {
                try self.push(.phi, null);

                self.state = .phi_enter;
            },
            .type_definition => {
                try self.push(.type, null);

                self.state = .type_enter;
            },
            .data_definition => {
                switch (self.state) {
                    .type_exit => try self.newline(),
                    else => {},
                }

                try self.push(.data, null);

                self.state = .data_enter;
            },
            .call => {
                try self.push(.call, null);

                self.state = .call_enter;
            },
            .function => {
                try self.newline();
                try self.push(.function, null);

                self.state = .function_enter;
            },
            .variadic_parameter => {
                try self.push(.variable_arguments, null);
                try self.push(.comma, null);
            },
            .block => self.state = .block_enter,
            .identifier => |identifier| {
                const token_type: token.TokenType = switch (identifier.scope) {
                    .global => .global_identifier,
                    .local => .local_identifier,
                    .type => .type_identifier,
                    .label => .label_identifier,
                };

                try self.push(
                    token_type,
                    statement.span,
                );

                if (self.state == .block_enter and token_type == .label_identifier) {
                    self.state = .default;

                    try self.newline();
                }
            },
            .primitive_type => |primitive| {
                try self.push(switch (primitive) {
                    .void => .zero,
                    .byte_unsigned => .byte_unsigned,
                    .byte => .byte,
                    .double => .double,
                    .half_word_unsigned => .half_word_unsigned,
                    .half_word => .half_word,
                    .long => .long,
                    .long_unsigned => .long_unsigned,
                    .single => .single,
                    .word_unsigned => .word_unsigned,
                    .word => .word,
                }, null);
            },
            .literal => |literal| {
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
            .function_parameter => switch (self.state) {
                .function_enter => try self.functionOpen(),
                else => {},
            },
            .call_parameter => switch (self.state) {
                .call_enter => try self.callOpen(),
                else => {},
            },
            .phi_parameter => switch (self.state) {
                .phi_enter => {
                    self.state = .default;

                    try self.rot2();
                },
                else => {},
            },
            .typed_data => switch (self.state) {
                .data_enter => {
                    self.state = .default;

                    try self.push(.assign, null);
                    try self.push(.open_curly_brace, null);
                },
                else => {},
            },
            .@"return" => {
                try self.push(.tab, null);
                try self.push(.@"return", null);
            },
            .jump => {
                try self.push(.tab, null);
                try self.push(.jump, null);
            },
            .halt => {
                try self.push(.tab, null);
                try self.push(.halt, null);
            },
            .branch => {
                try self.push(.tab, null);
                try self.push(.jump_not_zero, null);
            },
            .binary_operation => |op| {
                try self.push(switch (op.operation_type) {
                    .addition => .addition,
                    .divide => .divide,
                    .divide_unsigned => .divide_unsigned,
                    .multiply => .multiply,
                    .remainder => .remainder,
                    .remainder_unsigned => .remainder_unsigned,
                    .subtract => .subtract,
                    .arthimetic_shift_right => .arthimetic_shift_right,
                    .@"and" => .bitwise_and,
                    .@"or" => .bitwise_or,
                    .logical_shift_right => .shift_right,
                    .shift_left => .shift_left,
                    .xor => .bitwise_xor,
                }, null);

                self.state = .op_enter;
            },
        }
    }

    pub fn exit(self: *Self, statement: *ast.Statement) !void {
        switch (statement.data) {
            .literal,
            .node,
            .env_type,
            .zero_type,
            .variadic_parameter,
            .vastart,
            .assignment,
            .blit,
            .convert,
            .load,
            .halt,
            .jump,
            .@"return",
            => {},
            .module => try self.push(.module_end, null),
            .block,
            .line,
            => try self.newline(),
            .phi => _ = try self.pop(),
            .opaque_type,
            .union_type,
            => try self.push(.close_curly_brace, null),
            .typed_data => try self.push(.comma, null),
            .array_type => try self.rot2(),
            .struct_type => {
                _ = try self.pop();
                try self.push(.close_curly_brace, null);
            },
            .primitive_type => {
                switch (self.state) {
                    .op_enter => {
                        self.state = .default;
                        try self.rot2();
                    },
                    .type_scope => try self.push(.comma, null),
                    else => {},
                }
            },
            .negate => {
                const value = try self.pop();

                try self.rot2();
                try self.append(value);
            },
            .allocate => {
                const size = try self.pop();
                const alignment = try self.pop();

                try self.push(.allocate, null);
                try self.append(alignment);
                try self.append(size);
            },
            .binary_operation => {
                const right = try self.pop();

                try self.push(.comma, null);
                try self.append(right);
            },
            .branch => {
                const false_label = try self.pop();
                const true_label = try self.pop();

                try self.push(.comma, null);
                try self.append(true_label);

                try self.push(.comma, null);
                try self.append(false_label);
            },
            .identifier => |identifier| {
                switch (identifier.scope) {
                    .type => {
                        switch (self.state) {
                            .type_enter => {
                                self.state = .type_scope;

                                try self.push(.assign, null);
                            },
                            .type_scope => try self.push(.comma, null),
                            .op_enter => {
                                self.state = .type_scope;
                                try self.rot2();
                            },
                            else => {},
                        }
                    },
                    .local => {
                        if (self.state == .assignment_enter) {
                            self.state = .default;

                            try self.push(.assign, null);
                        }
                    },
                    else => {},
                }
            },
            .offset => {
                const literal = try self.pop();

                try self.push(.plus, null);
                try self.append(literal);
            },
            .function => {
                try self.push(.close_curly_brace, null);
                try self.newline();
            },
            .function_parameter, .call_parameter => {
                try self.rot2();
                try self.push(.comma, null);
            },
            .phi_parameter => try self.push(.comma, null),
            .vaarg,
            .copy,
            .cast,
            => {
                const value = try self.pop();

                try self.rot2();
                try self.append(value);
            },
            .data_definition => {
                _ = try self.pop();

                try self.push(.close_curly_brace, null);
                try self.newline();
            },
            .type_definition => {
                self.state = .type_exit;

                try self.newline();
            },
            .function_signature => {
                switch (self.state) {
                    .function_enter => try self.functionOpen(),
                    else => _ = try self.pop(),
                }

                try self.push(.close_parenthesis, null);
                try self.push(.open_curly_brace, null);
                try self.newline();
            },
            .call => {
                switch (self.state) {
                    .call_enter => try self.callOpen(),
                    else => _ = try self.pop(),
                }

                try self.push(.close_parenthesis, null);
            },
            .store => {
                const address = try self.pop();
                const value = try self.pop();

                const @"type" = try self.pop();
                const store: token.TokenType = switch (@"type".token_type) {
                    .byte => .byte_store,
                    .half_word => .word_store,
                    .word => .word_store,
                    .long => .long_store,
                    .single => .single_store,
                    .double => .double_store,
                    else => unreachable,
                };

                try self.push(store, null);
                try self.append(value);
                try self.push(.comma, null);
                try self.append(address);
            },
            .linkage => |linkage| {
                const flags: ?token.Token = switch (linkage.flags != null) {
                    true => try self.pop(),
                    false => null,
                };
                const section: ?token.Token = switch (linkage.section != null) {
                    true => try self.pop(),
                    false => null,
                };
                const global_name = try self.pop();
                const global_reserve = try self.pop();

                if (linkage.@"export") try self.push(.@"export", null);
                if (linkage.thread) try self.push(.thread, null);
                if (linkage.section != null) try self.push(.section, null);

                if (section) |s| try self.append(s);
                if (flags) |f| try self.append(f);
                try self.append(global_reserve);
                try self.append(global_name);
            },
            .comparison => |comparison| {
                const right = try self.pop();
                const left = try self.pop();
                const @"type" = try self.pop();

                const comparison_op: token.TokenType = switch (@"type".token_type) {
                    .word => switch (comparison.operation_type) {
                        .equal => .word_equal,
                        .greater_than_equal => .word_greater_than_equal,
                        .greater_than => .word_greater_than,
                        .less_than_equal => .word_less_than_equal,
                        .less_than => .word_less_than,
                        .not_equal => .word_not_equal,
                        .all_nan,
                        .any_nan,
                        => unreachable,
                    },
                    .word_unsigned => switch (comparison.operation_type) {
                        .greater_than_equal => .word_greater_than_equal_unsigned,
                        .greater_than => .word_greater_than_unsigned,
                        .less_than_equal => .word_less_than_equal_unsigned,
                        .less_than => .word_less_than_unsigned,
                        .equal,
                        .not_equal,
                        .all_nan,
                        .any_nan,
                        => unreachable,
                    },
                    .long => switch (comparison.operation_type) {
                        .equal => .long_equal,
                        .greater_than_equal => .long_greater_than_equal,
                        .greater_than => .long_greater_than,
                        .less_than_equal => .long_less_than_equal,
                        .less_than => .long_less_than,
                        .not_equal => .long_not_equal,
                        .all_nan,
                        .any_nan,
                        => unreachable,
                    },
                    .long_unsigned => switch (comparison.operation_type) {
                        .greater_than_equal => .long_greater_than_equal_unsigned,
                        .greater_than => .long_greater_than_unsigned,
                        .less_than_equal => .long_less_than_equal_unsigned,
                        .less_than => .long_less_than_unsigned,
                        .equal,
                        .not_equal,
                        .all_nan,
                        .any_nan,
                        => unreachable,
                    },
                    .single => switch (comparison.operation_type) {
                        .equal => .single_equal,
                        .greater_than_equal => .single_greater_than_equal,
                        .greater_than => .single_greater_than,
                        .less_than_equal => .single_less_than_equal,
                        .less_than => .single_less_than,
                        .not_equal => .single_not_equal,
                        .all_nan => .single_all_nan,
                        .any_nan => .single_any_nan,
                    },
                    .double => switch (comparison.operation_type) {
                        .equal => .double_equal,
                        .greater_than_equal => .double_greater_than_equal,
                        .greater_than => .double_greater_than,
                        .less_than_equal => .double_less_than_equal,
                        .less_than => .double_less_than,
                        .not_equal => .double_not_equal,
                        .all_nan => .double_all_nan,
                        .any_nan => .double_any_nan,
                    },
                    else => unreachable,
                };

                try self.push(comparison_op, null);
                try self.append(left);
                try self.push(.comma, null);
                try self.append(right);
            },
        }
    }
};

const EmitWriterState = enum {
    default,
    space,
    assign,
    done,
};

pub fn EmitWriter(comptime Reader: type, comptime Writer: type) type {
    return struct {
        reader: *Reader,
        writer: *Writer,
        tty_config: *const std.io.tty.Config,
        symbol_table: *const symbol.SymbolTable,

        state: EmitWriterState = .default,

        const Self = @This();

        fn reader_readToken(self: *Self) *const token.Token {
            return self.reader.readToken();
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
            try self.tty_config.setColor(self.writer, color);
            try self.writer_write(buffer);
            try self.tty_config.setColor(self.writer, .reset);
        }

        fn print(self: *Self, color: std.io.tty.Color, comptime format: []const u8, args: anytype) !void {
            try self.tty_config.setColor(self.writer, color);
            try self.writer_print(format, args);
            try self.tty_config.setColor(self.writer, .reset);
        }

        pub fn init(reader: *Reader, writer: *Writer, tty_config: *const std.io.tty.Config, symbol_table: *const symbol.SymbolTable) Self {
            return .{
                .reader = reader,
                .writer = writer,
                .tty_config = tty_config,
                .symbol_table = symbol_table,
            };
        }

        pub fn next(self: *Self) !bool {
            if (self.state == .done) return false;

            const tok = self.reader_readToken();
            switch (tok.token_type) {
                .module_start => return true,
                .module_end => {
                    self.state = .done;
                    return false;
                },
                else => {},
            }

            switch (self.state) {
                .space => switch (tok.token_type) {
                    .newline,
                    .comma,
                    .open_parenthesis,
                    .close_parenthesis,
                    => {},
                    else => try self.writer_writeByte(' '),
                },
                .assign => switch (tok.token_type) {
                    .byte,
                    .half_word,
                    .word,
                    .long,
                    .single,
                    .double,
                    .type_identifier,
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
                        const sym = self.symbol_table.getSymbolByInstance(&instance) orelse return error.NotFound;
                        const name = sym.identifier.name;

                        try switch (sym.identifier.scope) {
                            .local => self.print(color, "%{s}", .{name}),
                            .global => self.print(color, "${s}", .{name}),
                            .type => self.print(color, ":{s}", .{name}),
                            .label => self.print(color, "@{s}", .{name}),
                        };
                    },
                    .string_literal,
                    .double_literal,
                    .single_literal,
                    .integer_literal,
                    => {
                        const literal = self.symbol_table.getLiteralByInstance(&instance) orelse return error.NotFound;

                        try switch (literal.value) {
                            .integer => |v| self.print(color, "{}", .{v}),
                            .single => |v| self.print(color, "s_{d:.}", .{v}),
                            .double => |v| self.print(color, "d_{d:.}", .{v}),
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
                .allocate,
                => .default,
                .assign => .assign,
                else => .space,
            };

            return true;
        }
    };
}

test "Emit" {
    // Arrange
    const allocator = std.testing.allocator;
    const tty_config: std.io.tty.Config = .no_color;

    const file = @embedFile("../test/all.ssa");

    var tree = try symbol_test.testAST(allocator, file);
    defer tree.deinit();

    var symbol_table = symbol.SymbolTable.init(allocator);
    defer symbol_table.deinit();

    var output_array = std.ArrayList(u8).init(allocator);
    defer output_array.deinit();

    var output_writer = output_array.writer();

    // Act
    try symbol_test.testValidate(allocator, file, &tree, &symbol_table);

    const tokens = try emit(allocator, &tree);
    defer allocator.free(tokens);
    var token_reader = token.TokenReader.init(tokens);

    var emit_writer = EmitWriter(
        @TypeOf(token_reader),
        @TypeOf(output_writer),
    ).init(
        &token_reader,
        &output_writer,
        &tty_config,
        &symbol_table,
    );

    while (try emit_writer.next()) {}

    const actual = try output_array.toOwnedSlice();
    defer allocator.free(actual);

    // Assert
    try std.testing.expectEqualStrings(file, actual);
}
