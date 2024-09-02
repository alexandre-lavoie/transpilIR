const std = @import("std");

const ast = @import("../ast/lib.zig");
const common = @import("../common.zig");
const symbol = @import("../symbol/lib.zig");
const token = @import("token.zig");

pub const TokenWalkCallback = struct {
    allocator: std.mem.Allocator,
    tokens: TokenList,

    assignment_enter: bool = false,
    function_enter: bool = false,
    data_enter: bool = false,
    block_enter: bool = false,
    type_scope: bool = false,
    type_enter: bool = false,
    op_enter: bool = false,

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

    pub fn enter(self: *Self, statement: *ast.Statement) !void {
        switch (statement.data) {
            .node,
            .linkage,
            .offset,
            .array_type,
            .call,
            .call_parameter,
            .function_signature,
            .vaarg,
            .vastart,
            .allocate,
            .blit,
            .copy,
            .cast,
            .convert,
            .load,
            .store,
            .phi,
            .phi_parameter,
            .comparison,
            .negate,
            => {},
            .assignment => self.assignment_enter = true,
            .module => try self.push(.module_start, null),
            .line => try self.push(.tab, null),
            .env_type => try self.push(.env, null),
            .zero_type => try self.push(.zero, null),
            .opaque_type,
            .struct_type,
            .union_type,
            => try self.push(.open_curly_brace, null),
            .type_definition => {
                try self.push(.type, null);

                self.type_scope = true;
                self.type_enter = true;
            },
            .data_definition => {
                try self.push(.data, null);

                self.data_enter = true;
            },
            .function => {
                try self.push(.function, null);

                self.function_enter = true;
            },
            .variadic_parameter => {
                try self.push(.variable_arguments, null);
                try self.push(.comma, null);
            },
            .block => self.block_enter = true,
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

                if (self.block_enter and token_type == .label_identifier) {
                    self.block_enter = false;

                    try self.newline();
                }
            },
            .primitive_type => |primitive| {
                const optional: ?token.TokenType = switch (primitive) {
                    .void => null,
                    .byte_unsigned => .byte_unsigned,
                    .byte => .byte,
                    .double => .double,
                    .half_word_unsigned => .half_word_unsigned,
                    .half_word => .half_word,
                    .long, .long_unsigned => .long,
                    .single => .single,
                    .word_unsigned => .word_unsigned,
                    .word => .word,
                };

                if (optional) |token_type| {
                    try self.push(token_type, null);
                }
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
            .function_parameter => {
                if (self.function_enter) {
                    self.function_enter = false;

                    try self.rot2();
                    try self.push(.open_parenthesis, null);
                }
            },
            .typed_data => {
                if (self.data_enter) {
                    self.data_enter = false;

                    try self.push(.assign, null);
                    try self.push(.open_curly_brace, null);
                }
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
                    .multiply => .multiply,
                    .remainder => .remainder,
                    .subtract => .subtract,
                    .arthimetic_shift_right => .arthimetic_shift_right,
                    .@"and" => .bitwise_and,
                    .@"or" => .bitwise_or,
                    .logical_shift_right => .shift_right,
                    .shift_left => .shift_left,
                    .xor => .bitwise_xor,
                }, null);

                self.op_enter = true;
            },
        }
    }

    pub fn exit(self: *Self, statement: *ast.Statement) !void {
        switch (statement.data) {
            .literal,
            .node,
            .env_type,
            .zero_type,
            .call,
            .call_parameter,
            .variadic_parameter,
            .vaarg,
            .vastart,
            .allocate,
            .assignment,
            .blit,
            .copy,
            .cast,
            .convert,
            .load,
            .store,
            .halt,
            .jump,
            .@"return",
            .phi,
            .phi_parameter,
            .comparison,
            .negate,
            => {},
            .module => try self.push(.module_end, null),
            .block,
            .line,
            => try self.newline(),
            .opaque_type,
            .union_type,
            => try self.push(.close_curly_brace, null),
            .typed_data,
            => try self.push(.comma, null),
            .array_type => try self.rot2(),
            .struct_type => {
                _ = try self.pop();
                try self.push(.close_curly_brace, null);
            },
            .primitive_type => {
                if (self.type_scope) {
                    try self.push(.comma, null);
                } else if (self.op_enter) {
                    self.op_enter = false;
                    try self.rot2();
                }
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
                        if (self.type_enter) {
                            self.type_enter = false;

                            try self.push(.assign, null);
                        } else if (self.type_scope and identifier.scope == .type) {
                            try self.push(.comma, null);
                        } else if (self.op_enter) {
                            self.op_enter = false;
                            try self.rot2();
                        }
                    },
                    .local => {
                        if (self.assignment_enter) {
                            self.assignment_enter = false;

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
            .function_parameter => {
                try self.rot2();
                try self.push(.comma, null);
            },
            .data_definition => {
                _ = try self.pop();

                try self.push(.close_curly_brace, null);
                try self.newline();
            },
            .type_definition => {
                self.type_scope = false;

                try self.newline();
            },
            .function_signature => {
                if (self.function_enter) {
                    self.function_enter = false;

                    try self.rot2();
                    try self.push(.open_parenthesis, null);
                } else {
                    _ = try self.pop();
                }

                try self.push(.close_parenthesis, null);
                try self.push(.open_curly_brace, null);
                try self.newline();
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
        }
    }
};

pub fn Emit(comptime Reader: type, comptime Writer: type) type {
    return struct {
        reader: *Reader,
        writer: *Writer,
        tty_config: *const std.io.tty.Config,
        symbol_table: *const symbol.SymbolTable,

        done: bool = false,
        space: bool = false,

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
            if (self.done) return false;

            const tok = self.reader_readToken();
            switch (tok.token_type) {
                .module_start => return true,
                .module_end => {
                    self.done = true;
                    return false;
                },
                else => {},
            }

            if (self.space) {
                switch (tok.token_type) {
                    .newline,
                    .comma,
                    .open_parenthesis,
                    .close_parenthesis,
                    => {},
                    else => try self.writer_writeByte(' '),
                }
                self.space = false;
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

            self.space = switch (tok.token_type) {
                .newline,
                .tab,
                .open_parenthesis,
                => false,
                else => true,
            };

            return true;
        }
    };
}
