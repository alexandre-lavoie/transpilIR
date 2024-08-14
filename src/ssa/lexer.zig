const std = @import("std");

const token = @import("token.zig");

pub fn Lexer(comptime Reader: type, comptime Collection: type) type {
    return struct {
        reader: *Reader,
        collection: *Collection,
        offset: usize = 0,
        previous: u8 = 0,

        const Self = @This();

        //
        // Reader
        //

        fn next(self: *Self) u8 {
            self.offset += 1;

            self.previous = self.reader.readByte() catch 0;
            return self.previous;
        }

        //
        // Collection
        //

        fn append(self: *Self, next_token: token.Token) !void {
            try self.collection.append(next_token);
        }

        //
        // Lexer
        //

        pub fn init(reader: *Reader, collection: *Collection) Self {
            return Self{
                .reader = reader,
                .collection = collection,
            };
        }

        pub fn lex(self: *Self) !void {
            try self.append(token.Token.init(.module_start));

            _ = self.next();

            while (true) {
                const start = self.offset;

                var next_token: token.Token = try switch (self.previous) {
                    '\x00' => token.Token.init(.module_end),
                    '#' => {
                        try self.comment();
                        continue;
                    },
                    '$' => self.global_identifier(),
                    '%' => self.temporary_identifier(),
                    '@' => self.label_identifier(),
                    ':' => self.type_identifier(),
                    '"' => self.string_literal(),
                    ',' => self.punctuation(.comma),
                    '(' => self.punctuation(.open_parenthesis),
                    ')' => self.punctuation(.close_parenthesis),
                    '{' => self.punctuation(.open_curly_brace),
                    '}' => self.punctuation(.close_curly_brace),
                    '=' => self.assign(),
                    '.' => self.variable_arguments(),
                    'a'...'z' => self.reserved_word_or_floating_literal(),
                    '-', '0'...'9' => self.integer_literal(),
                    ' ', '\t', '\n', '\r' => {
                        _ = self.next();
                        continue;
                    },
                    else => {
                        std.log.err("{c}", .{self.previous});
                        return error.InvalidSymbol;
                    },
                };

                const end = self.offset;

                next_token.span.start = start;
                next_token.span.end = end;

                try self.append(next_token);

                if (next_token.token_type == .module_end) break;
            }
        }

        fn comment(self: *Self) !void {
            if (self.previous != '#') return error.CommentError;

            while (true) {
                switch (self.next()) {
                    '\x00', '\r', '\n' => break,
                    else => continue,
                }
            }
        }

        fn identifier(self: *Self, previous: u8, token_type: token.TokenType) !token.Token {
            if (previous != self.previous) return error.IdentifierSymbolError;

            while (true) {
                switch (self.next()) {
                    '0'...'9', 'A'...'Z', 'a'...'z', '_', '.' => continue,
                    else => break,
                }
            }

            return token.Token.init(token_type);
        }

        fn global_identifier(self: *Self) !token.Token {
            return self.identifier('$', .global_identifier);
        }

        fn temporary_identifier(self: *Self) !token.Token {
            return self.identifier('%', .temporary_identifier);
        }

        fn label_identifier(self: *Self) !token.Token {
            return self.identifier('@', .label_identifier);
        }

        fn type_identifier(self: *Self) !token.Token {
            return self.identifier(':', .type_identifier);
        }

        fn string_literal(self: *Self) !token.Token {
            if ('"' != self.previous) return error.StringOpenError;

            while (true) {
                switch (self.next()) {
                    '"' => {
                        _ = self.next();

                        return token.Token.init(.string_literal);
                    },
                    '\x00' => return error.StringCloseError,
                    '\\' => {
                        _ = self.next();
                        continue;
                    },
                    else => continue,
                }
            }
        }

        fn integer_literal(self: *Self) !token.Token {
            switch (self.previous) {
                '0'...'9' => {},
                '-' => switch (self.next()) {
                    '0'...'9' => {},
                    else => return error.IntegerNegativeError,
                },
                else => return error.IntegerDigitError,
            }

            while (true) {
                switch (self.next()) {
                    '0'...'9' => continue,
                    else => break,
                }
            }

            return token.Token.init(.integer_literal);
        }

        fn punctuation(self: *Self, token_type: token.TokenType) !token.Token {
            _ = self.next();

            return token.Token.init(token_type);
        }

        fn assign(self: *Self) !token.Token {
            const token_type: token.TokenType = switch (self.next()) {
                'w' => .word_assign,
                'l' => .long_assign,
                's' => .single_assign,
                'd' => .double_assign,
                else => return error.AssignInvalidType,
            };

            _ = self.next();

            return token.Token.init(token_type);
        }

        fn variable_arguments(self: *Self) !token.Token {
            if (self.previous != '.') return error.ArgumentSpreadError;
            if (self.next() != '.') return error.ArgumentSpreadError;
            if (self.next() != '.') return error.ArgumentSpreadError;

            _ = self.next();

            return token.Token.init(.variable_arguments);
        }

        fn reserved_word_or_floating_literal(self: *Self) !token.Token {
            var buffer: [token.LONGEST_RESERVED_WORD + 1]u8 = undefined;

            buffer[0] = self.previous;

            var i: usize = 1;
            while (i < token.LONGEST_RESERVED_WORD) : (i += 1) {
                const next_char = self.next();

                if (i == 1 and next_char == '_' and (buffer[0] == 'd' or buffer[0] == 's')) {
                    return self.floating_literal(buffer[0]);
                }

                switch (next_char) {
                    'a'...'z', '0'...'9' => buffer[i] = next_char,
                    else => break,
                }
            }

            if (token.RESERVED_WORDS.get(buffer[0..i])) |token_type| {
                return token.Token.init(token_type);
            } else {
                return error.ReservedWordError;
            }
        }

        fn floating_literal(self: *Self, ftype: u8) !token.Token {
            const token_type: token.TokenType = switch (ftype) {
                'd' => .double_literal,
                's' => .single_literal,
                else => return error.FloatingInvalidTypeError,
            };

            if ('_' != self.previous) return error.FloatingSymbolError;

            switch (self.next()) {
                '0'...'9' => {},
                '-' => switch (self.next()) {
                    '0'...'9' => {},
                    else => return error.FloatingNegativeError,
                },
                else => return error.FloatingDigitError,
            }

            scope: {
                while (true) {
                    switch (self.next()) {
                        '0'...'9' => continue,
                        '.' => switch (self.next()) {
                            '0'...'9' => break,
                            else => return error.FloatingDecimalError,
                        },
                        else => break :scope,
                    }
                }

                while (true) {
                    switch (self.next()) {
                        '0'...'9' => continue,
                        else => break,
                    }
                }
            }

            return token.Token.init(token_type);
        }
    };
}

//
// Test Utils
//

const test_allocator = std.testing.allocator;

fn test_lex(buffer: anytype) ![]token.Token {
    var stream = std.io.fixedBufferStream(buffer);

    var reader = stream.reader();

    var tokens = std.ArrayList(token.Token).init(test_allocator);
    defer tokens.deinit();

    var lexer = Lexer(@TypeOf(reader), @TypeOf(tokens)).init(&reader, &tokens);
    try lexer.lex();

    return try tokens.toOwnedSlice();
}

fn assert_lex(buffer: anytype, expected: []const token.TokenType) !void {
    const tokens = try test_lex(buffer);
    defer test_allocator.free(tokens);

    try assert_token_types(expected, tokens);
}

fn assert_token_types(types: []const token.TokenType, tokens: []const token.Token) !void {
    try std.testing.expectEqual(types.len, tokens.len);

    for (0..tokens.len) |i| {
        try std.testing.expectEqual(types[i], tokens[i].token_type);
    }
}

//
// Valid Tests
//

test "comment" {
    // Arrange
    const file = "\n# Comment\n # Comment!\r\n\t#Comment";
    const expected = [_]token.TokenType{ .module_start, .module_end };

    // Act + Assert
    try assert_lex(file, &expected);
}

test "global_identifier" {
    // Arrange
    const file = "$global# Comment";
    const expected = [_]token.TokenType{ .module_start, .global_identifier, .module_end };

    // Act + Assert
    try assert_lex(file, &expected);
}

test "local_identifier" {
    // Arrange
    const file = "%local# Comment";
    const expected = [_]token.TokenType{ .module_start, .temporary_identifier, .module_end };

    // Act + Assert
    try assert_lex(file, &expected);
}

test "label" {
    // Arrange
    const file = "@label# Comment";
    const expected = [_]token.TokenType{ .module_start, .label_identifier, .module_end };

    // Act + Assert
    try assert_lex(file, &expected);
}

test "reserved" {
    // Arrange
    const file = "alloc4 data function s";
    const expected = [_]token.TokenType{ .module_start, .allocate4, .data, .function, .single, .module_end };

    // Act + Assert
    try assert_lex(file, &expected);
}

test "string_literal" {
    // Arrange
    const file = "\"string\" \"escape\\\"\" \"escape\\\"again\"";
    const expected = [_]token.TokenType{ .module_start, .string_literal, .string_literal, .string_literal, .module_end };

    // Act + Assert
    try assert_lex(file, &expected);
}

test "single_literal" {
    // Arrange
    const file = "s_123 s_-1.2";
    const expected = [_]token.TokenType{ .module_start, .single_literal, .single_literal, .module_end };

    // Act + Assert
    try assert_lex(file, &expected);
}

test "double_literal" {
    // Arrange
    const file = "d_-2.4";
    const expected = [_]token.TokenType{ .module_start, .double_literal, .module_end };

    // Act + Assert
    try assert_lex(file, &expected);
}

test "integer_literal" {
    // Arrange
    const file = "-1 0 123";
    const expected = [_]token.TokenType{ .module_start, .integer_literal, .integer_literal, .integer_literal, .module_end };

    // Act + Assert
    try assert_lex(file, &expected);
}

test "assign" {
    // Arrange
    const file = "=w =s";
    const expected = [_]token.TokenType{ .module_start, .word_assign, .single_assign, .module_end };

    // Act + Assert
    try assert_lex(file, &expected);
}

test "variable_arguments" {
    // Arrange
    const file = "...";
    const expected = [_]token.TokenType{ .module_start, .variable_arguments, .module_end };

    // Act + Assert
    try assert_lex(file, &expected);
}

//
// Error Tests
//

test "error.StringCloseError" {
    // Arrange
    const file = "\"not closed";
    const expected = error.StringCloseError;

    const t = [_]token.TokenType{};

    // Act
    const res = assert_lex(file, &t);

    // Assert
    try std.testing.expectError(expected, res);
}

test "error.FloatingNegativeError" {
    // Arrange
    const file = "s_- ";
    const expected = error.FloatingNegativeError;

    // Act
    const res = test_lex(file);

    // Assert
    try std.testing.expectError(expected, res);
}

test "error.FloatingDecimalError" {
    // Arrange
    const file = "d_0. ";
    const expected = error.FloatingDecimalError;

    // Act
    const res = test_lex(file);

    // Assert
    try std.testing.expectError(expected, res);
}

test "error.FloatingDigitError" {
    // Arrange
    const file = "s_ ";
    const expected = error.FloatingDigitError;

    // Act
    const res = test_lex(file);

    // Assert
    try std.testing.expectError(expected, res);
}

test "error.AssignInvalidType" {
    // Arrange
    const file = "=| ";
    const expected = error.AssignInvalidType;

    // Act
    const res = test_lex(file);

    // Assert
    try std.testing.expectError(expected, res);
}

test "error.ArgumentSpreadError" {
    // Arrange
    const file = ".. ";
    const expected = error.ArgumentSpreadError;

    // Act
    const res = test_lex(file);

    // Assert
    try std.testing.expectError(expected, res);
}
