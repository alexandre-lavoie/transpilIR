const std = @import("std");

const token = @import("token.zig");

pub fn Lexer(comptime Reader: type, comptime Collection: type) type {
    return struct {
        reader: *Reader,
        collection: *Collection,
        offset: usize = 0,
        previous: u8 = 0,

        const Self = @This();

        fn reader_readByte(self: *Self) u8 {
            self.offset += 1;

            self.previous = self.reader.readByte() catch 0;
            return self.previous;
        }

        fn collection_append(self: *Self, next_token: token.Token) !void {
            try self.collection.append(next_token);
        }

        pub fn init(reader: *Reader, collection: *Collection) Self {
            return Self{
                .reader = reader,
                .collection = collection,
            };
        }

        pub fn lex(self: *Self) !void {
            try self.collection_append(token.Token.init(.module_start));

            _ = self.reader_readByte();

            while (true) {
                const start = self.offset - 1;

                var next_token: token.Token = try switch (self.previous) {
                    '\x00' => token.Token.init(.module_end),
                    '#' => {
                        try self.comment();
                        continue;
                    },
                    '$' => self.globalIdentifier(),
                    '%' => self.temporaryIdentifier(),
                    '@' => self.labelIdentifier(),
                    ':' => self.typeIdentifier(),
                    '"' => self.stringLiteral(),
                    ',' => self.punctuation(.comma),
                    '(' => self.punctuation(.open_parenthesis),
                    ')' => self.punctuation(.close_parenthesis),
                    '{' => self.punctuation(.open_curly_brace),
                    '}' => self.punctuation(.close_curly_brace),
                    '+' => self.punctuation(.plus),
                    '=' => self.punctuation(.assign),
                    '.' => self.variableArguments(),
                    'a'...'z' => self.reservedWordOrFloatingLiteral(),
                    '-', '0'...'9' => self.integerLiteral(),
                    ' ', '\t', '\n', '\r' => {
                        _ = self.reader_readByte();
                        continue;
                    },
                    else => {
                        return error.InvalidSymbol;
                    },
                };

                const end = self.offset - 1;

                next_token.span.start = start;
                next_token.span.end = end;

                try self.collection_append(next_token);

                if (next_token.token_type == .module_end) break;
            }
        }

        fn comment(self: *Self) !void {
            if (self.previous != '#') return error.InvalidComment;

            while (true) {
                switch (self.reader_readByte()) {
                    '\x00', '\r', '\n' => break,
                    else => continue,
                }
            }
        }

        fn identifier(self: *Self, previous: u8, token_type: token.TokenType) !token.Token {
            if (previous != self.previous) return error.InvalidIdentifier;

            while (true) {
                switch (self.reader_readByte()) {
                    '0'...'9', 'A'...'Z', 'a'...'z', '_', '.' => continue,
                    else => break,
                }
            }

            return token.Token.init(token_type);
        }

        fn globalIdentifier(self: *Self) !token.Token {
            return self.identifier('$', .global_identifier);
        }

        fn temporaryIdentifier(self: *Self) !token.Token {
            return self.identifier('%', .local_identifier);
        }

        fn labelIdentifier(self: *Self) !token.Token {
            return self.identifier('@', .label_identifier);
        }

        fn typeIdentifier(self: *Self) !token.Token {
            return self.identifier(':', .type_identifier);
        }

        fn stringLiteral(self: *Self) !token.Token {
            if ('"' != self.previous) return error.InvalidString;

            while (true) {
                switch (self.reader_readByte()) {
                    '"' => {
                        _ = self.reader_readByte();

                        return token.Token.init(.string_literal);
                    },
                    '\x00' => return error.StringNotClosed,
                    '\\' => {
                        _ = self.reader_readByte();
                        continue;
                    },
                    else => continue,
                }
            }
        }

        fn integerLiteral(self: *Self) !token.Token {
            switch (self.previous) {
                '0'...'9' => {},
                '-' => switch (self.reader_readByte()) {
                    '0'...'9' => {},
                    else => return error.InvalidNegativeDigit,
                },
                else => return error.InvalidDigit,
            }

            while (true) {
                switch (self.reader_readByte()) {
                    '0'...'9' => continue,
                    else => break,
                }
            }

            return token.Token.init(.integer_literal);
        }

        fn punctuation(self: *Self, token_type: token.TokenType) !token.Token {
            _ = self.reader_readByte();

            return token.Token.init(token_type);
        }

        fn variableArguments(self: *Self) !token.Token {
            if (self.previous != '.') return error.InvalidArgumentSpread;
            if (self.reader_readByte() != '.') return error.InvalidArgumentSpread;
            if (self.reader_readByte() != '.') return error.InvalidArgumentSpread;

            _ = self.reader_readByte();

            return token.Token.init(.variable_arguments);
        }

        fn reservedWordOrFloatingLiteral(self: *Self) !token.Token {
            var buffer: [token.longest_reserved_word + 1]u8 = undefined;

            buffer[0] = self.previous;

            var i: usize = 1;
            while (i < token.longest_reserved_word) : (i += 1) {
                const next_char = self.reader_readByte();

                if (i == 1 and next_char == '_' and (buffer[0] == 'd' or buffer[0] == 's')) {
                    return self.floating_literal(buffer[0]);
                }

                switch (next_char) {
                    'a'...'z' => buffer[i] = next_char,
                    else => break,
                }
            }

            if (token.reserved_words.get(buffer[0..i])) |token_type| {
                return token.Token.init(token_type);
            } else {
                return error.ReservedWordError;
            }
        }

        fn floating_literal(self: *Self, ftype: u8) !token.Token {
            const token_type: token.TokenType = switch (ftype) {
                'd' => .double_literal,
                's' => .single_literal,
                else => return error.InvalidFloatType,
            };

            if ('_' != self.previous) return error.InvalidFloatType;

            switch (self.reader_readByte()) {
                '0'...'9' => {},
                '-' => switch (self.reader_readByte()) {
                    '0'...'9' => {},
                    else => return error.InvalidNegativeDigit,
                },
                else => return error.InvalidDigit,
            }

            scope: {
                while (true) {
                    switch (self.reader_readByte()) {
                        '0'...'9' => continue,
                        '.' => switch (self.reader_readByte()) {
                            '0'...'9' => break,
                            else => return error.InvalidDecimal,
                        },
                        else => break :scope,
                    }
                }

                while (true) {
                    switch (self.reader_readByte()) {
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

fn testLex(buffer: anytype) ![]token.Token {
    var stream = std.io.fixedBufferStream(buffer);

    var reader = stream.reader();

    var tokens = std.ArrayList(token.Token).init(test_allocator);
    defer tokens.deinit();

    var lexer = Lexer(@TypeOf(reader), @TypeOf(tokens)).init(&reader, &tokens);
    try lexer.lex();

    return try tokens.toOwnedSlice();
}

fn assertLex(buffer: anytype, expected: []const token.Token) !void {
    const tokens = try testLex(buffer);
    defer test_allocator.free(tokens);

    try std.testing.expectEqualSlices(token.Token, expected, tokens);
}

//
// Valid Tests
//

test "comment" {
    // Arrange
    const file = "\n# Comment\n # Comment!\r\n\t#Comment";
    const expected = [_]token.Token{
        .{
            .token_type = .module_start,
            .span = .{ .start = 0, .end = 0 },
        },
        .{
            .token_type = .module_end,
            .span = .{ .start = file.len, .end = file.len },
        },
    };

    // Act + Assert
    try assertLex(file, &expected);
}

test "globalIdentifier" {
    // Arrange
    const file = "$global# Comment";
    const expected = [_]token.Token{
        .{
            .token_type = .module_start,
            .span = .{ .start = 0, .end = 0 },
        },
        .{
            .token_type = .global_identifier,
            .span = .{ .start = 0, .end = 7 },
        },
        .{
            .token_type = .module_end,
            .span = .{ .start = file.len, .end = file.len },
        },
    };

    // Act + Assert
    try assertLex(file, &expected);
}

test "localIdentifier" {
    // Arrange
    const file = "%local# Comment";
    const expected = [_]token.Token{
        .{
            .token_type = .module_start,
            .span = .{ .start = 0, .end = 0 },
        },
        .{
            .token_type = .local_identifier,
            .span = .{ .start = 0, .end = 6 },
        },
        .{
            .token_type = .module_end,
            .span = .{ .start = file.len, .end = file.len },
        },
    };

    // Act + Assert
    try assertLex(file, &expected);
}

test "label_identifier" {
    // Arrange
    const file = "@label# Comment";
    const expected = [_]token.Token{
        .{
            .token_type = .module_start,
            .span = .{ .start = 0, .end = 0 },
        },
        .{
            .token_type = .label_identifier,
            .span = .{ .start = 0, .end = 6 },
        },
        .{
            .token_type = .module_end,
            .span = .{ .start = file.len, .end = file.len },
        },
    };

    // Act + Assert
    try assertLex(file, &expected);
}

test "reserved" {
    // Arrange
    const file = "alloc4 data function s";
    const expected = [_]token.Token{
        .{
            .token_type = .module_start,
            .span = .{ .start = 0, .end = 0 },
        },
        .{
            .token_type = .allocate,
            .span = .{ .start = 0, .end = 5 },
        },
        .{
            .token_type = .integer_literal,
            .span = .{ .start = 5, .end = 6 },
        },
        .{
            .token_type = .data,
            .span = .{ .start = 7, .end = 11 },
        },
        .{
            .token_type = .function,
            .span = .{ .start = 12, .end = 20 },
        },
        .{
            .token_type = .single,
            .span = .{ .start = 21, .end = 22 },
        },
        .{
            .token_type = .module_end,
            .span = .{ .start = file.len, .end = file.len },
        },
    };

    // Act + Assert
    try assertLex(file, &expected);
}

test "stringLiteral" {
    // Arrange
    const file = "\"string\" \"escape\\\"\" \"escape\\\"again\"";
    const expected = [_]token.Token{
        .{
            .token_type = .module_start,
            .span = .{ .start = 0, .end = 0 },
        },
        .{
            .token_type = .string_literal,
            .span = .{ .start = 0, .end = 8 },
        },
        .{
            .token_type = .string_literal,
            .span = .{ .start = 9, .end = 19 },
        },
        .{
            .token_type = .string_literal,
            .span = .{ .start = 20, .end = 35 },
        },
        .{
            .token_type = .module_end,
            .span = .{ .start = file.len, .end = file.len },
        },
    };

    // Act + Assert
    try assertLex(file, &expected);
}

test "singleLiteral" {
    // Arrange
    const file = "s_123 s_-1.2";
    const expected = [_]token.Token{
        .{
            .token_type = .module_start,
            .span = .{ .start = 0, .end = 0 },
        },
        .{
            .token_type = .single_literal,
            .span = .{ .start = 0, .end = 5 },
        },
        .{
            .token_type = .single_literal,
            .span = .{ .start = 6, .end = 12 },
        },
        .{
            .token_type = .module_end,
            .span = .{ .start = file.len, .end = file.len },
        },
    };

    // Act + Assert
    try assertLex(file, &expected);
}

test "doubleLiteral" {
    // Arrange
    const file = "d_-2.4";
    const expected = [_]token.Token{
        .{
            .token_type = .module_start,
            .span = .{ .start = 0, .end = 0 },
        },
        .{
            .token_type = .double_literal,
            .span = .{ .start = 0, .end = 6 },
        },
        .{
            .token_type = .module_end,
            .span = .{ .start = file.len, .end = file.len },
        },
    };

    // Act + Assert
    try assertLex(file, &expected);
}

test "integerLiteral" {
    // Arrange
    const file = "-1 0 123";
    const expected = [_]token.Token{
        .{
            .token_type = .module_start,
            .span = .{ .start = 0, .end = 0 },
        },
        .{
            .token_type = .integer_literal,
            .span = .{ .start = 0, .end = 2 },
        },
        .{
            .token_type = .integer_literal,
            .span = .{ .start = 3, .end = 4 },
        },
        .{
            .token_type = .integer_literal,
            .span = .{ .start = 5, .end = 8 },
        },
        .{
            .token_type = .module_end,
            .span = .{ .start = file.len, .end = file.len },
        },
    };

    // Act + Assert
    try assertLex(file, &expected);
}

test "assign" {
    // Arrange
    const file = "=w =s =";
    const expected = [_]token.Token{
        .{
            .token_type = .module_start,
            .span = .{ .start = 0, .end = 0 },
        },
        .{
            .token_type = .assign,
            .span = .{ .start = 0, .end = 1 },
        },
        .{
            .token_type = .word,
            .span = .{ .start = 1, .end = 2 },
        },
        .{
            .token_type = .assign,
            .span = .{ .start = 3, .end = 4 },
        },
        .{
            .token_type = .single,
            .span = .{ .start = 4, .end = 5 },
        },
        .{
            .token_type = .assign,
            .span = .{ .start = 6, .end = 7 },
        },
        .{
            .token_type = .module_end,
            .span = .{ .start = file.len, .end = file.len },
        },
    };

    // Act + Assert
    try assertLex(file, &expected);
}

test "variableArguments" {
    // Arrange
    const file = "...";
    const expected = [_]token.Token{
        .{
            .token_type = .module_start,
            .span = .{ .start = 0, .end = 0 },
        },
        .{
            .token_type = .variable_arguments,
            .span = .{ .start = 0, .end = 3 },
        },
        .{
            .token_type = .module_end,
            .span = .{ .start = file.len, .end = file.len },
        },
    };

    // Act + Assert
    try assertLex(file, &expected);
}

//
// Error Tests
//

test "error.StringNotClosed" {
    // Arrange
    const file = "\"not closed";

    // Act
    const res = testLex(file);

    // Assert
    try std.testing.expectError(error.StringNotClosed, res);
}

test "error.InvalidNegativeDigit" {
    // Arrange
    const file = "s_- ";

    // Act
    const res = testLex(file);

    // Assert
    try std.testing.expectError(error.InvalidNegativeDigit, res);
}

test "error.InvalidDecimal" {
    // Arrange
    const file = "d_0. ";

    // Act
    const res = testLex(file);

    // Assert
    try std.testing.expectError(error.InvalidDecimal, res);
}

test "error.InvalidDigit" {
    // Arrange
    const file = "s_ ";

    // Act
    const res = testLex(file);

    // Assert
    try std.testing.expectError(error.InvalidDigit, res);
}

test "error.InvalidArgumentSpread" {
    // Arrange
    const file = ".. ";

    // Act
    const res = testLex(file);

    // Assert
    try std.testing.expectError(error.InvalidArgumentSpread, res);
}
