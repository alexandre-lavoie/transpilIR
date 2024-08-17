const std = @import("std");

const ast = @import("../ast/lib.zig");
const lexer = @import("lexer.zig");
const token = @import("token.zig");

const ParseRecord = struct {
    index: usize,
    statement: *ast.Statement,
};

pub fn Parser(comptime Reader: type, comptime Collection: type) type {
    return struct {
        reader: *Reader,
        collection: *Collection,
        offset: ast.StatementIndex = 0,
        previous: *const token.Token = undefined,

        const Self = @This();

        fn reader_readToken(self: *Self) *const token.Token {
            self.previous = self.reader.readToken();
            return self.previous;
        }

        fn collection_append(self: *Self, next_statement: ast.Statement) !ast.StatementIndex {
            try self.collection.append(next_statement);

            const index = self.offset;
            self.offset += 1;

            return index;
        }

        pub fn init(reader: *Reader, collection: *Collection) Self {
            return Self{
                .reader = reader,
                .collection = collection,
            };
        }

        pub fn parse(self: *Self) !usize {
            _ = self.reader_readToken();

            return try self.module();
        }

        fn module(self: *Self) !ast.StatementIndex {
            if (self.previous.token_type != .module_start) return error.ParseMissingModule;

            _ = self.reader_readToken();

            var function_definitions: ?ast.StatementIndex = undefined;
            var data_definitions: ?ast.StatementIndex = undefined;
            const type_definitions: ?ast.StatementIndex = undefined;

            const start = self.previous.span.start;
            while (true) {
                switch (self.previous.token_type) {
                    .@"export", .thread, .section, .function, .data => {
                        const def_start = self.previous.span.start;
                        const out = try self.linkageDefinition();
                        const def_end = self.previous.span.end;

                        switch (out.token_type) {
                            .function => {
                                function_definitions = try self.collection_append(ast.Statement.init(
                                    .{ .start = def_start, .end = def_end },
                                    .{ .node = .{
                                        .value = out.statement,
                                        .next = function_definitions,
                                    } },
                                ));
                            },
                            .data => {
                                data_definitions = try self.collection_append(ast.Statement.init(
                                    .{ .start = def_start, .end = def_end },
                                    .{ .node = .{
                                        .value = out.statement,
                                        .next = data_definitions,
                                    } },
                                ));
                            },
                            else => return error.ParseUnexpectedType,
                        }
                    },
                    .module_end => break,
                    else => return error.ParseModuleInvalidToken,
                }
            }
            const end = self.previous.span.end;

            return try self.collection_append(ast.Statement.init(
                .{ .start = start, .end = end },
                .{ .module = .{
                    .functions = function_definitions,
                    .data = data_definitions,
                    .types = type_definitions,
                } },
            ));
        }

        fn scopeIdentifier(self: *Self, token_type: token.TokenType, scope: ast.Scope) !ast.StatementIndex {
            if (self.previous.token_type != token_type) return error.ParseInvalidIdentifier;

            const start = self.previous.span.start + 1;
            const end = self.previous.span.end;

            _ = self.reader_readToken();

            return try self.collection_append(ast.Statement.init(
                .{ .start = start, .end = end },
                .{ .identifier = .{ .scope = scope } },
            ));
        }

        fn localIdentifier(self: *Self) !ast.StatementIndex {
            return self.scopeIdentifier(.local_identifier, .local);
        }

        fn globalIdentifier(self: *Self) !ast.StatementIndex {
            return self.scopeIdentifier(.global_identifier, .global);
        }

        fn typeIdentifier(self: *Self) !ast.StatementIndex {
            return self.scopeIdentifier(.type_identifier, .type);
        }

        fn labelIdentifier(self: *Self) !ast.StatementIndex {
            return self.scopeIdentifier(.label_identifier, .label);
        }

        fn block(self: *Self) !?ast.StatementIndex {
            if (self.previous.token_type != .open_curly_brace) return error.ParseMissingCurlyBrace;

            _ = self.reader_readToken();

            const lines: ?ast.StatementIndex = 0;

            while (true) {
                switch (self.previous.token_type) {
                    .close_curly_brace => break,
                    // TODO: support for block lines.
                    else => return error.TODO,
                }
            }

            _ = self.reader_readToken();

            return lines;
        }

        fn stackAlignment(self: *Self) !ast.StatementIndex {
            if (self.previous.token_type != .@"align") return error.ParseMissingAlign;

            _ = self.reader_readToken();

            return try self.integer();
        }

        fn primitiveType(self: *Self) !ast.StatementIndex {
            const primitive_type: ast.PrimitiveType = switch (self.previous.token_type) {
                .byte_unsigned => .byte_unsigned,
                .byte => .byte,
                .double => .double,
                .half_word_unsigned => .half_word_unsigned,
                .half_word => .half_word,
                .long => .long,
                .single => .single,
                .word_unsigned => .word_unsigned,
                .word => .word,
                else => return error.ParseInvalidPrimitiveType,
            };

            const start = self.previous.span.start;
            const end = self.previous.span.end;

            _ = self.reader_readToken();

            return try self.collection_append(ast.Statement.init(
                .{ .start = start, .end = end },
                .{ .primitive_type = primitive_type },
            ));
        }

        fn zeroType(self: *Self) !ast.StatementIndex {
            if (self.previous.token_type != .zero) return error.ParseMissingZero;

            const span = self.previous.span;
            _ = self.reader_readToken();

            return try self.collection_append(ast.Statement.init(
                span,
                .{ .zero_type = undefined },
            ));
        }

        fn variableType(self: *Self) !ast.StatementIndex {
            return switch (self.previous.token_type) {
                .type_identifier => self.typeIdentifier(),
                else => self.primitiveType(),
            };
        }

        fn envType(self: *Self) !ast.StatementIndex {
            if (self.previous.token_type != .env) return error.ParseMissingEnv;

            const span = self.previous.span;
            _ = self.reader_readToken();

            return try self.collection_append(ast.Statement.init(
                span,
                .{ .env_type = undefined },
            ));
        }

        fn integer(self: *Self) !ast.StatementIndex {
            if (self.previous.token_type != .integer_literal) return error.ParseMissingIntegerLiteral;

            const span = self.previous.span;
            _ = self.reader_readToken();

            return try self.collection_append(ast.Statement.init(
                span,
                .{ .literal = .{
                    .type = .integer,
                } },
            ));
        }

        fn single(self: *Self) !ast.StatementIndex {
            if (self.previous.token_type != .single_literal) return error.ParseMissingSingleLiteral;

            const span = self.previous.span;
            _ = self.reader_readToken();

            return try self.collection_append(ast.Statement.init(
                .{ .start = span.start + 2, .end = span.end },
                .{ .literal = .{
                    .type = .float,
                } },
            ));
        }

        fn double(self: *Self) !ast.StatementIndex {
            if (self.previous.token_type != .double_literal) return error.ParseMissingDoubleLiteral;

            const span = self.previous.span;
            _ = self.reader_readToken();

            return try self.collection_append(ast.Statement.init(
                .{ .start = span.start + 2, .end = span.end },
                .{ .literal = .{
                    .type = .float,
                } },
            ));
        }

        fn string(self: *Self) !ast.StatementIndex {
            if (self.previous.token_type != .string_literal) return error.ParseMissingStringLiteral;

            const span = self.previous.span;
            _ = self.reader_readToken();

            return try self.collection_append(ast.Statement.init(
                .{ .start = span.start + 1, .end = span.end - 1 },
                .{ .literal = .{
                    .type = .float,
                } },
            ));
        }

        fn linkage(self: *Self) !?ast.StatementIndex {
            const start = self.previous.span.start;

            var t = self.previous;

            var @"export" = false;
            if (t.token_type == .@"export") {
                @"export" = true;
                t = self.reader_readToken();
            }

            var thread = false;
            if (t.token_type == .thread) {
                thread = true;
                t = self.reader_readToken();
            }

            var section: ?ast.StatementIndex = undefined;
            var flags: ?ast.StatementIndex = undefined;
            if (t.token_type == .section) {
                _ = self.reader_readToken();

                section = try self.string();

                if (self.previous.token_type == .string_literal) flags = try self.string();
            }

            const end = self.previous.span.start;

            if (start == end) return undefined;

            return try self.collection_append(ast.Statement.init(
                .{ .start = start, .end = end },
                .{ .linkage = .{
                    .@"export" = @"export",
                    .thread = thread,
                    .section = section,
                    .flags = flags,
                } },
            ));
        }

        fn linkageDefinition(self: *Self) !struct { token_type: token.TokenType, statement: ast.StatementIndex } {
            const start = self.previous.span.start;

            const link = try self.linkage();

            return switch (self.previous.token_type) {
                .data => .{
                    .token_type = .data,
                    .statement = try self.dataDefinition(start, link),
                },
                .function => .{
                    .token_type = .function,
                    .statement = try self.functionDefinition(start, link),
                },
                else => return error.ParseInvalidLinkageType,
            };
        }

        fn dataDefinition(self: *Self, start: usize, link: ?ast.StatementIndex) !ast.StatementIndex {
            if (self.previous.token_type != .data) return error.ParseInvalidData;

            _ = self.reader_readToken();

            const identifier = try self.globalIdentifier();

            if (self.previous.token_type != .assign) return error.ParseMissingEqual;

            _ = self.reader_readToken();

            var alignment: ?ast.StatementIndex = undefined;
            switch (self.previous.token_type) {
                .@"align" => alignment = try self.stackAlignment(),
                else => {},
            }

            if (self.previous.token_type != .open_curly_brace) return error.ParseMissingCurlyBrace;

            _ = self.reader_readToken();

            var values: ?ast.StatementIndex = undefined;

            var first = true;
            while (self.previous.token_type != .close_curly_brace) {
                if (!first) {
                    if (self.previous.token_type != .comma) return error.ParseMissingComma;
                    _ = self.reader_readToken();
                }

                const value_type = try switch (self.previous.token_type) {
                    .zero => self.zeroType(),
                    else => self.primitiveType(),
                };

                while (true) {
                    switch (self.previous.token_type) {
                        .comma, .close_curly_brace => break,
                        else => {
                            const start_value = self.previous.span.start;
                            const value = try self.dataValue();
                            const end_value = self.previous.span.end;

                            const type_value = try self.collection_append(ast.Statement.init(
                                .{ .start = start_value, .end = end_value },
                                .{ .typed_data = .{
                                    .type = value_type,
                                    .value = value,
                                } },
                            ));

                            values = try self.collection_append(ast.Statement.init(
                                .{ .start = start_value, .end = end_value },
                                .{ .node = .{
                                    .value = type_value,
                                    .next = values,
                                } },
                            ));
                        },
                    }
                }

                first = false;
            }

            _ = self.reader_readToken();

            const end = self.previous.span.start;

            return try self.collection_append(ast.Statement.init(
                .{ .start = start, .end = end },
                .{ .data_definition = .{
                    .linkage = link,
                    .identifier = identifier,
                    .values = values,
                } },
            ));
        }

        fn dataValue(self: *Self) !ast.StatementIndex {
            const start = self.previous.span.start;

            const canOffset = self.previous.token_type == .global_identifier;

            const left = try switch (self.previous.token_type) {
                .global_identifier => self.globalIdentifier(),
                .integer_literal => self.integer(),
                .single_literal => self.single(),
                .double_literal => self.double(),
                .string_literal => self.string(),
                else => return error.ParseInvalidDataValue,
            };

            if (self.previous.token_type == .plus) {
                if (!canOffset) return error.ParseInvalidDataOffset;
            } else {
                return left;
            }

            if (self.reader_readToken().token_type != .integer_literal) return error.ParseInvalidOffset;

            const offset = try self.integer();

            const end = self.previous.span.end;

            return try self.collection_append(ast.Statement.init(
                .{ .start = start, .end = end },
                .{ .offset = .{
                    .identifier = left,
                    .value = offset,
                } },
            ));
        }

        fn functionDefinition(self: *Self, start: usize, link: ?ast.StatementIndex) !ast.StatementIndex {
            const function_signature = try self.functionSignature(start, link);
            const function_block = try self.block();

            const end = self.previous.span.start;

            return try self.collection_append(ast.Statement.init(
                .{ .start = start, .end = end },
                .{ .function = .{
                    .signature = function_signature,
                    .block = function_block,
                } },
            ));
        }

        fn functionSignature(self: *Self, start: usize, link: ?ast.StatementIndex) !ast.StatementIndex {
            var return_type: ?ast.StatementIndex = undefined;
            switch (self.reader_readToken().token_type) {
                .global_identifier => {},
                else => return_type = try self.variableType(),
            }

            const name = try self.globalIdentifier();

            const parameters = try self.functionParameters();

            const end = self.previous.span.start;

            return try self.collection_append(ast.Statement.init(
                .{ .start = start, .end = end },
                .{ .function_signature = .{
                    .linkage = link,
                    .name = name,
                    .return_type = return_type,
                    .parameters = parameters,
                } },
            ));
        }

        fn functionParameters(self: *Self) !?ast.StatementIndex {
            if (self.previous.token_type != .open_parenthesis) return error.ParseMissingOpenParenthesis;
            _ = self.reader_readToken();

            var parameters: ?ast.StatementIndex = undefined;

            var first = true;
            while (self.previous.token_type != .close_parenthesis) {
                if (!first) {
                    if (self.previous.token_type != .comma) return error.ParseMissingCommaError;

                    _ = self.reader_readToken();
                }

                switch (self.previous.token_type) {
                    .env => {
                        if (!first) return error.ParseInvalidEnv;

                        const env_start = self.previous.span.start;
                        const env = try self.envParameter();
                        const env_end = self.previous.span.end;

                        parameters = try self.collection_append(ast.Statement.init(
                            .{ .start = env_start, .end = env_end },
                            .{ .node = .{
                                .value = env,
                                .next = parameters,
                            } },
                        ));
                    },
                    .variable_arguments => {
                        if (first) return error.ParseInvalidVarArgs;

                        const vararg = try self.collection_append(ast.Statement.init(
                            self.previous.span,
                            .{ .variadic_parameter = undefined },
                        ));

                        parameters = try self.collection_append(ast.Statement.init(
                            self.previous.span,
                            .{ .node = .{
                                .value = vararg,
                                .next = parameters,
                            } },
                        ));

                        _ = self.reader_readToken();

                        break;
                    },
                    else => {
                        const param_start = self.previous.span.start;
                        const param = try self.functionParameter();
                        const param_end = self.previous.span.end;

                        parameters = try self.collection_append(ast.Statement.init(
                            .{ .start = param_start, .end = param_end },
                            .{ .node = .{
                                .value = param,
                                .next = parameters,
                            } },
                        ));
                    },
                }

                first = false;
            }

            switch (self.previous.token_type) {
                .close_parenthesis => {},
                else => return error.ParseMissingCloseParenthesis,
            }

            _ = self.reader_readToken();

            return parameters;
        }

        fn envParameter(self: *Self) !ast.StatementIndex {
            const start = self.previous.span.start;

            const type_statement = try self.envType();
            const identifier = try self.localIdentifier();

            const end = self.previous.span.start;

            return try self.collection_append(ast.Statement.init(
                .{ .start = start, .end = end },
                .{ .function_parameter = .{
                    .type_statement = type_statement,
                    .identifier = identifier,
                } },
            ));
        }

        fn functionParameter(self: *Self) !ast.StatementIndex {
            const start = self.previous.span.start;

            const type_statement = try self.variableType();
            const identifier = try self.localIdentifier();

            const end = self.previous.span.start;

            return try self.collection_append(ast.Statement.init(
                .{ .start = start, .end = end },
                .{ .function_parameter = .{
                    .type_statement = type_statement,
                    .identifier = identifier,
                } },
            ));
        }
    };
}

//
// Test Utils
//

const test_allocator = std.testing.allocator;

fn testParser(buffer: anytype) ![]ast.Statement {
    var file_stream = std.io.fixedBufferStream(buffer);

    var file_reader = file_stream.reader();

    var tokens = std.ArrayList(token.Token).init(test_allocator);
    defer tokens.deinit();

    var lex = lexer.Lexer(@TypeOf(file_reader), @TypeOf(tokens)).init(&file_reader, &tokens);
    try lex.lex();

    const token_slice = try tokens.toOwnedSlice();
    defer test_allocator.free(token_slice);

    var token_reader = token.TokenReader(@TypeOf(token_slice)).init(token_slice);

    var statements = std.ArrayList(ast.Statement).init(test_allocator);
    defer statements.deinit();

    var parser = Parser(@TypeOf(token_reader), @TypeOf(statements)).init(&token_reader, &statements);
    _ = try parser.parse();

    return try statements.toOwnedSlice();
}

fn assertParser(buffer: anytype, expected: []const ast.StatementType) !void {
    const statements = try testParser(buffer);
    defer test_allocator.free(statements);

    try assertStatementTypes(expected, statements);
}

fn assertStatementTypes(types: []const ast.StatementType, statements: []const ast.Statement) !void {
    try std.testing.expectEqual(types.len, statements.len);

    for (0..statements.len) |i| {
        const expected: ast.StatementType = @enumFromInt(@intFromEnum(types[i]));
        const actual: ast.StatementType = @enumFromInt(@intFromEnum(statements[i].data));

        try std.testing.expectEqual(expected, actual);
    }
}

//
// Valid Test
//

test "module" {
    // Arrange
    const file = "";
    const expected = [_]ast.StatementType{
        .module,
    };

    // Act + Assert
    try assertParser(file, &expected);
}

// TODO: Types
// test "type" {
//     // Arrange
//     const file = "type :t = {w}";
//     const expected = [_]ast.StatementType{
//         .identifier,
//         .primitive_type,
//         .literal,
//         .typed_data,
//         .node,
//         .data_definition,
//         .node,
//         .module,
//     };

//     // Act + Assert
//     try assertParser(file, &expected);
// }

test "data" {
    // Arrange
    const file = "data $d = {w 1}";
    const expected = [_]ast.StatementType{
        .identifier,
        .primitive_type,
        .literal,
        .typed_data,
        .node,
        .data_definition,
        .node,
        .module,
    };

    // Act + Assert
    try assertParser(file, &expected);
}

test "data with alignment" {
    // Arrange
    const file = "data $d = align 1 {w 1}";
    const expected = [_]ast.StatementType{
        .identifier,
        .literal,
        .primitive_type,
        .literal,
        .typed_data,
        .node,
        .data_definition,
        .node,
        .module,
    };

    // Act + Assert
    try assertParser(file, &expected);
}

test "data with global" {
    // Arrange
    const file = "data $d = {l $o}";
    const expected = [_]ast.StatementType{
        .identifier,
        .primitive_type,
        .identifier,
        .typed_data,
        .node,
        .data_definition,
        .node,
        .module,
    };

    // Act + Assert
    try assertParser(file, &expected);
}

test "data with global offset" {
    // Arrange
    const file = "data $d = {l $o + 32 0}";
    const expected = [_]ast.StatementType{
        .identifier,
        .primitive_type,
        .identifier,
        .literal,
        .offset,
        .typed_data,
        .node,
        .literal,
        .typed_data,
        .node,
        .data_definition,
        .node,
        .module,
    };

    // Act + Assert
    try assertParser(file, &expected);
}

test "data with linkage" {
    // Arrange
    const file = "export thread section \"data\" \"flags\" data $d = {w 1}";
    const expected = [_]ast.StatementType{
        .literal,
        .literal,
        .linkage,
        .identifier,
        .primitive_type,
        .literal,
        .typed_data,
        .node,
        .data_definition,
        .node,
        .module,
    };

    // Act + Assert
    try assertParser(file, &expected);
}

test "data with reused type" {
    // Arrange
    const file = "data $d = {w 1 2 3}";
    const expected = [_]ast.StatementType{
        .identifier,
        .primitive_type,
        .literal,
        .typed_data,
        .node,
        .literal,
        .typed_data,
        .node,
        .literal,
        .typed_data,
        .node,
        .data_definition,
        .node,
        .module,
    };

    // Act + Assert
    try assertParser(file, &expected);
}

test "data with many types" {
    // Arrange
    const file = "data $d = {w 1, h 0, b \"test\"}";
    const expected = [_]ast.StatementType{
        .identifier,
        .primitive_type,
        .literal,
        .typed_data,
        .node,
        .primitive_type,
        .literal,
        .typed_data,
        .node,
        .primitive_type,
        .literal,
        .typed_data,
        .node,
        .data_definition,
        .node,
        .module,
    };

    // Act + Assert
    try assertParser(file, &expected);
}

test "data with zeros" {
    // Arrange
    const file = "data $d = {z 1000}";
    const expected = [_]ast.StatementType{
        .identifier,
        .zero_type,
        .literal,
        .typed_data,
        .node,
        .data_definition,
        .node,
        .module,
    };

    // Act + Assert
    try assertParser(file, &expected);
}

test "function" {
    // Arrange
    const file = "function $fun() {}";
    const expected = [_]ast.StatementType{
        .identifier,
        .function_signature,
        .function,
        .node,
        .module,
    };

    // Act + Assert
    try assertParser(file, &expected);
}

test "function with linkage" {
    // Arrange
    const file = "export thread section \"function\" \"flag\" function $fun() {}";
    const expected = [_]ast.StatementType{
        .literal,
        .literal,
        .linkage,
        .identifier,
        .function_signature,
        .function,
        .node,
        .module,
    };

    // Act + Assert
    try assertParser(file, &expected);
}

test "function with primitive return type" {
    // Arrange
    const file = "function w $fun() {}";
    const expected = [_]ast.StatementType{
        .primitive_type,
        .identifier,
        .function_signature,
        .function,
        .node,
        .module,
    };

    // Act + Assert
    try assertParser(file, &expected);
}

test "function with custom return type" {
    // Arrange
    const file = "function :type $fun() {}";
    const expected = [_]ast.StatementType{
        .identifier,
        .identifier,
        .function_signature,
        .function,
        .node,
        .module,
    };

    // Act + Assert
    try assertParser(file, &expected);
}

test "function with custom type parameter" {
    // Arrange
    const file = "function $fun(:type %p) {}";
    const expected = [_]ast.StatementType{
        .identifier,
        .identifier,
        .identifier,
        .function_parameter,
        .node,
        .function_signature,
        .function,
        .node,
        .module,
    };

    // Act + Assert
    try assertParser(file, &expected);
}

test "function with one parameter" {
    // Arrange
    const file = "function $fun(w %p) {}";
    const expected = [_]ast.StatementType{
        .identifier,
        .primitive_type,
        .identifier,
        .function_parameter,
        .node,
        .function_signature,
        .function,
        .node,
        .module,
    };

    // Act + Assert
    try assertParser(file, &expected);
}

test "function with many parameters" {
    // Arrange
    const file = "function $fun(w %p0, b %p1, h %p2) {}";
    const expected = [_]ast.StatementType{
        .identifier,
        .primitive_type,
        .identifier,
        .function_parameter,
        .node,
        .primitive_type,
        .identifier,
        .function_parameter,
        .node,
        .primitive_type,
        .identifier,
        .function_parameter,
        .node,
        .function_signature,
        .function,
        .node,
        .module,
    };

    // Act + Assert
    try assertParser(file, &expected);
}

test "function with env parameter" {
    // Arrange
    const file = "function $fun(env %e) {}";
    const expected = [_]ast.StatementType{
        .identifier,
        .env_type,
        .identifier,
        .function_parameter,
        .node,
        .function_signature,
        .function,
        .node,
        .module,
    };

    // Act + Assert
    try assertParser(file, &expected);
}

test "function with variable parameter" {
    // Arrange
    const file = "function $fun(w %fmt, ...) {}";
    const expected = [_]ast.StatementType{
        .identifier,
        .primitive_type,
        .identifier,
        .function_parameter,
        .node,
        .variadic_parameter,
        .node,
        .function_signature,
        .function,
        .node,
        .module,
    };

    // Act + Assert
    try assertParser(file, &expected);
}

//
// Error Tests
//

test "data error.ParseInvalidIdentifier" {
    // Arrange
    const file = "data";
    const expected = error.ParseInvalidIdentifier;

    // Act
    const res = testParser(file);

    // Assert
    try std.testing.expectError(expected, res);
}

test "data error.ParseInvalidIdentifier 2" {
    // Arrange
    const file = "data @d";
    const expected = error.ParseInvalidIdentifier;

    // Act
    const res = testParser(file);

    // Assert
    try std.testing.expectError(expected, res);
}

test "data error.ParseMissingEqual" {
    // Arrange
    const file = "data $d 1";
    const expected = error.ParseMissingEqual;

    // Act
    const res = testParser(file);

    // Assert
    try std.testing.expectError(expected, res);
}

test "data error.ParseInvalidPrimitiveType" {
    // Arrange
    const file = "data $d = {1}";
    const expected = error.ParseInvalidPrimitiveType;

    // Act
    const res = testParser(file);

    // Assert
    try std.testing.expectError(expected, res);
}

test "function error.ParseInvalidPrimitiveType" {
    // Arrange
    const file = "function";
    const expected = error.ParseInvalidPrimitiveType;

    // Act
    const res = testParser(file);

    // Assert
    try std.testing.expectError(expected, res);
}

test "function error.ParseInvalidPrimitiveType 2" {
    // Arrange
    const file = "function @fun() {}";
    const expected = error.ParseInvalidPrimitiveType;

    // Act
    const res = testParser(file);

    // Assert
    try std.testing.expectError(expected, res);
}

test "function error.ParseInvalidPrimitiveType 3" {
    // Arrange
    const file = "function $fun(w %p, ) {}";
    const expected = error.ParseInvalidPrimitiveType;

    // Act
    const res = testParser(file);

    // Assert
    try std.testing.expectError(expected, res);
}

test "function error.ParseInvalidVarArgs 4" {
    // Arrange
    const file = "function $fun(...) {}";
    const expected = error.ParseInvalidVarArgs;

    // Act
    const res = testParser(file);

    // Assert
    try std.testing.expectError(expected, res);
}

test "function error.ParseInvalidIdentifier" {
    // Arrange
    const file = "function $fun(w) {}";
    const expected = error.ParseInvalidIdentifier;

    // Act
    const res = testParser(file);

    // Assert
    try std.testing.expectError(expected, res);
}

test "function error.ParseInvalidIdentifier 2" {
    // Arrange
    const file = "function $fun(w @a) {}";
    const expected = error.ParseInvalidIdentifier;

    // Act
    const res = testParser(file);

    // Assert
    try std.testing.expectError(expected, res);
}
