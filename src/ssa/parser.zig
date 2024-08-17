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

        pub fn parse(self: *Self) !void {
            _ = self.reader_readToken();

            _ = try self.module();
        }

        fn module(self: *Self) !ast.StatementIndex {
            if (self.previous.token_type != .module_start) return error.ParseModuleStartTokenError;

            _ = self.reader_readToken();

            var functions: ?ast.StatementIndex = undefined;

            const start = self.previous.span.start;
            while (true) {
                switch (self.previous.token_type) {
                    .function, .@"export" => {
                        const fn_start = self.previous.span.start;
                        const next_fn = try self.function();
                        const fn_end = self.previous.span.end;

                        functions = try self.collection_append(ast.Statement.init(
                            .{ .start = fn_start, .end = fn_end },
                            .{ .node = .{
                                .value = next_fn,
                                .next = functions,
                            } },
                        ));
                    },
                    .module_end => break,
                    else => return error.ParseModuleTokenError,
                }
            }
            const end = self.previous.span.end;

            return try self.collection_append(ast.Statement.init(
                .{ .start = start, .end = end },
                .{ .module = .{
                    .functions = functions,
                } },
            ));
        }

        fn localIdentifier(self: *Self) !ast.StatementIndex {
            if (self.previous.token_type != .local_identifier) return error.ParseLocalIdentifierError;

            const start = self.previous.span.start + 1;
            const end = self.previous.span.end;

            _ = self.reader_readToken();

            return try self.collection_append(ast.Statement.init(
                .{ .start = start, .end = end },
                .{ .identifier = .{ .scope = .local } },
            ));
        }

        fn globalIdentifier(self: *Self) !ast.StatementIndex {
            if (self.previous.token_type != .global_identifier) return error.ParseGlobalIdentifierError;

            const start = self.previous.span.start + 1;
            const end = self.previous.span.end;

            _ = self.reader_readToken();

            return try self.collection_append(ast.Statement.init(
                .{ .start = start, .end = end },
                .{ .identifier = .{ .scope = .global } },
            ));
        }

        fn typeIdentifier(self: *Self) !ast.StatementIndex {
            if (self.previous.token_type != .type_identifier) return error.ParseTypeIdentifierError;

            const start = self.previous.span.start + 1;
            const end = self.previous.span.end;

            _ = self.reader_readToken();

            return try self.collection_append(ast.Statement.init(
                .{ .start = start, .end = end },
                .{ .identifier = .{ .scope = .type } },
            ));
        }

        fn block(self: *Self) !?ast.StatementIndex {
            if (self.previous.token_type != .open_curly_brace) return error.ParseBlockStartTokenError;

            _ = self.reader_readToken();

            const lines: ?ast.StatementIndex = 0;

            while (true) {
                switch (self.previous.token_type) {
                    .close_curly_brace => break,
                    else => return error.TODO,
                }
            }

            _ = self.reader_readToken();

            return lines;
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
                else => return error.ParsePrimitiveTypeError,
            };

            const start = self.previous.span.start;
            const end = self.previous.span.end;

            _ = self.reader_readToken();

            return try self.collection_append(ast.Statement.init(
                .{ .start = start, .end = end },
                .{ .primitive_type = primitive_type },
            ));
        }

        fn variableType(self: *Self) !ast.StatementIndex {
            return switch (self.previous.token_type) {
                .type_identifier => self.typeIdentifier(),
                else => self.primitiveType(),
            };
        }

        fn envType(self: *Self) !ast.StatementIndex {
            if (self.previous.token_type != .env) return error.ParseEnvTypeError;

            const start = self.previous.span.start;
            const end = self.previous.span.end;

            _ = self.reader_readToken();

            return try self.collection_append(ast.Statement.init(
                .{ .start = start, .end = end },
                .{ .env_type = undefined },
            ));
        }

        fn function(self: *Self) !ast.StatementIndex {
            const start = self.previous.span.start;

            const function_signature = try self.functionSignature();
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

        fn functionSignature(self: *Self) !ast.StatementIndex {
            const start = self.previous.span.start;

            var @"export" = false;
            switch (self.previous.token_type) {
                .@"export" => {
                    @"export" = true;

                    switch (self.reader_readToken().token_type) {
                        .function => {},
                        else => return error.ParseFunctionStartTokenError,
                    }
                },
                .function => {},
                else => return error.ParseFunctionStartTokenError,
            }

            var return_type: ?ast.StatementIndex = undefined;
            switch (self.reader_readToken().token_type) {
                .global_identifier => {},
                else => return_type = try self.primitiveType(),
            }

            const name = try self.globalIdentifier();

            const parameters = try self.functionParameters();

            const end = self.previous.span.start;

            return try self.collection_append(ast.Statement.init(
                .{ .start = start, .end = end },
                .{ .function_signature = .{
                    .name = name,
                    .return_type = return_type,
                    .@"export" = @"export",
                    .parameters = parameters,
                } },
            ));
        }

        fn functionParameters(self: *Self) !?ast.StatementIndex {
            if (self.previous.token_type != .open_parenthesis) return error.ParseParameterStartError;

            _ = self.reader_readToken();

            var parameters: ?ast.StatementIndex = undefined;

            switch (self.previous.token_type) {
                .close_parenthesis => {},
                .env => {
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

            while (true) {
                switch (self.previous.token_type) {
                    .close_parenthesis => break,
                    .comma => {
                        switch (self.reader_readToken().token_type) {
                            .variable_arguments => {
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
                    },
                    else => return error.ParserParameterUnexpectedTokenError,
                }
            }

            switch (self.previous.token_type) {
                .close_parenthesis => {},
                else => return error.ParserParameterCloseError,
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
    try parser.parse();

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

test "function with export" {
    // Arrange
    const file = "export function $fun() {}";
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

test "function with return type" {
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

test "function error.ParsePrimitiveTypeError" {
    // Arrange
    const file = "function ";
    const expected = error.ParsePrimitiveTypeError;

    // Act
    const res = testParser(file);

    // Assert
    try std.testing.expectError(expected, res);
}

test "function error.ParsePrimitiveTypeError 2" {
    // Arrange
    const file = "function @fun() {}";
    const expected = error.ParsePrimitiveTypeError;

    // Act
    const res = testParser(file);

    // Assert
    try std.testing.expectError(expected, res);
}

test "function error.ParsePrimitiveTypeError 3" {
    // Arrange
    const file = "function $fun(w %p, ) {}";
    const expected = error.ParsePrimitiveTypeError;

    // Act
    const res = testParser(file);

    // Assert
    try std.testing.expectError(expected, res);
}

test "function error.ParsePrimitiveTypeError 4" {
    // Arrange
    const file = "function $fun(...) {}";
    const expected = error.ParsePrimitiveTypeError;

    // Act
    const res = testParser(file);

    // Assert
    try std.testing.expectError(expected, res);
}

test "function error.ParseLocalIdentifierError" {
    // Arrange
    const file = "function $fun(w) {}";
    const expected = error.ParseLocalIdentifierError;

    // Act
    const res = testParser(file);

    // Assert
    try std.testing.expectError(expected, res);
}

test "function error.ParseLocalIdentifierError 2" {
    // Arrange
    const file = "function $fun(w @a) {}";
    const expected = error.ParseLocalIdentifierError;

    // Act
    const res = testParser(file);

    // Assert
    try std.testing.expectError(expected, res);
}
