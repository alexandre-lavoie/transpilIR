const std = @import("std");

const ast = @import("../ast/lib.zig");
const common = @import("../common.zig");
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
            return self.reader.readToken();
        }

        fn collection_append(self: *Self, next_statement: ast.Statement) !void {
            try self.collection.append(next_statement);
        }

        pub fn init(reader: *Reader, collection: *Collection) Self {
            return Self{
                .reader = reader,
                .collection = collection,
            };
        }

        pub fn parse(self: *Self) !usize {
            _ = self.next();
            return try self.module();
        }

        fn new(self: *Self, span: common.SourceSpan, data: ast.StatementData) !ast.StatementIndex {
            try self.collection_append(ast.Statement.init(
                span,
                data,
            ));

            const index = self.offset;
            self.offset += 1;

            return index;
        }

        fn next(self: *Self) *const token.Token {
            self.previous = self.reader_readToken();
            return self.previous;
        }

        fn module(self: *Self) !ast.StatementIndex {
            const start = self.previous.span.start;

            if (self.previous.token_type != .module_start) return error.ParseMissingModule;
            _ = self.next();

            var function_definitions: ?ast.StatementIndex = null;
            var data_definitions: ?ast.StatementIndex = null;
            var type_definitions: ?ast.StatementIndex = null;

            while (true) {
                var token_type: token.TokenType = .type;

                const def_start = self.previous.span.start;
                const def_value = try switch (self.previous.token_type) {
                    .module_end => break,
                    .@"export", .thread, .section, .function, .data => self.linkageDefinition(&token_type),
                    .type => self.typeDefinition(),
                    else => return error.ParseModuleInvalidToken,
                };
                const def_end = self.previous.span.start;

                const target = switch (token_type) {
                    .function => &function_definitions,
                    .data => &data_definitions,
                    .type => &type_definitions,
                    else => return error.ParseUnexpectedType,
                };

                target.* = try self.new(
                    .{ .start = def_start, .end = def_end },
                    .{ .node = .{
                        .value = def_value,
                        .next = target.*,
                    } },
                );
            }

            const end = self.previous.span.end;

            return try self.new(
                .{ .start = start, .end = end },
                .{ .module = .{
                    .functions = function_definitions,
                    .data = data_definitions,
                    .types = type_definitions,
                } },
            );
        }

        fn scopeIdentifier(self: *Self, token_type: token.TokenType, scope: ast.Scope, skip_read: bool) !ast.StatementIndex {
            if (self.previous.token_type != token_type) return error.ParseInvalidIdentifier;

            const span = self.previous.span;
            if (!skip_read) _ = self.next();

            return try self.new(
                .{ .start = span.start + 1, .end = span.end },
                .{ .identifier = .{ .scope = scope } },
            );
        }

        fn localIdentifier(self: *Self) !ast.StatementIndex {
            return self.scopeIdentifier(.local_identifier, .local, false);
        }

        fn globalIdentifier(self: *Self) !ast.StatementIndex {
            return self.scopeIdentifier(.global_identifier, .global, false);
        }

        fn typeIdentifier(self: *Self) !ast.StatementIndex {
            return self.scopeIdentifier(.type_identifier, .type, false);
        }

        fn labelIdentifier(self: *Self) !ast.StatementIndex {
            return self.scopeIdentifier(.label_identifier, .label, false);
        }

        fn stackAlignment(self: *Self) !ast.StatementIndex {
            if (self.previous.token_type != .@"align") return error.ParseMissingAlign;

            _ = self.next();

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

            const span = self.previous.span;
            _ = self.next();

            return try self.new(
                span,
                .{ .primitive_type = primitive_type },
            );
        }

        fn zeroType(self: *Self) !ast.StatementIndex {
            if (self.previous.token_type != .zero) return error.ParseMissingZero;

            const span = self.previous.span;
            _ = self.next();

            return try self.new(
                span,
                .{ .zero_type = undefined },
            );
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
            _ = self.next();

            return try self.new(
                span,
                .{ .env_type = undefined },
            );
        }

        fn integer(self: *Self) !ast.StatementIndex {
            if (self.previous.token_type != .integer_literal) return error.ParseMissingIntegerLiteral;

            const span = self.previous.span;
            _ = self.next();

            return try self.new(
                span,
                .{ .literal = .{
                    .type = .integer,
                } },
            );
        }

        fn single(self: *Self) !ast.StatementIndex {
            if (self.previous.token_type != .single_literal) return error.ParseMissingSingleLiteral;

            const span = self.previous.span;
            _ = self.next();

            return try self.new(
                .{ .start = span.start + 2, .end = span.end },
                .{ .literal = .{
                    .type = .float,
                } },
            );
        }

        fn double(self: *Self) !ast.StatementIndex {
            if (self.previous.token_type != .double_literal) return error.ParseMissingDoubleLiteral;

            const span = self.previous.span;
            _ = self.next();

            return try self.new(
                .{ .start = span.start + 2, .end = span.end },
                .{ .literal = .{
                    .type = .float,
                } },
            );
        }

        fn string(self: *Self) !ast.StatementIndex {
            if (self.previous.token_type != .string_literal) return error.ParseMissingStringLiteral;

            const span = self.previous.span;
            _ = self.next();

            return try self.new(
                .{ .start = span.start + 1, .end = span.end - 1 },
                .{ .literal = .{
                    .type = .float,
                } },
            );
        }

        fn linkage(self: *Self) !?ast.StatementIndex {
            const start = self.previous.span.start;

            var t = self.previous;

            const @"export": bool = switch (t.token_type) {
                .@"export" => true,
                else => false,
            };
            if (@"export") t = self.next();

            const thread: bool = switch (t.token_type) {
                .thread => true,
                else => false,
            };
            if (thread) t = self.next();

            const section: ?ast.StatementIndex = switch (t.token_type) {
                .section => scope: {
                    _ = self.next();

                    break :scope try self.string();
                },
                else => null,
            };

            const flags: ?ast.StatementIndex = switch (section != null and self.previous.token_type == .string_literal) {
                true => try self.string(),
                false => null,
            };

            const end = self.previous.span.start;

            if (start == end) return undefined;

            return try self.new(
                .{ .start = start, .end = end },
                .{ .linkage = .{
                    .@"export" = @"export",
                    .thread = thread,
                    .section = section,
                    .flags = flags,
                } },
            );
        }

        fn typeDefinition(self: *Self) !ast.StatementIndex {
            const start = self.previous.span.start;

            if (self.previous.token_type != .type) return error.ParseMissingType;
            _ = self.next();

            const identifier = try self.typeIdentifier();

            if (self.previous.token_type != .assign) return error.ParseMissingEqual;
            _ = self.next();

            const body = try self.typeDefinitionBody();

            const end = self.previous.span.start;

            return try self.new(
                .{ .start = start, .end = end },
                .{ .type_definition = .{
                    .identifier = identifier,
                    .type = body,
                } },
            );
        }

        fn typeDefinitionBody(self: *Self) !ast.StatementIndex {
            const start = self.previous.span.start;

            const alignment: ?ast.StatementIndex = switch (self.previous.token_type) {
                .@"align" => try self.stackAlignment(),
                else => null,
            };

            if (self.previous.token_type != .open_curly_brace) return error.ParseMissingCurlyBrace;
            _ = self.next();

            return try switch (self.previous.token_type) {
                .open_curly_brace => self.unionType(start, alignment),
                .integer_literal => self.opaqueType(start, alignment),
                else => self.structType(start, alignment),
            };
        }

        fn unionType(self: *Self, start: usize, alignment: ?ast.StatementIndex) !ast.StatementIndex {
            var types: ?ast.StatementIndex = null;

            if (self.previous.token_type != .open_curly_brace) return error.ParseMissingCurlyBrace;

            while (true) {
                const out_start = self.previous.span.start;

                _ = self.next();

                const out = try self.structType(out_start, alignment);

                const out_end = self.previous.span.end;

                types = try self.new(
                    .{ .start = out_start, .end = out_end },
                    .{ .node = .{
                        .value = out,
                        .next = types,
                    } },
                );

                switch (self.previous.token_type) {
                    .close_curly_brace => break,
                    .open_curly_brace => continue,
                    else => return error.ParseMissingCurlyBrace,
                }
            }

            _ = self.next();

            const end = self.previous.span.start;

            return try self.new(
                .{ .start = start, .end = end },
                .{ .union_type = .{
                    .alignment = alignment,
                    .types = types orelse return error.ParseEmptyUnion,
                } },
            );
        }

        fn opaqueType(self: *Self, start: usize, alignment: ?ast.StatementIndex) !ast.StatementIndex {
            const size = try self.integer();

            if (self.previous.token_type != .close_curly_brace) return error.ParseMissingCurlyBrace;
            _ = self.next();

            const end = self.previous.span.start;

            return try self.new(
                .{ .start = start, .end = end },
                .{ .opaque_type = .{
                    .alignment = alignment,
                    .size = size,
                } },
            );
        }

        fn structType(self: *Self, start: usize, alignment: ?ast.StatementIndex) !ast.StatementIndex {
            var members: ?ast.StatementIndex = null;

            while (self.previous.token_type != .close_curly_brace) {
                const member_start = self.previous.span.start;

                const variable_type = try self.variableType();

                const member_type = switch (self.previous.token_type) {
                    .integer_literal => try self.arrayType(member_start, variable_type),
                    else => variable_type,
                };

                const member_end = self.previous.span.end;

                members = try self.new(
                    .{ .start = member_start, .end = member_end },
                    .{ .node = .{
                        .value = member_type,
                        .next = members,
                    } },
                );

                switch (self.previous.token_type) {
                    .close_curly_brace => break,
                    .comma => _ = self.next(),
                    else => return error.ParseMissingComma,
                }
            }

            _ = self.next();

            const end = self.previous.span.end;

            return try self.new(
                .{ .start = start, .end = end },
                .{ .struct_type = .{
                    .alignment = alignment,
                    .members = members orelse return error.ParseEmptyStruct,
                } },
            );
        }

        fn arrayType(self: *Self, start: usize, item: ast.StatementIndex) !ast.StatementIndex {
            const count = try self.integer();

            const end = self.previous.span.start;

            return try self.new(
                .{ .start = start, .end = end },
                .{ .array_type = .{
                    .item = item,
                    .count = count,
                } },
            );
        }

        fn linkageDefinition(self: *Self, out_token_type: *token.TokenType) !ast.StatementIndex {
            const start = self.previous.span.start;

            const link = try self.linkage();

            out_token_type.* = switch (self.previous.token_type) {
                .data => .data,
                .function => .function,
                else => return error.ParseInvalidLinkageType,
            };

            return try switch (self.previous.token_type) {
                .data => self.dataDefinition(start, link),
                .function => self.functionDefinition(start, link),
                else => return error.ParseInvalidLinkageType,
            };
        }

        fn dataDefinition(self: *Self, start: usize, link: ?ast.StatementIndex) !ast.StatementIndex {
            if (self.previous.token_type != .data) return error.ParseInvalidData;
            _ = self.next();

            const identifier = try self.globalIdentifier();

            if (self.previous.token_type != .assign) return error.ParseMissingEqual;
            _ = self.next();

            var alignment: ?ast.StatementIndex = null;
            switch (self.previous.token_type) {
                .@"align" => alignment = try self.stackAlignment(),
                else => {},
            }

            if (self.previous.token_type != .open_curly_brace) return error.ParseMissingCurlyBrace;
            _ = self.next();

            var values: ?ast.StatementIndex = null;

            while (self.previous.token_type != .close_curly_brace) {
                const value_type = try switch (self.previous.token_type) {
                    .zero => self.zeroType(),
                    else => self.primitiveType(),
                };

                var has_data = false;
                while (!(self.previous.token_type == .comma or self.previous.token_type == .close_curly_brace)) {
                    has_data = true;

                    const data_start = self.previous.span.start;
                    const data_value = try self.dataValue();
                    const data_end = self.previous.span.start;

                    const data_span = .{ .start = data_start, .end = data_end };

                    const type_value = try self.new(
                        data_span,
                        .{ .typed_data = .{
                            .type = value_type,
                            .value = data_value,
                        } },
                    );

                    values = try self.new(
                        data_span,
                        .{ .node = .{
                            .value = type_value,
                            .next = values,
                        } },
                    );
                }

                if (!has_data) return error.ParserEmptyData;

                switch (self.previous.token_type) {
                    .close_curly_brace => break,
                    .comma => _ = self.next(),
                    else => return error.ParseMissingComma,
                }
            }

            _ = self.next();

            const end = self.previous.span.start;

            return try self.new(
                .{ .start = start, .end = end },
                .{ .data_definition = .{
                    .linkage = link,
                    .identifier = identifier,
                    .values = values orelse return error.ParserEmptyData,
                } },
            );
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

            if (self.previous.token_type != .plus) return left;

            if (!canOffset) return error.ParseInvalidDataOffset;

            if (self.next().token_type != .integer_literal) return error.ParseInvalidOffset;

            const offset = try self.integer();

            const end = self.previous.span.end;

            return try self.new(
                .{ .start = start, .end = end },
                .{ .offset = .{
                    .identifier = left,
                    .value = offset,
                } },
            );
        }

        fn functionDefinition(self: *Self, start: usize, link: ?ast.StatementIndex) !ast.StatementIndex {
            const function_signature = try self.functionSignature(start, link);
            const function_body = try self.functionBody();

            const end = self.previous.span.start;

            return try self.new(
                .{ .start = start, .end = end },
                .{ .function = .{
                    .signature = function_signature,
                    .body = function_body,
                } },
            );
        }

        fn functionSignature(self: *Self, start: usize, link: ?ast.StatementIndex) !ast.StatementIndex {
            const return_type: ?ast.StatementIndex = switch (self.next().token_type) {
                .global_identifier => null,
                else => try self.variableType(),
            };

            const name = try self.globalIdentifier();

            const parameters = try self.functionParameters();

            const end = self.previous.span.start;

            return try self.new(
                .{ .start = start, .end = end },
                .{ .function_signature = .{
                    .linkage = link,
                    .name = name,
                    .return_type = return_type,
                    .parameters = parameters,
                } },
            );
        }

        fn functionParameters(self: *Self) !?ast.StatementIndex {
            if (self.previous.token_type != .open_parenthesis) return error.ParseMissingOpenParenthesis;
            _ = self.next();

            var parameters: ?ast.StatementIndex = null;

            var first = true;
            while (self.previous.token_type != .close_parenthesis) {
                const param_start = self.previous.span.start;

                const param = try switch (self.previous.token_type) {
                    .env => switch (first) {
                        true => self.envParameter(),
                        false => return error.ParseInvalidEnv,
                    },
                    .variable_arguments => switch (first) {
                        true => return error.ParseInvalidVarArgs,
                        false => self.varArgParameter(),
                    },
                    else => self.typeParameter(),
                };

                const param_end = self.previous.span.start;

                parameters = try self.new(
                    .{ .start = param_start, .end = param_end },
                    .{ .node = .{
                        .value = param,
                        .next = parameters,
                    } },
                );

                switch (self.previous.token_type) {
                    .close_parenthesis => break,
                    .comma => _ = self.next(),
                    else => return error.ParseMissingCommaError,
                }

                first = false;
            }

            if (self.previous.token_type != .close_parenthesis) return error.ParseMissingCloseParenthesis;

            _ = self.next();

            return parameters;
        }

        fn envParameter(self: *Self) !ast.StatementIndex {
            const start = self.previous.span.start;

            const type_statement = try self.envType();
            const identifier = try self.localIdentifier();

            const end = self.previous.span.start;

            return try self.new(
                .{ .start = start, .end = end },
                .{ .type_parameter = .{
                    .type_statement = type_statement,
                    .identifier = identifier,
                } },
            );
        }

        fn varArgParameter(self: *Self) !ast.StatementIndex {
            const start = self.previous.span.start;

            if (self.previous.token_type != .variable_arguments) return error.ParserMissingVarArg;
            _ = self.next();

            const end = self.previous.span.start;

            return try self.new(
                .{ .start = start, .end = end },
                .{ .variadic_parameter = undefined },
            );
        }

        fn typeParameter(self: *Self) !ast.StatementIndex {
            const start = self.previous.span.start;

            const type_statement = try self.variableType();
            const identifier = try self.localIdentifier();

            const end = self.previous.span.start;

            return try self.new(
                .{ .start = start, .end = end },
                .{ .type_parameter = .{
                    .type_statement = type_statement,
                    .identifier = identifier,
                } },
            );
        }

        fn functionBody(self: *Self) !ast.StatementIndex {
            if (self.previous.token_type != .open_curly_brace) return error.ParseMissingCurlyBrace;

            _ = self.next();

            var blocks: ?ast.StatementIndex = null;

            while (true) {
                const block_start = self.previous.span.start;
                const block_value = switch (self.previous.token_type) {
                    .close_curly_brace => break,
                    else => try self.block(),
                };
                const block_end = self.previous.span.start;

                blocks = try self.new(
                    .{ .start = block_start, .end = block_end },
                    .{ .node = .{
                        .value = block_value,
                        .next = blocks,
                    } },
                );
            }

            _ = self.next();

            return blocks orelse error.ParseEmptyFunctionBody;
        }

        fn block(self: *Self) !ast.StatementIndex {
            const start = self.previous.span.start;

            if (self.previous.token_type != .label_identifier) return error.ParseMissingLabel;

            const label = try self.labelIdentifier();

            var phi_statements: ?ast.StatementIndex = null;
            var statements: ?ast.StatementIndex = null;

            const flow_statement: ast.StatementIndex = scope: {
                while (true) {
                    const statement_start = self.previous.span.start;

                    var is_phi = false;
                    const next_statement: ast.StatementIndex = try switch (self.previous.token_type) {
                        // Assignment
                        .local_identifier => self.assignment(&is_phi),
                        // Flow
                        .halt => break :scope try self.halt(),
                        .jump => break :scope try self.jump(),
                        .jump_not_zero => break :scope try self.branch(),
                        .@"return" => break :scope try self.@"return"(),
                        .label_identifier => break :scope try self.fallThroughJump(),
                        // TODO: Support all instructions.
                        else => return error.TODO,
                    };

                    const statement_end = self.previous.span.start;

                    const target = switch (is_phi) {
                        true => &phi_statements,
                        false => &statements,
                    };

                    target.* = try self.new(
                        .{ .start = statement_start, .end = statement_end },
                        .{ .node = .{
                            .value = next_statement,
                            .next = target.*,
                        } },
                    );
                }
            };

            const end = self.previous.span.start;

            return try self.new(
                .{ .start = start, .end = end },
                .{ .block = .{
                    .label = label,
                    .phi_statements = phi_statements,
                    .statements = statements,
                    .flow_statement = flow_statement,
                } },
            );
        }

        fn blockValue(self: *Self) !ast.StatementIndex {
            return try switch (self.previous.token_type) {
                .global_identifier => self.globalIdentifier(),
                .local_identifier => self.localIdentifier(),
                .integer_literal => self.integer(),
                .single_literal => self.single(),
                .double_literal => self.double(),
                else => return error.ParserInvalidBlockValue,
            };
        }

        fn assignment(self: *Self, is_phi: *bool) !ast.StatementIndex {
            const start = self.previous.span.start;

            const identifier = try self.localIdentifier();

            const assignment_span = self.previous.span;
            const primitive_type: ast.PrimitiveType = switch (self.previous.token_type) {
                .long_assign => .long,
                .word_assign => .word,
                .single_assign => .single,
                .double_assign => .double,
                else => return error.ParserInvalidAssignmentSymbol,
            };

            const data_type = try self.new(
                .{ .start = assignment_span.start + 1, .end = assignment_span.end },
                .{ .primitive_type = primitive_type },
            );

            _ = self.next();

            is_phi.* = switch (self.previous.token_type) {
                .phi => true,
                else => false,
            };

            const statement = try switch (self.previous.token_type) {
                .phi => self.phi(data_type),
                else => return error.TODO,
            };

            const end = self.previous.span.start;

            return try self.new(
                .{ .start = start, .end = end },
                .{
                    .assignment = .{
                        .identifier = identifier,
                        .statement = statement,
                    },
                },
            );
        }

        fn phi(self: *Self, data_type: ast.StatementIndex) !ast.StatementIndex {
            const start = self.previous.span.start;

            if (self.previous.token_type != .phi) return error.ParserMissingPhi;
            _ = self.next();

            var parameters: ?ast.StatementIndex = null;

            var first = true;
            while (true) {
                if (!first) {
                    if (self.previous.token_type != .comma) break;
                    _ = self.next();
                }

                const parameter_start = self.previous.span.start;
                const parameter = try self.phiParameter();
                const parameter_end = self.previous.span.start;

                parameters = try self.new(
                    .{ .start = parameter_start, .end = parameter_end },
                    .{
                        .node = .{
                            .value = parameter,
                            .next = parameters,
                        },
                    },
                );

                first = false;
            }

            const end = self.previous.span.start;

            return self.new(
                .{ .start = start, .end = end },
                .{
                    .phi = .{
                        .data_type = data_type,
                        .parameters = parameters orelse return error.ParserEmptyPhi,
                    },
                },
            );
        }

        fn phiParameter(self: *Self) !ast.StatementIndex {
            const start = self.previous.span.start;

            const identifier = try self.labelIdentifier();
            const value = try self.blockValue();

            const end = self.previous.span.end;

            return self.new(
                .{ .start = start, .end = end },
                .{
                    .phi_parameter = .{
                        .identifier = identifier,
                        .value = value,
                    },
                },
            );
        }

        fn branch(self: *Self) !ast.StatementIndex {
            const start = self.previous.span.start;

            if (self.previous.token_type != .jump_not_zero) return error.ParseMissingJump;
            _ = self.next();

            const condition = try self.localIdentifier();

            if (self.previous.token_type != .comma) return error.ParseMissingComma;
            _ = self.next();

            const true_label = try self.labelIdentifier();

            if (self.previous.token_type != .comma) return error.ParseMissingComma;
            _ = self.next();

            const false_label = try self.labelIdentifier();

            const end = self.previous.span.start;

            return try self.new(
                .{ .start = start, .end = end },
                .{ .branch = .{
                    .condition = condition,
                    .true = true_label,
                    .false = false_label,
                } },
            );
        }

        fn halt(self: *Self) !ast.StatementIndex {
            if (self.previous.token_type != .halt) return error.ParseMissingHalt;

            const span = self.previous.span;
            _ = self.next();

            return try self.new(
                span,
                .{ .halt = undefined },
            );
        }

        fn fallThroughJump(self: *Self) !ast.StatementIndex {
            const start = self.previous.span.start;
            const label = try self.scopeIdentifier(.label_identifier, .label, true);
            const end = self.previous.span.end;

            return try self.new(
                .{ .start = start, .end = end },
                .{ .jump = .{
                    .identifier = label,
                } },
            );
        }

        fn jump(self: *Self) !ast.StatementIndex {
            const start = self.previous.span.start;

            if (self.previous.token_type != .jump) return error.ParseMissingJump;
            _ = self.next();

            const label = try self.labelIdentifier();

            const end = self.previous.span.start;

            return try self.new(
                .{ .start = start, .end = end },
                .{ .jump = .{
                    .identifier = label,
                } },
            );
        }

        fn @"return"(self: *Self) !ast.StatementIndex {
            const start = self.previous.span.start;

            if (self.previous.token_type != .@"return") return error.ParseMissingReturn;
            _ = self.next();

            const value: ?ast.StatementIndex = switch (self.previous.token_type) {
                .local_identifier => try self.localIdentifier(),
                else => undefined,
            };

            const end = self.previous.span.start;

            return try self.new(
                .{ .start = start, .end = end },
                .{ .@"return" = .{
                    .value = value,
                } },
            );
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

fn printStatements(file: []const u8) !void {
    const statements = try testParser(file);
    defer test_allocator.free(statements);

    for (statements) |statement| {
        std.log.err("{any}", .{@as(ast.StatementType, @enumFromInt(@intFromEnum(statement.data)))});
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

test "type struct" {
    // Arrange
    const file = "type :t = {w}";
    const expected = [_]ast.StatementType{
        .identifier,
        .primitive_type,
        .node,
        .struct_type,
        .type_definition,
        .node,
        .module,
    };

    // Act + Assert
    try assertParser(file, &expected);
}

test "type struct alignment" {
    // Arrange
    const file = "type :t = align 32 {w}";
    const expected = [_]ast.StatementType{
        .identifier,
        .literal,
        .primitive_type,
        .node,
        .struct_type,
        .type_definition,
        .node,
        .module,
    };

    // Act + Assert
    try assertParser(file, &expected);
}

test "type struct trailing comma" {
    // Arrange
    const file = "type :t = {w, }";
    const expected = [_]ast.StatementType{
        .identifier,
        .primitive_type,
        .node,
        .struct_type,
        .type_definition,
        .node,
        .module,
    };

    // Act + Assert
    try assertParser(file, &expected);
}

test "type struct array" {
    // Arrange
    const file = "type :t = {w 1}";
    const expected = [_]ast.StatementType{
        .identifier,
        .primitive_type,
        .literal,
        .array_type,
        .node,
        .struct_type,
        .type_definition,
        .node,
        .module,
    };

    // Act + Assert
    try assertParser(file, &expected);
}

test "type struct custom type" {
    // Arrange
    const file = "type :t = {:o}";
    const expected = [_]ast.StatementType{
        .identifier,
        .identifier,
        .node,
        .struct_type,
        .type_definition,
        .node,
        .module,
    };

    // Act + Assert
    try assertParser(file, &expected);
}

test "type struct many members" {
    // Arrange
    const file = "type :t = {w, :o, s 1}";
    const expected = [_]ast.StatementType{
        .identifier,
        .primitive_type,
        .node,
        .identifier,
        .node,
        .primitive_type,
        .literal,
        .array_type,
        .node,
        .struct_type,
        .type_definition,
        .node,
        .module,
    };

    // Act + Assert
    try assertParser(file, &expected);
}

test "type union" {
    // Arrange
    const file = "type :t = { {w} {s} }";
    const expected = [_]ast.StatementType{
        .identifier,
        .primitive_type,
        .node,
        .struct_type,
        .node,
        .primitive_type,
        .node,
        .struct_type,
        .node,
        .union_type,
        .type_definition,
        .node,
        .module,
    };

    // Act + Assert
    try assertParser(file, &expected);
}

test "type union alignment" {
    // Arrange
    const file = "type :t = align 32 { {w} }";
    const expected = [_]ast.StatementType{
        .identifier,
        .literal,
        .primitive_type,
        .node,
        .struct_type,
        .node,
        .union_type,
        .type_definition,
        .node,
        .module,
    };

    // Act + Assert
    try assertParser(file, &expected);
}

test "type opaque" {
    // Arrange
    const file = "type :t = { 32 }";
    const expected = [_]ast.StatementType{
        .identifier,
        .literal,
        .opaque_type,
        .type_definition,
        .node,
        .module,
    };

    // Act + Assert
    try assertParser(file, &expected);
}

test "type opaque alignment" {
    // Arrange
    const file = "type :t = align 16 { 32 }";
    const expected = [_]ast.StatementType{
        .identifier,
        .literal,
        .literal,
        .opaque_type,
        .type_definition,
        .node,
        .module,
    };

    // Act + Assert
    try assertParser(file, &expected);
}

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

test "data with trailing comma" {
    // Arrange
    const file = "data $d = {w 1, }";
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
    const file = "function $fun() {@s ret}";
    const expected = [_]ast.StatementType{
        .identifier,
        .function_signature,
        .identifier,
        .@"return",
        .block,
        .node,
        .function,
        .node,
        .module,
    };

    // Act + Assert
    try assertParser(file, &expected);
}

test "function with linkage" {
    // Arrange
    const file = "export thread section \"function\" \"flag\" function $fun() {@s ret}";
    const expected = [_]ast.StatementType{
        .literal,
        .literal,
        .linkage,
        .identifier,
        .function_signature,
        .identifier,
        .@"return",
        .block,
        .node,
        .function,
        .node,
        .module,
    };

    // Act + Assert
    try assertParser(file, &expected);
}

test "function with primitive return type" {
    // Arrange
    const file = "function w $fun() {@s ret}";
    const expected = [_]ast.StatementType{
        .primitive_type,
        .identifier,
        .function_signature,
        .identifier,
        .@"return",
        .block,
        .node,
        .function,
        .node,
        .module,
    };

    // Act + Assert
    try assertParser(file, &expected);
}

test "function with custom return type" {
    // Arrange
    const file = "function :type $fun() {@s ret}";
    const expected = [_]ast.StatementType{
        .identifier,
        .identifier,
        .function_signature,
        .identifier,
        .@"return",
        .block,
        .node,
        .function,
        .node,
        .module,
    };

    // Act + Assert
    try assertParser(file, &expected);
}

test "function with custom type parameter" {
    // Arrange
    const file = "function $fun(:type %p) {@s ret}";
    const expected = [_]ast.StatementType{
        .identifier,
        .identifier,
        .identifier,
        .type_parameter,
        .node,
        .function_signature,
        .identifier,
        .@"return",
        .block,
        .node,
        .function,
        .node,
        .module,
    };

    // Act + Assert
    try assertParser(file, &expected);
}

test "function with one parameter" {
    // Arrange
    const file = "function $fun(w %p) {@s ret}";
    const expected = [_]ast.StatementType{
        .identifier,
        .primitive_type,
        .identifier,
        .type_parameter,
        .node,
        .function_signature,
        .identifier,
        .@"return",
        .block,
        .node,
        .function,
        .node,
        .module,
    };

    // Act + Assert
    try assertParser(file, &expected);
}

test "function with trailing comma" {
    // Arrange
    const file = "function $fun(w %p, ) {@s ret}";
    const expected = [_]ast.StatementType{
        .identifier,
        .primitive_type,
        .identifier,
        .type_parameter,
        .node,
        .function_signature,
        .identifier,
        .@"return",
        .block,
        .node,
        .function,
        .node,
        .module,
    };

    // Act + Assert
    try assertParser(file, &expected);
}

test "function with many parameters" {
    // Arrange
    const file = "function $fun(w %p0, b %p1, h %p2) {@s ret}";
    const expected = [_]ast.StatementType{
        .identifier,
        .primitive_type,
        .identifier,
        .type_parameter,
        .node,
        .primitive_type,
        .identifier,
        .type_parameter,
        .node,
        .primitive_type,
        .identifier,
        .type_parameter,
        .node,
        .function_signature,
        .identifier,
        .@"return",
        .block,
        .node,
        .function,
        .node,
        .module,
    };

    // Act + Assert
    try assertParser(file, &expected);
}

test "function with env parameter" {
    // Arrange
    const file = "function $fun(env %e) {@s ret}";
    const expected = [_]ast.StatementType{
        .identifier,
        .env_type,
        .identifier,
        .type_parameter,
        .node,
        .function_signature,
        .identifier,
        .@"return",
        .block,
        .node,
        .function,
        .node,
        .module,
    };

    // Act + Assert
    try assertParser(file, &expected);
}

test "function with variable parameter" {
    // Arrange
    const file = "function $fun(w %fmt, ...) {@s ret}";
    const expected = [_]ast.StatementType{
        .identifier,
        .primitive_type,
        .identifier,
        .type_parameter,
        .node,
        .variadic_parameter,
        .node,
        .function_signature,
        .identifier,
        .@"return",
        .block,
        .node,
        .function,
        .node,
        .module,
    };

    // Act + Assert
    try assertParser(file, &expected);
}

test "function many flow" {
    // Arrange
    const file = "function $fun() {@a jmp @b @b jnz %p, @c, @d @c hlt @d ret}";
    const expected = [_]ast.StatementType{
        .identifier,
        .function_signature,
        .identifier,
        .identifier,
        .jump,
        .block,
        .node,
        .identifier,
        .identifier,
        .identifier,
        .identifier,
        .branch,
        .block,
        .node,
        .identifier,
        .halt,
        .block,
        .node,
        .identifier,
        .@"return",
        .block,
        .node,
        .function,
        .node,
        .module,
    };

    // Act + Assert
    try assertParser(file, &expected);
}

test "function fall-through block" {
    // Arrange
    const file = "function $fun() {@s @n ret}";
    const expected = [_]ast.StatementType{
        .identifier,
        .function_signature,
        .identifier,
        .identifier,
        .jump,
        .block,
        .node,
        .identifier,
        .@"return",
        .block,
        .node,
        .function,
        .node,
        .module,
    };

    // Act + Assert
    try assertParser(file, &expected);
}

test "phi" {
    // Arrange
    const file = "function $fun() {@s %x =w phi @a 1, @b %l, @c $g ret}";
    const expected = [_]ast.StatementType{
        .identifier,
        .function_signature,
        .identifier,
        .identifier,
        .primitive_type,
        .identifier,
        .literal,
        .phi_parameter,
        .node,
        .identifier,
        .identifier,
        .phi_parameter,
        .node,
        .identifier,
        .identifier,
        .phi_parameter,
        .node,
        .phi,
        .assignment,
        .node,
        .@"return",
        .block,
        .node,
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
    const file = "function @fun() {@s ret}";
    const expected = error.ParseInvalidPrimitiveType;

    // Act
    const res = testParser(file);

    // Assert
    try std.testing.expectError(expected, res);
}

test "function error.ParseInvalidVarArgs" {
    // Arrange
    const file = "function $fun(...) {@s ret}";
    const expected = error.ParseInvalidVarArgs;

    // Act
    const res = testParser(file);

    // Assert
    try std.testing.expectError(expected, res);
}

test "function error.ParseInvalidIdentifier" {
    // Arrange
    const file = "function $fun(w) {@s ret}";
    const expected = error.ParseInvalidIdentifier;

    // Act
    const res = testParser(file);

    // Assert
    try std.testing.expectError(expected, res);
}

test "function error.ParseInvalidIdentifier 2" {
    // Arrange
    const file = "function $fun(w @a) {@s ret}";
    const expected = error.ParseInvalidIdentifier;

    // Act
    const res = testParser(file);

    // Assert
    try std.testing.expectError(expected, res);
}

test "function error.ParseEmptyFunctionBody" {
    // Arrange
    const file = "function $fun() {}";
    const expected = error.ParseEmptyFunctionBody;

    // Act
    const res = testParser(file);

    // Assert
    try std.testing.expectError(expected, res);
}
