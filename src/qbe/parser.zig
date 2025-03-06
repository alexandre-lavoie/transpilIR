const std = @import("std");

const ast = @import("../ast/lib.zig");
const common = @import("../common.zig");
const lexer = @import("lexer.zig");
const token = @import("token.zig");

const test_lib = @import("../test/lib.zig");

const ParseRecord = struct {
    index: usize,
    statement: *ast.Statement,
};

pub fn QBEParser(comptime Reader: type) type {
    return struct {
        reader: *Reader,
        ast: *ast.AST,
        previous: *const token.QBEToken = undefined,
        previous_previous: *const token.QBEToken = undefined,

        const Self = @This();

        fn reader_next(self: *Self) *const token.QBEToken {
            return self.reader.next();
        }

        pub fn init(reader: *Reader, tree: *ast.AST) Self {
            return Self{
                .reader = reader,
                .ast = tree,
            };
        }

        pub fn parse(self: *Self) !usize {
            _ = self.next();
            return try self.module();
        }

        fn new(self: *Self, span: common.SourceSpan, data: ast.StatementData) !ast.StatementIndex {
            return try self.ast.append(ast.Statement.init(
                span,
                data,
            ));
        }

        fn next(self: *Self) *const token.QBEToken {
            self.previous_previous = self.previous;
            self.previous = self.reader_next();

            return self.previous;
        }

        fn node(self: *Self, head: *?ast.StatementIndex, tail: *?ast.StatementIndex, span: common.SourceSpan, value: ast.StatementIndex) !ast.StatementIndex {
            const next_node = try self.new(
                span,
                .{ .node = .{
                    .value = value,
                    .previous = tail.*,
                    .next = null,
                } },
            );

            if (tail.*) |ti| {
                var ts = self.ast.getPtrMut(ti) orelse return error.NotFound;

                switch (ts.data) {
                    .node => |*n| {
                        n.next = next_node;
                    },
                    else => {},
                }
            } else {
                head.* = next_node;
            }

            tail.* = next_node;

            return next_node;
        }

        fn module(self: *Self) !ast.StatementIndex {
            const start = self.previous.span.start;

            if (self.previous.token_type != .module_start) return error.MissingModule;
            _ = self.next();

            var function_head: ?ast.StatementIndex = null;
            var function_tail: ?ast.StatementIndex = null;

            var data_head: ?ast.StatementIndex = null;
            var data_tail: ?ast.StatementIndex = null;

            var type_head: ?ast.StatementIndex = null;
            var type_tail: ?ast.StatementIndex = null;

            while (true) {
                var token_type: token.QBETokenType = .type;

                const def_start = self.previous.span.start;
                const def_value = try switch (self.previous.token_type) {
                    .module_end => break,
                    .@"export", .thread, .section, .function, .data => self.linkageDefinition(&token_type),
                    .type => self.typeDefinition(),
                    else => return error.ModuleInvalidToken,
                };
                const def_end = self.previous_previous.span.end;

                const head = switch (token_type) {
                    .function => &function_head,
                    .data => &data_head,
                    .type => &type_head,
                    else => return error.UnexpectedType,
                };

                const tail = switch (token_type) {
                    .function => &function_tail,
                    .data => &data_tail,
                    .type => &type_tail,
                    else => return error.UnexpectedType,
                };

                _ = try self.node(
                    head,
                    tail,
                    .{ .start = def_start, .end = def_end },
                    def_value,
                );
            }

            const end = self.previous.span.end;

            return try self.new(
                .{ .start = start, .end = end },
                .{ .module = .{
                    .functions = function_head,
                    .data = data_head,
                    .types = type_head,
                } },
            );
        }

        fn scopeIdentifier(self: *Self, token_type: token.QBETokenType, scope: ast.Scope, thread: bool, skip_read: bool) !ast.StatementIndex {
            if (self.previous.token_type != token_type) return error.InvalidIdentifier;

            const span = self.previous.span;
            if (!skip_read) _ = self.next();

            return try self.new(
                .{ .start = span.start + 1, .end = span.end },
                .{ .identifier = .{
                    .scope = scope,
                    .thread = thread,
                } },
            );
        }

        fn localIdentifier(self: *Self) !ast.StatementIndex {
            return self.scopeIdentifier(.local_identifier, .local, false, false);
        }

        fn globalIdentifier(self: *Self) !ast.StatementIndex {
            return self.scopeIdentifier(.global_identifier, .global, false, false);
        }

        fn typeIdentifier(self: *Self) !ast.StatementIndex {
            return self.scopeIdentifier(.type_identifier, .type, false, false);
        }

        fn labelIdentifier(self: *Self) !ast.StatementIndex {
            return self.scopeIdentifier(.label_identifier, .label, false, false);
        }

        fn threadIdentifier(self: *Self) !ast.StatementIndex {
            if (self.previous.token_type != .thread) return error.MissingThread;
            _ = self.next();

            return try switch (self.previous.token_type) {
                .global_identifier => self.scopeIdentifier(.global_identifier, .global, true, false),
                .local_identifier => self.scopeIdentifier(.local_identifier, .local, true, false),
                else => return error.InvalidThread,
            };
        }

        fn stackAlignment(self: *Self) !ast.StatementIndex {
            if (self.previous.token_type != .@"align") return error.MissingAlign;

            _ = self.next();

            return try self.integer();
        }

        fn primitiveTypeInner(self: *Self) !ast.PrimitiveType {
            return switch (self.previous.token_type) {
                .byte_unsigned => .u8,
                .byte => .i8,
                .double => .f64,
                .half_word_unsigned => .u16,
                .half_word => .i16,
                .long => .i64,
                .long_unsigned => .u64,
                .single => .f32,
                .word_unsigned => .u32,
                .word => .i32,
                else => return error.InvalidPrimitiveType,
            };
        }

        fn primitiveType(self: *Self) !ast.StatementIndex {
            const primitive_type = try self.primitiveTypeInner();

            const span = self.previous.span;
            _ = self.next();

            return try self.new(
                span,
                .{ .primitive_type = primitive_type },
            );
        }

        fn zeroType(self: *Self) !ast.StatementIndex {
            if (self.previous.token_type != .zero) return error.MissingZero;

            const span = self.previous.span;
            _ = self.next();

            return try self.new(
                span,
                .zero_type,
            );
        }

        fn variableType(self: *Self) !ast.StatementIndex {
            return switch (self.previous.token_type) {
                .type_identifier => self.typeIdentifier(),
                else => self.primitiveType(),
            };
        }

        fn envType(self: *Self) !ast.StatementIndex {
            if (self.previous.token_type != .env) return error.MissingEnv;

            const span = self.previous.span;
            _ = self.next();

            return try self.new(
                span,
                .env_type,
            );
        }

        fn integer(self: *Self) !ast.StatementIndex {
            if (self.previous.token_type != .integer_literal) return error.MissingIntegerLiteral;

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
            if (self.previous.token_type != .single_literal) return error.MissingSingleLiteral;

            const span = self.previous.span;
            _ = self.next();

            return try self.new(
                .{ .start = span.start + 2, .end = span.end },
                .{ .literal = .{
                    .type = .single,
                } },
            );
        }

        fn double(self: *Self) !ast.StatementIndex {
            if (self.previous.token_type != .double_literal) return error.MissingDoubleLiteral;

            const span = self.previous.span;
            _ = self.next();

            return try self.new(
                .{ .start = span.start + 2, .end = span.end },
                .{ .literal = .{
                    .type = .double,
                } },
            );
        }

        fn string(self: *Self) !ast.StatementIndex {
            if (self.previous.token_type != .string_literal) return error.MissingStringLiteral;

            const span = self.previous.span;
            _ = self.next();

            return try self.new(
                .{ .start = span.start + 1, .end = span.end - 1 },
                .{ .literal = .{
                    .type = .string,
                } },
            );
        }

        fn linkage(self: *Self) !ast.StatementIndex {
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

            const span: common.SourceSpan = scope: {
                if (self.previous.span.start == start) {
                    break :scope .{ .start = start, .end = start };
                } else {
                    break :scope .{ .start = start, .end = t.span.end };
                }
            };

            return try self.new(
                span,
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

            if (self.previous.token_type != .type) return error.MissingType;
            _ = self.next();

            const identifier = try self.typeIdentifier();

            if (self.previous.token_type != .assign) return error.MissingEqual;
            _ = self.next();

            const body = try self.typeDefinitionBody();

            const end = self.previous_previous.span.end;

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

            if (self.previous.token_type != .open_curly_brace) return error.MissingOpenCurlyBrace;
            _ = self.next();

            return try switch (self.previous.token_type) {
                .open_curly_brace => self.unionType(start, alignment),
                .integer_literal => self.opaqueType(start, alignment),
                else => self.structType(start, alignment),
            };
        }

        fn unionType(self: *Self, start: usize, alignment: ?ast.StatementIndex) !ast.StatementIndex {
            var type_head: ?ast.StatementIndex = null;
            var type_tail: ?ast.StatementIndex = null;

            if (self.previous.token_type != .open_curly_brace) return error.MissingOpenCurlyBrace;

            while (true) {
                const out_start = self.previous.span.start;

                _ = self.next();

                const out = try self.structType(out_start, null);

                const out_end = self.previous_previous.span.end;

                _ = try self.node(
                    &type_head,
                    &type_tail,
                    .{ .start = out_start, .end = out_end },
                    out,
                );

                switch (self.previous.token_type) {
                    .close_curly_brace => break,
                    .open_curly_brace => continue,
                    else => return error.MissingOpenCurlyBrace,
                }
            }

            _ = self.next();

            const end = self.previous_previous.span.end;

            return try self.new(
                .{ .start = start, .end = end },
                .{ .union_type = .{
                    .alignment = alignment,
                    .types = type_head orelse return error.EmptyUnion,
                } },
            );
        }

        fn opaqueType(self: *Self, start: usize, alignment: ?ast.StatementIndex) !ast.StatementIndex {
            const size = try self.integer();

            if (self.previous.token_type != .close_curly_brace) return error.MissingCloseCurlyBrace;
            _ = self.next();

            const end = self.previous_previous.span.end;

            return try self.new(
                .{ .start = start, .end = end },
                .{ .opaque_type = .{
                    .alignment = alignment,
                    .size = size,
                } },
            );
        }

        fn structType(self: *Self, start: usize, alignment: ?ast.StatementIndex) !ast.StatementIndex {
            var member_head: ?ast.StatementIndex = null;
            var member_tail: ?ast.StatementIndex = null;

            while (self.previous.token_type != .close_curly_brace) {
                const member_start = self.previous.span.start;

                const variable_type = try self.variableType();

                const member_type = switch (self.previous.token_type) {
                    .integer_literal => try self.arrayType(member_start, variable_type),
                    else => variable_type,
                };

                const member_end = self.previous_previous.span.end;

                _ = try self.node(
                    &member_head,
                    &member_tail,
                    .{ .start = member_start, .end = member_end },
                    member_type,
                );

                switch (self.previous.token_type) {
                    .close_curly_brace => break,
                    .comma => _ = self.next(),
                    else => return error.MissingComma,
                }
            }

            _ = self.next();

            const end = self.previous_previous.span.end;

            return try self.new(
                .{ .start = start, .end = end },
                .{ .struct_type = .{
                    .alignment = alignment,
                    .members = member_head orelse return error.EmptyStruct,
                } },
            );
        }

        fn arrayType(self: *Self, start: usize, item: ast.StatementIndex) !ast.StatementIndex {
            const count = try self.integer();

            const end = self.previous_previous.span.end;

            return try self.new(
                .{ .start = start, .end = end },
                .{ .array_type = .{
                    .item = item,
                    .count = count,
                } },
            );
        }

        fn linkageDefinition(self: *Self, out_token_type: *token.QBETokenType) !ast.StatementIndex {
            const start = self.previous.span.start;

            const link = try self.linkage();

            out_token_type.* = switch (self.previous.token_type) {
                .data => .data,
                .function => .function,
                else => return error.InvalidLinkageType,
            };

            return try switch (self.previous.token_type) {
                .data => self.dataDefinition(start, link),
                .function => self.functionDefinition(start, link),
                else => return error.InvalidLinkageType,
            };
        }

        fn dataDefinition(self: *Self, start: usize, link: ast.StatementIndex) !ast.StatementIndex {
            if (self.previous.token_type != .data) return error.InvalidData;
            _ = self.next();

            const identifier = try self.globalIdentifier();

            if (self.previous.token_type != .assign) return error.MissingEqual;
            _ = self.next();

            var alignment: ?ast.StatementIndex = null;
            switch (self.previous.token_type) {
                .@"align" => alignment = try self.stackAlignment(),
                else => {},
            }

            if (self.previous.token_type != .open_curly_brace) return error.MissingOpenCurlyBrace;
            _ = self.next();

            var value_head: ?ast.StatementIndex = null;
            var value_tail: ?ast.StatementIndex = null;

            while (self.previous.token_type != .close_curly_brace) {
                const value_span = self.previous.span;
                const value_data: ast.StatementData = switch (self.previous.token_type) {
                    .zero => .zero_type,
                    else => .{
                        .primitive_type = try self.primitiveTypeInner(),
                    },
                };

                _ = self.next();

                var has_data = false;
                while (!(self.previous.token_type == .comma or self.previous.token_type == .close_curly_brace)) {
                    const value_type = try self.new(
                        value_span,
                        value_data,
                    );

                    has_data = true;

                    const data_start = self.previous.span.start;
                    const data_value = try self.dataValue();
                    const data_end = self.previous_previous.span.end;

                    const data_span = .{ .start = data_start, .end = data_end };

                    const type_value = try self.new(
                        data_span,
                        .{ .typed_data = .{
                            .type = value_type,
                            .value = data_value,
                        } },
                    );

                    _ = try self.node(
                        &value_head,
                        &value_tail,
                        data_span,
                        type_value,
                    );
                }

                if (!has_data) return error.EmptyData;

                switch (self.previous.token_type) {
                    .close_curly_brace => break,
                    .comma => _ = self.next(),
                    else => return error.MissingComma,
                }
            }

            _ = self.next();

            const end = self.previous_previous.span.end;

            return try self.new(
                .{ .start = start, .end = end },
                .{ .data_definition = .{
                    .linkage = link,
                    .identifier = identifier,
                    .values = value_head orelse return error.EmptyData,
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
                else => return error.InvalidDataValue,
            };

            if (self.previous.token_type != .plus) return left;

            if (!canOffset) return error.InvalidDataOffset;

            if (self.next().token_type != .integer_literal) return error.InvalidOffset;

            const offset = try self.integer();

            const end = self.previous_previous.span.end;

            return try self.new(
                .{ .start = start, .end = end },
                .{ .offset = .{
                    .identifier = left,
                    .value = offset,
                } },
            );
        }

        fn functionDefinition(self: *Self, start: usize, link: ast.StatementIndex) !ast.StatementIndex {
            const function_signature = try self.functionSignature(start, link);
            const function_body = try self.functionBody();

            const end = self.previous_previous.span.end;

            return try self.new(
                .{ .start = start, .end = end },
                .{ .function = .{
                    .signature = function_signature,
                    .body = function_body,
                } },
            );
        }

        fn functionSignature(self: *Self, start: usize, link: ast.StatementIndex) !ast.StatementIndex {
            const return_type: ast.StatementIndex = switch (self.next().token_type) {
                .global_identifier => try self.new(
                    .{ .start = self.previous.span.start, .end = self.previous.span.start },
                    .{ .primitive_type = .void },
                ),
                else => try self.variableType(),
            };

            const name = try self.globalIdentifier();

            const parameters = try self.functionParameters();

            const end = self.previous_previous.span.end;

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
            if (self.previous.token_type != .open_parenthesis) return error.MissingOpenParenthesis;
            _ = self.next();

            var parameter_head: ?ast.StatementIndex = null;
            var parameter_tail: ?ast.StatementIndex = null;

            var first = true;
            while (self.previous.token_type != .close_parenthesis) {
                const param_start = self.previous.span.start;

                const param_token_type = self.previous.token_type;

                const param = try switch (param_token_type) {
                    .env => switch (first) {
                        true => self.functionEnvParameter(),
                        false => return error.InvalidEnv,
                    },
                    .variable_arguments => switch (first) {
                        true => return error.InvalidVarArgs,
                        false => self.varArgParameter(),
                    },
                    else => self.functionParameter(),
                };

                const param_end = self.previous_previous.span.end;

                _ = try self.node(
                    &parameter_head,
                    &parameter_tail,
                    .{ .start = param_start, .end = param_end },
                    param,
                );

                if (param_token_type == .variable_arguments) break;

                switch (self.previous.token_type) {
                    .close_parenthesis => break,
                    .comma => _ = self.next(),
                    else => return error.MissingComma,
                }

                first = false;
            }

            if (self.previous.token_type != .close_parenthesis) return error.MissingCloseParenthesis;

            _ = self.next();

            return parameter_head;
        }

        fn functionEnvParameter(self: *Self) !ast.StatementIndex {
            const start = self.previous.span.start;

            const type_statement = try self.envType();
            const value = try self.blockValue();

            const end = self.previous_previous.span.end;

            return try self.new(
                .{ .start = start, .end = end },
                .{
                    .function_parameter = .{
                        .type = type_statement,
                        .value = value,
                    },
                },
            );
        }

        fn callEnvParameter(self: *Self) !ast.StatementIndex {
            const start = self.previous.span.start;

            const type_statement = try self.envType();
            const value = try self.blockValue();

            const end = self.previous_previous.span.end;

            return try self.new(
                .{ .start = start, .end = end },
                .{
                    .call_parameter = .{
                        .type = type_statement,
                        .value = value,
                    },
                },
            );
        }

        fn varArgParameter(self: *Self) !ast.StatementIndex {
            const start = self.previous.span.start;

            if (self.previous.token_type != .variable_arguments) return error.MissingVarArg;
            _ = self.next();

            const end = self.previous_previous.span.end;

            return try self.new(
                .{ .start = start, .end = end },
                .variadic_parameter,
            );
        }

        fn functionParameter(self: *Self) !ast.StatementIndex {
            const start = self.previous.span.start;

            const type_statement = try self.variableType();
            const value = try self.localIdentifier();

            const end = self.previous_previous.span.end;

            return try self.new(
                .{ .start = start, .end = end },
                .{ .function_parameter = .{
                    .type = type_statement,
                    .value = value,
                } },
            );
        }

        fn callParameter(self: *Self) !ast.StatementIndex {
            const start = self.previous.span.start;

            const type_statement = try self.variableType();
            const value = try self.blockValue();

            const end = self.previous_previous.span.end;

            return try self.new(
                .{ .start = start, .end = end },
                .{ .call_parameter = .{
                    .type = type_statement,
                    .value = value,
                } },
            );
        }

        fn functionBody(self: *Self) !ast.StatementIndex {
            if (self.previous.token_type != .open_curly_brace) return error.MissingOpenCurlyBrace;

            _ = self.next();

            var block_head: ?ast.StatementIndex = null;
            var block_tail: ?ast.StatementIndex = null;

            while (true) {
                const block_start = self.previous.span.start;
                const block_value = switch (self.previous.token_type) {
                    .close_curly_brace => break,
                    else => try self.block(),
                };
                const block_end = self.previous_previous.span.end;

                _ = try self.node(
                    &block_head,
                    &block_tail,
                    .{ .start = block_start, .end = block_end },
                    block_value,
                );
            }

            _ = self.next();

            return block_head orelse error.EmptyFunctionBody;
        }

        fn block(self: *Self) !ast.StatementIndex {
            const start = self.previous.span.start;

            if (self.previous.token_type != .label_identifier) return error.MissingLabel;

            const label = try self.labelIdentifier();

            var phi_statement_head: ?ast.StatementIndex = null;
            var phi_statement_tail: ?ast.StatementIndex = null;

            var statement_head: ?ast.StatementIndex = null;
            var statement_tail: ?ast.StatementIndex = null;

            const flow_statement: ast.StatementIndex = scope: {
                while (true) {
                    const statement_start = self.previous.span.start;

                    var is_phi = false;
                    var is_flow = false;
                    const next_statement: ast.StatementIndex = try self.blockLine(&is_phi, &is_flow);

                    if (is_flow) break :scope next_statement;

                    const statement_end = self.previous_previous.span.end;

                    const head = switch (is_phi) {
                        true => &phi_statement_head,
                        false => &statement_head,
                    };

                    const tail = switch (is_phi) {
                        true => &phi_statement_tail,
                        false => &statement_tail,
                    };

                    _ = try self.node(
                        head,
                        tail,
                        .{ .start = statement_start, .end = statement_end },
                        next_statement,
                    );
                }
            };

            const end = self.previous_previous.span.end;

            return try self.new(
                .{ .start = start, .end = end },
                .{ .block = .{
                    .label = label,
                    .phis = phi_statement_head,
                    .lines = statement_head,
                    .flow = flow_statement,
                } },
            );
        }

        fn blockLine(self: *Self, is_phi: *bool, is_flow: *bool) !ast.StatementIndex {
            is_flow.* = switch (self.previous.token_type) {
                .halt, .jump, .jump_not_zero, .@"return", .label_identifier => true,
                else => false,
            };

            const start = self.previous.span.start;

            var out = try switch (self.previous.token_type) {
                // Block
                .blit => self.blit(),
                .call => self.call(null),
                .vastart => self.vastart(),
                .byte_store,
                .double_store,
                .half_word_store,
                .long_store,
                .single_store,
                .word_store,
                => self.store(),
                .local_identifier => self.assignment(is_phi),
                // Flow
                .halt => self.halt(),
                .jump => self.jump(),
                .jump_not_zero => self.branch(),
                .@"return" => try self.@"return"(),
                .label_identifier => try self.fallThroughJump(),
                else => return error.InvalidBlockLine,
            };

            const end = self.previous_previous.span.end;

            if (!is_flow.*) {
                out = try self.new(
                    .{ .start = start, .end = end },
                    .{
                        .line = out,
                    },
                );
            }

            return out;
        }

        fn blockValue(self: *Self) !ast.StatementIndex {
            return try switch (self.previous.token_type) {
                .thread => self.threadIdentifier(),
                .global_identifier => self.globalIdentifier(),
                .local_identifier => self.localIdentifier(),
                .integer_literal => self.integer(),
                .single_literal => self.single(),
                .double_literal => self.double(),
                else => return error.InvalidBlockValue,
            };
        }

        fn assignment(self: *Self, is_phi: *bool) !ast.StatementIndex {
            const start = self.previous.span.start;

            const identifier = try self.localIdentifier();

            if (self.previous.token_type != .assign) return error.MissingEqual;
            _ = self.next();

            const data_type = try self.variableType();

            is_phi.* = switch (self.previous.token_type) {
                .phi => true,
                else => false,
            };

            const statement = try self.assignmentBody(data_type);

            const end = self.previous_previous.span.end;

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

        fn assignmentBody(self: *Self, data_type: ast.StatementIndex) !ast.StatementIndex {
            return try switch (self.previous.token_type) {
                .allocate => self.allocate(data_type),
                .call => self.call(data_type),
                .vaarg => self.vaarg(data_type),
                .negate => self.negate(data_type),
                .phi => self.phi(data_type),
                .cast => self.cast(data_type),
                .copy => self.copy(data_type),
                .byte_to_integer,
                .byte_to_integer_unsigned,
                .double_to_single,
                .double_to_integer_unsigned,
                .double_to_integer,
                .half_word_to_integer_unsigned,
                .half_word_to_integer,
                .long_to_float_unsigned,
                .long_to_float,
                .single_to_double,
                .single_to_integer_unsigned,
                .single_to_integer,
                .word_to_long_unsigned,
                .word_to_long,
                .word_to_float_unsigned,
                .word_to_float,
                => self.convert(data_type),
                .any_load,
                .byte_load_unsigned,
                .byte_load,
                .double_load,
                .half_word_load_unsigned,
                .half_word_load,
                .long_load,
                .single_load,
                .word_load_unsigned,
                .word_load,
                => self.load(data_type),
                .addition,
                .divide,
                .divide_unsigned,
                .multiply,
                .remainder,
                .remainder_unsigned,
                .subtract,
                .arthimetic_shift_right,
                .bitwise_and,
                .bitwise_or,
                .shift_right,
                .shift_left,
                .bitwise_xor,
                => self.binaryOperation(data_type),
                .double_equal,
                .long_equal,
                .single_equal,
                .word_equal,
                .double_not_equal,
                .long_not_equal,
                .single_not_equal,
                .word_not_equal,
                .double_greater_than_equal,
                .long_greater_than_equal,
                .long_greater_than_equal_unsigned,
                .single_greater_than_equal,
                .word_greater_than_equal,
                .word_greater_than_equal_unsigned,
                .double_greater_than,
                .long_greater_than,
                .long_greater_than_unsigned,
                .single_greater_than,
                .word_greater_than,
                .word_greater_than_unsigned,
                .double_less_than_equal,
                .long_less_than_equal,
                .long_less_than_equal_unsigned,
                .single_less_than_equal,
                .word_less_than_equal,
                .word_less_than_equal_unsigned,
                .double_less_than,
                .long_less_than,
                .long_less_than_unsigned,
                .single_less_than,
                .word_less_than,
                .word_less_than_unsigned,
                .double_all_nan,
                .single_all_nan,
                .double_any_nan,
                .single_any_nan,
                => self.comparison(data_type),
                else => {
                    std.log.err("{s}", .{@tagName(self.previous.token_type)});
                    return error.InvalidAssignment;
                },
            };
        }

        fn allocate(self: *Self, data_type: ast.StatementIndex) !ast.StatementIndex {
            const start = self.previous.span.start;

            if (self.previous.token_type != .allocate) return error.MissingAllocate;
            _ = self.next();

            const alignment = try self.integer();
            const size = try self.integer();

            const end = self.previous_previous.span.end;

            return self.new(
                .{ .start = start, .end = end },
                .{ .allocate = .{
                    .data_type = data_type,
                    .alignment = alignment,
                    .size = size,
                } },
            );
        }

        fn vastart(self: *Self) !ast.StatementIndex {
            const start = self.previous.span.start;

            if (self.previous.token_type != .vastart) return error.MissingVastart;
            _ = self.next();

            const parameter = try self.blockValue();

            const end = self.previous_previous.span.end;

            return self.new(
                .{ .start = start, .end = end },
                .{ .vastart = .{ .parameter = parameter } },
            );
        }

        fn vaarg(self: *Self, data_type: ast.StatementIndex) !ast.StatementIndex {
            const start = self.previous.span.start;

            if (self.previous.token_type != .vaarg) return error.MissingVaarg;
            _ = self.next();

            const parameter = try self.blockValue();

            const end = self.previous_previous.span.end;

            return self.new(
                .{ .start = start, .end = end },
                .{ .vaarg = .{
                    .data_type = data_type,
                    .parameter = parameter,
                } },
            );
        }

        fn negate(self: *Self, data_type: ast.StatementIndex) !ast.StatementIndex {
            const start = self.previous.span.start;

            if (self.previous.token_type != .negate) return error.MissingVaarg;
            _ = self.next();

            const value = try self.blockValue();

            const end = self.previous_previous.span.end;

            return self.new(
                .{ .start = start, .end = end },
                .{ .negate = .{
                    .data_type = data_type,
                    .value = value,
                } },
            );
        }

        fn blit(self: *Self) !ast.StatementIndex {
            const start = self.previous.span.start;

            if (self.previous.token_type != .blit) return error.MissingBlit;
            _ = self.next();

            const source = try self.blockValue();

            if (self.previous.token_type != .comma) return error.MissingComma;
            _ = self.next();

            const target = try self.blockValue();

            if (self.previous.token_type != .comma) return error.MissingComma;
            _ = self.next();

            const size = try self.blockValue();

            const end = self.previous_previous.span.end;

            return self.new(
                .{ .start = start, .end = end },
                .{ .blit = .{
                    .source = source,
                    .target = target,
                    .size = size,
                } },
            );
        }

        fn convert(self: *Self, data_type: ast.StatementIndex) !ast.StatementIndex {
            const start = self.previous.span.start;

            const data_type_statement = self.ast.get(data_type).?;

            const from_type: ast.StatementIndex = scope: {
                const primitive_type: ast.PrimitiveType = switch (self.previous.token_type) {
                    .byte_to_integer => .i8,
                    .byte_to_integer_unsigned => .u8,
                    .double_to_single => .f64,
                    .double_to_integer_unsigned => .f64,
                    .double_to_integer => .f64,
                    .half_word_to_integer_unsigned => .u16,
                    .half_word_to_integer => .i16,
                    .long_to_float_unsigned => .u64,
                    .long_to_float => .i64,
                    .single_to_double => .f32,
                    .single_to_integer_unsigned => .f32,
                    .single_to_integer => .f32,
                    .word_to_long_unsigned => .u32,
                    .word_to_long => .i32,
                    .word_to_float_unsigned => .u32,
                    .word_to_float => .i32,
                    else => unreachable,
                };

                break :scope try self.new(
                    self.previous.span,
                    .{ .primitive_type = primitive_type },
                );
            };

            const to_type: ast.StatementIndex = scope: {
                const primitive_type: ast.PrimitiveType = switch (self.previous.token_type) {
                    .byte_to_integer,
                    .byte_to_integer_unsigned,
                    .double_to_integer_unsigned,
                    .double_to_integer,
                    .half_word_to_integer_unsigned,
                    .half_word_to_integer,
                    .long_to_float_unsigned,
                    .long_to_float,
                    .single_to_integer_unsigned,
                    .single_to_integer,
                    .word_to_float_unsigned,
                    .word_to_float,
                    => break :scope try self.new(
                        data_type_statement.span,
                        data_type_statement.data,
                    ),
                    .double_to_single => .f32,
                    .single_to_double => .f64,
                    .word_to_long_unsigned => .u64,
                    .word_to_long => .i64,
                    else => unreachable,
                };

                break :scope try self.new(
                    self.previous.span,
                    .{ .primitive_type = primitive_type },
                );
            };

            const signed: bool = switch (self.previous.token_type) {
                .byte_to_integer_unsigned,
                .double_to_integer_unsigned,
                .half_word_to_integer_unsigned,
                .long_to_float_unsigned,
                .single_to_integer_unsigned,
                .word_to_float_unsigned,
                .word_to_long_unsigned,
                => false,
                else => true,
            };

            _ = self.next();

            const value = try self.blockValue();

            const end = self.previous_previous.span.end;

            return self.new(
                .{ .start = start, .end = end },
                .{ .convert = .{
                    .signed = signed,
                    .data_type = data_type,
                    .to_type = to_type,
                    .from_type = from_type,
                    .value = value,
                } },
            );
        }

        fn cast(self: *Self, data_type: ast.StatementIndex) !ast.StatementIndex {
            const start = self.previous.span.start;

            _ = self.next();

            const value = try self.blockValue();

            const end = self.previous_previous.span.end;

            return self.new(
                .{ .start = start, .end = end },
                .{ .cast = .{
                    .data_type = data_type,
                    .value = value,
                } },
            );
        }

        fn copy(self: *Self, data_type: ast.StatementIndex) !ast.StatementIndex {
            const start = self.previous.span.start;

            _ = self.next();

            const value = try self.blockValue();

            const end = self.previous_previous.span.end;

            return self.new(
                .{ .start = start, .end = end },
                .{ .copy = .{
                    .data_type = data_type,
                    .value = value,
                } },
            );
        }

        fn store(self: *Self) !ast.StatementIndex {
            const start = self.previous.span.start;

            const store_span = self.previous.span;
            const primitive_type: ast.PrimitiveType = switch (self.previous.token_type) {
                .byte_store => .i8,
                .double_store => .f64,
                .half_word_store => .i16,
                .long_store => .i64,
                .single_store => .f32,
                .word_store => .i32,
                else => return error.MissingStore,
            };

            const memory_type = try self.new(
                // +5 to skip "store"
                .{ .start = store_span.start + 5, .end = store_span.end },
                .{ .primitive_type = primitive_type },
            );

            _ = self.next();

            const value = try self.blockValue();

            if (self.previous.token_type != .comma) return error.MissingComma;
            _ = self.next();

            const address = try self.blockValue();

            const end = self.previous_previous.span.end;

            return self.new(
                .{ .start = start, .end = end },
                .{ .store = .{
                    .memory_type = memory_type,
                    .address = address,
                    .value = value,
                } },
            );
        }

        fn load(self: *Self, data_type: ast.StatementIndex) !ast.StatementIndex {
            const start = self.previous.span.start;

            const load_span = self.previous.span;
            const primitive_type: ast.PrimitiveType = switch (self.previous.token_type) {
                .any_load => .void,
                .byte_load_unsigned => .u8,
                .byte_load => .i8,
                .double_load => .f64,
                .half_word_load_unsigned => .u16,
                .half_word_load => .i16,
                .long_load => .i64,
                .single_load => .f32,
                .word_load_unsigned => .u32,
                .word_load => .i32,
                else => return error.MissingStore,
            };

            const memory_type = try self.new(
                // +4 to skip "load"
                .{ .start = load_span.start + 4, .end = load_span.end },
                .{ .primitive_type = primitive_type },
            );

            _ = self.next();

            const address = try self.blockValue();

            const end = self.previous_previous.span.end;

            return try self.new(
                .{ .start = start, .end = end },
                .{ .load = .{
                    .memory_type = memory_type,
                    .data_type = data_type,
                    .address = address,
                } },
            );
        }

        fn binaryOperation(self: *Self, data_type: ast.StatementIndex) !ast.StatementIndex {
            const start = self.previous.span.start;

            const operation_type: ast.BinaryOperationtype = switch (self.previous.token_type) {
                .addition => .addition,
                .divide => .divide,
                .divide_unsigned => .divide_unsigned,
                .multiply => .multiply,
                .remainder => .remainder,
                .remainder_unsigned => .remainder_unsigned,
                .subtract => .subtract,
                .arthimetic_shift_right => .arthimetic_shift_right,
                .bitwise_and => .@"and",
                .bitwise_or => .@"or",
                .shift_right => .logical_shift_right,
                .shift_left => .shift_left,
                .bitwise_xor => .xor,
                else => return error.InvalidBinaryOperation,
            };
            _ = self.next();

            const left = try self.blockValue();

            if (self.previous.token_type != .comma) return error.MissingComma;
            _ = self.next();

            const right = try self.blockValue();

            const end = self.previous_previous.span.end;

            return try self.new(
                .{ .start = start, .end = end },
                .{ .binary_operation = .{
                    .data_type = data_type,
                    .operation_type = operation_type,
                    .left = left,
                    .right = right,
                } },
            );
        }

        fn comparison(self: *Self, data_type: ast.StatementIndex) !ast.StatementIndex {
            const start = self.previous.span.start;

            const operation_type: ast.ComparisonOperationType = switch (self.previous.token_type) {
                .double_equal, .long_equal, .single_equal, .word_equal => .equal,
                .double_not_equal, .long_not_equal, .single_not_equal, .word_not_equal => .not_equal,
                .double_greater_than_equal, .long_greater_than_equal, .long_greater_than_equal_unsigned, .single_greater_than_equal, .word_greater_than_equal, .word_greater_than_equal_unsigned => .greater_than_equal,
                .double_greater_than, .long_greater_than, .long_greater_than_unsigned, .single_greater_than, .word_greater_than, .word_greater_than_unsigned => .greater_than,
                .double_less_than_equal, .long_less_than_equal, .long_less_than_equal_unsigned, .single_less_than_equal, .word_less_than_equal, .word_less_than_equal_unsigned => .less_than_equal,
                .double_less_than, .long_less_than, .long_less_than_unsigned, .single_less_than, .word_less_than, .word_less_than_unsigned => .less_than,
                .double_all_nan, .single_all_nan => .all_nan,
                .double_any_nan, .single_any_nan => .any_nan,
                else => return error.InvalidComparison,
            };

            const primitive_type: ast.PrimitiveType = switch (self.previous.token_type) {
                .double_equal, .double_not_equal, .double_greater_than_equal, .double_greater_than, .double_less_than_equal, .double_less_than, .double_all_nan, .double_any_nan => .f64,
                .long_equal, .long_not_equal, .long_greater_than_equal, .long_greater_than, .long_less_than_equal, .long_less_than => .i64,
                .long_greater_than_equal_unsigned, .long_greater_than_unsigned, .long_less_than_equal_unsigned, .long_less_than_unsigned => .u64,
                .single_equal, .single_not_equal, .single_greater_than_equal, .single_greater_than, .single_less_than_equal, .single_less_than, .single_all_nan, .single_any_nan => .f32,
                .word_equal, .word_not_equal, .word_greater_than_equal, .word_greater_than, .word_less_than_equal, .word_less_than => .i32,
                .word_greater_than_equal_unsigned, .word_greater_than_unsigned, .word_less_than_equal_unsigned, .word_less_than_unsigned => .u32,
                else => return error.InvalidComparison,
            };

            const comparison_type = try self.new(
                self.previous.span,
                .{ .primitive_type = primitive_type },
            );

            _ = self.next();

            const left = try self.blockValue();

            if (self.previous.token_type != .comma) return error.MissingComma;
            _ = self.next();

            const right = try self.blockValue();

            const end = self.previous_previous.span.end;

            return self.new(
                .{ .start = start, .end = end },
                .{ .comparison = .{
                    .data_type = data_type,
                    .operation_type = operation_type,
                    .comparison_type = comparison_type,
                    .left = left,
                    .right = right,
                } },
            );
        }

        fn phi(self: *Self, data_type: ast.StatementIndex) !ast.StatementIndex {
            const start = self.previous.span.start;

            if (self.previous.token_type != .phi) return error.MissingPhi;
            _ = self.next();

            var parameter_head: ?ast.StatementIndex = null;
            var parameter_tail: ?ast.StatementIndex = null;

            var first = true;
            while (true) {
                if (!first) {
                    if (self.previous.token_type != .comma) break;
                    _ = self.next();
                }

                const parameter_start = self.previous.span.start;
                const parameter = try self.phiParameter();
                const parameter_end = self.previous_previous.span.end;

                _ = try self.node(
                    &parameter_head,
                    &parameter_tail,
                    .{ .start = parameter_start, .end = parameter_end },
                    parameter,
                );

                first = false;
            }

            const end = self.previous_previous.span.end;

            return self.new(
                .{ .start = start, .end = end },
                .{
                    .phi = .{
                        .data_type = data_type,
                        .parameters = parameter_head orelse return error.EmptyPhi,
                    },
                },
            );
        }

        fn phiParameter(self: *Self) !ast.StatementIndex {
            const start = self.previous.span.start;

            const identifier = try self.labelIdentifier();
            const value = try self.blockValue();

            const end = self.previous_previous.span.end;

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

        fn call(self: *Self, data_type: ?ast.StatementIndex) !ast.StatementIndex {
            const start = self.previous.span.start;

            const return_type = scope: {
                if (data_type) |dt| {
                    break :scope dt;
                } else break :scope try self.new(
                    .{ .start = start, .end = start },
                    .{
                        .primitive_type = .void,
                    },
                );
            };

            if (self.previous.token_type != .call) return error.MissingCall;
            _ = self.next();

            const target = try self.blockValue();

            const parameters = try self.callParameters();

            const end = self.previous_previous.span.end;

            return self.new(
                .{ .start = start, .end = end },
                .{
                    .call = .{
                        .target = target,
                        .return_type = return_type,
                        .parameters = parameters,
                    },
                },
            );
        }

        fn callParameters(self: *Self) !?ast.StatementIndex {
            if (self.previous.token_type != .open_parenthesis) return error.MissingOpenParenthesis;
            _ = self.next();

            var parameter_head: ?ast.StatementIndex = null;
            var parameter_tail: ?ast.StatementIndex = null;

            var first = true;
            var hasVarArgs = false;
            while (self.previous.token_type != .close_parenthesis) {
                const param_start = self.previous.span.start;

                const param = try switch (self.previous.token_type) {
                    .env => switch (first) {
                        true => self.callEnvParameter(),
                        false => return error.InvalidEnv,
                    },
                    .variable_arguments => switch (first or hasVarArgs) {
                        true => return error.InvalidVarArgs,
                        false => scope: {
                            hasVarArgs = true;
                            break :scope self.varArgParameter();
                        },
                    },
                    else => self.callParameter(),
                };

                const param_end = self.previous_previous.span.end;

                _ = try self.node(
                    &parameter_head,
                    &parameter_tail,
                    .{ .start = param_start, .end = param_end },
                    param,
                );

                switch (self.previous.token_type) {
                    .close_parenthesis => break,
                    .comma => _ = self.next(),
                    else => return error.MissingComma,
                }

                first = false;
            }

            if (self.previous.token_type != .close_parenthesis) return error.MissingCloseParenthesis;

            _ = self.next();

            return parameter_head;
        }

        fn branch(self: *Self) !ast.StatementIndex {
            const start = self.previous.span.start;

            if (self.previous.token_type != .jump_not_zero) return error.MissingJump;
            _ = self.next();

            const condition = try self.blockValue();

            if (self.previous.token_type != .comma) return error.MissingComma;
            _ = self.next();

            const true_label = try self.labelIdentifier();

            if (self.previous.token_type != .comma) return error.MissingComma;
            _ = self.next();

            const false_label = try self.labelIdentifier();

            const end = self.previous_previous.span.end;

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
            if (self.previous.token_type != .halt) return error.MissingHalt;

            const span = self.previous.span;
            _ = self.next();

            return try self.new(
                span,
                .halt,
            );
        }

        fn fallThroughJump(self: *Self) !ast.StatementIndex {
            const start = self.previous.span.start;
            const label = try self.scopeIdentifier(.label_identifier, .label, false, true);
            const end = self.previous_previous.span.end;

            return try self.new(
                .{ .start = start, .end = end },
                .{ .jump = .{
                    .identifier = label,
                } },
            );
        }

        fn jump(self: *Self) !ast.StatementIndex {
            const start = self.previous.span.start;

            if (self.previous.token_type != .jump) return error.MissingJump;
            _ = self.next();

            const label = try self.labelIdentifier();

            const end = self.previous_previous.span.end;

            return try self.new(
                .{ .start = start, .end = end },
                .{ .jump = .{
                    .identifier = label,
                } },
            );
        }

        fn @"return"(self: *Self) !ast.StatementIndex {
            const start = self.previous.span.start;

            if (self.previous.token_type != .@"return") return error.MissingReturn;
            _ = self.next();

            const value: ?ast.StatementIndex = self.blockValue() catch undefined;

            const end = self.previous_previous.span.end;

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

fn testParser(allocator: std.mem.Allocator, buffer: anytype) ![]ast.Statement {
    var tree = try test_lib.testAST(allocator, buffer);
    defer tree.deinit();

    return try tree.toSlice(allocator);
}

fn assertParser(allocator: std.mem.Allocator, buffer: anytype, expected: []const ast.Statement) !void {
    const statements = try testParser(allocator, buffer);
    defer allocator.free(statements);

    try std.testing.expectEqualDeep(expected, statements);
}

fn assertParserType(allocator: std.mem.Allocator, buffer: anytype, expected: []const ast.StatementType) !void {
    const statements = try testParser(allocator, buffer);
    defer allocator.free(statements);

    try test_lib.assertStatementTypes(allocator, expected, statements);
}

//
// Valid Test
//

test "module" {
    // Arrange
    const allocator = std.testing.allocator;

    const file = "";
    const expected = [_]ast.Statement{
        .{
            .span = .{ .start = 0, .end = 0 },
            .data = .{
                .module = .{
                    .types = null,
                    .data = null,
                    .functions = null,
                },
            },
        },
    };

    // Act + Assert
    try assertParser(allocator, file, &expected);
}

test "type struct" {
    // Arrange
    const allocator = std.testing.allocator;

    const file = "type :t = { w } ";
    const expected = [_]ast.Statement{
        .{
            .span = .{ .start = 6, .end = 7 },
            .data = .{
                .identifier = .{
                    .scope = .type,
                },
            },
        },
        .{
            .span = .{ .start = 12, .end = 13 },
            .data = .{
                .primitive_type = .i32,
            },
        },
        .{
            .span = .{ .start = 12, .end = 13 },
            .data = .{
                .node = .{
                    .value = 1,
                    .next = null,
                    .previous = null,
                },
            },
        },
        .{
            .span = .{ .start = 10, .end = 15 },
            .data = .{
                .struct_type = .{
                    .alignment = null,
                    .members = 2,
                },
            },
        },
        .{
            .span = .{ .start = 0, .end = 15 },
            .data = .{
                .type_definition = .{
                    .identifier = 0,
                    .type = 3,
                },
            },
        },
        .{
            .span = .{ .start = 0, .end = 15 },
            .data = .{
                .node = .{
                    .value = 4,
                    .next = null,
                    .previous = null,
                },
            },
        },
        .{
            .span = .{ .start = 0, .end = file.len },
            .data = .{
                .module = .{
                    .types = 5,
                    .data = null,
                    .functions = null,
                },
            },
        },
    };

    // Act + Assert
    try assertParser(allocator, file, &expected);
}

test "type struct alignment" {
    // Arrange
    const allocator = std.testing.allocator;

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
    try assertParserType(allocator, file, &expected);
}

test "type struct trailing comma" {
    // Arrange
    const allocator = std.testing.allocator;

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
    try assertParserType(allocator, file, &expected);
}

test "type struct array" {
    // Arrange
    const allocator = std.testing.allocator;

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
    try assertParserType(allocator, file, &expected);
}

test "type struct custom type" {
    // Arrange
    const allocator = std.testing.allocator;

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
    try assertParserType(allocator, file, &expected);
}

test "type struct many members" {
    // Arrange
    const allocator = std.testing.allocator;

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
    try assertParserType(allocator, file, &expected);
}

test "type union" {
    // Arrange
    const allocator = std.testing.allocator;

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
    try assertParserType(allocator, file, &expected);
}

test "type union alignment" {
    // Arrange
    const allocator = std.testing.allocator;

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
    try assertParserType(allocator, file, &expected);
}

test "type opaque" {
    // Arrange
    const allocator = std.testing.allocator;

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
    try assertParserType(allocator, file, &expected);
}

test "type opaque alignment" {
    // Arrange
    const allocator = std.testing.allocator;

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
    try assertParserType(allocator, file, &expected);
}

test "data" {
    // Arrange
    const allocator = std.testing.allocator;

    const file = "data $d = { w 1 } ";
    const expected = [_]ast.Statement{
        .{
            .span = .{ .start = 0, .end = 0 },
            .data = .{
                .linkage = .{
                    .@"export" = false,
                    .thread = false,
                    .section = null,
                    .flags = null,
                },
            },
        },
        .{
            .span = .{ .start = 6, .end = 7 },
            .data = .{
                .identifier = .{
                    .scope = .global,
                },
            },
        },
        .{
            .span = .{ .start = 12, .end = 13 },
            .data = .{
                .primitive_type = .i32,
            },
        },
        .{
            .span = .{ .start = 14, .end = 15 },
            .data = .{
                .literal = .{
                    .type = .integer,
                },
            },
        },
        .{
            .span = .{ .start = 14, .end = 15 },
            .data = .{
                .typed_data = .{
                    .type = 2,
                    .value = 3,
                },
            },
        },
        .{
            .span = .{ .start = 14, .end = 15 },
            .data = .{
                .node = .{
                    .value = 4,
                    .previous = null,
                    .next = null,
                },
            },
        },
        .{
            .span = .{ .start = 0, .end = 17 },
            .data = .{
                .data_definition = .{
                    .linkage = 0,
                    .identifier = 1,
                    .values = 5,
                },
            },
        },
        .{
            .span = .{ .start = 0, .end = 17 },
            .data = .{
                .node = .{
                    .value = 6,
                    .previous = null,
                    .next = null,
                },
            },
        },
        .{
            .span = .{ .start = 0, .end = file.len },
            .data = .{
                .module = .{
                    .types = null,
                    .data = 7,
                    .functions = null,
                },
            },
        },
    };

    // Act + Assert
    try assertParser(allocator, file, &expected);
}

test "data with trailing comma" {
    // Arrange
    const allocator = std.testing.allocator;

    const file = "data $d = {w 1, }";
    const expected = [_]ast.StatementType{
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
    try assertParserType(allocator, file, &expected);
}

test "data with alignment" {
    // Arrange
    const allocator = std.testing.allocator;

    const file = "data $d = align 1 {w 1}";
    const expected = [_]ast.StatementType{
        .linkage,
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
    try assertParserType(allocator, file, &expected);
}

test "data with global" {
    // Arrange
    const allocator = std.testing.allocator;

    const file = "data $d = {l $o}";
    const expected = [_]ast.StatementType{
        .linkage,
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
    try assertParserType(allocator, file, &expected);
}

test "data with global offset" {
    // Arrange
    const allocator = std.testing.allocator;

    const file = "data $d = {l $o + 32 0}";
    const expected = [_]ast.StatementType{
        .linkage,
        .identifier,
        .primitive_type,
        .identifier,
        .literal,
        .offset,
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
    try assertParserType(allocator, file, &expected);
}

test "data with linkage" {
    // Arrange
    const allocator = std.testing.allocator;

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
    try assertParserType(allocator, file, &expected);
}

test "data with reused type" {
    // Arrange
    const allocator = std.testing.allocator;

    const file = "data $d = {w 1 2 3}";
    const expected = [_]ast.StatementType{
        .linkage,
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
    try assertParserType(allocator, file, &expected);
}

test "data with many types" {
    // Arrange
    const allocator = std.testing.allocator;

    const file = "data $d = {w 1, h 0, b \"test\"}";
    const expected = [_]ast.StatementType{
        .linkage,
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
    try assertParserType(allocator, file, &expected);
}

test "data with zeros" {
    // Arrange
    const allocator = std.testing.allocator;

    const file = "data $d = {z 1000}";
    const expected = [_]ast.StatementType{
        .linkage,
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
    try assertParserType(allocator, file, &expected);
}

test "function" {
    // Arrange
    const allocator = std.testing.allocator;

    const file = "function $fun() {@s ret}";
    const expected = [_]ast.Statement{
        .{
            .span = .{ .start = 0, .end = 0 },
            .data = .{
                .linkage = .{
                    .@"export" = false,
                    .thread = false,
                    .section = null,
                    .flags = null,
                },
            },
        },
        .{
            .span = .{ .start = 9, .end = 9 },
            .data = .{
                .primitive_type = .void,
            },
        },
        .{
            .span = .{ .start = 10, .end = 13 },
            .data = .{
                .identifier = .{
                    .scope = .global,
                },
            },
        },
        .{
            .span = .{ .start = 0, .end = 15 },
            .data = .{
                .function_signature = .{
                    .linkage = 0,
                    .return_type = 1,
                    .name = 2,
                    .parameters = null,
                },
            },
        },
        .{
            .span = .{ .start = 18, .end = 19 },
            .data = .{
                .identifier = .{
                    .scope = .label,
                },
            },
        },
        .{
            .span = .{ .start = 20, .end = 23 },
            .data = .{
                .@"return" = .{
                    .value = null,
                },
            },
        },
        .{
            .span = .{ .start = 17, .end = 23 },
            .data = .{
                .block = .{
                    .label = 4,
                    .phis = null,
                    .lines = null,
                    .flow = 5,
                },
            },
        },
        .{
            .span = .{ .start = 17, .end = 23 },
            .data = .{
                .node = .{
                    .value = 6,
                    .previous = null,
                    .next = null,
                },
            },
        },
        .{
            .span = .{ .start = 0, .end = 24 },
            .data = .{
                .function = .{
                    .signature = 3,
                    .body = 7,
                },
            },
        },
        .{
            .span = .{ .start = 0, .end = 24 },
            .data = .{
                .node = .{
                    .value = 8,
                    .previous = null,
                    .next = null,
                },
            },
        },
        .{
            .span = .{ .start = 0, .end = file.len },
            .data = .{
                .module = .{
                    .types = null,
                    .data = null,
                    .functions = 9,
                },
            },
        },
    };

    // Act + Assert
    try assertParser(allocator, file, &expected);
}

test "function with linkage" {
    // Arrange
    const allocator = std.testing.allocator;

    const file = "export thread section \"function\" \"flag\" function $fun() {@s ret}";
    const expected = [_]ast.StatementType{
        .literal,
        .literal,
        .linkage,
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
    try assertParserType(allocator, file, &expected);
}

test "function with primitive return type" {
    // Arrange
    const allocator = std.testing.allocator;

    const file = "function w $fun() {@s ret}";
    const expected = [_]ast.StatementType{
        .linkage,
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
    try assertParserType(allocator, file, &expected);
}

test "function with custom return type" {
    // Arrange
    const allocator = std.testing.allocator;

    const file = "function :type $fun() {@s ret}";
    const expected = [_]ast.StatementType{
        .linkage,
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
    try assertParserType(allocator, file, &expected);
}

test "function with custom type parameter" {
    // Arrange
    const allocator = std.testing.allocator;

    const file = "function $fun(:type %p) {@s ret}";
    const expected = [_]ast.StatementType{
        .linkage,
        .primitive_type,
        .identifier,
        .identifier,
        .identifier,
        .function_parameter,
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
    try assertParserType(allocator, file, &expected);
}

test "function with one parameter" {
    // Arrange
    const allocator = std.testing.allocator;

    const file = "function $fun(w %p) {@s ret}";
    const expected = [_]ast.StatementType{
        .linkage,
        .primitive_type,
        .identifier,
        .primitive_type,
        .identifier,
        .function_parameter,
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
    try assertParserType(allocator, file, &expected);
}

test "function with trailing comma" {
    // Arrange
    const allocator = std.testing.allocator;

    const file = "function $fun(w %p, ) {@s ret}";
    const expected = [_]ast.StatementType{
        .linkage,
        .primitive_type,
        .identifier,
        .primitive_type,
        .identifier,
        .function_parameter,
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
    try assertParserType(allocator, file, &expected);
}

test "function with many parameters" {
    // Arrange
    const allocator = std.testing.allocator;

    const file = "function $fun(w %p0, b %p1, h %p2) {@s ret}";
    const expected = [_]ast.StatementType{
        .linkage,
        .primitive_type,
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
        .identifier,
        .@"return",
        .block,
        .node,
        .function,
        .node,
        .module,
    };

    // Act + Assert
    try assertParserType(allocator, file, &expected);
}

test "function with env parameter" {
    // Arrange
    const allocator = std.testing.allocator;

    const file = "function $fun(env %e) {@s ret}";
    const expected = [_]ast.StatementType{
        .linkage,
        .primitive_type,
        .identifier,
        .env_type,
        .identifier,
        .function_parameter,
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
    try assertParserType(allocator, file, &expected);
}

test "function with variable parameter" {
    // Arrange
    const allocator = std.testing.allocator;

    const file = "function $fun(w %fmt, ...) {@s ret}";
    const expected = [_]ast.StatementType{
        .linkage,
        .primitive_type,
        .identifier,
        .primitive_type,
        .identifier,
        .function_parameter,
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
    try assertParserType(allocator, file, &expected);
}

test "function many flow" {
    // Arrange
    const allocator = std.testing.allocator;

    const file = "function $fun() {@a jmp @b @b jnz %p, @c, @d @c hlt @d ret}";
    const expected = [_]ast.StatementType{
        .linkage,
        .primitive_type,
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
    try assertParserType(allocator, file, &expected);
}

test "function fall-through block" {
    // Arrange
    const allocator = std.testing.allocator;

    const file = "function $fun() {@s @n ret}";
    const expected = [_]ast.StatementType{
        .linkage,
        .primitive_type,
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
    try assertParserType(allocator, file, &expected);
}

test "allocate" {
    // Arrange
    const allocator = std.testing.allocator;

    const file = "function $fun() {@s %x =w alloc4 32 %y =:type alloc16 64 ret}";
    const expected = [_]ast.StatementType{
        .linkage,
        .primitive_type,
        .identifier,
        .function_signature,
        .identifier,
        .identifier,
        .primitive_type,
        .literal,
        .literal,
        .allocate,
        .assignment,
        .line,
        .node,
        .identifier,
        .identifier,
        .literal,
        .literal,
        .allocate,
        .assignment,
        .line,
        .node,
        .@"return",
        .block,
        .node,
        .function,
        .node,
        .module,
    };

    // Act + Assert
    try assertParserType(allocator, file, &expected);
}

test "convert" {
    // Arrange
    const allocator = std.testing.allocator;

    const file = "function $fun() {@s %w =w extsw %l %z =s ultof $g ret}";
    const expected = [_]ast.StatementType{
        .linkage,
        .primitive_type,
        .identifier,
        .function_signature,
        .identifier,
        .identifier,
        .primitive_type,
        .primitive_type,
        .primitive_type,
        .identifier,
        .convert,
        .assignment,
        .line,
        .node,
        .identifier,
        .primitive_type,
        .primitive_type,
        .primitive_type,
        .identifier,
        .convert,
        .assignment,
        .line,
        .node,
        .@"return",
        .block,
        .node,
        .function,
        .node,
        .module,
    };

    // Act + Assert
    try assertParserType(allocator, file, &expected);
}

test "copy" {
    // Arrange
    const allocator = std.testing.allocator;

    const file = "function $fun() {@s %x =w copy 0 ret}";
    const expected = [_]ast.StatementType{
        .linkage,
        .primitive_type,
        .identifier,
        .function_signature,
        .identifier,
        .identifier,
        .primitive_type,
        .literal,
        .copy,
        .assignment,
        .line,
        .node,
        .@"return",
        .block,
        .node,
        .function,
        .node,
        .module,
    };

    // Act + Assert
    try assertParserType(allocator, file, &expected);
}

test "cast" {
    // Arrange
    const allocator = std.testing.allocator;

    const file = "function $fun() {@s %x =w cast 0 ret}";
    const expected = [_]ast.StatementType{
        .linkage,
        .primitive_type,
        .identifier,
        .function_signature,
        .identifier,
        .identifier,
        .primitive_type,
        .literal,
        .cast,
        .assignment,
        .line,
        .node,
        .@"return",
        .block,
        .node,
        .function,
        .node,
        .module,
    };

    // Act + Assert
    try assertParserType(allocator, file, &expected);
}

test "vastart" {
    // Arrange
    const allocator = std.testing.allocator;

    const file = "function $fun() {@s vastart %ap ret}";
    const expected = [_]ast.StatementType{
        .linkage,
        .primitive_type,
        .identifier,
        .function_signature,
        .identifier,
        .identifier,
        .vastart,
        .line,
        .node,
        .@"return",
        .block,
        .node,
        .function,
        .node,
        .module,
    };

    // Act + Assert
    try assertParserType(allocator, file, &expected);
}

test "vaarg" {
    // Arrange
    const allocator = std.testing.allocator;

    const file = "function $fun() {@s %l =w vaarg %ap ret}";
    const expected = [_]ast.StatementType{
        .linkage,
        .primitive_type,
        .identifier,
        .function_signature,
        .identifier,
        .identifier,
        .primitive_type,
        .identifier,
        .vaarg,
        .assignment,
        .line,
        .node,
        .@"return",
        .block,
        .node,
        .function,
        .node,
        .module,
    };

    // Act + Assert
    try assertParserType(allocator, file, &expected);
}

test "negate" {
    // Arrange
    const allocator = std.testing.allocator;

    const file = "function $fun() {@s %l =w neg %v ret}";
    const expected = [_]ast.StatementType{
        .linkage,
        .primitive_type,
        .identifier,
        .function_signature,
        .identifier,
        .identifier,
        .primitive_type,
        .identifier,
        .negate,
        .assignment,
        .line,
        .node,
        .@"return",
        .block,
        .node,
        .function,
        .node,
        .module,
    };

    // Act + Assert
    try assertParserType(allocator, file, &expected);
}

test "blit" {
    // Arrange
    const allocator = std.testing.allocator;

    const file = "function $fun() {@s blit %a, $g, 16 ret}";
    const expected = [_]ast.StatementType{
        .linkage,
        .primitive_type,
        .identifier,
        .function_signature,
        .identifier,
        .identifier,
        .identifier,
        .literal,
        .blit,
        .line,
        .node,
        .@"return",
        .block,
        .node,
        .function,
        .node,
        .module,
    };

    // Act + Assert
    try assertParserType(allocator, file, &expected);
}

test "binaryOperation" {
    // Arrange
    const allocator = std.testing.allocator;

    const file = "function $fun() {@s %t =w add %l, 0 %u =s div 0, $g ret}";
    const expected = [_]ast.StatementType{
        .linkage,
        .primitive_type,
        .identifier,
        .function_signature,
        .identifier,
        .identifier,
        .primitive_type,
        .identifier,
        .literal,
        .binary_operation,
        .assignment,
        .line,
        .node,
        .identifier,
        .primitive_type,
        .literal,
        .identifier,
        .binary_operation,
        .assignment,
        .line,
        .node,
        .@"return",
        .block,
        .node,
        .function,
        .node,
        .module,
    };

    // Act + Assert
    try assertParserType(allocator, file, &expected);
}

test "comparison" {
    // Arrange
    const allocator = std.testing.allocator;

    const file = "function $fun() {@s %t =w ceqw $g, 1 %u =s ceqs 0, %f ret}";
    const expected = [_]ast.StatementType{
        .linkage,
        .primitive_type,
        .identifier,
        .function_signature,
        .identifier,
        .identifier,
        .primitive_type,
        .primitive_type,
        .identifier,
        .literal,
        .comparison,
        .assignment,
        .line,
        .node,
        .identifier,
        .primitive_type,
        .primitive_type,
        .literal,
        .identifier,
        .comparison,
        .assignment,
        .line,
        .node,
        .@"return",
        .block,
        .node,
        .function,
        .node,
        .module,
    };

    // Act + Assert
    try assertParserType(allocator, file, &expected);
}

test "store" {
    // Arrange
    const allocator = std.testing.allocator;

    const file = "function $fun() {@s storew %l, 1 storeh 0, $g ret}";
    const expected = [_]ast.StatementType{
        .linkage,
        .primitive_type,
        .identifier,
        .function_signature,
        .identifier,
        .primitive_type,
        .identifier,
        .literal,
        .store,
        .line,
        .node,
        .primitive_type,
        .literal,
        .identifier,
        .store,
        .line,
        .node,
        .@"return",
        .block,
        .node,
        .function,
        .node,
        .module,
    };

    // Act + Assert
    try assertParserType(allocator, file, &expected);
}

test "load" {
    // Arrange
    const allocator = std.testing.allocator;

    const file = "function $fun() {@s %l1 =w loadw %x %l2 =w loadsb $g %l3 =d loadd 0 ret}";
    const expected = [_]ast.StatementType{
        .linkage,
        .primitive_type,
        .identifier,
        .function_signature,
        .identifier,
        .identifier,
        .primitive_type,
        .primitive_type,
        .identifier,
        .load,
        .assignment,
        .line,
        .node,
        .identifier,
        .primitive_type,
        .primitive_type,
        .identifier,
        .load,
        .assignment,
        .line,
        .node,
        .identifier,
        .primitive_type,
        .primitive_type,
        .literal,
        .load,
        .assignment,
        .line,
        .node,
        .@"return",
        .block,
        .node,
        .function,
        .node,
        .module,
    };

    // Act + Assert
    try assertParserType(allocator, file, &expected);
}

test "phi" {
    // Arrange
    const allocator = std.testing.allocator;

    const file = "function $fun() {@s %x =w phi @a 1, @b %l, @c $g ret}";
    const expected = [_]ast.StatementType{
        .linkage,
        .primitive_type,
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
        .line,
        .node,
        .@"return",
        .block,
        .node,
        .function,
        .node,
        .module,
    };

    // Act + Assert
    try assertParserType(allocator, file, &expected);
}

test "call" {
    // Arrange
    const allocator = std.testing.allocator;

    const file = "function $fun() {@s call $f() ret}";
    const expected = [_]ast.StatementType{
        .linkage,
        .primitive_type,
        .identifier,
        .function_signature,
        .identifier,
        .primitive_type,
        .identifier,
        .call,
        .line,
        .node,
        .@"return",
        .block,
        .node,
        .function,
        .node,
        .module,
    };

    // Act + Assert
    try assertParserType(allocator, file, &expected);
}

test "call assign" {
    // Arrange
    const allocator = std.testing.allocator;

    const file = "function $fun() {@s %x =w call $f() ret}";
    const expected = [_]ast.StatementType{
        .linkage,
        .primitive_type,
        .identifier,
        .function_signature,
        .identifier,
        .identifier,
        .primitive_type,
        .identifier,
        .call,
        .assignment,
        .line,
        .node,
        .@"return",
        .block,
        .node,
        .function,
        .node,
        .module,
    };

    // Act + Assert
    try assertParserType(allocator, file, &expected);
}

test "call parameters" {
    // Arrange
    const allocator = std.testing.allocator;

    const file = "function $fun() {@s call $f(w %a, :type %b) ret}";
    const expected = [_]ast.StatementType{
        .linkage,
        .primitive_type,
        .identifier,
        .function_signature,
        .identifier,
        .primitive_type,
        .identifier,
        .primitive_type,
        .identifier,
        .call_parameter,
        .node,
        .identifier,
        .identifier,
        .call_parameter,
        .node,
        .call,
        .line,
        .node,
        .@"return",
        .block,
        .node,
        .function,
        .node,
        .module,
    };

    // Act + Assert
    try assertParserType(allocator, file, &expected);
}

test "call varargs" {
    // Arrange
    const allocator = std.testing.allocator;

    const file = "function $fun() {@s call $f(w %a, ..., w %b) ret}";
    const expected = [_]ast.StatementType{
        .linkage,
        .primitive_type,
        .identifier,
        .function_signature,
        .identifier,
        .primitive_type,
        .identifier,
        .primitive_type,
        .identifier,
        .call_parameter,
        .node,
        .variadic_parameter,
        .node,
        .primitive_type,
        .identifier,
        .call_parameter,
        .node,
        .call,
        .line,
        .node,
        .@"return",
        .block,
        .node,
        .function,
        .node,
        .module,
    };

    // Act + Assert
    try assertParserType(allocator, file, &expected);
}

//
// Error Tests
//

test "data error.InvalidIdentifier" {
    // Arrange
    const allocator = std.testing.allocator;

    const file = "data";
    const expected = error.InvalidIdentifier;

    // Act
    const res = testParser(allocator, file);

    // Assert
    try std.testing.expectError(expected, res);
}

test "data error.InvalidIdentifier 2" {
    // Arrange
    const allocator = std.testing.allocator;

    const file = "data @d";
    const expected = error.InvalidIdentifier;

    // Act
    const res = testParser(allocator, file);

    // Assert
    try std.testing.expectError(expected, res);
}

test "data error.MissingEqual" {
    // Arrange
    const allocator = std.testing.allocator;

    const file = "data $d 1";
    const expected = error.MissingEqual;

    // Act
    const res = testParser(allocator, file);

    // Assert
    try std.testing.expectError(expected, res);
}

test "data error.InvalidPrimitiveType" {
    // Arrange
    const allocator = std.testing.allocator;

    const file = "data $d = {1}";
    const expected = error.InvalidPrimitiveType;

    // Act
    const res = testParser(allocator, file);

    // Assert
    try std.testing.expectError(expected, res);
}

test "function error.InvalidPrimitiveType" {
    // Arrange
    const allocator = std.testing.allocator;

    const file = "function";
    const expected = error.InvalidPrimitiveType;

    // Act
    const res = testParser(allocator, file);

    // Assert
    try std.testing.expectError(expected, res);
}

test "function error.InvalidPrimitiveType 2" {
    // Arrange
    const allocator = std.testing.allocator;

    const file = "function @fun() {@s ret}";
    const expected = error.InvalidPrimitiveType;

    // Act
    const res = testParser(allocator, file);

    // Assert
    try std.testing.expectError(expected, res);
}

test "function error.InvalidVarArgs" {
    // Arrange
    const allocator = std.testing.allocator;

    const file = "function $fun(...) {@s ret}";
    const expected = error.InvalidVarArgs;

    // Act
    const res = testParser(allocator, file);

    // Assert
    try std.testing.expectError(expected, res);
}

test "function error.InvalidIdentifier" {
    // Arrange
    const allocator = std.testing.allocator;

    const file = "function $fun(w) {@s ret}";
    const expected = error.InvalidIdentifier;

    // Act
    const res = testParser(allocator, file);

    // Assert
    try std.testing.expectError(expected, res);
}

test "function error.InvalidIdentifier 2" {
    // Arrange
    const allocator = std.testing.allocator;

    const file = "function $fun(w @a) {@s ret}";
    const expected = error.InvalidIdentifier;

    // Act
    const res = testParser(allocator, file);

    // Assert
    try std.testing.expectError(expected, res);
}

test "function error.EmptyFunctionBody" {
    // Arrange
    const allocator = std.testing.allocator;

    const file = "function $fun() {}";
    const expected = error.EmptyFunctionBody;

    // Act
    const res = testParser(allocator, file);

    // Assert
    try std.testing.expectError(expected, res);
}

test "function error.MissingCloseParenthesis" {
    // Arrange
    const allocator = std.testing.allocator;

    const file = "function $fun(w %a, ..., w %b) {}";
    const expected = error.MissingCloseParenthesis;

    // Act
    const res = testParser(allocator, file);

    // Assert
    try std.testing.expectError(expected, res);
}

test "function error.MissingCloseParenthesis 2" {
    // Arrange
    const allocator = std.testing.allocator;

    const file = "function $fun(w %a, ..., ) {}";
    const expected = error.MissingCloseParenthesis;

    // Act
    const res = testParser(allocator, file);

    // Assert
    try std.testing.expectError(expected, res);
}
