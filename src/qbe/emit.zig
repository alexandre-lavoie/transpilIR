const std = @import("std");

const ast = @import("../ast/lib.zig");
const common = @import("../common.zig");
const symbol = @import("../symbol/lib.zig");

pub fn EmitWalkCallback(Writer: type) type {
    return struct {
        allocator: std.mem.Allocator,
        writer: *Writer,
        symbol_table: *symbol.SymbolTable,
        tty_config: *const std.io.tty.Config,

        const Self = @This();

        fn writer_write(self: *Self, bytes: []const u8) !void {
            _ = try self.writer.write(bytes);
        }

        fn writer_writeByte(self: *Self, byte: u8) !void {
            _ = try self.writer.writeByte(byte);
        }

        fn writer_print(self: *Self, comptime format: []const u8, args: anytype) !void {
            _ = try self.writer.print(format, args);
        }

        pub fn init(allocator: std.mem.Allocator, writer: *Writer, symbol_table: *symbol.SymbolTable, tty_config: *const std.io.tty.Config) Self {
            return Self{
                .allocator = allocator,
                .writer = writer,
                .tty_config = tty_config,
                .symbol_table = symbol_table,
            };
        }

        pub fn deinit(self: *Self) void {
            _ = self;
        }

        pub fn enter(self: *Self, statement: *ast.Statement) !void {
            const instance: symbol.Instance = .{ .span = statement.span };

            switch (statement.data) {
                .literal,
                .linkage,
                .node,
                .module,
                .data_definition,
                .typed_data,
                .offset,
                .array_type,
                .env_type,
                .opaque_type,
                .primitive_type,
                .struct_type,
                .type_definition,
                .union_type,
                .zero_type,
                .block,
                .line,
                .call,
                .call_parameter,
                .function,
                .function_signature,
                .function_parameter,
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
                .branch,
                .halt,
                .jump,
                .phi,
                .phi_parameter,
                .@"return",
                .binary_operation,
                .comparison,
                .negate,
                => {},
                .identifier => {
                    const symbol_index = self.symbol_table.getSymbolIndexByInstance(&instance) orelse unreachable;
                    const sym = &self.symbol_table.symbols.items[symbol_index];

                    try self.tty_config.setColor(self.writer, switch (sym.identifier.scope) {
                        .global => common.GLOBAL_COLOR,
                        .local => common.LOCAL_COLOR,
                        .type => common.TYPE_COLOR,
                        .label => common.LABEL_COLOR,
                    });

                    try switch (sym.identifier.scope) {
                        .local => self.writer_print("%l_{}", .{symbol_index}),
                        .type => self.writer_print(":t_{}", .{symbol_index}),
                        .global => self.writer_print("${s}", .{sym.identifier.name}),
                        .label => self.writer_print("@p_{}", .{symbol_index}),
                    };

                    try self.tty_config.setColor(self.writer, .reset);
                },
            }
        }

        pub fn exit(self: *Self, statement: *ast.Statement) !void {
            _ = self;

            switch (statement.data) {
                .identifier,
                .literal,
                .linkage,
                .node,
                .module,
                .data_definition,
                .typed_data,
                .offset,
                .array_type,
                .env_type,
                .opaque_type,
                .primitive_type,
                .struct_type,
                .type_definition,
                .union_type,
                .zero_type,
                .block,
                .line,
                .call,
                .call_parameter,
                .function,
                .function_signature,
                .function_parameter,
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
                .branch,
                .halt,
                .jump,
                .phi,
                .phi_parameter,
                .@"return",
                .binary_operation,
                .comparison,
                .negate,
                => {},
            }
        }
    };
}
