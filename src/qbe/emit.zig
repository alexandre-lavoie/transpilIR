const std = @import("std");

const ast = @import("../ast/lib.zig");
const symbol = @import("../symbol/lib.zig");

pub fn EmitWalkCallback(Writer: type) type {
    return struct {
        allocator: std.mem.Allocator,
        writer: *Writer,
        symbol_table: *symbol.SymbolTable,

        const Self = @This();

        fn writer_write(self: *Self, bytes: []const u8) !void {
            _ = try self.writer.write(bytes);
        }

        fn writer_writeByte(self: *Self, byte: u8) !void {
            _ = try self.writer.writeByte(byte);
        }

        pub fn init(allocator: std.mem.Allocator, writer: *Writer, symbol_table: *symbol.SymbolTable) Self {
            return Self{
                .allocator = allocator,
                .writer = writer,
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

                    const identifier = try switch (sym.identifier.scope) {
                        .local => std.fmt.allocPrint(self.allocator, "%l_{}", .{symbol_index}),
                        .type => std.fmt.allocPrint(self.allocator, ":t_{}", .{symbol_index}),
                        .global => std.fmt.allocPrint(self.allocator, "${s}", .{sym.identifier.name}),
                        .label => std.fmt.allocPrint(self.allocator, "@p_{}", .{symbol_index}),
                    };
                    defer self.allocator.free(identifier);

                    try self.writer_write(identifier);
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
