const std = @import("std");

const lib = @import("lib.zig");

pub fn main() !void {
    var gp = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gp.deinit();
    const allocator = gp.allocator();

    var args = std.process.args();
    _ = args.skip();

    var files = std.ArrayList([]const u8).init(allocator);
    defer files.deinit();

    while (args.next()) |file_arg| {
        try files.append(file_arg);
    }

    for (files.items) |file_arg| {
        try run(allocator, file_arg);
    }
}

pub fn run(allocator: std.mem.Allocator, path: []const u8) !void {
    var buffer: [4096]u8 = undefined;
    const file_path = try std.fs.cwd().realpath(path, &buffer);

    std.log.info("=== File ===", .{});
    std.log.info("{s}", .{file_path});

    const file = try std.fs.openFileAbsolute(file_path, .{});
    defer file.close();

    var file_stream = std.io.StreamSource{ .file = file };

    try file.seekTo(0);
    var line_reader = file.reader();

    const newline_offsets = try lib.common.fileNewLines(allocator, &line_reader);
    defer allocator.free(newline_offsets);

    try file.seekTo(0);
    var file_reader = file.reader();

    std.log.info("=== Lexer ===", .{});

    var tokens = std.ArrayList(lib.ssa.Token).init(allocator);
    defer tokens.deinit();

    var lexer = lib.ssa.Lexer(@TypeOf(file_reader), @TypeOf(tokens)).init(&file_reader, &tokens);
    lexer.lex() catch |err| {
        const position = try file_stream.getPos();

        lib.common.logError(
            err,
            path,
            newline_offsets,
            &.{ .start = position, .end = position + 1 },
        );

        return;
    };

    for (tokens.items) |token| {
        var type_column: [32]u8 = undefined;
        @memset(&type_column, ' ');
        const tag_name = @tagName(token.token_type);
        @memcpy(type_column[0..tag_name.len], tag_name);

        const start = lib.common.indexToFile(newline_offsets, token.span.start);
        const end = lib.common.indexToFile(newline_offsets, switch (token.span.end) {
            0 => 0,
            else => token.span.end - 1,
        });

        std.log.info("{s}{s}:{}:{}, {s}:{}:{}", .{
            type_column,
            path,
            start.line,
            start.column,
            path,
            end.line,
            end.column,
        });
    }

    const token_slice = try tokens.toOwnedSlice();
    defer tokens.allocator.free(token_slice);

    var token_reader = lib.ssa.TokenReader(@TypeOf(token_slice)).init(token_slice);

    std.log.info("=== Parser ===", .{});

    var ast = lib.ast.AST.init(allocator);
    defer ast.deinit();

    var parser = lib.ssa.Parser(@TypeOf(token_reader)).init(&token_reader, &ast);
    _ = parser.parse() catch |err| {
        const span: lib.common.SourceSpan = scope: {
            if (parser.previous == undefined) {
                break :scope .{ .start = 0, .end = 0 };
            } else if (parser.previous == undefined) {
                break :scope parser.previous.span;
            } else {
                break :scope parser.previous_previous.span;
            }
        };

        lib.common.logError(
            err,
            path,
            newline_offsets,
            &span,
        );

        return;
    };

    const entrypoint = ast.entrypoint() orelse unreachable;

    var walk = lib.ast.ASTWalk.init(allocator, &ast);
    defer walk.deinit();

    var depth: usize = 0;

    try walk.start(entrypoint);
    while (try walk.next()) |out| {
        switch (out.enter) {
            true => {
                var type_column: [32]u8 = undefined;
                @memset(&type_column, ' ');
                const tag_name = @tagName(out.value.data);
                @memcpy(type_column[0..tag_name.len], tag_name);

                var depth_column: [4]u8 = undefined;
                @memset(&depth_column, ' ');
                _ = try std.fmt.bufPrint(&depth_column, "{}", .{depth});

                const start = lib.common.indexToFile(newline_offsets, out.value.span.start);
                const end = lib.common.indexToFile(newline_offsets, switch (out.value.span.end) {
                    0 => 0,
                    else => out.value.span.end - 1,
                });

                std.log.info("{s}{s}{s}:{}:{}, {s}:{}:{}", .{
                    depth_column,
                    type_column,
                    path,
                    start.line,
                    start.column,
                    path,
                    end.line,
                    end.column,
                });

                depth += 1;
            },
            false => {
                depth -= 1;
            },
        }
    }

    std.log.info("=== Symbols ===", .{});

    var symbol_table = lib.symbol.SymbolTable.init(allocator);
    defer symbol_table.deinit();

    try file.seekTo(0);

    var source_callback = lib.symbol.SymbolSourceWalkCallback.init(allocator, &symbol_table, &file_stream);

    var error_exit = false;

    try walk.start(entrypoint);
    while (try walk.next()) |out| {
        _ = switch (out.enter) {
            true => source_callback.enter(out.value),
            false => source_callback.exit(out.value),
        } catch |err| {
            error_exit = true;

            lib.common.logError(
                err,
                path,
                newline_offsets,
                &out.value.span,
            );
        };
    }

    if (error_exit) return;

    var memory_callback = lib.symbol.SymbolMemoryWalkCallback.init(allocator, &symbol_table);
    defer memory_callback.deinit();

    error_exit = false;

    try walk.start(entrypoint);
    while (try walk.next()) |out| {
        _ = switch (out.enter) {
            true => memory_callback.enter(out.value),
            false => memory_callback.exit(out.value),
        } catch |err| {
            error_exit = true;

            lib.common.logError(
                err,
                path,
                newline_offsets,
                &out.value.span,
            );
        };
    }

    if (error_exit) return;

    for (0..symbol_table.symbols.items.len) |i| {
        const symbol = &symbol_table.symbols.items[i];

        var index_column: [8]u8 = undefined;
        @memset(&index_column, ' ');
        _ = try std.fmt.bufPrint(&index_column, "{}", .{i});

        if (symbol.identifier.function) |index| {
            std.log.info("{s}{s} {s} {s}:{}", .{ index_column, @tagName(symbol.identifier.scope), memoryLabel(&symbol.memory), symbol.identifier.name, index });
        } else {
            std.log.info("{s}{s} {s} {s}", .{ index_column, @tagName(symbol.identifier.scope), memoryLabel(&symbol.memory), symbol.identifier.name });
        }
    }

    std.log.info("=== Validate ===", .{});

    var validate_callback = lib.symbol.SymbolValidateWalkCallback.init(allocator, &symbol_table);
    defer validate_callback.deinit();

    error_exit = false;

    try walk.start(entrypoint);
    while (try walk.next()) |out| {
        _ = switch (out.enter) {
            true => validate_callback.enter(out.value),
            false => validate_callback.exit(out.value),
        } catch |err| {
            error_exit = true;

            lib.common.logError(
                err,
                path,
                newline_offsets,
                &out.value.span,
            );
        };
    }

    if (error_exit) return;
}

fn memoryLabel(memory: *const lib.symbol.SymbolMemory) []const u8 {
    const value = memory.*;

    return switch (value) {
        .primitive => |p| @tagName(p),
        .empty => "_",
        .label => "@",
        else => @tagName(value),
    };
}
