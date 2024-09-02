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

    var output = std.io.getStdOut().writer().any();
    const tty_config: std.io.tty.Config = .escape_codes;

    for (files.items) |file_arg| {
        try run(allocator, file_arg, &output, &tty_config);
    }
}

pub fn run(allocator: std.mem.Allocator, path: []const u8, output: *std.io.AnyWriter, tty_config: *const std.io.tty.Config) !void {
    _ = try output.write("=== File ===\n");

    const file_path = try std.fs.cwd().realpathAlloc(allocator, path);
    defer allocator.free(file_path);

    try tty_config.setColor(output, lib.common.PATH_COLOR);
    _ = try output.print("{s}\n", .{file_path});
    try tty_config.setColor(output, .reset);

    const file = try std.fs.openFileAbsolute(file_path, .{});
    defer file.close();

    var file_stream = std.io.StreamSource{ .file = file };

    try file.seekTo(0);
    var line_reader = file.reader();

    const newline_offsets = try lib.common.fileNewLines(allocator, &line_reader);
    defer allocator.free(newline_offsets);

    try file.seekTo(0);
    var file_reader = file.reader();

    _ = try output.write("=== Lexer ===\n");

    var tokens = std.ArrayList(lib.qbe.Token).init(allocator);
    defer tokens.deinit();

    var lexer = lib.qbe.Lexer(@TypeOf(file_reader), @TypeOf(tokens)).init(&file_reader, &tokens);
    lexer.lex() catch |err| {
        const position = try file_stream.getPos();

        try lib.common.printError(
            output,
            tty_config,
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

        try tty_config.setColor(output, lib.common.TYPE_COLOR);
        _ = try output.write(&type_column);
        try tty_config.setColor(output, .reset);

        try lib.common.printSpan(
            output,
            tty_config,
            path,
            newline_offsets,
            &token.span,
        );
        _ = try output.writeByte('\n');
    }

    const token_slice = try tokens.toOwnedSlice();
    defer tokens.allocator.free(token_slice);

    var token_reader = lib.qbe.TokenReader.init(token_slice);

    _ = try output.write("=== Parser ===\n");

    var ast = lib.ast.AST.init(allocator);
    defer ast.deinit();

    var parser = lib.qbe.Parser(@TypeOf(token_reader)).init(&token_reader, &ast);
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

        try lib.common.printError(
            output,
            tty_config,
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

                try tty_config.setColor(output, lib.common.LOCATION_COLOR);
                _ = try output.write(&depth_column);

                try tty_config.setColor(output, lib.common.TYPE_COLOR);
                _ = try output.write(&type_column);

                try tty_config.setColor(output, .reset);

                try lib.common.printSpan(
                    output,
                    tty_config,
                    path,
                    newline_offsets,
                    &out.value.span,
                );

                _ = try output.writeByte('\n');

                depth += 1;
            },
            false => {
                depth -= 1;
            },
        }
    }

    _ = try output.write("=== Symbols ===\n");

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

            try lib.common.printError(
                output,
                tty_config,
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

            try lib.common.printError(
                output,
                tty_config,
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

        try tty_config.setColor(output, lib.common.LOCATION_COLOR);
        _ = try output.write(&index_column);

        try tty_config.setColor(output, lib.common.RESERVED_COLOR);
        _ = try output.print("{s} ", .{@tagName(symbol.identifier.scope)});

        try tty_config.setColor(output, lib.common.TYPE_COLOR);
        _ = try output.print("{s} ", .{memoryLabel(&symbol.memory)});

        try tty_config.setColor(output, lib.common.IDENTIFIER_COLOR);
        _ = try output.print("{s}", .{symbol.identifier.name});

        try tty_config.setColor(output, lib.common.LOCATION_COLOR);
        if (symbol.identifier.function) |index| {
            _ = try output.print(":{}\n", .{
                index,
            });
        } else {
            try output.writeByte('\n');
        }

        try tty_config.setColor(output, .reset);
    }

    _ = try output.write("=== Validate ===\n");

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

            try lib.common.printError(
                output,
                tty_config,
                err,
                path,
                newline_offsets,
                &out.value.span,
            );
        };
    }

    if (error_exit) return;

    try tty_config.setColor(output, lib.common.OK_COLOR);
    _ = try output.write("OK\n");
    try tty_config.setColor(output, .reset);

    _ = try output.write("=== QBE ===\n");

    var token_callback = lib.qbe.TokenWalkCallback.init(
        allocator,
    );
    defer token_callback.deinit();

    try walk.start(entrypoint);
    while (try walk.next()) |out| {
        try switch (out.enter) {
            true => token_callback.enter(out.value),
            false => token_callback.exit(out.value),
        };
    }

    const emit_tokens = try token_callback.tokens.toOwnedSlice();
    defer allocator.free(emit_tokens);
    var emit_token_reader = lib.qbe.TokenReader.init(emit_tokens);

    var emit = lib.qbe.Emit(@TypeOf(emit_token_reader), std.io.AnyWriter).init(
        &emit_token_reader,
        output,
        tty_config,
        &symbol_table,
    );

    while (try emit.next()) {}
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
