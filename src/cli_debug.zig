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

    const target: lib.Target = .{
        .arch = .a64,
    };

    var output = std.io.getStdOut().writer().any();
    const config: lib.EmitWriterConfig = .{
        .tty = .escape_codes,
    };

    for (files.items) |file_arg| {
        try run(
            allocator,
            file_arg,
            &output,
            &config,
            &target,
        );
    }
}

pub fn run(
    allocator: std.mem.Allocator,
    path: []const u8,
    output: *std.io.AnyWriter,
    config: *const lib.EmitWriterConfig,
    target: *const lib.Target,
) !void {
    _ = try output.write("=== File ===\n");

    const file_path = try std.fs.cwd().realpathAlloc(allocator, path);
    defer allocator.free(file_path);

    try config.tty.setColor(output, lib.Color.path);
    _ = try output.print("{s}\n", .{file_path});
    try config.tty.setColor(output, .reset);

    const file = try std.fs.openFileAbsolute(file_path, .{});
    defer file.close();

    var file_stream = std.io.StreamSource{ .file = file };

    try file.seekTo(0);
    var line_reader = file.reader();

    const newline_offsets = try lib.fileNewLines(allocator, &line_reader);
    defer allocator.free(newline_offsets);

    try file.seekTo(0);
    var file_reader = file.reader();

    _ = try output.write("=== Lexer ===\n");

    var tokens = std.ArrayList(lib.QBEToken).init(allocator);
    defer tokens.deinit();

    var lexer = lib.QBELexer(@TypeOf(file_reader), @TypeOf(tokens)).init(&file_reader, &tokens);
    lexer.lex() catch |err| {
        const position = try file_stream.getPos();

        try lib.printError(
            output,
            &config.tty,
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

        try config.tty.setColor(output, lib.Color.type);
        _ = try output.write(&type_column);
        try config.tty.setColor(output, .reset);

        try lib.printSpan(
            output,
            &config.tty,
            path,
            newline_offsets,
            &token.span,
        );
        _ = try output.writeByte('\n');
    }

    const token_slice = try tokens.toOwnedSlice();
    defer tokens.allocator.free(token_slice);

    var token_reader = lib.CollectionIterator(lib.QBEToken).init(token_slice);

    _ = try output.write("=== Parser ===\n");

    var ast = lib.AST.init(allocator);
    defer ast.deinit();

    var parser = lib.QBEParser(@TypeOf(token_reader)).init(&token_reader, &ast);
    _ = parser.parse() catch |err| {
        const span: lib.SourceSpan = scope: {
            if (parser.previous == undefined) {
                break :scope .{ .start = 0, .end = 0 };
            } else if (parser.previous == undefined) {
                break :scope parser.previous.span;
            } else {
                break :scope parser.previous_previous.span;
            }
        };

        try lib.printError(
            output,
            &config.tty,
            err,
            path,
            newline_offsets,
            &span,
        );

        return;
    };

    const entrypoint = ast.entrypoint().?;

    var ast_walk = lib.ASTWalk.init(allocator, &ast);
    defer ast_walk.deinit();

    var depth: usize = 0;

    try ast_walk.start(entrypoint);
    while (try ast_walk.next()) |out| {
        switch (out.enter) {
            true => {
                var type_column: [32]u8 = undefined;
                @memset(&type_column, ' ');
                const tag_name = @tagName(out.value.data);
                @memcpy(type_column[0..tag_name.len], tag_name);

                var depth_column: [4]u8 = undefined;
                @memset(&depth_column, ' ');
                _ = try std.fmt.bufPrint(&depth_column, "{}", .{depth});

                try config.tty.setColor(output, lib.Color.location);
                _ = try output.write(&depth_column);

                try config.tty.setColor(output, lib.Color.type);
                _ = try output.write(&type_column);

                try config.tty.setColor(output, .reset);

                try lib.printSpan(
                    output,
                    &config.tty,
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

    var symbol_table = lib.SymbolTable.init(allocator);
    defer symbol_table.deinit();

    try file.seekTo(0);

    var source_callback = lib.SymbolSourceWalkCallback.init(
        allocator,
        &symbol_table,
        &file_stream,
    );

    var error_exit = false;

    try ast_walk.start(entrypoint);
    while (try ast_walk.next()) |out| {
        _ = switch (out.enter) {
            true => source_callback.enter(out.value),
            false => source_callback.exit(out.value),
        } catch |err| {
            error_exit = true;

            try lib.printError(
                output,
                &config.tty,
                err,
                path,
                newline_offsets,
                &out.value.span,
            );
        };
    }

    if (error_exit) return;

    var memory_callback = lib.SymbolMemoryWalkCallback.init(
        allocator,
        &symbol_table,
    );
    defer memory_callback.deinit();

    error_exit = false;

    try ast_walk.start(entrypoint);
    while (try ast_walk.next()) |out| {
        _ = switch (out.enter) {
            true => memory_callback.enter(out.value),
            false => memory_callback.exit(out.value),
        } catch |err| {
            error_exit = true;

            try lib.printError(
                output,
                &config.tty,
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

        try config.tty.setColor(output, lib.Color.location);
        _ = try output.write(&index_column);

        try config.tty.setColor(output, lib.Color.reserved);
        _ = try output.print("{s} ", .{@tagName(symbol.identifier.scope)});

        try config.tty.setColor(output, lib.Color.type);
        _ = try output.print("{s} ", .{memoryLabel(&symbol.memory)});

        try config.tty.setColor(output, lib.Color.identifier);
        _ = try output.print("{s}", .{symbol.identifier.name});

        try config.tty.setColor(output, lib.Color.location);
        if (symbol.identifier.function) |index| {
            _ = try output.print(":{}\n", .{
                index,
            });
        } else {
            try output.writeByte('\n');
        }

        try config.tty.setColor(output, .reset);
    }

    _ = try output.write("=== Validate ===\n");

    var validate_callback = lib.SymbolValidateWalkCallback.init(
        allocator,
        &symbol_table,
        target,
    );
    defer validate_callback.deinit();

    error_exit = false;

    try ast_walk.start(entrypoint);
    while (try ast_walk.next()) |out| {
        _ = switch (out.enter) {
            true => validate_callback.enter(out.value),
            false => validate_callback.exit(out.value),
        } catch |err| {
            error_exit = true;

            try lib.printError(
                output,
                &config.tty,
                err,
                path,
                newline_offsets,
                &out.value.span,
            );
        };
    }

    if (error_exit) return;

    try config.tty.setColor(output, lib.Color.ok);
    _ = try output.write("OK\n");
    try config.tty.setColor(output, .reset);

    _ = try output.write("=== CFG ===\n");

    var cfg = lib.CFG.init(allocator);
    defer cfg.deinit();

    var cfg_walk = lib.CFGWalk.init(allocator, &cfg);
    defer cfg_walk.deinit();

    var cfg_callback = lib.CFGWalkCallback.init(
        allocator,
        &symbol_table,
        &cfg,
    );
    defer cfg_callback.deinit();

    try ast_walk.start(entrypoint);
    while (try ast_walk.next()) |out| {
        try switch (out.enter) {
            true => cfg_callback.enter(out.value),
            false => cfg_callback.exit(out.value),
        };
    }

    for (cfg.entrypoints.items) |fn_index| {
        try cfg_walk.start(fn_index);

        while (try cfg_walk.next()) |entry_index| {
            const entry_symbol = symbol_table.symbols.items[entry_index];
            const entry_node = cfg.nodes.get(entry_index).?;

            const identifier_color = switch (entry_symbol.memory) {
                .function => lib.Color.global,
                .label => lib.Color.label,
                else => unreachable,
            };

            try config.tty.setColor(output, identifier_color);
            _ = try output.write(entry_symbol.identifier.name);

            try config.tty.setColor(output, .reset);
            _ = try output.write(" -> ");

            switch (entry_node) {
                .enter, .jump => |next| {
                    const next_symbol = symbol_table.symbols.items[next];
                    try config.tty.setColor(output, lib.Color.label);
                    _ = try output.write(next_symbol.identifier.name);
                },
                .branch => |next| {
                    const left_symbol = symbol_table.symbols.items[next.left];
                    try config.tty.setColor(output, lib.Color.label);
                    _ = try output.write(left_symbol.identifier.name);

                    try config.tty.setColor(output, .reset);
                    _ = try output.write(", ");

                    const right_symbol = symbol_table.symbols.items[next.right];
                    try config.tty.setColor(output, lib.Color.label);
                    _ = try output.write(right_symbol.identifier.name);
                },
                .exit => {
                    try config.tty.setColor(output, lib.Color.literal);
                    _ = try output.write("$");
                },
            }

            _ = try output.write("\n");
        }

        try config.tty.setColor(output, .reset);
    }

    _ = try output.write("=== Dominance ===\n");

    var dom_sets = lib.DomSets.init(allocator);
    defer dom_sets.deinit();
    try dom_sets.build(&cfg);

    var dom_trees = lib.DomTrees.init(allocator);
    defer dom_trees.deinit();
    try dom_trees.build(&dom_sets);

    for (cfg.entrypoints.items) |fn_index| {
        try cfg_walk.start(fn_index);

        while (try cfg_walk.next()) |entry_index| {
            const entry_symbol = symbol_table.symbols.items[entry_index];

            const identifier_color = switch (entry_symbol.memory) {
                .function => lib.Color.global,
                .label => lib.Color.label,
                else => unreachable,
            };

            try config.tty.setColor(output, identifier_color);
            _ = try output.write(entry_symbol.identifier.name);

            if (dom_trees.collection.get(entry_index)) |next_index| {
                try config.tty.setColor(output, .reset);
                _ = try output.write(" <- ");

                const next_symbol = symbol_table.symbols.items[next_index];
                const next_color = switch (next_symbol.memory) {
                    .function => lib.Color.global,
                    .label => lib.Color.label,
                    else => unreachable,
                };

                try config.tty.setColor(output, next_color);
                _ = try output.write(next_symbol.identifier.name);
            }

            _ = try output.write("\n");
        }

        try config.tty.setColor(output, .reset);
    }

    _ = try output.write("=== QBE ===\n");

    var emit_callback = lib.QBEEmitWalkCallback.init(
        allocator,
        target,
    );
    defer emit_callback.deinit();

    try ast_walk.start(entrypoint);
    while (try ast_walk.next()) |out| {
        try switch (out.enter) {
            true => emit_callback.enter(out.value),
            false => emit_callback.exit(out.value),
        };
    }

    const emit_tokens = try emit_callback.tokens.toOwnedSlice();
    defer allocator.free(emit_tokens);
    var emit_token_reader = lib.CollectionIterator(lib.QBEToken).init(emit_tokens);

    var emit = lib.QBEEmitWriter(@TypeOf(emit_token_reader), std.io.AnyWriter).init(
        &emit_token_reader,
        output,
        config,
        &symbol_table,
    );

    while (try emit.next()) {}
}

fn memoryLabel(memory: *const lib.SymbolMemory) []const u8 {
    const value = memory.*;

    return switch (value) {
        .primitive => |p| @tagName(p),
        .empty => "_",
        .label => "@",
        else => @tagName(value),
    };
}
