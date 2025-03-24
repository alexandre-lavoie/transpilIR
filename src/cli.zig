const std = @import("std");

const lib = @import("lib.zig");

const Flag = enum(u8) {
    none = '\x00',
    no_color = 'b',
    help = 'h',
    output = 'o',
    source = 's',
    ir = 'r',
    compiler = 'c',
    target = 't',
    optimization = 'z',
    debug = 'd',
};

const DebugFlag = enum(usize) {
    source = 0b1,
    lexer = 0b10,
    parser = 0b100,
    memory = 0b1000,
    typing = 0b10000,
    optimization = 0b100000,
    ir = 0b1000000,
    target = 0b10000000,

    pub fn isEnabled(f: DebugFlag) bool {
        _ = f;

        return true;
    }
};

pub fn main() !void {
    const allocator = std.heap.page_allocator;

    var arg_iter = try std.process.ArgIterator.initWithAllocator(allocator);
    defer arg_iter.deinit();

    const debug = std.io.getStdErr();

    var args = Args.init(allocator, debug.writer().any());
    defer args.deinit();

    try args.parse(&arg_iter);

    if (args.help) {
        try writeHelp(args);
        return;
    }

    var ctx = Context.init(allocator);
    defer ctx.deinit();

    try run(
        allocator,
        &args,
        &ctx,
    );
}

const Source = enum {
    none,
    qbe,

    pub fn isEnabled(s: Source) bool {
        return switch (s) {
            .none => false,
            else => true,
        };
    }
};

const Compiler = enum {
    auto,
    gcc,
    llvm,
    qbe,
    irc,

    pub fn isEnabled(c: Compiler) bool {
        _ = c;

        return true;
    }
};

const Args = struct {
    allocator: std.mem.Allocator,
    executable: []const u8 = "",
    source: Source = default_source,
    source_path: []const u8 = "",
    ir: lib.IR = default_ir,
    assembly: lib.Assembly = default_assembly,
    assembly_path: []const u8 = "",
    arch: lib.Target = .{ .arch = .a64 },
    debug_flags: usize = 0,
    help: bool = false,
    config: lib.EmitWriterConfig = .{ .tty = .escape_codes },
    optimization: lib.Optimization = default_optimization,
    compiler: Compiler = .auto,
    debug: std.io.AnyWriter,

    const Self = @This();

    const default_source: Source = getDefault(Source);
    const default_ir: lib.IR = getDefault(lib.IR);
    const default_assembly: lib.Assembly = getDefault(lib.Assembly);
    const default_optimization: lib.Optimization = getDefault(lib.Optimization);

    pub fn init(allocator: std.mem.Allocator, debug: std.io.AnyWriter) Args {
        return .{
            .allocator = allocator,
            .debug = debug,
        };
    }

    pub fn deinit(self: Self) void {
        _ = self;
    }

    pub fn hasDebug(self: *const Self, flag: DebugFlag) bool {
        return (self.debug_flags & @intFromEnum(flag)) != 0;
    }

    pub fn setDebugColor(self: *const Self, color: std.io.tty.Color) !void {
        return self.config.tty.setColor(
            self.debug,
            color,
        );
    }

    pub fn parse(self: *Self, arg_iter: *std.process.ArgIterator) !void {
        self.executable = arg_iter.next() orelse return error.NoExecutable;

        var flag: Flag = .none;
        while (arg_iter.next()) |p| {
            const is_flag = p[0] == '-' and p.len == 2 and flag == .none;

            if (is_flag) {
                flag = std.meta.intToEnum(Flag, p[1]) catch return error.InvalidFlag;

                switch (flag) {
                    .help => {
                        self.help = true;
                        flag = .none;
                    },
                    .no_color => {
                        self.config.tty = .no_color;
                        flag = .none;
                    },
                    else => {},
                }
            } else {
                switch (flag) {
                    .help,
                    .no_color,
                    => {},
                    .none => {
                        if (self.source_path.len != 0) {
                            return error.InputAlreadySet;
                        }

                        self.source_path = p;
                    },
                    .source => {
                        self.source = std.meta.stringToEnum(Source, p) orelse return error.InvalidSource;

                        if (!self.source.isEnabled()) return error.InvalidSource;
                    },
                    .ir => {
                        self.ir = std.meta.stringToEnum(lib.IR, p) orelse return error.InvalidIR;

                        if (!self.ir.isEnabled()) return error.InvalidIR;
                    },
                    .target => {
                        self.assembly = std.meta.stringToEnum(lib.Assembly, p) orelse return error.InvalidTarget;

                        if (!self.assembly.isEnabled()) return error.InvalidTarget;
                    },
                    .output => {
                        self.assembly_path = p;
                    },
                    .optimization => {
                        self.optimization = std.meta.stringToEnum(lib.Optimization, p) orelse return error.InvalidOptimization;

                        if (!self.optimization.isEnabled()) return error.InvalidOptimization;
                    },
                    .compiler => {
                        self.compiler = std.meta.stringToEnum(Compiler, p) orelse return error.InvalidOptimization;

                        if (!self.compiler.isEnabled()) return error.InvalidOptimization;
                    },
                    .debug => {
                        var it = std.mem.split(u8, p, ",");

                        while (it.next()) |v| {
                            const debug_flag = std.meta.stringToEnum(DebugFlag, v) orelse return error.InvalidDebugFlag;

                            self.debug_flags |= @intFromEnum(debug_flag);
                        }
                    },
                }

                flag = .none;
            }
        }

        if (!self.help and self.source_path.len == 0) {
            return error.NoInput;
        }
    }

    fn getDefault(@"enum": type) @"enum" {
        inline for (std.meta.fields(@"enum")) |f| {
            const e: @"enum" = @enumFromInt(f.value);

            if (e.isEnabled()) return e;
        }

        return .none;
    }
};

pub fn writeHelp(args: Args) !void {
    try args.debug.print("{s} [OPTIONS] {{file,-}}\n", .{args.executable});

    inline for (std.meta.fields(Flag)) |f| {
        const flag: Flag = @enumFromInt(f.value);

        switch (flag) {
            .help => try args.debug.print("\t-h\t  prints this help message\n", .{}),
            .output => try args.debug.print("\t-o file\t  path to output file\n", .{}),
            .no_color => try args.debug.print("\t-b\t  disables colors\n", .{}),
            .source => {
                try args.debug.print("\t-s src\t  source IR\n\t\t  ", .{});

                try writeHelpEnum(args.debug, Source, Args.default_source);
            },
            .ir => {
                try args.debug.print("\t-r ir\t  target IR\n\t\t  ", .{});

                try writeHelpEnum(args.debug, lib.IR, Args.default_ir);
            },
            .target => {
                try args.debug.print("\t-t trgt\t  output target\n\t\t  ", .{});

                try writeHelpEnum(args.debug, lib.Assembly, Args.default_assembly);
            },
            .optimization => {
                try args.debug.print("\t-z opt\t  optimization level of compiler\n\t\t  ", .{});

                try writeHelpEnum(args.debug, lib.Optimization, Args.default_optimization);
            },
            .compiler => {
                try args.debug.print("\t-c cmp\t  IR compiler\n\t\t  ", .{});

                try writeHelpEnum(args.debug, Compiler, .auto);
            },
            .debug => {
                try args.debug.print("\t-d flgs\t  comma list of debug flags to activate\n\t\t  ", .{});

                try writeHelpEnum(args.debug, DebugFlag, null);
            },
            else => {},
        }
    }
}

fn writeHelpEnum(w: std.io.AnyWriter, @"enum": type, default: ?@"enum") !void {
    var is_first = true;
    inline for (std.meta.fields(@"enum")) |s| {
        const e: @"enum" = @enumFromInt(s.value);

        if (e.isEnabled()) {
            if (is_first) {
                is_first = false;
            } else {
                try w.print(", ", .{});
            }

            const def = switch (e == default) {
                true => " (default)",
                false => "",
            };

            try w.print("{s}{s}", .{ s.name, def });
        }
    }

    try w.print("\n", .{});
}

fn getMemoryLabel(memory: *const lib.SymbolMemory) []const u8 {
    const value = memory.*;

    return switch (value) {
        .primitive => |p| @tagName(p),
        .empty => "_",
        .label => "@",
        else => @tagName(value),
    };
}

const Context = struct {
    allocator: std.mem.Allocator,
    ast: lib.AST,
    symbol_table: lib.SymbolTable,
    transpile_buffer: std.ArrayList(u8),

    const Self = @This();

    fn init(allocator: std.mem.Allocator) Self {
        return .{
            .allocator = allocator,
            .ast = lib.AST.init(allocator),
            .symbol_table = lib.SymbolTable.init(allocator),
            .transpile_buffer = std.ArrayList(u8).init(allocator),
        };
    }

    fn deinit(self: *Self) void {
        self.ast.deinit();
        self.symbol_table.deinit();
        self.transpile_buffer.deinit();
    }
};

fn run(allocator: std.mem.Allocator, args: *const Args, ctx: *Context) !void {
    const Reader = std.io.AnyReader;
    const Writer = std.io.AnyWriter;

    const compiler_type: Compiler = l: {
        if (args.compiler != .auto) {
            break :l args.compiler;
        }

        if (lib.LLVM.isSupported(args.ir, args.assembly)) {
            break :l .llvm;
        }

        if (lib.GCC.isSupported(args.ir, args.assembly)) {
            break :l .gcc;
        }

        if (lib.QBE.isSupported(args.ir, args.assembly)) {
            break :l .qbe;
        }

        if (lib.IRC.isSupported(args.ir, args.assembly)) {
            break :l .irc;
        }

        return error.UnsupportedIRTarget;
    };

    const is_parsed = l: {
        switch (args.source) {
            .qbe => {
                const Token = lib.QBEToken;
                const TokenCollection = std.ArrayList(Token);
                const TokenIterator = lib.CollectionIterator(Token);
                const Lexer = lib.QBELexer(Reader, TokenCollection);
                const Parser = lib.QBEParser(TokenIterator);

                break :l try parse(
                    Token,
                    Lexer,
                    Parser,
                    allocator,
                    args,
                    ctx,
                );
            },
            .none => unreachable,
        }
    };

    if (!is_parsed) {
        return;
    }

    try optimize(
        allocator,
        args,
        ctx,
    );

    switch (args.ir) {
        .qbe => {
            const Token = lib.QBEToken;
            const TokenIterator = lib.CollectionIterator(Token);
            const EmitWalkCallback = lib.QBEEmitWalkCallback;
            const EmitWriter = lib.QBEEmitWriter(TokenIterator, Writer);

            try transpile(
                Token,
                EmitWalkCallback,
                EmitWriter,
                allocator,
                args,
                ctx,
            );
        },
        .c => {
            const Token = lib.CToken;
            const TokenIterator = lib.CollectionIterator(Token);
            const EmitWalkCallback = lib.CEmitWalkCallback;
            const EmitWriter = lib.CEmitWriter(TokenIterator, Writer);

            try transpile(
                Token,
                EmitWalkCallback,
                EmitWriter,
                allocator,
                args,
                ctx,
            );
        },
        .none => unreachable,
    }

    switch (compiler_type) {
        .irc => {
            var compiler = lib.IRC.init(allocator);
            compiler.deinit();

            try emit(
                &compiler,
                allocator,
                args,
                ctx,
            );
        },
        .gcc => {
            var compiler = lib.GCC.init(allocator, args.optimization);
            compiler.deinit();

            try emit(
                &compiler,
                allocator,
                args,
                ctx,
            );
        },
        .llvm => {
            var compiler = lib.LLVM.init(allocator, args.optimization);
            compiler.deinit();

            try emit(
                &compiler,
                allocator,
                args,
                ctx,
            );
        },
        .qbe => {
            var compiler = lib.QBE.init(allocator);
            compiler.deinit();

            try emit(
                &compiler,
                allocator,
                args,
                ctx,
            );
        },
        .auto => unreachable,
    }
}

fn parse(
    Token: type,
    Lexer: type,
    Parser: type,
    allocator: std.mem.Allocator,
    args: *const Args,
    ctx: *Context,
) !bool {
    const TokenCollection = std.ArrayList(Token);
    const TokenIterator = lib.CollectionIterator(Token);

    const debug = args.debug;

    if (args.hasDebug(.source)) {
        _ = try debug.write("=== Source ===\n");
    }

    const is_stdin = args.source_path.len == 1 and args.source_path[0] == '-';

    var file_stream: std.io.StreamSource = switch (is_stdin) {
        true => l: {
            const stdin = std.io.getStdIn();
            const reader = stdin.reader();

            var buffer = std.ArrayList(u8).init(allocator);
            const writer = buffer.writer();

            try lib.readerToWriter(reader, writer);

            const output: []u8 = try buffer.toOwnedSlice();

            break :l .{ .buffer = std.io.fixedBufferStream(output) };
        },
        false => l: {
            if (args.hasDebug(.source)) {
                try args.setDebugColor(lib.Color.path);
                _ = try debug.print("{s}\n", .{args.source_path});
                try args.setDebugColor(.reset);
            }

            const file = try std.fs.cwd().openFile(args.source_path, .{});

            break :l .{ .file = file };
        },
    };
    defer {
        switch (file_stream) {
            .buffer => |s| allocator.free(s.buffer),
            .file => |f| f.close(),
            else => unreachable,
        }
    }

    const input_newlines = l: {
        try file_stream.seekTo(0);
        var line_reader = file_stream.reader();

        break :l try lib.fileNewLines(allocator, &line_reader);
    };
    defer allocator.free(input_newlines);

    {
        if (args.hasDebug(.lexer)) {
            _ = try debug.write("=== Lexer ===\n");
        }

        try file_stream.seekTo(0);
        var file_reader = file_stream.reader().any();

        var input_tokens = TokenCollection.init(allocator);
        defer input_tokens.deinit();

        {
            var lexer = Lexer.init(&file_reader, &input_tokens);
            defer lexer.deinit();

            lexer.lex() catch |err| {
                const position = try file_stream.getPos();

                try lib.printError(
                    debug,
                    &args.config.tty,
                    err,
                    args.source_path,
                    input_newlines,
                    &.{ .start = position, .end = position + 1 },
                );

                return false;
            };
        }

        if (args.hasDebug(.lexer)) {
            try debugTokens(input_tokens.items, args, input_newlines);
        }

        const input_token_slice = try input_tokens.toOwnedSlice();
        defer input_tokens.allocator.free(input_token_slice);

        var input_token_reader = TokenIterator.init(input_token_slice);

        if (args.hasDebug(.parser)) {
            _ = try debug.write("=== Parser ===\n");
        }

        {
            var parser = Parser.init(&input_token_reader, &ctx.ast);
            defer parser.deinit();

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
                    debug,
                    &args.config.tty,
                    err,
                    args.source_path,
                    input_newlines,
                    &span,
                );

                return false;
            };
        }
    }

    const entrypoint = ctx.ast.entrypoint() orelse return error.NoEntrypoint;

    if (args.hasDebug(.parser)) {
        var depth: usize = 0;

        var ast_walk = lib.ASTWalk.init(allocator, &ctx.ast);
        defer ast_walk.deinit();

        try ast_walk.start(entrypoint);
        while (try ast_walk.next()) |out| {
            const stat = ctx.ast.getPtr(out.index) orelse return error.NotFound;

            switch (out.state) {
                .enter => {
                    var type_column: [32]u8 = undefined;
                    @memset(&type_column, ' ');
                    const tag_name = @tagName(stat.data);
                    @memcpy(type_column[0..tag_name.len], tag_name);

                    var depth_column: [4]u8 = undefined;
                    @memset(&depth_column, ' ');
                    _ = try std.fmt.bufPrint(&depth_column, "{}", .{depth});

                    try args.setDebugColor(lib.Color.location);
                    _ = try debug.write(&depth_column);

                    try args.setDebugColor(lib.Color.type);
                    _ = try debug.write(&type_column);

                    try args.setDebugColor(.reset);

                    try lib.printSpan(
                        debug,
                        &args.config.tty,
                        args.source_path,
                        input_newlines,
                        &stat.span,
                    );

                    _ = try debug.writeByte('\n');

                    depth += 1;
                },
                .exit => {
                    depth -= 1;
                },
                .middle => {},
            }
        }
    }

    {
        if (args.hasDebug(.memory)) {
            _ = try debug.write("=== Symbols ===\n");
        }

        try file_stream.seekTo(0);

        var error_exit = false;

        var callback = lib.SymbolSourceWalkCallback.init(
            allocator,
            &ctx.symbol_table,
            &file_stream,
        );

        var ast_walk = lib.ASTWalk.init(allocator, &ctx.ast);
        defer ast_walk.deinit();

        try ast_walk.start(entrypoint);
        while (try ast_walk.next()) |out| {
            const stat = ctx.ast.get(out.index) orelse return error.NotFound;

            _ = switch (out.state) {
                .enter => callback.enter(stat),
                .exit => callback.exit(stat),
                .middle => {},
            } catch |err| {
                error_exit = true;

                try lib.printError(
                    debug,
                    &args.config.tty,
                    err,
                    args.source_path,
                    input_newlines,
                    &stat.span,
                );
            };
        }

        if (error_exit) return false;

        if (args.hasDebug(.memory)) {
            try args.setDebugColor(lib.Color.ok);
            _ = try debug.write("OK\n");
            try args.setDebugColor(.reset);
        }
    }

    {
        if (args.hasDebug(.memory)) {
            _ = try debug.write("=== Memory ===\n");
        }

        var error_exit = false;

        var callback = lib.SymbolMemoryWalkCallback.init(
            allocator,
            &ctx.symbol_table,
        );
        defer callback.deinit();

        var ast_walk = lib.ASTWalk.init(allocator, &ctx.ast);
        defer ast_walk.deinit();

        try ast_walk.start(entrypoint);
        while (try ast_walk.next()) |out| {
            const stat = ctx.ast.get(out.index) orelse return error.NotFound;

            _ = switch (out.state) {
                .enter => callback.enter(stat),
                .exit => callback.exit(stat),
                .middle => {},
            } catch |err| {
                error_exit = true;

                try lib.printError(
                    debug,
                    &args.config.tty,
                    err,
                    args.source_path,
                    input_newlines,
                    &stat.span,
                );
            };
        }

        if (error_exit) return false;

        if (args.hasDebug(.memory)) {
            for (0..ctx.symbol_table.symbols.items.len) |i| {
                const symbol = &ctx.symbol_table.symbols.items[i];

                var index_column: [8]u8 = undefined;
                @memset(&index_column, ' ');
                _ = try std.fmt.bufPrint(&index_column, "{}", .{i});

                try args.setDebugColor(lib.Color.location);
                _ = try debug.write(&index_column);

                try args.setDebugColor(lib.Color.reserved);
                _ = try debug.print("{s} ", .{@tagName(symbol.identifier.scope)});

                try args.setDebugColor(lib.Color.type);
                _ = try debug.print("{s} ", .{getMemoryLabel(&symbol.memory)});

                try args.setDebugColor(lib.Color.identifier);
                _ = try debug.print("{s}", .{symbol.identifier.name});

                try args.setDebugColor(lib.Color.location);
                if (symbol.identifier.function) |index| {
                    _ = try debug.print(":{}\n", .{
                        index,
                    });
                } else {
                    try debug.writeByte('\n');
                }

                try args.setDebugColor(.reset);
            }
        }
    }

    {
        if (args.hasDebug(.typing)) {
            _ = try debug.write("=== Typing ===\n");
        }

        var error_exit = false;

        var callback = lib.SymbolValidateWalkCallback.init(
            allocator,
            &ctx.symbol_table,
            &args.arch,
        );
        defer callback.deinit();

        var ast_walk = lib.ASTWalk.init(allocator, &ctx.ast);
        defer ast_walk.deinit();

        try ast_walk.start(entrypoint);
        while (try ast_walk.next()) |out| {
            const stat = ctx.ast.getPtr(out.index) orelse return error.NotFound;

            _ = switch (out.state) {
                .enter => callback.enter(stat),
                .exit => callback.exit(stat),
                .middle => {},
            } catch |err| {
                error_exit = true;

                try lib.printError(
                    debug,
                    &args.config.tty,
                    err,
                    args.source_path,
                    input_newlines,
                    &stat.span,
                );
            };
        }

        if (error_exit) return false;

        if (args.hasDebug(.typing)) {
            try args.setDebugColor(lib.Color.ok);
            _ = try debug.write("OK\n");
            try args.setDebugColor(.reset);
        }
    }

    return true;
}

fn optimize(allocator: std.mem.Allocator, args: *const Args, ctx: *Context) !void {
    const debug = args.debug;

    const ssa_optimization = false;

    const entrypoint = ctx.ast.entrypoint().?;

    var ast_walk = lib.ASTWalk.init(allocator, &ctx.ast);
    defer ast_walk.deinit();

    if (ssa_optimization) {
        if (args.hasDebug(.optimization)) {
            _ = try debug.write("=== CFG ===\n");
        }

        var cfg = lib.CFG.init(allocator);
        defer cfg.deinit();

        var cfg_walk = lib.CFGWalk.init(allocator, &cfg);
        defer cfg_walk.deinit();

        var cfg_callback = lib.CFGWalkCallback.init(
            allocator,
            &ctx.symbol_table,
            &cfg,
        );
        defer cfg_callback.deinit();

        try ast_walk.start(entrypoint);
        while (try ast_walk.next()) |out| {
            const stat = ctx.ast.getPtr(out.index) orelse return error.NotFound;

            try switch (out.state) {
                .enter => cfg_callback.enter(out.index, stat),
                .exit => cfg_callback.exit(stat),
                .middle => {},
            };
        }

        if (args.hasDebug(.optimization)) {
            for (cfg.entrypoints.items) |fn_index| {
                try cfg_walk.start(fn_index);

                while (try cfg_walk.next()) |entry_index| {
                    const entry_symbol_index = try lib.ast_utils.getStatementIdentifierByIndex(
                        entry_index,
                        &ctx.ast,
                        &ctx.symbol_table,
                    );
                    const entry_symbol = ctx.symbol_table.getSymbolPtr(entry_symbol_index).?;

                    const entry_node = cfg.nodes.get(entry_index).?;

                    const identifier_color = switch (entry_symbol.memory) {
                        .function => lib.Color.global,
                        .label => lib.Color.label,
                        else => unreachable,
                    };

                    try args.setDebugColor(identifier_color);
                    _ = try debug.write(entry_symbol.identifier.name);

                    try args.setDebugColor(.reset);
                    _ = try debug.write(" -> ");

                    switch (entry_node) {
                        .enter, .jump => |next| {
                            const next_symbol_index = try lib.ast_utils.getStatementIdentifierByIndex(
                                next,
                                &ctx.ast,
                                &ctx.symbol_table,
                            );
                            const next_symbol = ctx.symbol_table.getSymbolPtr(next_symbol_index).?;

                            try args.setDebugColor(lib.Color.label);
                            _ = try debug.write(next_symbol.identifier.name);
                        },
                        .branch => |next| {
                            const left_symbol_index = try lib.ast_utils.getStatementIdentifierByIndex(
                                next.left,
                                &ctx.ast,
                                &ctx.symbol_table,
                            );
                            const left_symbol = ctx.symbol_table.getSymbolPtr(left_symbol_index).?;

                            try args.setDebugColor(lib.Color.label);
                            _ = try debug.write(left_symbol.identifier.name);

                            try args.setDebugColor(.reset);
                            _ = try debug.write(", ");

                            const right_symbol_index = try lib.ast_utils.getStatementIdentifierByIndex(
                                next.right,
                                &ctx.ast,
                                &ctx.symbol_table,
                            );
                            const right_symbol = ctx.symbol_table.getSymbolPtr(right_symbol_index).?;

                            try args.setDebugColor(lib.Color.label);
                            _ = try debug.write(right_symbol.identifier.name);
                        },
                        .exit => {
                            try args.setDebugColor(lib.Color.literal);
                            _ = try debug.write("$");
                        },
                    }

                    _ = try debug.write("\n");
                }

                try args.setDebugColor(.reset);
            }
        }

        if (args.hasDebug(.optimization)) {
            _ = try debug.write("=== Dominance ===\n");
        }

        var dom_sets = lib.DomSets.init(allocator);
        defer dom_sets.deinit();
        try dom_sets.build(&cfg);

        var dom_trees = lib.DomTrees.init(allocator);
        defer dom_trees.deinit();
        try dom_trees.build(&dom_sets);

        if (args.hasDebug(.optimization)) {
            for (cfg.entrypoints.items) |fn_index| {
                try cfg_walk.start(fn_index);

                while (try cfg_walk.next()) |entry_index| {
                    const entry_symbol_index = try lib.ast_utils.getStatementIdentifierByIndex(
                        entry_index,
                        &ctx.ast,
                        &ctx.symbol_table,
                    );
                    const entry_symbol = ctx.symbol_table.getSymbolPtr(entry_symbol_index).?;

                    const identifier_color = switch (entry_symbol.memory) {
                        .function => lib.Color.global,
                        .label => lib.Color.label,
                        else => unreachable,
                    };

                    try args.setDebugColor(identifier_color);
                    _ = try debug.write(entry_symbol.identifier.name);

                    if (dom_trees.collection.get(entry_index)) |next_index| {
                        try args.setDebugColor(.reset);
                        _ = try debug.write(" <- ");

                        const next_symbol_index = try lib.ast_utils.getStatementIdentifierByIndex(
                            next_index,
                            &ctx.ast,
                            &ctx.symbol_table,
                        );
                        const next_symbol = ctx.symbol_table.getSymbolPtr(next_symbol_index).?;

                        const next_color = switch (next_symbol.memory) {
                            .function => lib.Color.global,
                            .label => lib.Color.label,
                            else => unreachable,
                        };

                        try args.setDebugColor(next_color);
                        _ = try debug.write(next_symbol.identifier.name);
                    }

                    _ = try debug.write("\n");
                }

                try args.setDebugColor(.reset);
            }
        }

        if (args.hasDebug(.optimization)) {
            _ = try debug.write("=== SSA ===\n");
        }

        var ssa = lib.SSA.init(
            allocator,
            &ctx.ast,
            &ctx.symbol_table,
        );
        try ssa.build();

        if (args.hasDebug(.optimization)) {
            try args.setDebugColor(lib.Color.ok);
            _ = try debug.write("OK\n");
            try args.setDebugColor(.reset);
        }
    }
}

fn transpile(
    Token: type,
    EmitWalkCallback: type,
    EmitWriter: type,
    allocator: std.mem.Allocator,
    args: *const Args,
    ctx: *Context,
) !void {
    const TokenIterator = lib.CollectionIterator(Token);

    var debug = args.debug;

    if (args.hasDebug(.ir)) {
        _ = try debug.write("=== IR ===\n");
    }

    var callback = EmitWalkCallback.init(
        allocator,
        &args.arch,
        &ctx.symbol_table,
    );
    defer callback.deinit();

    const entrypoint = ctx.ast.entrypoint().?;

    var ast_walk = lib.ASTWalk.init(allocator, &ctx.ast);
    defer ast_walk.deinit();

    try ast_walk.start(entrypoint);
    while (try ast_walk.next()) |out| {
        const stat = ctx.ast.getPtr(out.index) orelse return error.NotFound;

        try switch (out.state) {
            .enter => callback.enter(stat),
            .middle => callback.middle(stat, out.previous.?, out.next.?),
            .exit => callback.exit(stat),
        };
    }

    const tokens = try callback.tokens.toOwnedSlice();
    defer allocator.free(tokens);

    if (args.hasDebug(.ir)) {
        var token_reader = TokenIterator.init(tokens);

        var ew = EmitWriter.init(
            &token_reader,
            &debug,
            &args.config,
            &ctx.symbol_table,
        );

        while (try ew.next()) {}
    }

    {
        var token_reader = TokenIterator.init(tokens);

        var writer = ctx.transpile_buffer.writer().any();

        const config: lib.EmitWriterConfig = .{ .tty = .no_color };

        var ew = EmitWriter.init(
            &token_reader,
            &writer,
            &config,
            &ctx.symbol_table,
        );

        while (try ew.next()) {}
    }
}

fn emit(
    compiler: anytype,
    allocator: std.mem.Allocator,
    args: *const Args,
    ctx: *Context,
) !void {
    _ = allocator;

    var debug = args.debug;

    if (args.hasDebug(.target)) {
        _ = try debug.write("=== Target ===\n");
    }

    const is_stdout = args.assembly_path.len == 0;

    const file = switch (is_stdout) {
        true => std.io.getStdOut(),
        false => l: {
            if (args.hasDebug(.target)) {
                try args.setDebugColor(lib.Color.path);
                _ = try debug.print("{s}\n", .{args.assembly_path});
                try args.setDebugColor(.reset);
            }

            break :l try std.fs.cwd().createFile(args.assembly_path, .{});
        },
    };
    defer file.close();

    var buffer = std.io.fixedBufferStream(ctx.transpile_buffer.items);
    const reader = buffer.reader().any();

    const writer = file.writer().any();

    const is_stdin = args.source_path.len == 1 and args.source_path[0] == '-';

    const file_path = switch (is_stdin) {
        true => "<stdin>",
        else => args.source_path,
    };

    try compiler.assemble(
        file_path,
        args.ir,
        args.assembly,
        reader,
        writer,
    );
}

fn debugTokens(tokens: anytype, args: *const Args, newline_offsets: []usize) !void {
    for (tokens) |token| {
        var type_column: [32]u8 = undefined;
        @memset(&type_column, ' ');
        const tag_name = @tagName(token.token_type);
        @memcpy(type_column[0..tag_name.len], tag_name);

        try args.setDebugColor(lib.Color.type);
        _ = try args.debug.write(&type_column);
        try args.setDebugColor(.reset);

        try lib.printSpan(
            args.debug,
            &args.config.tty,
            args.source_path,
            newline_offsets,
            &token.span,
        );
        _ = try args.debug.writeByte('\n');
    }
}
