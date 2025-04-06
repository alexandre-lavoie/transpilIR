const std = @import("std");

const common = @import("../common.zig");

pub const LLVM = struct {
    allocator: std.mem.Allocator,
    optimization: common.Optimization,

    const Self = @This();

    pub fn init(allocator: std.mem.Allocator, optimization: common.Optimization) Self {
        return .{
            .allocator = allocator,
            .optimization = optimization,
        };
    }

    pub fn deinit(self: *Self) void {
        _ = self;
    }

    pub fn isSupported(ir: common.IR, assembly: common.Assembly) bool {
        switch (ir) {
            .c => {},
            .none,
            .qbe,
            => return false,
        }

        _ = Self.getTarget(assembly) catch return false;

        return true;
    }

    fn getTarget(assembly: common.Assembly) ![]const u8 {
        return switch (assembly) {
            .native => "",
            .amd64,
            .amd64_sysv,
            .x86_64,
            => "--target=x86_64",
            .wasm32 => "--target=wasm32",
            else => error.UnsupportedTarget,
        };
    }

    fn getExecutable(self: *const Self) !?[]const u8 {
        var env_map = std.process.getEnvMap(self.allocator) catch return null;
        defer env_map.deinit();

        if (env_map.get("LLVM")) |v| {
            const buffer = try self.allocator.alloc(u8, v.len);
            @memcpy(buffer, v);

            return buffer;
        } else {
            return null;
        }
    }

    pub fn assemble(
        self: *Self,
        file_path: []const u8,
        ir: common.IR,
        assembly: common.Assembly,
        reader: std.io.AnyReader,
        writer: std.io.AnyWriter,
    ) !bool {
        var args = std.ArrayList([]const u8).init(self.allocator);
        defer args.deinit();

        const executable = try self.getExecutable();
        defer {
            if (executable) |e| {
                self.allocator.free(e);
            }
        }

        try args.append(executable orelse "clang");

        try args.append("-Wno-int-conversion");
        try args.append("-Wno-incompatible-pointer-types");

        const optimization = switch (self.optimization) {
            .o1 => "-O1",
            .o2 => "-O2",
            .o3 => "-O3",
            else => "",
        };

        if (optimization.len > 0) {
            try args.append(optimization);
        }

        try args.append("-S");
        try args.append("-o-");

        const extension: []const u8 = switch (ir) {
            .c => "c",
            else => return error.UnsupportedIR,
        };

        try args.append("-x");
        try args.append(extension);

        const arch = try Self.getTarget(assembly);
        if (arch.len > 0) {
            try args.append(arch);
        }

        const file_map = try std.fmt.allocPrint(
            self.allocator,
            "-ffile-prefix-map=<stdin>={s}",
            .{file_path},
        );
        defer self.allocator.free(file_map);

        try args.append(file_map);

        try args.append("-");

        var cmd = std.process.Child.init(
            args.items,
            self.allocator,
        );
        cmd.stdin_behavior = .Pipe;
        cmd.stdout_behavior = .Pipe;

        try cmd.spawn();

        if (cmd.stdin) |stdin| {
            defer stdin.close();
            cmd.stdin = null;

            try common.readerToWriter(reader, stdin);
        }

        if (cmd.stdout) |stdout| {
            try common.readerToWriter(stdout, writer);
        }

        const term = try cmd.wait();

        return term.Exited == 0;
    }
};
