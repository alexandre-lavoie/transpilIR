const std = @import("std");

const common = @import("../common.zig");

pub const GCC = struct {
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

        switch (assembly) {
            .native => {},
            .none,
            .ir,
            => return false,
        }

        return true;
    }

    fn getExecutable(self: *const Self) !?[]const u8 {
        var env_map = std.process.getEnvMap(self.allocator) catch return null;
        defer env_map.deinit();

        if (env_map.get("GCC")) |v| {
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
    ) !void {
        var args = std.ArrayList([]const u8).init(self.allocator);
        defer args.deinit();

        const executable = try self.getExecutable();
        defer {
            if (executable) |e| {
                self.allocator.free(e);
            }
        }

        try args.append(executable orelse "gcc");

        try args.append("-fpermissive");

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

        const arch = switch (assembly) {
            .native => "",
            else => return error.UnsupportedTarget,
        };
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

        _ = try cmd.wait();
    }
};
