const std = @import("std");

const common = @import("../common.zig");

pub const QBE = struct {
    allocator: std.mem.Allocator,

    const Self = @This();

    pub fn init(allocator: std.mem.Allocator) Self {
        return .{
            .allocator = allocator,
        };
    }

    pub fn deinit(self: *Self) void {
        _ = self;
    }

    pub fn isSupported(ir: common.IR, assembly: common.Assembly) bool {
        switch (ir) {
            .qbe => {},
            else => return false,
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

        if (env_map.get("QBE")) |v| {
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
        _ = file_path;
        _ = ir;

        var args = std.ArrayList([]const u8).init(self.allocator);
        defer args.deinit();

        const executable = try self.getExecutable();
        defer {
            if (executable) |e| {
                self.allocator.free(e);
            }
        }

        try args.append(executable orelse "qbe");

        const target = switch (assembly) {
            .native => "",
            else => return error.UnsupportedTarget,
        };

        if (target.len > 0) {
            args.append("-t");
            args.append(target);
        }

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
