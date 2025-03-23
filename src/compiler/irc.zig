const std = @import("std");

const common = @import("../common.zig");

pub const IRC = struct {
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
        _ = ir;

        return assembly == .ir;
    }

    pub fn assemble(
        self: *Self,
        file_path: []const u8,
        ir: common.IR,
        assembly: common.Assembly,
        reader: std.io.AnyReader,
        writer: std.io.AnyWriter,
    ) !void {
        _ = self;
        _ = file_path;
        _ = ir;
        _ = assembly;

        try common.readerToWriter(reader, writer);
    }
};
