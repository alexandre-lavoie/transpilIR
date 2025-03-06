const std = @import("std");

const types = @import("types.zig");

pub fn copySymbolMemory(allocator: std.mem.Allocator, mem: *const types.SymbolMemory) !types.SymbolMemory {
    var copy = mem.*;

    switch (copy) {
        .@"struct" => |*s| {
            s.members = try allocator.dupe(types.SymbolMemoryStructEntry, s.members);
        },
        .@"union" => |*u| {
            const structs = try allocator.dupe(types.SymbolMemoryStruct, u.structs);

            for (structs) |*s| {
                s.members = try allocator.dupe(types.SymbolMemoryStructEntry, s.members);
            }

            u.structs = structs;
        },
        .data => |*d| {
            d.entries = try allocator.dupe(types.SymbolMemoryDataEntry, d.entries);
        },
        .function => |*f| {
            f.parameters = try allocator.dupe(types.SymbolMemoryParameterType, f.parameters);
        },
        else => {},
    }

    return copy;
}

pub fn freeSymbolMemory(allocator: std.mem.Allocator, mem: types.SymbolMemory) void {
    switch (mem) {
        .@"struct" => |s| {
            allocator.free(s.members);
        },
        .@"union" => |u| {
            for (u.structs) |s| {
                allocator.free(s.members);
            }

            allocator.free(u.structs);
        },
        .data => |d| {
            allocator.free(d.entries);
        },
        .function => |f| {
            allocator.free(f.parameters);
        },
        else => {},
    }
}
