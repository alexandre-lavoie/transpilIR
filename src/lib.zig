const std = @import("std");

pub const ast = @import("ast/lib.zig");
pub const ssa = @import("ssa/lib.zig");
pub const symbol = @import("symbol/lib.zig");

pub const common = @import("common.zig");

test {
    std.testing.refAllDeclsRecursive(@This());
}
