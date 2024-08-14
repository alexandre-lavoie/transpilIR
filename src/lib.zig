const std = @import("std");

pub const ssa = @import("ssa/lib.zig");

test {
    std.testing.refAllDeclsRecursive(@This());
}
