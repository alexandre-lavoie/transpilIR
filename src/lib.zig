const std = @import("std");

pub const ast = @import("ast/lib.zig");
pub const ssa = @import("ssa/lib.zig");

test {
    std.testing.refAllDeclsRecursive(@This());
}
