const std = @import("std");

// Span inside source file.
// start is inclusive and end is exclusive -> [start, end)
pub const SourceSpan = struct {
    start: usize = 0,
    end: usize = 0,

    const Self = @This();

    fn size(self: *const Self) usize {
        return self.end - self.start;
    }
};

pub fn parseString(input: []const u8, output: []u8) ![]const u8 {
    var i: usize = 0;
    var o: usize = 0;
    while (i < input.len) {
        output[o] = switch (input[i]) {
            '\\' => scope: {
                i += 1;

                break :scope switch (input[i]) {
                    '0' => '\x00',
                    't' => '\t',
                    'n' => '\n',
                    'r' => '\r',
                    'x' => hex: {
                        const vi = i + 1;
                        i += 2;

                        break :hex try std.fmt.parseInt(u8, input[vi .. vi + 2], 16);
                    },
                    else => input[i],
                };
            },
            else => input[i],
        };

        i += 1;
        o += 1;
    }

    return output[0..o];
}
