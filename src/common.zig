const std = @import("std");

pub const PATH_COLOR: std.io.tty.Color = .bright_blue;
pub const ERROR_COLOR: std.io.tty.Color = .red;
pub const OK_COLOR: std.io.tty.Color = .green;
pub const IDENTIFIER_COLOR: std.io.tty.Color = .white;
pub const LOCATION_COLOR: std.io.tty.Color = .yellow;
pub const RESERVED_COLOR: std.io.tty.Color = .magenta;
pub const TYPE_COLOR: std.io.tty.Color = .green;
pub const GLOBAL_COLOR: std.io.tty.Color = .blue;
pub const LOCAL_COLOR: std.io.tty.Color = .white;
pub const LABEL_COLOR: std.io.tty.Color = .bright_red;

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

pub fn fileNewLines(allocator: std.mem.Allocator, reader: anytype) ![]usize {
    var out = std.ArrayList(usize).init(allocator);
    defer out.deinit();

    try out.append(0);

    var i: usize = 0;
    while (true) : (i += 1) {
        switch (reader.readByte() catch break) {
            '\n' => try out.append(i),
            else => {},
        }
    }

    return try out.toOwnedSlice();
}

pub fn indexToFile(newline_offsets: []const usize, index: usize) struct { line: usize, column: usize } {
    var left: usize = 0;
    const size = newline_offsets.len;
    var right = size;

    var mid: usize = 0;
    while (left < right) {
        mid = left + (right - left) / 2;

        if (mid == size - 1) break;

        const min = newline_offsets[mid];
        const max = newline_offsets[mid + 1];

        if (min <= index and index < max) break;

        if (index < min) {
            right = mid;
        } else {
            left = mid + 1;
        }
    }

    const line = newline_offsets[mid];

    var column: usize = 0;
    if (line < index) column = index - line;
    if (mid == 0) column += 1;

    return .{
        .line = mid + 1,
        .column = column,
    };
}

pub fn printFileLocation(writer: anytype, tty_config: *const std.io.tty.Config, file: []const u8, line: usize, column: usize) !void {
    try tty_config.setColor(writer, PATH_COLOR);
    _ = try writer.print("{s}:{}:{}", .{
        file,
        line,
        column,
    });
    try tty_config.setColor(writer, .reset);
}

pub fn printSpan(writer: anytype, tty_config: *const std.io.tty.Config, file: []const u8, offsets: []const usize, span: *const SourceSpan) !void {
    const start = indexToFile(offsets, span.start);
    const end = indexToFile(offsets, switch (span.end) {
        0 => 0,
        else => span.end - 1,
    });

    try printFileLocation(writer, tty_config, file, start.line, start.column);
    _ = try writer.write(", ");
    try printFileLocation(writer, tty_config, file, end.line, end.column);
}

pub fn printError(writer: anytype, tty_config: *const std.io.tty.Config, err: anytype, file: []const u8, offsets: []const usize, span: *const SourceSpan) !void {
    try tty_config.setColor(writer, ERROR_COLOR);
    _ = try writer.write("error: ");

    try tty_config.setColor(writer, IDENTIFIER_COLOR);
    _ = try writer.print("{any} ", .{err});

    try tty_config.setColor(writer, .reset);

    try printSpan(writer, tty_config, file, offsets, span);
    try writer.writeByte('\n');
}

test "indexToFile" {
    // Arrange
    const offsets = [_]usize{ 0, 10, 20, 30 };
    const index = 15;

    // Act
    const out = indexToFile(&offsets, index);

    // Assert
    try std.testing.expectEqual(2, out.line);
    try std.testing.expectEqual(5, out.column);
}
