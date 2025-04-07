const std = @import("std");

// TTY colors
pub const Color = struct {
    pub const path: std.io.tty.Color = .bright_blue;
    pub const @"error": std.io.tty.Color = .red;
    pub const ok: std.io.tty.Color = .green;
    pub const identifier: std.io.tty.Color = .white;
    pub const location: std.io.tty.Color = .yellow;
    pub const reserved: std.io.tty.Color = .magenta;
    pub const @"type": std.io.tty.Color = .green;
    pub const global: std.io.tty.Color = .blue;
    pub const local: std.io.tty.Color = .white;
    pub const label: std.io.tty.Color = .bright_red;
    pub const punctuation: std.io.tty.Color = .white;
    pub const literal: std.io.tty.Color = .yellow;
};

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

// Information about target to compile to.
pub const Target = struct {
    arch: Architecture,
};

// CPU architecture.
// values are addresses in bits.
pub const Architecture = enum {
    a8,
    a16,
    a32,
    a64,
};

// Config for EmitWriters
pub const EmitWriterConfig = struct {
    tty: std.io.tty.Config,
};

// Type of IR
pub const IR = enum {
    none,
    c,
    qbe,

    pub fn isEnabled(t: IR) bool {
        return switch (t) {
            .none => false,
            else => true,
        };
    }
};

// Type of assembly
pub const Assembly = enum {
    none,
    native,
    ir,

    // aarch64
    aarch64,
    aarch64_32,
    aarch64_be,

    // arm
    arm,
    armeb,

    // arm64
    arm64,
    arm64_apple,
    arm64_32,

    // mips
    mips,
    mipsel,

    // mips64
    mips64,
    mips64el,

    // riscv32
    riscv32,
    rv32,

    // riscv64
    riscv64,
    rv64,

    // thumb
    thumb,
    thumbeb,

    // wasm32
    wasm,
    wasm32,

    // wasm64
    wasm64,

    // x86_64
    x86_64,
    amd64,
    amd64_sysv,
    amd64_apple,

    pub fn isEnabled(t: Assembly) bool {
        return switch (t) {
            .none => false,
            else => true,
        };
    }

    pub fn getArchitecture(t: Assembly) ?Architecture {
        return switch (t) {
            .none,
            .native,
            .ir,
            => null,

            .aarch64 => .a64,
            .aarch64_32 => .a64,
            .aarch64_be => .a64,

            .arm => .a32,
            .armeb => .a32,

            .arm64,
            .arm64_apple,
            .arm64_32,
            => .a64,

            .mips,
            .mipsel,
            => .a32,

            .mips64,
            .mips64el,
            => .a64,

            .riscv32,
            .rv32,
            => .a32,

            .riscv64,
            .rv64,
            => .a64,

            .thumb,
            .thumbeb,
            => .a32,

            .wasm,
            .wasm32,
            => .a32,

            .wasm64 => .a64,

            .x86_64,
            .amd64,
            .amd64_sysv,
            .amd64_apple,
            => .a64,
        };
    }
};

// Optimization level for compiler
pub const Optimization = enum {
    o0,
    o1,
    o2,
    o3,

    pub fn isEnabled(o: Optimization) bool {
        _ = o;

        return true;
    }
};

// Iterator over a collection of items
pub fn CollectionIterator(comptime Item: type) type {
    return struct {
        collection: Collection,
        offset: usize = 0,

        const Self = @This();
        const Collection = []const Item;

        pub fn init(collection: Collection) Self {
            return Self{ .collection = collection };
        }

        pub fn next(self: *Self) *const Item {
            if (self.offset >= self.collection.len) return &self.collection[self.collection.len - 1];

            const index = self.offset;
            self.offset += 1;

            return &self.collection[index];
        }
    };
}

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
    if (newline_offsets.len == 0) {
        return .{
            .line = 0,
            .column = 0,
        };
    }

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
    try tty_config.setColor(writer, Color.path);
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
    try tty_config.setColor(writer, Color.@"error");
    _ = try writer.write("error: ");

    try tty_config.setColor(writer, Color.identifier);
    _ = try writer.print("{any} ", .{err});

    try tty_config.setColor(writer, .reset);

    try printSpan(writer, tty_config, file, offsets, span);
    try writer.writeByte('\n');
}

// Copy contents of a reader to a writer
pub fn readerToWriter(reader: anytype, writer: anytype) !void {
    var chunk: [1024]u8 = undefined;
    while (true) {
        const size = try reader.read(&chunk);
        if (size == 0) {
            break;
        }

        _ = try writer.write(chunk[0..size]);

        if (size < 1024) {
            break;
        }
    }
}

// Add escape characters to string
pub fn escapeString(allocator: std.mem.Allocator, s: []const u8) ![]const u8 {
    var buffer = try std.ArrayList(u8).initCapacity(allocator, s.len);

    try std.json.encodeJsonString(s, .{ .escape_unicode = true }, buffer.writer());

    return try buffer.toOwnedSlice();
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
