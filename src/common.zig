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
