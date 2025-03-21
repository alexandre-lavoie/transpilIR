const std = @import("std");

const common = @import("../common.zig");

pub const CToken = struct {
    token_type: CTokenType,
    span: common.SourceSpan = common.SourceSpan{},

    const Self = @This();

    pub fn init(token_type: CTokenType) Self {
        return Self{ .token_type = token_type };
    }
};

pub const CTokenType = enum(u8) {
    // Module
    module_start,
    module_end,

    // Identifiers
    global_identifier,
    label_identifier,
    local_identifier,
    type_identifier,

    // Literals
    double_literal,
    integer_literal,
    single_literal,
    string_literal,

    // Punctuations
    assign,
    bitwise_and,
    bitwise_not,
    bitwise_or,
    bitwise_xor,
    close_bracket,
    close_curly_brace,
    close_parenthesis,
    colon,
    comma,
    dereference,
    divide,
    equal,
    greater_than,
    greater_than_equal,
    less_than,
    less_than_equal,
    logical_not,
    minus,
    multiply,
    not_equal,
    open_bracket,
    open_curly_brace,
    open_parenthesis,
    pointer,
    plus,
    remainder,
    semi_colon,
    shift_left,
    shift_right,
    variable_arguments,

    // Reserved Words
    @"else",
    @"if",
    @"return",
    @"struct",
    @"switch",
    @"union",
    @"while",
    i8,
    u8,
    i16,
    u16,
    i32,
    u32,
    i64,
    u64,
    f32,
    f64,
    goto,
    static,
    void,

    // Interal spacing
    newline,
    tab,
};

pub fn tokenString(token_type: CTokenType) []const u8 {
    return switch (token_type) {
        .@"else" => "else",
        .@"if" => "if",
        .@"return" => "return",
        .@"struct" => "struct",
        .@"switch" => "switch",
        .@"union" => "union",
        .@"while" => "while",
        .assign => "=",
        .bitwise_and => "&",
        .bitwise_not => "~",
        .bitwise_or => "|",
        .bitwise_xor => "^",
        .close_bracket => "]",
        .close_curly_brace => "}",
        .close_parenthesis => ")",
        .colon => ":",
        .comma => ",",
        .dereference => "&",
        .divide => "/",
        .equal => "==",
        .greater_than => ">",
        .greater_than_equal => ">=",
        .goto => "goto",
        .less_than => "<",
        .less_than_equal => "<=",
        .logical_not => "!",
        .minus => "-",
        .multiply => "*",
        .newline => "\n",
        .not_equal => "!=",
        .open_bracket => "[",
        .open_curly_brace => "{",
        .open_parenthesis => "(",
        .plus => "+",
        .pointer => "*",
        .remainder => "%",
        .semi_colon => ";",
        .shift_left => "<<",
        .shift_right => ">>",
        .static => "static",
        .tab => "    ",
        .variable_arguments => "...",
        .void => "void",
        .i8 => "char",
        .u8 => "unsigned char",
        .i16 => "short",
        .u16 => "unsigned short",
        .i32 => "int",
        .u32 => "unsigned int",
        .i64 => "long",
        .u64 => "unsigned long",
        .f32 => "float",
        .f64 => "double",
        .double_literal,
        .global_identifier,
        .integer_literal,
        .label_identifier,
        .local_identifier,
        .module_end,
        .module_start,
        .single_literal,
        .string_literal,
        .type_identifier,
        => "",
    };
}

pub fn tokenColor(token_type: CTokenType) std.io.tty.Color {
    return switch (token_type) {
        .module_start,
        .module_end,
        .newline,
        .tab,
        => .reset,

        .global_identifier => common.Color.global,
        .label_identifier => common.Color.label,
        .local_identifier => common.Color.local,
        .type_identifier => common.Color.type,

        .double_literal,
        .integer_literal,
        .single_literal,
        .string_literal,
        => common.Color.literal,

        .assign,
        .bitwise_and,
        .bitwise_not,
        .bitwise_or,
        .bitwise_xor,
        .close_bracket,
        .close_curly_brace,
        .close_parenthesis,
        .colon,
        .comma,
        .dereference,
        .divide,
        .equal,
        .greater_than,
        .greater_than_equal,
        .logical_not,
        .less_than,
        .less_than_equal,
        .minus,
        .multiply,
        .not_equal,
        .open_bracket,
        .open_curly_brace,
        .open_parenthesis,
        .plus,
        .pointer,
        .remainder,
        .semi_colon,
        .shift_left,
        .shift_right,
        .variable_arguments,
        => common.Color.punctuation,

        .@"else",
        .@"if",
        .@"return",
        .@"switch",
        .@"struct",
        .@"union",
        .@"while",
        .i8,
        .u8,
        .i16,
        .u16,
        .i32,
        .u32,
        .i64,
        .u64,
        .f32,
        .f64,
        .goto,
        .static,
        .void,
        => common.Color.reserved,
    };
}
