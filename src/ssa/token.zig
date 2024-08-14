const std = @import("std");

const common = @import("../common.zig");

pub const Token = struct {
    token_type: TokenType,
    span: common.SourceSpan = common.SourceSpan{},

    const Self = @This();

    pub fn init(token_type: TokenType) Self {
        return Self{ .token_type = token_type };
    }
};

pub const TokenType = enum(u8) {
    // Module
    module_start,
    module_end,

    // Identifiers
    global_identifier,
    temporary_identifier,
    label_identifier,
    token_typeidentifier,

    // Literals
    string_literal,
    single_literal,
    double_literal,
    integer_literal,

    // Punctuations
    comma,
    open_parenthesis,
    close_parenthesis,
    open_curly_brace,
    close_curly_brace,
    variable_arguments,
    word_assign,
    long_assign,
    single_assign,
    double_assign,

    // Reserved Words
    byte,
    common,
    data,
    debug_file,
    debug_location,
    double,
    env,
    export_,
    function,
    long,
    section,
    single,
    thread,
    type_,
    word,
    zero,

    // Operations
    addition,
    align_,
    allocate1,
    allocate16,
    allocate2,
    allocate4,
    allocate8,
    arthimetic_shift_right,
    bitwise_and,
    bitwise_or,
    bitwise_xor,
    blit,
    byte_load,
    byte_store,
    byte_to_word,
    byte_unsigned_load,
    byte_unsigned_to_word,
    byte_unsigned,
    call,
    cast,
    copy,
    divide,
    double_all_nan,
    double_any_nan,
    double_equal,
    double_greater_than_equal,
    double_greater_than,
    double_less_than_equal,
    double_less_than,
    double_load,
    double_not_equal,
    double_store,
    double_to_single,
    double_to_word_unsigned,
    double_to_word,
    half_word_load,
    half_word_store,
    half_word_to_word,
    half_word_unsigned_load,
    half_word_unsigned_to_word,
    half_word_unsigned,
    half_word,
    halt,
    jump_not_zero,
    jump,
    long_equal,
    long_greater_than_equal,
    long_greater_than,
    long_less_than_equal,
    long_less_than,
    long_load,
    long_not_equal,
    long_store,
    long_to_single,
    long_unsigned_greater_than_equal,
    long_unsigned_greater_than,
    long_unsigned_less_than_equal,
    long_unsigned_less_than,
    long_unsigned_to_single,
    multiply,
    negate,
    phi,
    remainder,
    return_,
    shift_left,
    shift_right,
    single_all_nan,
    single_any_nan,
    single_equal,
    single_greater_than_equal,
    single_greater_than,
    single_less_than_equal,
    single_less_than,
    single_load,
    single_not_equal,
    single_store,
    single_to_double,
    single_to_word_unsigned,
    single_to_word,
    subtract,
    unsigned_divide,
    unsigned_remainder,
    vaarg,
    vastart,
    word_equal,
    word_greater_than_equal,
    word_greater_than,
    word_less_than_equal,
    word_less_than,
    word_load,
    word_not_equal,
    word_store,
    word_to_long,
    word_to_single,
    word_unsigned_greater_than_equal,
    word_unsigned_greater_than,
    word_unsigned_less_than_equal,
    word_unsigned_less_than,
    word_unsigned_load,
    word_unsigned_to_long,
    word_unsigned_to_single,
    word_unsigned,
};

pub const LONGEST_RESERVED_WORD = 10;
pub const RESERVED_WORDS = std.StaticStringMap(TokenType).initComptime(.{
    .{ "add", .addition },
    .{ "align", .align_ },
    .{ "alloc1", .allocate1 },
    .{ "alloc16", .allocate16 },
    .{ "alloc2", .allocate2 },
    .{ "alloc4", .allocate4 },
    .{ "alloc8", .allocate8 },
    .{ "and", .bitwise_and },
    .{ "b", .byte },
    .{ "blit", .blit },
    .{ "call", .call },
    .{ "cast", .cast },
    .{ "ceqd", .double_equal },
    .{ "ceql", .long_equal },
    .{ "ceqs", .single_equal },
    .{ "ceqw", .word_equal },
    .{ "cged", .double_greater_than_equal },
    .{ "cges", .single_greater_than_equal },
    .{ "cgtd", .double_greater_than },
    .{ "cgts", .single_greater_than },
    .{ "cled", .double_less_than_equal },
    .{ "cles", .single_less_than_equal },
    .{ "cltd", .double_less_than },
    .{ "clts", .single_less_than },
    .{ "cned", .double_not_equal },
    .{ "cnel", .long_not_equal },
    .{ "cnes", .single_not_equal },
    .{ "cnew", .word_not_equal },
    .{ "cod", .double_all_nan },
    .{ "common", .common },
    .{ "copy", .copy },
    .{ "cos", .single_all_nan },
    .{ "csgel", .long_greater_than_equal },
    .{ "csgew", .word_greater_than_equal },
    .{ "csgtl", .long_greater_than },
    .{ "csgtw", .word_greater_than },
    .{ "cslel", .long_less_than_equal },
    .{ "cslew", .word_less_than_equal },
    .{ "csltl", .long_less_than },
    .{ "csltw", .word_less_than },
    .{ "cugel", .long_unsigned_greater_than_equal },
    .{ "cugew", .word_unsigned_greater_than_equal },
    .{ "cugtl", .long_unsigned_greater_than },
    .{ "cugtw", .word_unsigned_greater_than },
    .{ "culel", .long_unsigned_less_than_equal },
    .{ "culew", .word_unsigned_less_than_equal },
    .{ "cultl", .long_unsigned_less_than },
    .{ "cultw", .word_unsigned_less_than },
    .{ "cuod", .double_any_nan },
    .{ "cuos", .single_any_nan },
    .{ "d", .double },
    .{ "data", .data },
    .{ "dbgfile", .debug_file },
    .{ "dbgloc", .debug_location },
    .{ "div", .divide },
    .{ "dtosi", .double_to_word },
    .{ "dtoui", .double_to_word_unsigned },
    .{ "env", .env },
    .{ "export", .export_ },
    .{ "exts", .single_to_double },
    .{ "extsb", .byte_to_word },
    .{ "extsh", .half_word_to_word },
    .{ "extsw", .word_to_long },
    .{ "extub", .byte_unsigned_to_word },
    .{ "extuh", .half_word_unsigned_to_word },
    .{ "extuw", .word_unsigned_to_long },
    .{ "function", .function },
    .{ "h", .half_word },
    .{ "hlt", .halt },
    .{ "jmp", .jump },
    .{ "jnz", .jump_not_zero },
    .{ "l", .long },
    .{ "loadd", .double_load },
    .{ "loadl", .long_load },
    .{ "loads", .single_load },
    .{ "loadsb", .byte_load },
    .{ "loadsh", .half_word_load },
    .{ "loadsw", .word_load },
    .{ "loadub", .byte_unsigned_load },
    .{ "loaduh", .half_word_unsigned_load },
    .{ "loaduw", .word_unsigned_load },
    .{ "loadw", .word_load },
    .{ "mul", .multiply },
    .{ "neg", .negate },
    .{ "or", .bitwise_or },
    .{ "phi", .phi },
    .{ "rem", .remainder },
    .{ "ret", .return_ },
    .{ "s", .single },
    .{ "sar", .arthimetic_shift_right },
    .{ "sb", .byte },
    .{ "section", .section },
    .{ "sh", .half_word },
    .{ "shl", .shift_left },
    .{ "shr", .shift_right },
    .{ "sltof", .long_to_single },
    .{ "storeb", .byte_store },
    .{ "stored", .double_store },
    .{ "storeh", .half_word_store },
    .{ "storel", .long_store },
    .{ "stores", .single_store },
    .{ "storew", .word_store },
    .{ "stosi", .single_to_word },
    .{ "stoui", .single_to_word_unsigned },
    .{ "sub", .subtract },
    .{ "sw", .word },
    .{ "swtof", .word_to_single },
    .{ "thread", .thread },
    .{ "truncd", .double_to_single },
    .{ "type", .type_ },
    .{ "ub", .byte_unsigned },
    .{ "udiv", .unsigned_divide },
    .{ "uh", .half_word_unsigned },
    .{ "ultof", .long_unsigned_to_single },
    .{ "urem", .unsigned_remainder },
    .{ "uw", .word_unsigned },
    .{ "uwtof", .word_unsigned_to_single },
    .{ "vaarg", .vaarg },
    .{ "vastart", .vastart },
    .{ "w", .word },
    .{ "xor", .bitwise_xor },
    .{ "z", .zero },
});
