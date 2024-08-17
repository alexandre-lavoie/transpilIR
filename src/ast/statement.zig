const std = @import("std");

const common = @import("../common.zig");

pub const StatementIndex = usize;

pub const Statement = struct {
    span: common.SourceSpan = common.SourceSpan{},
    data: StatementData,

    const Self = @This();

    pub fn init(span: common.SourceSpan, data: StatementData) Self {
        return Self{ .span = span, .data = data };
    }
};

pub const StatementType = enum {
    // Identifiers

    identifier,

    // Literals

    literal,

    // List

    node,

    // Module

    module,

    // Data

    data_definition,
    typed_data,
    offset,

    // Type

    array_type,
    env_type,
    primitive_type,
    struct_type,
    type_definition,
    union_type,
    zero_type,

    // Function

    call,
    function,
    function_signature,
    function_parameter,
    variadic_parameter,
    vaarg,
    vastart,

    // Memory

    allocate,
    assignment,
    blit,
    cast,
    copy,
    load,
    store,

    // Flow

    branch,
    label,
    halt,
    jump,
    phi,
    phi_parameter,
    @"return",

    // Opertations

    binary_operation,
    comparision,
    negate,
};

pub const StatementData = union(StatementType) {
    // Identifiers

    identifier: struct {
        scope: Scope,
    },

    // Literals

    literal: struct {
        type: LiteralType,
    },

    // List

    node: struct {
        value: StatementIndex,
        next: ?StatementIndex,
    },

    // Module

    module: struct {
        types: ?StatementIndex = undefined,
        data: ?StatementIndex = undefined,
        functions: ?StatementIndex = undefined,
    },

    // Data

    data_definition: struct {
        thread: bool = false,
        identifier: StatementIndex,
        values: ?StatementIndex,
    },

    typed_data: struct {
        type: StatementIndex,
        value: StatementIndex,
    },

    offset: struct {
        identifier: StatementIndex,
        value: StatementIndex,
    },

    // Type

    array_type: struct {
        item: StatementIndex,
        count: StatementIndex,
    },

    env_type: void,

    primitive_type: PrimitiveType,

    struct_type: struct {
        alignment: ?StatementIndex,
        members: StatementIndex,
    },

    type_definition: struct {
        identifier: StatementIndex,
        type: StatementIndex,
    },

    union_type: struct {
        members: StatementIndex,
    },

    zero_type: void,

    // Function

    call: struct {
        identifier: StatementIndex,
        parameters: ?StatementIndex,
    },

    function: struct {
        signature: StatementIndex,
        block: ?StatementIndex,
    },

    function_signature: struct {
        name: StatementIndex,
        @"export": bool = false,
        return_type: ?StatementIndex,
        parameters: ?StatementIndex,
    },

    function_parameter: struct {
        type_statement: StatementIndex,
        identifier: StatementIndex,
    },

    variadic_parameter: void,

    vaarg: struct {
        data_type: StatementIndex,
        parameter: StatementIndex,
    },

    vastart: struct {
        parameter: StatementIndex,
    },

    // Memory

    allocate: struct {
        data_type: StatementIndex,
        size: StatementIndex,
    },

    assignment: struct {
        identifier: StatementIndex,
        statement: StatementIndex,
    },

    blit: struct {
        size: StatementIndex,
        source: StatementIndex,
        target: StatementIndex,
    },

    cast: struct {
        data_type: StatementIndex,
        value: StatementIndex,
    },

    copy: struct {
        data_type: StatementIndex,
        value: StatementIndex,
    },

    load: struct {
        data_type: StatementIndex,
        target: StatementIndex,
    },

    store: struct {
        data_type: StatementIndex,
        source: StatementIndex,
        target: StatementIndex,
    },

    // Flow

    branch: struct {
        condition: StatementIndex,
        true: StatementIndex,
        false: StatementIndex,
    },

    label: struct {
        identifier: StatementIndex,
    },

    halt: void,

    jump: struct {
        identifier: StatementIndex,
    },

    phi: struct {
        parameters: StatementIndex,
    },

    phi_parameter: struct {
        identifier: StatementIndex,
        value: StatementIndex,
    },

    @"return": struct {
        data_type: StatementIndex,
        parameter: ?StatementIndex,
    },

    // Opertations

    binary_operation: struct {
        operation_type: BinaryOperationType,
        data_type: StatementIndex,
        left: StatementIndex,
        right: StatementIndex,
    },

    comparision: struct {
        operation_type: ComparisonOperationType,
        data_type: StatementIndex,
        left: StatementIndex,
        right: StatementIndex,
    },

    negate: struct {
        data_type: StatementIndex,
        value: StatementIndex,
    },
};

pub const Scope = enum {
    global,
    local,
    label,
    type,
};

pub const PrimitiveType = enum(u4) {
    byte_unsigned,
    byte,
    double,
    half_word_unsigned,
    half_word,
    long_unsigned,
    long,
    single,
    word_unsigned,
    word,
};

pub const LiteralType = enum(u2) {
    integer,
    float,
    string,
};

pub const BinaryOperationType = enum {
    // Basic
    addition,
    divide,
    multiply,
    remainder,
    subtract,

    // Bitwise
    arthimetic_shift_right,
    and_,
    or_,
    logical_shift_right,
    shift_left,
    xor,

    // Float
    all_nan,
    any_nan,
};

pub const ComparisonOperationType = enum {
    equal,
    greater_than_equal,
    greater_than,
    less_than_equal,
    less_than,
    not_equal,
};
