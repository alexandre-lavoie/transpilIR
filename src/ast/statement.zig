const common = @import("../common.zig");

pub const StatementIndex = usize;

pub const Statement = struct {
    span: common.SourceSpan,
    data: StatementData,
};

pub const StatementData = union(enum) {
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
        types: ?StatementIndex,
        data: ?StatementIndex,
        functions: ?StatementIndex,
    },

    // Data

    data_definition: struct {
        identifier: StatementIndex,
        values: StatementIndex,
    },

    typed_data: struct {
        type: StatementIndex,
        value: StatementIndex,
    },

    zero_data: struct {
        length: StatementIndex,
    },

    // Type

    array_type: struct {
        item: StatementIndex,
        count: usize,
    },

    primitive_type: PrimitiveType,

    struct_type: struct {
        alignment: usize,
        members: StatementIndex,
    },

    type_definition: struct {
        identifier: StatementIndex,
        type: StatementIndex,
    },

    union_type: struct {
        members: StatementIndex,
    },

    void_type: void,

    // Function

    call: struct {
        identifier: StatementIndex,
        parameters: ?StatementIndex,
    },

    function: struct {
        signature: StatementIndex,
        block: StatementIndex,
    },

    function_signature: struct {
        name: StatementIndex,
        return_type: StatementIndex,
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
        size: usize,
    },

    assignment: struct {
        identifier: StatementIndex,
        statement: StatementIndex,
    },

    blit: struct {
        size: usize,
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
    address,
};

pub const PrimitiveType = enum(u4) {
    byte_unsigned,
    byte,
    double,
    half_word_unsigned,
    half_word,
    long_unsigned,
    long,
    short,
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
