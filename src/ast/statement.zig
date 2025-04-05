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

    // Common

    linkage,
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
    opaque_type,
    primitive_type,
    struct_type,
    type_definition,
    union_type,
    zero_type,

    // Function

    block,
    line,
    call,
    call_parameter,
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
    copy,
    cast,
    convert,
    load,
    store,

    // Flow

    branch,
    halt,
    jump,
    phi,
    phi_parameter,
    @"return",

    // Math

    binary_operation,
    comparison,
    negate,
};

pub const StatementData = union(StatementType) {
    // Identifiers

    identifier: struct {
        scope: Scope,
        thread: bool = false,
    },

    // Literals

    literal: struct {
        type: LiteralType,
    },

    // Common

    linkage: struct {
        @"export": bool = false,
        thread: bool = false,
        section: ?StatementIndex,
        flags: ?StatementIndex,
    },

    node: struct {
        value: StatementIndex,
        previous: ?StatementIndex,
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
        alignment: ?StatementIndex = null,
        linkage: StatementIndex,
        identifier: StatementIndex,
        values: StatementIndex,
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

    env_type,

    opaque_type: struct {
        alignment: ?StatementIndex,
        size: StatementIndex,
    },

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
        alignment: ?StatementIndex,
        types: StatementIndex,
    },

    zero_type,

    // Function

    block: struct {
        label: StatementIndex,
        phis: ?StatementIndex,
        lines: ?StatementIndex,
        flow: StatementIndex,
    },

    line: StatementIndex,

    call: struct {
        target: StatementIndex,
        return_type: StatementIndex,
        parameters: ?StatementIndex,
    },

    call_parameter: struct {
        type: StatementIndex,
        value: StatementIndex,
    },

    function: struct {
        signature: StatementIndex,
        body: StatementIndex,
    },

    function_signature: struct {
        linkage: StatementIndex,
        name: StatementIndex,
        return_type: StatementIndex,
        parameters: ?StatementIndex,
    },

    function_parameter: struct {
        type: StatementIndex,
        value: StatementIndex,
    },

    variadic_parameter,

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
        alignment: StatementIndex,
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

    copy: struct {
        data_type: StatementIndex,
        value: StatementIndex,
    },

    cast: struct {
        data_type: StatementIndex,
        value: StatementIndex,
    },

    convert: struct {
        signed: bool,
        data_type: StatementIndex,
        to_type: StatementIndex,
        from_type: StatementIndex,
        value: StatementIndex,
    },

    load: struct {
        data_type: StatementIndex,
        memory_type: StatementIndex,
        address: StatementIndex,
    },

    store: struct {
        memory_type: StatementIndex,
        value: StatementIndex,
        address: StatementIndex,
    },

    // Flow

    branch: struct {
        condition: StatementIndex,
        true: StatementIndex,
        false: StatementIndex,
    },

    halt,

    jump: struct {
        identifier: StatementIndex,
    },

    phi: struct {
        data_type: StatementIndex,
        parameters: ?StatementIndex,
    },

    phi_parameter: struct {
        identifier: StatementIndex,
        value: StatementIndex,
    },

    @"return": struct {
        value: ?StatementIndex,
    },

    // Math

    binary_operation: struct {
        operation_type: BinaryOperationType,
        data_type: StatementIndex,
        left: StatementIndex,
        right: StatementIndex,
    },

    comparison: struct {
        operation_type: ComparisonOperationType,
        data_type: StatementIndex,
        comparison_type: StatementIndex,
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
    void,
    bool,
    ptr,
    u8,
    i8,
    u16,
    i16,
    u64,
    i64,
    u32,
    i32,
    f32,
    f64,
};

pub const LiteralType = enum(u2) {
    integer,
    single,
    double,
    string,
};

pub const BinaryOperationType = enum {
    // Basic
    addition,
    divide,
    divide_unsigned,
    multiply,
    remainder,
    remainder_unsigned,
    subtract,

    // Bitwise
    arthimetic_shift_right,
    @"and",
    @"or",
    logical_shift_right,
    shift_left,
    xor,
};

pub const ComparisonOperationType = enum {
    // Basic
    equal,
    greater_than_equal,
    greater_than,
    less_than_equal,
    less_than,
    not_equal,

    // Float
    not_nan,
    any_nan,
};
