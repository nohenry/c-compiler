const std = @import("std");
const Unit = @import("unit.zig").Unit;
const DefineValue = @import("unit.zig").DefineValue;
const tok = @import("tokenizer.zig");
const Tokenizer = tok.Tokenizer;
const TokenIndex = tok.TokenIndex;

pub const ParseError = error{ OutOfTokens, UnexpectedToken };

pub const NodeKind = enum(u32) {
    empty,
    int_literal,
    float_literal,
    string_literal,
    char_literal,

    /// node_data: a(u32) = index of lhs, b(u16) = relative index of rhs, c(u16) = operator
    binary_lr_operator,
    /// node_data: a(u32) = index of expr, b(u16) = operator
    unary_prefix_operator,
    /// node_data: a(u32) = index of expr, b(u16) = operator
    unary_suffix_operator,
    /// node_data: a(u32) = relative index of invokee
    invoke,
    /// node_data: a(u16) = relative index of invokee, b(u32) = arg_index
    invoke_one_arg,
    /// node_data: a(u16) = relative index of invokee, b(u16) = count, c(u32) = start index of parameters
    invoke_args,
    /// node_data: a(u32) = index of array/ptr, b(u32) = index of index expr
    index,

    /// variable declaration
    /// node_data: a(u16) = relative offset, b(u16) = count, c(u16) = relative type, d(u8) = storage
    /// node_data: a(u32) = index of identifier, b(u32) = index of type
    /// node_data: a(u32) = index, b(u16) = count, c(u8) = storage
    declaration,

    /// node_data: a(u32) = index of identifier, b(u16) = relative index of type
    var_declaration,

    /// node_data: a(u32) = index of identifier, c(u16) = relative index of type, d(u16) = relative index of init
    var_declaration_init,

    /// node_data: a(u32) = index of identifier, c(u16) = relative index of type
    function_declaration,
    /// node_data: a(u32) = index of identifier, c(u16) = relative index of type, d(u16) = relative index of body
    function_declaration_body,

    /// node_data: a(u32) = index of element type, b(u16) = relative start of type_qualifier, c(u16) = count of type qualifiers
    /// node_data: a(u32) = index of base type, b(u32) = start of type_qualifier
    pointer,

    /// For all types:
    /// node_data: h(u8) = TypeQualifier.Type
    char_type,
    signed_char_type,
    unsigned_char_type,
    short_type,
    signed_short_type,
    unsigned_short_type,
    int_type,
    signed_int_type,
    unsigned_int_type,
    long_type,
    signed_long_type,
    unsigned_long_type,
    long_long_type,
    signed_long_long_type,
    unsigned_long_long_type,
    float_type,
    double_type,
    long_double_type,
    bool_type,
    unsigned,
    signed,

    void_type,
    complex_type,
    imaginary_type,

    /// []
    /// node_data: a(u32) = base type index, h(u8) = type_qualifiers
    array_type,
    /// [*]
    /// node_data: a(u32) = base type index, h(u8) = type_qualifiers
    array_type_variable,
    /// [10]
    /// node_data: a(u32) = base type index, b(u16) = relative index of size, h(u8) = type_qualifiers
    array_type_fixed,
    /// [static 10]
    /// node_data: a(u32) = base type index, b(u16) = relative index of size, h(u8) = type_qualifiers    array_type_static,
    array_type_static,

    /// node_data: a(u16) = relative index of return type
    function_type,
    /// node_data: a(u16) = relative index of return type,
    ///            b(u16) = relative index of parameter type
    function_type_one_parameter,
    /// node_data: a(u16) = relative index of return type,
    ///            b(u16) = relative index of parameter type start,
    ///            g(u8)  = parameter type count
    function_type_parameter,

    /// node_data: a(u16) = relative index of type, c(u8) = storage class
    parameter,
    /// node_data: a(u16) = relative index of type, c(u8) = storage class, b(u32) = index of identifier
    parameter_ident,
    /// ...
    parameter_ellipsis,

    /// struct name;
    /// node_data: a(u32) = identifier
    struct_forward,
    /// struct { int a; }
    /// node_data: a(u32) = member range start, b(u32) = member range count
    @"struct",
    /// struct foobar { int a; }
    /// node_data: a(u32) = identifier
    /// members are specified by "range" node immediately before this node
    struct_ident,
    /// struct name;
    /// node_data: a(u32) = identifier
    union_forward,
    /// node_data: a(u32) = member range start, b(u32) = member range count
    @"union",
    /// node_data: a(u32) = identifier
    union_ident,
    /// node_data: c(u16) = relative index of type
    member,
    /// node_data: a(u32) = identifier index, c(u16) = relative index of type
    member_ident,
    /// node_data: c(u16) = relative index of type, d(u16) = relative index of bitfield
    member_bitfield,
    /// node_data: a(u32) = identifier index, c(u16) = relative index of type, d(u16) = relative index of bitfield
    member_ident_bitfield,
    /// node_data: a(u32) = identifier index
    enum_member,
    /// node_data: a(u32) = identifier index, b(u32) = value index
    enum_member_value,

    /// node_data: a(u32) member range start, b(u32) member range count
    @"enum",
    /// node_data: a(u32) = identifier
    /// members are specified by "range" node immediately before this node
    enum_ident,
    /// node_data: a(u32) = identifier
    enum_forward,
    /// node_Data: a(u32) = identifier, h(u8) = type qualifiers
    atomic_type,

    /// node_data: a(u32) = index expression index, b(u32) = last designator index
    designator_index,
    /// node_data: a(u32) = identifier index, b(u32) = last designator index
    designator_field,
    /// node_data: a(u32) = index expression index
    designator_index_terminal,
    /// node_data: a(u32) = identifier index
    designator_field_terminal,
    /// node_data: a(u32) = index of designator, b(u32) = index of initializer
    designation,
    /// node_data:
    initializer_list,
    /// node_data: a(u32) = index of designation
    initializer_list_one,
    /// node_data: a(u32) = start range index, b(u32) = range count
    initializer_list_many,

    /// node_data: a(u32) = token index of type name identifier, h(u8) = type qualifiers
    type_name,
    /// node_data: a(u32) = token index of identifier
    identifier,
    /// node_data: a(u32) = index of test expression
    static_assert,
    /// node_data: a(u32) = index of test expression, b(u32) = index of string
    static_assert_str,

    /// node_data:
    compound_empty,
    /// node_data: a(u32) = index of item
    compound_one,
    /// node_data: a(u32) = start range, b(u32) = count
    compound,
    /// node_data: a(u32) = index of condition, b(u32) = index of body
    while_loop,
    /// node_data: a(u32) = index of condition
    while_loop_empty,
    /// node_data: a(u32) = index of condition, b(u32) = index of body
    do_while_loop,
    /// node_data: a(u32) = index of condition
    do_while_loop_empty,
    /// node_data: a(u16) = relative index of init, b(u16) = relative index of condition, c(u16) = relative index of increment, d(u16) = relative index of body
    for_loop,
    /// node_data: a(u16) = relative index of init, b(u16) = relative index of condition, d(u16) = relative index of body
    for_loop_inc,
    /// node_data: a(u16) = relative index of init, b(u16) = relative index of condition, c(u16) = relative index of increment
    for_loop_empty,
    /// node_data: a(u16) = relative index of init, b(u16) = relative index of condition, d(u16) = relative index of body
    for_loop_empty_inc,
    /// node_data: a(u32) = index of condition, b(u32) = index of body
    switch_case,
    /// node_data: a(u32) = index of expression, b(u32) = index of body
    case,
    /// node_data: a(u32) = index of body
    default,
    /// node_data: a(u32) = index of label, b(u32) = index of statement
    label,
    /// node_data: a(u32) = index of label identifier
    goto,
    /// node_data:
    continue_statement,
    /// node_data:
    break_statement,
    /// node_data:
    return_statement,
    /// node_data: a(u32) = index of expression
    return_statement_value,

    /// node_data: a(u32) = index of condition, b(u32) = index of body
    if_statement,
    /// node_data: a(u32) = index of condition
    if_statement_no_body,
    /// node_data: a(u32) = index of condition, c(u16) = relative index of body, d(u16) = relative index of else body
    if_statement_else,
    /// node_data: a(u32) = index of condition, b(u32) = index of else body
    if_statement_no_body_else,
    empty_statement,

    /// Represents Node Range
    /// node_data: a = start, b = end
    range,
};

pub const NodeData = extern union {
    long: u64,
    // range: NodeRange,
    two: packed struct {
        a: u32,
        b: u32,
    },
    four: packed struct {
        a: u16,
        b: u16,
        c: u16,
        d: u16,
    },
    eight: packed struct {
        a: u8,
        b: u8,
        c: u8,
        d: u8,
        e: u8,
        f: u8,
        g: u8,
        h: u8,
    },
    double: f64,
    float: packed struct {
        a: f32,
        b: f32,
    },

    pub inline fn as(self: *@This(), comptime field: std.meta.FieldEnum(@This())) *std.meta.FieldType(@This(), field) {
        return &@field(self, @tagName(field));
    }
};

comptime {
    std.debug.assert(@sizeOf(Node) == 16);
}

pub const StorageClass = struct {
    pub const Type = u8;
    pub const typedef: Type = (1 << 0);
    pub const @"extern": Type = (1 << 1);
    pub const static: Type = (1 << 2);
    pub const thread_local: Type = (1 << 3);
    pub const auto: Type = (1 << 4);
    pub const register: Type = (1 << 5);
    pub const @"inline": Type = (1 << 6);
    pub const @"noreturn": Type = (1 << 7);

    pub fn write(storage_class: Type, writer: anytype) !void {
        if ((storage_class & typedef) > 0) {
            _ = try writer.write("TYPEDEF ");
        }
        if ((storage_class & @"extern") > 0) {
            _ = try writer.write("EXTERN ");
        }
        if ((storage_class & static) > 0) {
            _ = try writer.write("STATIC ");
        }
        if ((storage_class & thread_local) > 0) {
            _ = try writer.write("THREAD_LOCAL ");
        }
        if ((storage_class & auto) > 0) {
            _ = try writer.write("AUTO ");
        }
        if ((storage_class & register) > 0) {
            _ = try writer.write("REGISTER ");
        }
        if ((storage_class & @"inline") > 0) {
            _ = try writer.write("INLINE ");
        }
        if ((storage_class & @"noreturn") > 0) {
            _ = try writer.write("NORETURN ");
        }
    }
};

pub const TypeQualifier = struct {
    pub const Type = u8;
    pub const @"const": Type = (1 << 0);
    pub const restrict: Type = (1 << 1);
    pub const @"volatile": Type = (1 << 2);
    pub const atomic: Type = (1 << 3);

    pub fn write(type_qualifier: Type, writer: anytype) !void {
        if ((type_qualifier & @"const") > 0) {
            _ = try writer.write("CONST ");
        }
        if ((type_qualifier & restrict) > 0) {
            _ = try writer.write("RESTRICT ");
        }
        if ((type_qualifier & @"volatile") > 0) {
            _ = try writer.write("VOLATILE ");
        }
        if ((type_qualifier & atomic) > 0) {
            _ = try writer.write("ATOMIC ");
        }
    }
};

pub const NodeIndex = u32;
pub const Node = extern struct {
    kind: NodeKind,
    data: NodeData,

    const NodeRangeOrNode = union(enum) {
        node_range: NodeRange,
        node: NodeIndex,
        nodes: struct {
            indicies: [32]NodeIndex,
            count: u32,
        },
        node_and_range: struct {
            node: NodeIndex,
            range: NodeRange,
        },
        none: void,

        pub fn initNodes(nodes: []const NodeIndex) NodeRangeOrNode {
            var result: NodeRangeOrNode = undefined;
            @memcpy(result.nodes.indicies[0..nodes.len], nodes);
            result.nodes.count = @truncate(nodes.len);
            return result;
        }

        pub fn initNodeAndRange(node: NodeIndex, start: u32, count: u32) NodeRangeOrNode {
            const result: NodeRangeOrNode = .{
                .node_and_range = .{
                    .node = node,
                    .range = .{ .start = start, .count = count },
                },
            };
            return result;
        }
    };

    pub inline fn absoluteIndex(current_index: NodeIndex, relative_index: u32) u32 {
        return current_index - relative_index;
    }

    pub fn writeTree(index: NodeIndex, unit: *Unit, indent: u32, last: bool, writer: anytype) !void {
        const self = &unit.nodes.items[index];
        for (0..indent) |_| {
            _ = try writer.write("    ");
        }
        if (last) {
            _ = try writer.write("\x1b[34m└── \x1b[0m");
        } else {
            _ = try writer.write("\x1b[34m├── \x1b[0m");
        }

        var result = NodeRangeOrNode{ .none = {} };
        switch (self.kind) {
            .int_literal => try writer.print("\x1b[1;35mIntLiteral\x1b[0m \x1b[1;33m{}\x1b[0m", .{self.data.long}),
            .float_literal => try writer.print("\x1b[1;35mFloatLiteral\x1b[0m \x1b[1;33m{}\x1b[0m", .{self.data.double}),
            .declaration => {
                const next_index = self.data.as(.two).a;
                const count = self.data.as(.four).c;
                const storage = self.data.as(.eight).g;

                try writer.print("\x1b[1;35mDeclaration\x1b[0m ", .{});
                try StorageClass.write(@truncate(storage), writer);

                result = .{
                    .node_range = .{ .start = next_index, .count = count },
                };
            },
            .var_declaration => {
                const ident_index: TokenIndex = @bitCast(self.data.as(.two).a);
                const type_index = absoluteIndex(index, self.data.as(.four).c);

                try writer.print("\x1b[1;35mVarDeclaration\x1b[0m \x1b[1;36m'{s}'\x1b[0m", .{unit.identifierAt(ident_index)});

                result = .{
                    .node = type_index,
                };
            },
            .var_declaration_init => {
                const ident_index: TokenIndex = @bitCast(self.data.as(.two).a);
                const type_index = absoluteIndex(index, self.data.as(.four).c);
                const init_index = absoluteIndex(index, self.data.as(.four).d);

                try writer.print("\x1b[1;35mVarDeclaration\x1b[0m \x1b[1;36m'{s}'\x1b[0m", .{unit.identifierAt(ident_index)});

                result = NodeRangeOrNode.initNodes(&.{
                    type_index,
                    init_index,
                });
            },
            .function_declaration_body => {
                const ident_index: TokenIndex = @bitCast(self.data.as(.two).a);
                const type_index = absoluteIndex(index, self.data.as(.four).c);
                const body_index = absoluteIndex(index, self.data.as(.four).d);

                try writer.print("\x1b[1;35mFunctionDeclaration\x1b[0m \x1b[1;36m'{s}'\x1b[0m", .{unit.identifierAt(ident_index)});

                result = NodeRangeOrNode.initNodes(&.{
                    type_index,
                    body_index,
                });
            },
            .function_declaration => {
                const ident_index: TokenIndex = @bitCast(self.data.as(.two).a);
                const type_index = absoluteIndex(index, self.data.as(.four).c);

                try writer.print("\x1b[1;35mFunctionDeclaration\x1b[0m \x1b[1;36m'{s}'\x1b[0m", .{unit.identifierAt(ident_index)});

                result = .{
                    .node = type_index,
                };
            },
            .pointer => {
                const base_type = self.data.as(.two).a;
                const type_qualifier = self.data.as(.two).b;
                try writer.print("\x1b[1;35mPointer\x1b[0m ", .{});

                try TypeQualifier.write(@truncate(type_qualifier), writer);

                result = .{
                    .node = base_type,
                };
            },

            .char_type,
            .signed_char_type,
            .unsigned_char_type,
            .short_type,
            .signed_short_type,
            .unsigned_short_type,
            .int_type,
            .signed_int_type,
            .unsigned_int_type,
            .long_type,
            .signed_long_type,
            .unsigned_long_type,
            .long_long_type,
            .signed_long_long_type,
            .unsigned_long_long_type,
            .float_type,
            .double_type,
            .long_double_type,
            .bool_type,
            .unsigned,
            .signed,
            .void_type,
            .complex_type,
            .imaginary_type,
            => {
                const qualifier = self.data.as(.eight).h;
                try writer.print("\x1b[1;35mType\x1b[0m ", .{});
                try TypeQualifier.write(qualifier, writer);
                try writer.print("\x1b[32m{s}\x1b[0m", .{typeToStr(self.kind)});
            },
            .array_type => {
                @setEvalBranchQuota(100000);
                const base_index = self.data.as(.two).a;
                const qualifier = self.data.as(.eight).h;

                try writer.print("\x1b[1;35mArrayType\x1b[0m [", .{});
                try TypeQualifier.write(qualifier, writer);
                try writer.print("]", .{});
                result = .{ .node = base_index };
            },
            .array_type_variable => {
                @setEvalBranchQuota(100000);
                const base_index = self.data.as(.two).a;
                const qualifier = self.data.as(.eight).h;

                try writer.print("\x1b[1;35mArrayType\x1b[0m [", .{});
                try TypeQualifier.write(qualifier, writer);
                try writer.print("*]", .{});
                result = .{ .node = base_index };
            },
            .array_type_static => {
                @setEvalBranchQuota(100000);
                const base_index = self.data.as(.two).a;
                const size_index = self.data.as(.four).c;
                const qualifier = self.data.as(.eight).h;

                try writer.print("\x1b[1;35mArrayType\x1b[0m [", .{});
                try TypeQualifier.write(qualifier, writer);
                try writer.print("static <child_value>]", .{});

                result = NodeRangeOrNode.initNodes(&.{
                    absoluteIndex(index, size_index),
                    base_index,
                });
            },
            .array_type_fixed => {
                @setEvalBranchQuota(100000);
                const base_index = self.data.as(.two).a;
                const size_index = self.data.as(.four).c;
                const qualifier = self.data.as(.eight).h;

                try writer.print("\x1b[1;35mArrayType\x1b[0m [", .{});
                try TypeQualifier.write(qualifier, writer);
                try writer.print("<child_value>] {}", .{size_index});

                result = NodeRangeOrNode.initNodes(&.{
                    absoluteIndex(index, size_index),
                    base_index,
                });
            },
            .function_type => {
                const return_type_index = absoluteIndex(index, self.data.as(.four).a);
                try writer.print("\x1b[1;35mFunctionType\x1b[0m", .{});

                result = .{
                    .node = return_type_index,
                };
            },
            .function_type_one_parameter => {
                const return_type_index = absoluteIndex(index, self.data.as(.four).a);
                const parameter_index = absoluteIndex(index, self.data.as(.four).b);
                try writer.print("\x1b[1;35mFunctionType\x1b[0m", .{});

                result = NodeRangeOrNode.initNodes(&.{ return_type_index, parameter_index });
            },
            .function_type_parameter => {
                const return_type_index = absoluteIndex(index, self.data.as(.four).a);
                const parameter_start_index = absoluteIndex(index, self.data.as(.four).b);
                const parameter_count = self.data.as(.eight).g;
                try writer.print("\x1b[1;35mFunctionType\x1b[0m", .{});

                result = NodeRangeOrNode.initNodeAndRange(return_type_index, parameter_start_index, parameter_count);
            },
            .@"struct" => {
                try writer.print("\x1b[1;35mStruct\x1b[0m", .{});
                const member_range_start = self.data.as(.two).a;
                const member_range_count = self.data.as(.two).b;

                result = .{
                    .node_range = .{
                        .start = member_range_start,
                        .count = member_range_count,
                    },
                };
            },
            .struct_forward => {
                const ident_index: TokenIndex = @bitCast(self.data.as(.two).a);
                try writer.print("\x1b[1;35mStruct\x1b[0m \x1b[1;36m'{s}'\x1b[0m", .{unit.identifierAt(ident_index)});
            },
            .struct_ident => {
                const ident_index: TokenIndex = @bitCast(self.data.as(.two).a);
                const member_range_index = absoluteIndex(index, 1);
                const member_range = unit.nodes.items[member_range_index];
                try writer.print("\x1b[1;35mStruct\x1b[0m \x1b[1;36m'{s}'\x1b[0m", .{unit.identifierAt(ident_index)});
                std.debug.assert(member_range.kind == .range);

                result = .{
                    .node_range = .{
                        .start = member_range.data.two.a,
                        .count = member_range.data.two.b,
                    },
                };
            },
            .member => {
                const type_index = absoluteIndex(index, self.data.as(.four).c);
                try writer.print("\x1b[1;35mMember\x1b[0m", .{});

                result = .{
                    .node = type_index,
                };
            },
            .member_ident => {
                const ident_index: TokenIndex = @bitCast(self.data.as(.two).a);
                const type_index = absoluteIndex(index, self.data.as(.four).c);
                try writer.print("\x1b[1;35mMember\x1b[0m \x1b[1;36m'{s}'\x1b[0m", .{unit.identifierAt(ident_index)});

                result = .{
                    .node = type_index,
                };
            },
            .member_bitfield => {
                const type_index = absoluteIndex(index, self.data.as(.four).c);
                const bitfield_index = absoluteIndex(index, self.data.as(.four).d);
                try writer.print("\x1b[1;35mMember\x1b[0m", .{});

                result = NodeRangeOrNode.initNodes(&.{ type_index, bitfield_index });
            },
            .member_ident_bitfield => {
                const ident_index: TokenIndex = @bitCast(self.data.as(.two).a);
                const type_index = absoluteIndex(index, self.data.as(.four).c);
                const bitfield_index = absoluteIndex(index, self.data.as(.four).d);
                try writer.print("\x1b[1;35mMember\x1b[0m \x1b[1;36m'{s}'\x1b[0m", .{unit.identifierAt(ident_index)});

                result = NodeRangeOrNode.initNodes(&.{ type_index, bitfield_index });
            },
            .@"enum" => {
                try writer.print("\x1b[1;35mEnum\x1b[0m", .{});
                const member_range_start = self.data.as(.two).a;
                const member_range_count = self.data.as(.two).b;

                result = .{
                    .node_range = .{
                        .start = member_range_start,
                        .count = member_range_count,
                    },
                };
            },
            .enum_ident => {
                const ident_index: TokenIndex = @bitCast(self.data.as(.two).a);
                try writer.print("\x1b[1;35mEnum\x1b[0m \x1b[1;36m'{s}'\x1b[0m", .{unit.identifierAt(ident_index)});

                const member_range_index = absoluteIndex(index, 1);
                const member_range = unit.nodes.items[member_range_index];
                std.debug.assert(member_range.kind == .range);

                result = .{
                    .node_range = .{
                        .start = member_range.data.two.a,
                        .count = member_range.data.two.b,
                    },
                };
            },
            .enum_forward => {
                const ident_index: TokenIndex = @bitCast(self.data.as(.two).a);
                try writer.print("\x1b[1;35mEnum\x1b[0m \x1b[1;36m'{s}'\x1b[0m", .{unit.identifierAt(ident_index)});
            },
            .enum_member => {
                const ident_index: TokenIndex = @bitCast(self.data.as(.two).a);
                try writer.print("\x1b[1;35mMember\x1b[0m \x1b[1;36m'{s}'\x1b[0m", .{unit.identifierAt(ident_index)});
            },
            .enum_member_value => {
                const ident_index: TokenIndex = @bitCast(self.data.as(.two).a);
                const value_index = self.data.as(.two).b;
                try writer.print("\x1b[1;35mMember\x1b[0m \x1b[1;36m'{s}'\x1b[0m", .{unit.identifierAt(ident_index)});

                result = .{ .node = value_index };
            },
            .parameter => {
                const type_index = absoluteIndex(index, self.data.as(.four).a);
                const storage_class = self.data.as(.eight).c;
                try writer.print("\x1b[1;35mParameter \x1b[0m", .{});
                try StorageClass.write(storage_class, writer);

                result = .{ .node = type_index };
            },
            .parameter_ident => {
                const type_index = absoluteIndex(index, self.data.as(.four).a);
                const ident_index: TokenIndex = @bitCast(self.data.as(.two).a);
                const storage_class = self.data.as(.eight).c;
                try writer.print("\x1b[1;35mParameter\x1b[0m \x1b[1;36m'{s}'\x1b[0m ", .{unit.identifierAt(ident_index)});
                try StorageClass.write(storage_class, writer);

                result = .{ .node = type_index };
            },
            .parameter_ellipsis => {
                try writer.print("\x1b[1;35mParameterEllipsis\x1b[0m ", .{});
            },
            .type_name => {
                const ident_index: TokenIndex = @bitCast(self.data.as(.two).a);
                const qualifier = self.data.as(.eight).h;

                try writer.print("\x1b[1;35mType\x1b[0m \x1b[32m{s}\x1b[0m ", .{unit.identifierAt(ident_index)});
                try TypeQualifier.write(qualifier, writer);
            },
            .atomic_type => {
                const ident_index: TokenIndex = @bitCast(self.data.as(.two).a);
                const qualifier = self.data.as(.eight).h;

                try writer.print("\x1b[1;35mAtomic\x1b[0m \x1b[32m{s}\x1b[0m ", .{unit.identifierAt(ident_index)});
                try TypeQualifier.write(qualifier, writer);
            },
            .identifier => {
                const ident_index: TokenIndex = @bitCast(self.data.as(.two).a);

                try writer.print("\x1b[1;35mIdentifier\x1b[0m \x1b[33m{s}\x1b[0m", .{unit.identifierAt(ident_index)});
            },
            .binary_lr_operator => {
                const lhs = self.data.as(.two).a;
                const rhs = self.data.as(.four).c;
                const operator: tok.TokenKind = @enumFromInt(self.data.as(.four).d);

                try writer.print("\x1b[1;35mBinOp\x1b[0m \x1b[33m{s}\x1b[0m", .{operator.toStr()});

                result = NodeRangeOrNode.initNodes(&.{
                    lhs,
                    absoluteIndex(index, rhs),
                });
            },
            .unary_prefix_operator => {
                const expr = self.data.as(.two).a;
                const operator: tok.TokenKind = @enumFromInt(self.data.as(.four).c);

                try writer.print("\x1b[1;35mUnaryPrefixOp\x1b[0m \x1b[33m{s}\x1b[0m", .{operator.toStr()});

                result = .{
                    .node = expr,
                };
            },
            .unary_suffix_operator => {
                const expr = self.data.as(.two).a;
                const operator: tok.TokenKind = @enumFromInt(self.data.as(.four).c);

                try writer.print("\x1b[1;35mUnarySuffixOp\x1b[0m \x1b[33m{s}\x1b[0m", .{operator.toStr()});

                result = .{
                    .node = expr,
                };
            },
            .invoke => {
                const expr = self.data.as(.two).a;

                try writer.print("\x1b[1;35mInvoke\x1b[0m", .{});

                result = .{
                    .node = expr,
                };
            },
            .invoke_one_arg => {
                const expr = self.data.as(.two).a;
                const arg = self.data.as(.two).a;

                try writer.print("\x1b[1;35mInvoke\x1b[0m", .{});

                result = NodeRangeOrNode.initNodes(&.{ expr, arg });
            },
            .invoke_args => {
                const expr = self.data.as(.four).a;
                const count = self.data.as(.four).b;
                const start_index = self.data.as(.two).b;

                try writer.print("\x1b[1;35mInvoke\x1b[0m", .{});

                result = NodeRangeOrNode.initNodeAndRange(absoluteIndex(index, expr), start_index, count);
            },
            .index => {
                const expr = self.data.as(.two).a;
                const index_expr = self.data.as(.two).b;

                try writer.print("\x1b[1;35mIndex\x1b[0m", .{});

                result = NodeRangeOrNode.initNodes(&.{ expr, index_expr });
            },
            .designator_index_terminal => {
                const index_expr = self.data.as(.two).a;
                try writer.print("\x1b[1;35mDesignatorIndex\x1b[0m", .{});

                result = .{ .node = index_expr };
            },
            .designator_field_terminal => {
                const ident_index: TokenIndex = @bitCast(self.data.as(.two).a);
                try writer.print("\x1b[1;35mDesignatorField\x1b[0m \x1b[1;36m'{s}'\x1b[0m", .{unit.identifierAt(ident_index)});
            },
            .designator_index => {
                const index_expr = self.data.as(.two).a;
                const prev = self.data.as(.two).b;
                try writer.print("\x1b[1;35mDesignatorIndex\x1b[0m", .{});

                result = NodeRangeOrNode.initNodes(&.{ prev, index_expr });
            },
            .designator_field => {
                const ident_index: TokenIndex = @bitCast(self.data.as(.two).a);
                const prev = self.data.as(.two).b;
                try writer.print("\x1b[1;35mDesignatorField\x1b[0m \x1b[1;36m'{s}'\x1b[0m", .{unit.identifierAt(ident_index)});
                result = .{ .node = prev };
            },
            .designation => {
                const designator_index = self.data.as(.two).a;
                const initializer = self.data.as(.two).b;
                try writer.print("\x1b[1;35mDesignation\x1b[0m", .{});

                result = NodeRangeOrNode.initNodes(&.{ designator_index, initializer });
            },
            .initializer_list => {
                try writer.print("\x1b[1;35mInitializerList\x1b[0m", .{});
            },
            .initializer_list_one => {
                const designator_index = self.data.as(.two).a;
                try writer.print("\x1b[1;35mInitializerList\x1b[0m", .{});

                result = .{ .node = designator_index };
            },
            .initializer_list_many => {
                const start = self.data.as(.two).a;
                const count = self.data.as(.two).b;
                try writer.print("\x1b[1;35mInitializerList\x1b[0m", .{});

                result = .{
                    .node_range = .{
                        .start = start,
                        .count = count,
                    },
                };
            },
            .static_assert => {
                const expr = self.data.as(.two).a;
                try writer.print("\x1b[1;35mStaticAssert\x1b[0m", .{});
                result = .{ .node = expr };
            },
            .static_assert_str => {
                const test_expr = self.data.as(.two).a;
                const expr = self.data.as(.two).b;
                try writer.print("\x1b[1;35mStaticAssert\x1b[0m", .{});
                result = NodeRangeOrNode.initNodes(&.{ test_expr, expr });
            },
            .compound_empty => {
                try writer.print("\x1b[1;35mCompound\x1b[0m", .{});
            },
            .compound_one => {
                const stmt_index = self.data.as(.two).a;
                try writer.print("\x1b[1;35mCompound\x1b[0m", .{});

                result = .{ .node = stmt_index };
            },
            .compound => {
                const start = self.data.as(.two).a;
                const count = self.data.as(.two).b;
                try writer.print("\x1b[1;35mCompound\x1b[0m", .{});

                result = .{
                    .node_range = .{ .start = start, .count = count },
                };
            },
            .while_loop => {
                const condition = self.data.as(.two).a;
                const body = self.data.as(.two).b;
                try writer.print("\x1b[1;35mWhile\x1b[0m", .{});
                result = NodeRangeOrNode.initNodes(&.{ condition, body });
            },
            .while_loop_empty => {
                const condition = self.data.as(.two).a;
                try writer.print("\x1b[1;35mWhile\x1b[0m", .{});
                result = .{ .node = condition };
            },
            .do_while_loop => {
                const condition = self.data.as(.two).a;
                const body = self.data.as(.two).b;
                try writer.print("\x1b[1;35mDoWhile\x1b[0m", .{});
                result = NodeRangeOrNode.initNodes(&.{ condition, body });
            },
            .do_while_loop_empty => {
                const condition = self.data.as(.two).a;
                try writer.print("\x1b[1;35mDoWhile\x1b[0m", .{});
                result = .{ .node = condition };
            },
            .for_loop => {
                const init = absoluteIndex(index, self.data.as(.four).a);
                const condition = absoluteIndex(index, self.data.as(.four).b);
                const body = absoluteIndex(index, self.data.as(.four).d);
                try writer.print("\x1b[1;35mFor\x1b[0m", .{});
                result = NodeRangeOrNode.initNodes(&.{ init, condition, body });
            },
            .for_loop_inc => {
                const init = absoluteIndex(index, self.data.as(.four).a);
                const condition = absoluteIndex(index, self.data.as(.four).b);
                const increment = absoluteIndex(index, self.data.as(.four).c);
                const body = absoluteIndex(index, self.data.as(.four).d);
                try writer.print("\x1b[1;35mFor\x1b[0m", .{});
                result = NodeRangeOrNode.initNodes(&.{ init, condition, increment, body });
            },
            .for_loop_empty => {
                const init = absoluteIndex(index, self.data.as(.four).a);
                const condition = absoluteIndex(index, self.data.as(.four).b);
                try writer.print("\x1b[1;35mFor\x1b[0m", .{});
                result = NodeRangeOrNode.initNodes(&.{
                    init,
                    condition,
                });
            },
            .for_loop_empty_inc => {
                const init = absoluteIndex(index, self.data.as(.four).a);
                const condition = absoluteIndex(index, self.data.as(.four).b);
                const increment = absoluteIndex(index, self.data.as(.four).c);
                try writer.print("\x1b[1;35mFor\x1b[0m", .{});
                result = NodeRangeOrNode.initNodes(&.{ init, condition, increment });
            },
            .switch_case => {
                const value = self.data.as(.two).a;
                const body = self.data.as(.two).b;
                try writer.print("\x1b[1;35mSwitch\x1b[0m", .{});
                result = NodeRangeOrNode.initNodes(&.{ value, body });
            },
            .case => {
                const expr = self.data.as(.two).a;
                const statement = self.data.as(.two).b;
                try writer.print("\x1b[1;35mCase\x1b[0m", .{});
                result = NodeRangeOrNode.initNodes(&.{ expr, statement });
            },
            .default => {
                const statement = self.data.as(.two).a;
                try writer.print("\x1b[1;35mDefault\x1b[0m", .{});
                result = .{ .node = statement };
            },
            .label => {
                const ident: TokenIndex = @bitCast(self.data.as(.two).a);
                const statement = self.data.as(.two).b;
                try writer.print("\x1b[1;35mLabel\x1b[0m \x1b[1;36m'{s}'\x1b[0m", .{unit.identifierAt(ident)});
                result = .{ .node = statement };
            },
            .goto => {
                const ident: TokenIndex = @bitCast(self.data.as(.two).a);
                try writer.print("\x1b[1;35mLabel\x1b[0m \x1b[1;36m'{s}'\x1b[0m", .{unit.identifierAt(ident)});
            },
            .continue_statement => {
                try writer.print("\x1b[1;35mContinue\x1b[0m", .{});
            },
            .break_statement => {
                try writer.print("\x1b[1;35mBreak\x1b[0m", .{});
            },
            .return_statement => {
                try writer.print("\x1b[1;35mReturn\x1b[0m", .{});
            },
            .return_statement_value => {
                const expr = self.data.as(.two).a;
                try writer.print("\x1b[1;35mReturn\x1b[0m", .{});
                result = .{ .node = expr };
            },
            .if_statement => {
                const condition = self.data.as(.two).a;
                const body = self.data.as(.two).b;
                try writer.print("\x1b[1;35mIf\x1b[0m", .{});
                result = NodeRangeOrNode.initNodes(&.{ condition, body });
            },
            .if_statement_no_body => {
                const condition = self.data.as(.two).a;
                try writer.print("\x1b[1;35mIf\x1b[0m", .{});
                result = .{ .node = condition };
            },
            .if_statement_else => {
                const condition = self.data.as(.two).a;
                const body = absoluteIndex(index, self.data.as(.four).c);
                const else_clause = absoluteIndex(index, self.data.as(.four).d);
                try writer.print("\x1b[1;35mIf\x1b[0m", .{});
                result = NodeRangeOrNode.initNodes(&.{ condition, body, else_clause });
            },
            .if_statement_no_body_else => {
                const condition = self.data.as(.two).a;
                const else_clause = self.data.as(.two).b;
                try writer.print("\x1b[1;35mIf\x1b[0m", .{});
                result = NodeRangeOrNode.initNodes(&.{ condition, else_clause });
            },
            .empty_statement => {
                try writer.print("\x1b[1;35mEmptyStatement\x1b[0m", .{});
            },
            else => try writer.print("tbd {}", .{self.kind}),
        }
        try writer.writeByte('\n');

        switch (result) {
            .none => {},
            .node => |idx| {
                try writeTree(idx, unit, indent + 1, true, writer);
            },
            .node_range => |rng| {
                for (rng.start..rng.start + rng.count) |i| {
                    const node_index = unit.node_ranges.items[i];

                    const is_last = i == rng.start + rng.count - 1;
                    try writeTree(node_index, unit, indent + 1, is_last, writer);
                }
            },
            .nodes => |rng| {
                for (0..rng.count) |i| {
                    const node_index = rng.indicies[i];

                    const is_last = i == rng.count - 1;
                    try writeTree(node_index, unit, indent + 1, is_last, writer);
                }
            },
            .node_and_range => |rng| {
                try writeTree(rng.node, unit, indent + 1, rng.range.count == 0, writer);

                for (rng.range.start..rng.range.start + rng.range.count) |i| {
                    const node_index = unit.node_ranges.items[i];

                    const is_last = i == rng.range.start + rng.range.count - 1;
                    try writeTree(node_index, unit, indent + 1, is_last, writer);
                }
            },
        }
    }

    pub inline fn typeToStr(node_kind: NodeKind) []const u8 {
        return switch (node_kind) {
            .char_type => "char",
            .signed_char_type => "signed char",
            .unsigned_char_type => "unsigned char",
            .short_type => "short",
            .signed_short_type => "signed short",
            .unsigned_short_type => "unsigned short",
            .int_type => "int",
            .signed_int_type => "signed int",
            .unsigned_int_type => "unsigned int",
            .long_type => "long",
            .signed_long_type => "signed long",
            .unsigned_long_type => "unsigned long",
            .long_long_type => "long long",
            .signed_long_long_type => "signed long long",
            .unsigned_long_long_type => "unsigned long long",
            .float_type => "float",
            .double_type => "double",
            .long_double_type => "long double",
            .bool_type => "bool",
            .unsigned => "unsigned",
            .signed => "signed",
            .void_type => "void",
            .complex_type => "complex",
            .imaginary_type => "imaginary",
            else => @panic(""),
        };
    }

    pub fn write(self: *@This(), writer: anytype) !void {
        switch (self.kind) {
            .int_literal => try writer.print("{}", .{self.data.long}),
            .float_literal => try writer.print("{}", .{self.data.double}),

            .declaration => try writer.print("ident offset: {}, type_offset(rel): {}, storage: {x}", .{ self.data.two.a, self.data.as(.four).c, self.data.as(.eight).g }),

            .char_type,
            .unsigned_char_type,
            .signed_char_type,
            .short_type,
            .unsigned_short_type,
            .signed_short_type,
            .int_type,
            .unsigned_int_type,
            .signed_int_type,
            .long_type,
            .unsigned_long_type,
            .signed_long_type,
            .long_long_type,
            .unsigned_long_long_type,
            .signed_long_long_type,
            .unsigned,
            .signed,
            .float_type,
            .double_type,
            .long_double_type,
            .bool_type,
            => try writer.print("{x}", .{self.data.eight.a}),

            else => try writer.print("{}", .{self.data.long}),
            // .declaration => try writer.print("offset(rel): {}, count: {}, type(rel): {}, storage: {}", )
        }
    }
};

pub const NodeRange = struct {
    start: NodeIndex,
    count: u32,
};

pub const Parser = struct {
    allocator: std.mem.Allocator,
    unit: *Unit,
    tokenizer: *Tokenizer,
    // virtual_tokens: std.ArrayList(tok.Token),
    // virtual_token_next: u32 = 0,
    in_handle_pp: bool = false,

    expansion_argument_map: std.StringHashMap(DefineValue),

    const Self = @This();
    pub fn init(unit: *Unit, tokenizer: *Tokenizer) Self {
        return .{
            .allocator = unit.allocator,
            .unit = unit,
            .tokenizer = tokenizer,
            // .virtual_tokens = std.ArrayList(tok.Token).init(unit.allocator),
            .expansion_argument_map = std.StringHashMap(DefineValue).init(unit.allocator),
        };
    }

    pub fn parseUnit(self: *Self) !NodeRange {
        var these_nodes = std.ArrayList(NodeIndex).init(self.allocator);
        try self.handlePP();

        var ptok = self.peekToken();
        while (ptok) |_| : (ptok = self.peekToken()) {
            const node_index = try self.parseExternalDeclaration();
            try these_nodes.append(node_index);
        }

        const starti = self.unit.node_ranges.items.len;
        try self.unit.node_ranges.appendSlice(these_nodes.items);

        return .{
            .start = @truncate(starti),
            .count = @truncate(these_nodes.items.len),
        };
    }

    pub fn parseExternalDeclaration(self: *Self) !NodeIndex {
        try self.handlePP();
        return try self.parseDeclaration(true);
        // const ptok = self.peekToken() orelse return null;

        // const node = switch (ptok.kind) {
        //     .int_literal => Node{
        //         .kind = .int_literal,
        //         .data = .{
        //             .long = ptok.ivalue(self.unit),
        //         },
        //     },
        //     .float_literal => Node{
        //         .kind = .float_literal,
        //         .data = .{
        //             .double = ptok.fvalue(self.unit),
        //         },
        //     },
        //     else => return null,
        // };
        // _ = self.nextToken();

        // return try self.createNode(node);
    }

    pub fn parseDeclaration(self: *Self, toplevel: bool) !NodeIndex {
        var storage_class: StorageClass.Type = 0;

        var ptok = self.peekToken();
        if (ptok != null and ptok.?.kind == .static_assert) {
            const result = try self.parseStaticAssert();
            _ = try self.expect(.semicolon);
            return result;
        }

        const type_node = try self.parseDeclarationSpecifiers(&storage_class);
        ptok = self.peekToken();

        var these_nodes = std.ArrayList(NodeIndex).init(self.allocator);
        defer these_nodes.deinit();
        while (ptok) |p| : (ptok = self.peekToken()) {
            switch (p.kind) {
                .semicolon => {
                    _ = self.nextToken();
                    if (these_nodes.items.len == 0) {
                        return type_node;
                    }
                    break;
                },
                else => {
                    var this_ident: ?TokenIndex = null;
                    const this_type = try self.parseDeclarator(type_node, &this_ident);

                    if (this_type.is_function and !this_type.is_function_ptr) {
                        var decl_node_data: NodeData = undefined;
                        decl_node_data.as(.two).a = @bitCast(this_ident orelse @panic("TODO: expected identifier in variable decl"));

                        ptok = self.peekToken();
                        if (toplevel and these_nodes.items.len == 0 and ptok != null and ptok.?.kind == .open_brace) {
                            const body = try self.parseCompoundStatement();
                            decl_node_data.as(.four).c = @truncate(self.relativeOffset(this_type.node.?));
                            decl_node_data.as(.four).d = @truncate(self.relativeOffset(body));

                            return try self.createNode(Node{
                                .kind = .function_declaration_body,
                                .data = decl_node_data,
                            });
                        }

                        decl_node_data.as(.four).c = @truncate(self.relativeOffset(this_type.node.?));
                        try these_nodes.append(try self.createNode(Node{
                            .kind = .function_declaration,
                            .data = decl_node_data,
                        }));
                    } else {
                        if (this_ident == null) {
                            try these_nodes.append(this_type.node.?);
                            continue;
                        }

                        var kind = NodeKind.var_declaration;
                        var decl_node_data: NodeData = undefined;
                        decl_node_data.as(.two).a = @bitCast(this_ident orelse @panic("TODO: expected identifier in variable decl"));

                        ptok = self.peekToken();
                        if (ptok != null and ptok.?.kind == .assignment) {
                            _ = self.nextToken();

                            const decl_init = try self.parseInitializer();
                            decl_node_data.as(.four).d = @truncate(self.relativeOffset(decl_init));
                            kind = .var_declaration_init;
                        } else if ((storage_class & StorageClass.typedef) > 0) {
                            try self.unit.type_names.put(self.unit.identifierAt(@bitCast(this_ident.?)), {});
                        }

                        decl_node_data.as(.four).c = @truncate(self.relativeOffset(this_type.node.?));

                        try these_nodes.append(try self.createNode(Node{
                            .kind = kind,
                            .data = decl_node_data,
                        }));
                    }
                },
            }

            ptok = self.peekToken();
            if (ptok == null) continue;
            switch (ptok.?.kind) {
                .comma => {
                    _ = self.nextToken();
                },
                .semicolon => {
                    _ = self.nextToken();
                    break;
                },
                else => std.debug.panic("TODO: unexpected token {}", .{ptok.?.kind}),
            }
        }

        const starti = self.unit.node_ranges.items.len;
        try self.unit.node_ranges.appendSlice(these_nodes.items);

        var decl_node_data: NodeData = undefined;
        decl_node_data.as(.two).a = @truncate(starti);
        decl_node_data.as(.four).c = @truncate(these_nodes.items.len);
        decl_node_data.as(.four).d = storage_class;

        const decl_node = try self.createNode(Node{
            .kind = .declaration,
            .data = decl_node_data,
        });

        return decl_node;
    }

    pub fn parseDeclarationSpecifiers(self: *Self, storage_class: *StorageClass.Type) (ParseError || std.mem.Allocator.Error)!NodeIndex {
        var tidx = self.peekTokenIndex();
        var ptok = self.peekToken();
        var type_qualifier: TypeQualifier.Type = 0;
        var type_kind: ?NodeKind = null;
        var type_name_token: tok.TokenIndex = undefined;

        while (ptok) |p| : ({
            ptok = self.peekToken();
            tidx = self.peekTokenIndex();
        }) {
            switch (p.kind) {
                .atomic => {
                    _ = self.nextToken();
                    ptok = self.peekToken();
                    if (ptok != null and ptok.?.kind == .open_paren) {
                        _ = self.nextToken();
                        type_kind = .atomic_type;
                        type_name_token = try self.expectIndex(.type_name);
                        _ = try self.expect(.close_paren);
                    } else {
                        type_qualifier |= TypeQualifier.atomic;
                    }
                    continue;
                },
                .auto => storage_class.* |= StorageClass.auto,
                .@"const" => type_qualifier |= TypeQualifier.@"const",
                .@"extern" => storage_class.* |= StorageClass.@"extern",
                .register => storage_class.* |= StorageClass.register,
                .restrict => type_qualifier |= TypeQualifier.restrict,
                .static => storage_class.* |= StorageClass.static,
                .thread_local => storage_class.* |= StorageClass.thread_local,
                .typedef => storage_class.* |= StorageClass.typedef,
                .@"volatile" => type_qualifier |= TypeQualifier.@"volatile",

                .unsigned => {
                    if (type_kind) |tk| {
                        type_kind = makeUnsigned(tk);
                    } else {
                        type_kind = .unsigned;
                    }
                },
                .signed => {
                    if (type_kind) |tk| {
                        type_kind = makeSigned(tk);
                    } else {
                        type_kind = .signed;
                    }
                },
                .long => {
                    if (type_kind) |tk| {
                        type_kind = switch (tk) {
                            .long_type => .long_long_type,
                            .int_type => .long_type,
                            .unsigned => .unsigned_long_type,
                            .unsigned_long_type => .unsigned_long_long_type,
                            .unsigned_int_type => .unsigned_long_type,
                            .signed => .signed_long_type,
                            .signed_long_type => .signed_long_long_type,
                            .signed_int_type => .signed_long_type,
                            .double_type => .long_double_type,
                            else => @panic("TODO: long incompatible with type"),
                        };
                    } else {
                        type_kind = .long_type;
                    }
                },
                .int => {
                    if (type_kind) |tk| {
                        type_kind = switch (tk) {
                            .short_type,
                            .long_type,
                            .long_long_type,
                            .unsigned_short_type,
                            .unsigned_long_type,
                            .unsigned_long_long_type,
                            .signed_short_type,
                            .signed_long_type,
                            .signed_long_long_type,
                            => tk,
                            .unsigned => .unsigned_int_type,
                            .signed => .signed_int_type,
                            else => @panic("TODO: long incompatible with type"),
                        };
                    } else {
                        type_kind = .int_type;
                    }
                },
                .short => {
                    if (type_kind) |tk| {
                        type_kind = switch (tk) {
                            .int_type => .short_type,
                            .unsigned => .unsigned_short_type,
                            .unsigned_int_type => .unsigned_short_type,
                            .signed => .signed_short_type,
                            .signed_int_type => .signed_short_type,
                            else => @panic("TODO: long incompatible with type"),
                        };
                    } else {
                        type_kind = .short_type;
                    }
                },
                .char => {
                    if (type_kind) |tk| {
                        type_kind = switch (tk) {
                            .unsigned => .unsigned_char_type,
                            .signed => .signed_char_type,
                            else => @panic("TODO: long incompatible with type"),
                        };
                    } else {
                        type_kind = .char_type;
                    }
                },
                .float => type_kind = .float_type,
                .double => {
                    if (type_kind) |tk| {
                        type_kind = switch (tk) {
                            .long_type => .long_double_type,
                            else => @panic("TODO: long incompatible with type"),
                        };
                    } else {
                        type_kind = .double_type;
                    }
                },
                .bool => type_kind = .bool_type,
                .@"struct", .@"union" => return try self.parseStructOrUnion(),
                .@"enum" => return try self.parseEnum(),

                .type_name => {
                    type_kind = .type_name;
                    type_name_token = tidx;
                },

                else => break,
            }
            _ = self.nextToken();
        }

        var type_node_data: NodeData = undefined;
        switch (type_kind.?) {
            .type_name, .atomic_type => {
                type_node_data.two.a = @bitCast(type_name_token);
            },
            else => {},
        }
        type_node_data.eight.h = type_qualifier;

        const type_node = try self.createNode(Node{
            .kind = type_kind.?,
            .data = type_node_data,
        });

        return type_node;
    }

    pub const DeclaratorResult = struct {
        node: ?NodeIndex = null,
        is_function: bool = false,
        is_function_ptr: bool = false,
        is_pointer: bool = false,
    };

    pub fn parseDeclarator(self: *Self, base_type: NodeIndex, identifier: *?TokenIndex) !DeclaratorResult {
        const ptok = self.peekToken() orelse @panic("ran out of tokens");
        switch (ptok.kind) {
            .star => {
                _ = self.nextToken();
                const type_qualifier = self.parseAnyTypeQualifiers();
                const element_type = try self.parseDeclarator(base_type, identifier);

                return .{
                    .node = try self.createNode(Node{
                        .kind = .pointer,
                        .data = .{
                            .two = .{
                                .a = element_type.node.?,
                                .b = type_qualifier,
                            },
                        },
                    }),
                    .is_function = element_type.is_function,
                    .is_function_ptr = element_type.is_function_ptr,
                    .is_pointer = true,
                };
            },
            else => return try self.parseDirectDeclarator(base_type, identifier),
        }
    }

    fn parseDirectDeclarator(self: *Self, base_type: NodeIndex, identifier: *?TokenIndex) (ParseError || std.mem.Allocator.Error)!DeclaratorResult {
        var ptok = self.peekToken() orelse @panic("ran out of tokens");
        var left = DeclaratorResult{};

        switch (ptok.kind) {
            .identifier => {
                const tidx = self.peekTokenIndex();
                _ = self.nextToken();
                identifier.* = tidx;
                left = .{ .node = base_type };
            },
            .open_paren => {
                _ = self.nextToken();
                const ntok = self.peekToken();
                // if (ntok != null and ntok.?.kind != .open_paren and ntok.?.kind != .open_bracket and ntok.?.kind != ) {
                // }
                if (ntok != null and (ntok.?.kind == .star or ntok.?.kind == .identifier)) {
                    left = try self.parseDeclarator(base_type, identifier);
                    _ = try self.expect(.close_paren);
                } else {
                    left = .{
                        .node = try self.parseParameters(base_type),
                        .is_function = true,
                        .is_function_ptr = left.is_pointer,
                    };
                }
            },
            else => {
                left = .{ .node = base_type };
            },
        }

        ptok = self.peekToken() orelse return left;
        while (true) : (ptok = self.peekToken() orelse break) {
            switch (ptok.kind) {
                .open_bracket => {
                    _ = self.nextToken();
                    var type_qualifiers = self.parseAnyTypeQualifiers();
                    ptok = self.peekToken() orelse @panic("unexpecteed EOF");

                    var node_data: NodeData = undefined;
                    node_data.as(.two).a = left.node.?;
                    node_data.as(.eight).h = type_qualifiers;

                    switch (ptok.kind) {
                        .close_bracket => {
                            _ = self.nextToken();
                            // node_data.as(.two).a = (try self.parseDirectDeclarator(base_type, base_ident, identifier)) orelse base_ident.*.?;

                            left.node = try self.createNode(Node{
                                .kind = .array_type,
                                .data = node_data,
                            });
                        },
                        .star => {
                            _ = self.nextToken();
                            _ = try self.expect(.close_bracket);
                            // node_data.as(.two).a = (try self.parseDirectDeclarator(base_type, base_ident, identifier)) orelse base_ident.*.?;

                            left.node = try self.createNode(Node{
                                .kind = .array_type_variable,
                                .data = node_data,
                            });
                        },
                        .static => {
                            _ = self.nextToken();
                            type_qualifiers |= self.parseAnyTypeQualifiers();
                            node_data.as(.eight).h = type_qualifiers;
                            const size_expr = try self.parseExpression();
                            _ = try self.expect(.close_bracket);
                            // node_data.as(.two).a = (try self.parseDirectDeclarator(base_type, base_ident, identifier)) orelse base_ident.*.?;

                            node_data.as(.four).c = @truncate(self.relativeOffset(size_expr));

                            left.node = try self.createNode(Node{
                                .kind = .array_type_static,
                                .data = node_data,
                            });
                        },
                        else => {
                            const size_expr = try self.parseExpression();
                            _ = try self.expect(.close_bracket);
                            // std.log.info("{} {} {}", .{ node_data.as(.two).a, size_expr, self.unit.nodes.items.len });
                            // node_data.as(.two).a = (try self.parseDirectDeclarator(base_type, base_ident, identifier)) orelse base_ident.*.?;

                            node_data.as(.four).c = @truncate(self.relativeOffset(size_expr));

                            left.node = try self.createNode(Node{
                                .kind = .array_type_fixed,
                                .data = node_data,
                            });
                        },
                    }
                },
                .open_paren => {
                    _ = self.nextToken();
                    left.node = try self.parseParameters(left.node.?);
                    left.is_function = true;
                    left.is_function_ptr = left.is_pointer;
                },
                else => return left,
            }
        }

        return left;
    }

    pub fn parseParameters(self: *Self, replacement: NodeIndex) !NodeIndex {
        const parameters = try self.parseParameterList();

        var node_kind: NodeKind = undefined;
        var node_data: NodeData = undefined;
        node_data.as(.four).a = @truncate(self.relativeOffset(replacement));
        switch (parameters) {
            .none => {
                node_kind = .function_type;
            },
            .one => |o| {
                node_kind = .function_type_one_parameter;
                node_data.as(.four).b = @truncate(self.relativeOffset(o));
            },
            .range => |rng| {
                node_kind = .function_type_parameter;
                node_data.as(.four).b = @truncate(self.relativeOffset(rng.start));
                node_data.as(.eight).g = @truncate(rng.count);
            },
        }

        return try self.createNode(Node{
            .kind = node_kind,
            .data = node_data,
        });
    }

    const Parameters = union(enum) {
        none: void,
        one: NodeIndex,
        range: NodeRange,
    };

    pub fn parseParameterList(self: *Self) !Parameters {
        var these_nodes = std.ArrayList(NodeIndex).init(self.allocator);
        defer these_nodes.deinit();

        var ptok = self.peekToken();
        while (ptok) |p| : (ptok = self.peekToken()) {
            switch (p.kind) {
                .comma => {
                    _ = self.nextToken();
                    continue;
                },
                .close_paren => {
                    _ = self.nextToken();
                    break;
                },
                .ellipsis => {
                    _ = self.nextToken();
                    _ = try self.expect(.close_paren);
                    try these_nodes.append(try self.createNode(Node{
                        .kind = .parameter_ellipsis,
                        .data = undefined,
                    }));
                    break;
                },
                else => {},
            }

            var storage_class: StorageClass.Type = 0;
            var identifier: ?TokenIndex = null;
            const type_node = try self.parseDeclarationSpecifiers(&storage_class);

            const full_type = try self.parseDeclarator(type_node, &identifier);

            var node_data: NodeData = undefined;
            node_data.as(.four).a = @truncate(self.relativeOffset(full_type.node.?));
            node_data.as(.eight).c = storage_class;

            if (identifier) |ident| {
                node_data.as(.two).b = @bitCast(ident);
                try these_nodes.append(try self.createNode(Node{
                    .kind = .parameter_ident,
                    .data = node_data,
                }));
            } else {
                try these_nodes.append(try self.createNode(Node{
                    .kind = .parameter,
                    .data = node_data,
                }));
            }
        }

        var result: Parameters = undefined;

        if (these_nodes.items.len == 0) {
            result = .{ .none = {} };
        } else if (these_nodes.items.len == 1) {
            result = .{ .one = these_nodes.items[0] };
        } else {
            const start_type_nodes = self.unit.node_ranges.items.len;
            try self.unit.node_ranges.appendSlice(these_nodes.items);
            result = .{
                .range = .{
                    .start = @truncate(start_type_nodes),
                    .count = @truncate(these_nodes.items.len),
                },
            };
        }

        return result;
    }

    pub fn parseStructOrUnion(self: *Self) !NodeIndex {
        const is_struct = self.nextToken().?.kind == .@"struct";
        const ptok = self.peekToken() orelse @panic("unexpcted eof");
        const ident = if (ptok.kind == .identifier) blk: {
            _ = self.nextToken();
            break :blk ptok.start;
        } else null;

        const ntok = self.peekToken();
        if (ntok != null and ntok.?.kind == .open_brace) {
            _ = self.nextToken();

            const members_range = try self.parseStructDeclarations();
            defer {
                _ = self.expect(.close_brace) catch @panic("uhoh");
            }

            if (ident) |i| {
                _ = try self.createNode(Node{
                    .kind = .range,
                    .data = .{
                        .two = .{ .a = members_range.start, .b = members_range.count },
                    },
                });

                return try self.createNode(Node{
                    .kind = if (is_struct) .struct_ident else .union_ident,
                    .data = .{
                        .two = .{
                            .a = i,
                            .b = undefined,
                        },
                    },
                });
            } else {
                return try self.createNode(Node{
                    .kind = if (is_struct) .@"struct" else .union_ident,
                    .data = .{
                        .two = .{ .a = members_range.start, .b = members_range.count },
                    },
                });
            }
        } else {
            return try self.createNode(Node{
                .kind = if (is_struct) .struct_forward else .union_forward,
                .data = .{
                    .two = .{ .a = ident.?, .b = undefined },
                },
            });
        }
    }

    pub fn parseStructDeclarations(self: *Self) !NodeRange {
        var these_nodes = std.ArrayList(NodeIndex).init(self.allocator);
        defer these_nodes.deinit();
        var storage_class: StorageClass.Type = 0;

        var ptok = self.peekToken();
        while (ptok) |p| : (ptok = self.peekToken()) {
            if (p.kind == .close_brace) {
                break;
            } else if (p.kind == .static_assert) {
                try these_nodes.append(try self.parseStaticAssert());
                _ = try self.expect(.semicolon);
                continue;
            }
            const member_type = try self.parseDeclarationSpecifiers(&storage_class);
            try self.parseStructDeclarator(member_type, &these_nodes);

            _ = try self.expect(.semicolon);
        }

        const starti = self.unit.node_ranges.items.len;
        try self.unit.node_ranges.appendSlice(these_nodes.items);

        return .{
            .start = @truncate(starti),
            .count = @truncate(these_nodes.items.len),
        };
    }

    pub fn parseStructDeclarator(self: *Self, member_type: NodeIndex, nodes: *std.ArrayList(NodeIndex)) !void {
        var ptok = self.peekToken();
        while (ptok) |p| : (ptok = self.peekToken()) {
            switch (p.kind) {
                .semicolon => {
                    break;
                },
                .comma => {
                    _ = self.nextToken();
                },
                .colon => {
                    var node_data: NodeData = undefined;

                    _ = self.nextToken();
                    const expr = try self.parseOperatorExpression(30);
                    node_data.as(.four).c = @truncate(self.relativeOffset(member_type));
                    node_data.as(.four).d = @truncate(self.relativeOffset(expr));

                    try nodes.append(try self.createNode(Node{
                        .kind = .member_bitfield,
                        .data = node_data,
                    }));
                },
                else => {
                    var identifier: ?TokenIndex = null;
                    const declarator = try self.parseDeclarator(member_type, &identifier);

                    var node_data: NodeData = undefined;
                    if (identifier) |i| node_data.as(.two).a = @bitCast(i);

                    const ntok = self.peekToken();
                    if (ntok != null and ntok.?.kind == .colon) {
                        _ = self.nextToken();
                        const expr = try self.parseOperatorExpression(30);
                        node_data.as(.four).c = @truncate(self.relativeOffset(declarator.node.?));
                        node_data.as(.four).d = @truncate(self.relativeOffset(expr));

                        try nodes.append(try self.createNode(Node{
                            .kind = if (identifier == null) .member_bitfield else .member_ident_bitfield,
                            .data = node_data,
                        }));
                    } else {
                        node_data.as(.four).c = @truncate(self.relativeOffset(declarator.node.?));
                        try nodes.append(try self.createNode(Node{
                            .kind = if (identifier == null) .member else .member_ident,
                            .data = node_data,
                        }));
                    }
                },
            }
        }
    }

    pub fn parseEnum(self: *Self) !NodeIndex {
        _ = self.nextToken();
        const ptok = self.peekToken() orelse @panic("unexpcted eof");
        const ident = if (ptok.kind == .identifier) blk: {
            _ = self.nextToken();
            break :blk ptok.start;
        } else null;

        const ntok = self.peekToken();
        if (ntok != null and ntok.?.kind == .open_brace) {
            _ = self.nextToken();

            const members = try self.parseEnumMembers();

            defer {
                _ = self.expect(.close_brace) catch @panic("no brace");
            }

            if (ident) |i| {
                _ = try self.createNode(Node{
                    .kind = .range,
                    .data = .{
                        .two = .{ .a = members.start, .b = members.count },
                    },
                });
                return try self.createNode(Node{
                    .kind = .enum_ident,
                    .data = .{
                        .two = .{ .a = i, .b = undefined },
                    },
                });
            } else {
                return try self.createNode(Node{
                    .kind = .@"enum",
                    .data = .{
                        .two = .{ .a = members.start, .b = members.count },
                    },
                });
            }
        } else {
            return try self.createNode(Node{
                .kind = .enum_forward,
                .data = .{
                    .two = .{ .a = ident.?, .b = undefined },
                },
            });
        }
    }

    pub fn parseEnumMembers(self: *Self) !NodeRange {
        var these_nodes = std.ArrayList(NodeIndex).init(self.allocator);
        var ptok = self.peekToken();
        while (ptok) |p| : (ptok = self.peekToken()) {
            switch (p.kind) {
                .open_brace => {
                    break;
                },
                .identifier => {
                    const ident = self.nextToken().?;

                    ptok = self.peekToken();
                    if (ptok != null and ptok.?.kind == .assignment) {
                        _ = self.nextToken();
                        const expr = try self.parseOperatorExpression(30);

                        try these_nodes.append(try self.createNode(Node{
                            .kind = .enum_member_value,
                            .data = .{
                                .two = .{
                                    .a = ident.start,
                                    .b = expr,
                                },
                            },
                        }));
                    } else {
                        try these_nodes.append(try self.createNode(Node{
                            .kind = .enum_member,
                            .data = .{
                                .two = .{
                                    .a = ident.start,
                                    .b = undefined,
                                },
                            },
                        }));
                    }

                    ptok = self.peekToken();
                    if (ptok != null and ptok.?.kind == .comma) {
                        _ = self.nextToken();
                    } else {
                        break;
                    }
                },
                else => {},
            }
        }

        const starti = self.unit.node_ranges.items.len;
        try self.unit.node_ranges.appendSlice(these_nodes.items);

        return .{
            .start = @truncate(starti),
            .count = @truncate(these_nodes.items.len),
        };
    }

    fn parseAnyTypeQualifiers(self: *Self) TypeQualifier.Type {
        var ptok = self.peekToken();
        var result: TypeQualifier.Type = 0;
        while (ptok) |p| : (ptok = self.peekToken()) {
            switch (p.kind) {
                .@"const" => result |= TypeQualifier.@"const",
                .@"volatile" => result |= TypeQualifier.@"volatile",
                .restrict => result |= TypeQualifier.restrict,
                .atomic => result |= TypeQualifier.atomic,
                else => break,
            }
            _ = self.nextToken();
        }
        return result;
    }

    fn makeUnsigned(kind: NodeKind) NodeKind {
        return switch (kind) {
            .char_type => .unsigned_char_type,
            .short_type => .unsigned_short_type,
            .int_type => .unsigned_int_type,
            .long_type => .unsigned_long_type,
            .long_long_type => .unsigned_long_long_type,
            else => @panic("TODO: unsigned incompatible with type"),
        };
    }

    fn makeSigned(kind: NodeKind) NodeKind {
        return switch (kind) {
            .char_type => .signed_char_type,
            .short_type => .signed_short_type,
            .int_type => .signed_int_type,
            .long_type => .signed_long_type,
            .long_long_type => .signed_long_long_type,
            else => @panic("TODO: signed incompatible with type"),
        };
    }

    pub fn parseInitializer(self: *Self) (ParseError || std.mem.Allocator.Error)!NodeIndex {
        const ptok = self.peekToken() orelse @panic("Unexpected eof");
        switch (ptok.kind) {
            .open_brace => return self.parseInitializerList(),
            else => return try self.parseOperatorExpression(20),
        }
    }

    pub fn parseInitializerList(self: *Self) !NodeIndex {
        _ = self.nextToken();
        var these_nodes = std.ArrayList(NodeIndex).init(self.allocator);
        defer these_nodes.deinit();

        var ptok = self.peekToken();
        while (ptok) |p| : (ptok = self.peekToken()) {
            switch (p.kind) {
                .close_brace => {
                    _ = self.nextToken();
                    break;
                },
                .dot, .open_bracket => {
                    const designator = try self.parseDesignator();
                    _ = try self.expect(.assignment);
                    const expr = try self.parseInitializer();

                    try these_nodes.append(try self.createNode(Node{
                        .kind = .designation,
                        .data = .{
                            .two = .{ .a = designator, .b = expr },
                        },
                    }));

                    ptok = self.peekToken();
                    if (ptok != null and ptok.?.kind == .comma) {
                        _ = self.nextToken();
                    } else {
                        _ = try self.expect(.close_brace);
                        break;
                    }
                },
                else => {
                    try these_nodes.append(try self.parseInitializer());
                },
            }
        }

        if (these_nodes.items.len == 0) {
            return try self.createNode(Node{
                .kind = .initializer_list,
                .data = undefined,
            });
        } else if (these_nodes.items.len == 1) {
            return try self.createNode(Node{
                .kind = .initializer_list_one,
                .data = .{
                    .two = .{ .a = these_nodes.items[0], .b = undefined },
                },
            });
        } else {
            const starti = self.unit.node_ranges.items.len;
            try self.unit.node_ranges.appendSlice(these_nodes.items);

            return try self.createNode(Node{
                .kind = .initializer_list_many,
                .data = .{
                    .two = .{
                        .a = @truncate(starti),
                        .b = @truncate(these_nodes.items.len),
                    },
                },
            });
        }
    }

    pub fn parseDesignator(self: *Self) !NodeIndex {
        var ptok = self.peekToken();
        var left: NodeIndex = undefined;
        switch (ptok.?.kind) {
            .dot => {
                _ = self.nextToken();
                const ident = try self.expect(.identifier);
                left = try self.createNode(Node{ .kind = .designator_field_terminal, .data = .{
                    .two = .{ .a = ident.start, .b = undefined },
                } });
            },
            .open_bracket => {
                _ = self.nextToken();
                const expr = try self.parseOperatorExpression(30);
                _ = try self.expect(.close_bracket);
                left = try self.createNode(Node{ .kind = .designator_index_terminal, .data = .{
                    .two = .{ .a = expr, .b = undefined },
                } });
            },
            else => std.debug.panic("Unexpected token, expected . or [ found {}", .{ptok.?.kind}),
        }

        ptok = self.peekToken();
        while (ptok) |p| : (ptok = self.peekToken()) {
            switch (p.kind) {
                .dot => {
                    _ = self.nextToken();
                    const ident = try self.expect(.identifier);
                    left = try self.createNode(Node{
                        .kind = .designator_field,
                        .data = .{
                            .two = .{ .a = ident.start, .b = left },
                        },
                    });
                },
                .open_bracket => {
                    _ = self.nextToken();
                    const expr = try self.parseOperatorExpression(30);
                    _ = try self.expect(.close_bracket);
                    left = try self.createNode(Node{
                        .kind = .designator_index,
                        .data = .{
                            .two = .{ .a = expr, .b = left },
                        },
                    });
                },
                else => break,
            }
        }

        return left;
    }

    pub fn parseStaticAssert(self: *Self) !NodeIndex {
        _ = self.nextToken();
        _ = try self.expect(.open_paren);
        defer {
            _ = self.expect(.close_paren) catch @panic("failed to expect!!!");
        }
        const test_expr = try self.parseOperatorExpression(30);

        const ptok = self.peekToken();
        if (ptok != null and ptok.?.kind == .comma) {
            _ = self.nextToken();
            const expr = try self.parsePrimaryExpression();
            return try self.createNode(Node{
                .kind = .static_assert_str,
                .data = .{
                    .two = .{ .a = test_expr, .b = expr },
                },
            });
        }

        return try self.createNode(Node{
            .kind = .static_assert,
            .data = .{
                .two = .{ .a = test_expr, .b = undefined },
            },
        });
    }

    pub fn parseStatement(self: *Self) !NodeIndex {
        const ptok = self.peekToken() orelse @panic("unexpected eof");
        return switch (ptok.kind) {
            .open_brace => try self.parseCompoundStatement(),
            .@"while" => {
                _ = self.nextToken();
                _ = try self.expect(.open_paren);
                const condition = try self.parseExpression();
                _ = try self.expect(.close_paren);

                const ntok = self.peekToken();
                if (ntok != null and ntok.?.kind == .semicolon) {
                    _ = self.nextToken();
                    return try self.createNode(Node{
                        .kind = .while_loop_empty,
                        .data = .{
                            .two = .{ .a = condition, .b = undefined },
                        },
                    });
                }

                const body = try self.parseStatement();
                return try self.createNode(Node{
                    .kind = .while_loop,
                    .data = .{
                        .two = .{ .a = condition, .b = body },
                    },
                });
            },
            .do => {
                _ = self.nextToken();
                const body = try self.parseStatement();

                _ = try self.expect(.open_paren);
                const condition = try self.parseExpression();
                _ = try self.expect(.close_paren);

                const result = try self.createNode(Node{
                    .kind = .do_while_loop,
                    .data = .{
                        .two = .{ .a = condition, .b = body },
                    },
                });

                _ = try self.expect(.semicolon);

                return result;
            },
            .@"for" => {
                _ = self.nextToken();

                _ = try self.expect(.open_paren);

                const init_expr = try self.parseDeclOrExprStmt();

                var ntok = self.peekToken();
                const condition = if (ntok != null and ntok.?.kind == .semicolon)
                    try self.createNode(Node{ .kind = .empty_statement, .data = undefined })
                else
                    try self.parseExpression();

                _ = try self.expect(.semicolon);

                ntok = self.peekToken();
                const increment = if (ntok != null and ntok.?.kind != .close_paren)
                    try self.parseExpression()
                else
                    null;

                _ = try self.expect(.close_paren);

                var node_data: NodeData = undefined;

                ntok = self.peekToken();
                if (ntok != null and ntok.?.kind == .semicolon) {
                    node_data.as(.four).a = @truncate(self.relativeOffset(init_expr));
                    node_data.as(.four).b = @truncate(self.relativeOffset(condition));
                    if (increment) |i| {
                        node_data.as(.four).c = @truncate(self.relativeOffset(i));
                    }

                    return try self.createNode(Node{
                        .kind = if (increment == null) .for_loop_empty else .for_loop_empty_inc,
                        .data = node_data,
                    });
                }

                const body = try self.parseStatement();
                node_data.as(.four).a = @truncate(self.relativeOffset(init_expr));
                node_data.as(.four).b = @truncate(self.relativeOffset(condition));
                if (increment) |i| {
                    node_data.as(.four).c = @truncate(self.relativeOffset(i));
                }
                node_data.as(.four).d = @truncate(self.relativeOffset(body));

                return try self.createNode(Node{
                    .kind = if (increment == null) .for_loop else .for_loop_inc,
                    .data = node_data,
                });
            },
            .@"if" => {
                _ = self.nextToken();
                _ = try self.expect(.open_paren);
                const condition = try self.parseExpression();
                _ = try self.expect(.close_paren);

                const body = try self.parseStatement();
                var node_data: NodeData = undefined;
                node_data.as(.two).a = condition;

                const ntok = self.peekToken();
                if (ntok != null and ntok.?.kind == .@"else") {
                    _ = self.nextToken();
                    const else_body = try self.parseStatement();
                    node_data.as(.four).c = @truncate(self.relativeOffset(body));
                    node_data.as(.four).d = @truncate(self.relativeOffset(else_body));

                    return try self.createNode(Node{
                        .kind = .if_statement_else,
                        .data = node_data,
                    });
                }

                node_data.as(.two).b = body;
                return try self.createNode(Node{
                    .kind = .if_statement,
                    .data = node_data,
                });
            },
            .@"switch" => {
                _ = self.nextToken();
                _ = try self.expect(.open_paren);
                const value = try self.parseExpression();
                _ = try self.expect(.close_paren);

                const body = try self.parseStatement();

                return try self.createNode(Node{
                    .kind = .switch_case,
                    .data = .{
                        .two = .{ .a = value, .b = body },
                    },
                });
            },
            .case => {
                _ = self.nextToken();
                const value = try self.parseOperatorExpression(30);
                _ = try self.expect(.colon);

                const body = try self.parseStatement();

                return try self.createNode(Node{
                    .kind = .case,
                    .data = .{
                        .two = .{ .a = value, .b = body },
                    },
                });
            },
            .default => {
                _ = self.nextToken();
                _ = try self.expect(.colon);

                const body = try self.parseStatement();

                return try self.createNode(Node{
                    .kind = .default,
                    .data = .{
                        .two = .{ .a = body, .b = undefined },
                    },
                });
            },
            .goto => {
                _ = self.nextToken();
                const ident = try self.expect(.identifier);
                _ = try self.expect(.semicolon);

                return try self.createNode(Node{
                    .kind = .continue_statement,
                    .data = .{
                        .two = .{ .a = ident.start, .b = undefined },
                    },
                });
            },
            .@"continue" => {
                _ = self.nextToken();
                _ = try self.expect(.semicolon);

                return try self.createNode(Node{
                    .kind = .continue_statement,
                    .data = undefined,
                });
            },
            .@"break" => {
                _ = self.nextToken();
                _ = try self.expect(.semicolon);

                return try self.createNode(Node{
                    .kind = .break_statement,
                    .data = undefined,
                });
            },
            .@"return" => {
                _ = self.nextToken();
                const ntok = self.peekToken();
                if (ntok != null and ntok.?.kind == .semicolon) {
                    _ = self.nextToken();
                    return try self.createNode(Node{
                        .kind = .return_statement,
                        .data = undefined,
                    });
                }
                const expr = try self.parseExpression();
                _ = try self.expect(.semicolon);

                return try self.createNode(Node{
                    .kind = .break_statement,
                    .data = .{
                        .two = .{
                            .a = expr,
                            .b = undefined,
                        },
                    },
                });
            },
            .semicolon => {
                _ = self.nextToken();
                return try self.createNode(Node{
                    .kind = .empty_statement,
                    .data = undefined,
                });
            },
            else => return try self.parseExpression(),
        };
    }

    pub fn parseBlockItem(self: *Self, ptok: *const tok.Token) (ParseError || std.mem.Allocator.Error)!NodeIndex {
        switch (ptok.kind) {
            .auto,
            .atomic,
            .@"const",
            .@"enum",
            .@"extern",
            .@"inline",
            .noreturn,
            .register,
            .restrict,
            .static,
            .static_assert,
            .@"struct",
            .thread_local,
            .typedef,
            .@"union",
            .void,
            .@"volatile",
            .type_name,
            .unsigned,
            .signed,
            .char,
            .short,
            .int,
            .long,
            .float,
            .double,
            .bool,
            => return try self.parseDeclaration(false),
            else => return try self.parseStatement(),
        }
    }

    pub fn parseDeclOrExprStmt(self: *Self) (ParseError || std.mem.Allocator.Error)!NodeIndex {
        const ptok = self.peekToken() orelse @panic("Unexpected eof");
        switch (ptok.kind) {
            .auto,
            .atomic,
            .@"const",
            .@"enum",
            .@"extern",
            .@"inline",
            .noreturn,
            .register,
            .restrict,
            .static,
            .static_assert,
            .@"struct",
            .thread_local,
            .typedef,
            .@"union",
            .void,
            .@"volatile",
            .type_name,
            .unsigned,
            .signed,
            .char,
            .short,
            .int,
            .long,
            .float,
            .double,
            .bool,
            => return try self.parseDeclaration(false),
            .semicolon => {
                _ = self.nextToken();
                return try self.createNode(Node{
                    .kind = .empty_statement,
                    .data = undefined,
                });
            },
            else => {
                const result = try self.parseExpression();
                _ = try self.expect(.semicolon);
                return result;
            },
        }
    }

    pub fn parseCompoundStatement(self: *Self) !NodeIndex {
        _ = self.nextToken(); // consume {
        var ptok = self.peekToken();
        if (ptok != null and ptok.?.kind == .close_brace) {
            return try self.createNodeAndNext(Node{
                .kind = .compound_empty,
                .data = undefined,
            });
        }

        const first_node = if (ptok != null) blk: {
            const result = try self.parseBlockItem(&ptok.?);
            ptok = self.peekToken();
            if (ptok != null and ptok.?.kind == .close_brace) {
                _ = self.nextToken();
                return try self.createNode(Node{
                    .kind = .compound_one,
                    .data = .{
                        .two = .{ .a = result, .b = undefined },
                    },
                });
            }

            break :blk result;
        } else @panic("unexpected end of input");

        var these_nodes = std.ArrayList(NodeIndex).init(self.allocator);
        defer these_nodes.deinit();
        try these_nodes.append(first_node);

        ptok = self.peekToken();
        while (ptok) |p| : (ptok = self.peekToken()) {
            switch (p.kind) {
                .close_brace => {
                    _ = self.nextToken();
                    break;
                },
                else => try these_nodes.append(try self.parseBlockItem(&p)),
            }
        }

        const starti = self.unit.node_ranges.items.len;
        try self.unit.node_ranges.appendSlice(these_nodes.items);

        return try self.createNode(Node{
            .kind = .compound,
            .data = .{
                .two = .{
                    .a = @truncate(starti),
                    .b = @truncate(these_nodes.items.len),
                },
            },
        });
    }

    pub fn parseExpression(self: *Self) (ParseError || std.mem.Allocator.Error)!NodeIndex {
        return try self.parseOperatorExpression(0);
    }

    fn unaryPrecedence(token: *const tok.Token) u16 {
        return switch (token.kind) {
            .plusplus,
            .minusminus,
            .open_paren,
            .open_bracket,
            => 150,
            else => 0,
        };
    }

    fn unaryPrecedenceRight(token: *const tok.Token) u16 {
        return switch (token.kind) {
            .plusplus,
            .minusminus,
            .plus,
            .minus,
            .exclamation,
            .tilde,
            .star,
            .ampersand,
            => 140,
            else => 0,
        };
    }

    fn binaryPrecedence(token: *const tok.Token) u16 {
        return switch (token.kind) {
            .dot, .arrow => 150,

            .star, .slash, .percent => 130,
            .plus, .minus => 120,
            .bit_left_shift, .bit_right_shift => 110,
            .gt, .gte, .lt, .lte => 100,
            .equality, .nequality => 90,
            .ampersand => 80,
            .carot => 70,
            .pipe => 60,
            .double_ampersand => 50,
            .double_pipe => 40,

            .assignment,
            .plus_eq,
            .minus_eq,
            .star_eq,
            .slash_eq,
            .percent_eq,
            .left_shift_eq,
            .right_shift_eq,
            .ampersand_eq,
            .carot_eq,
            .pipe_eq,
            => 20,
            .comma => 10,
            else => 0,
        };
    }

    fn binaryPrecedenceRight(token: *const tok.Token) u16 {
        return switch (token.kind) {
            .assignment,
            .plus_eq,
            .minus_eq,
            .star_eq,
            .slash_eq,
            .percent_eq,
            .left_shift_eq,
            .right_shift_eq,
            .ampersand_eq,
            .carot_eq,
            .pipe_eq,
            => 20,
            else => 0,
        };
    }

    pub fn parseOperatorExpression(self: *Self, last_prec: u16) (ParseError || std.mem.Allocator.Error)!NodeIndex {
        var op = self.peekToken();
        var left: NodeIndex = undefined;
        blk: {
            if (op != null) {
                const unary_prec_right = unaryPrecedenceRight(&op.?);

                if (unary_prec_right != 0 and unary_prec_right >= last_prec) {
                    _ = self.nextToken();
                    left = try self.parseOperatorExpression(unary_prec_right);

                    var unary_data: NodeData = undefined;
                    unary_data.as(.two).a = left;
                    unary_data.as(.four).c = @truncate(@intFromEnum(op.?.kind));
                    left = try self.createNode(Node{
                        .kind = .unary_prefix_operator,
                        .data = unary_data,
                    });

                    break :blk;
                }
            }

            left = try self.parsePrimaryExpression();
        }

        while (true) {
            op = self.peekToken();
            if (op == null) break;
            const prec = binaryPrecedence(&op.?);
            const prec_right = binaryPrecedence(&op.?);
            const unary_prec_right = unaryPrecedence(&op.?);

            if ((prec > last_prec and prec != 0) or (prec_right >= last_prec and prec != 0)) {
                _ = self.nextToken(); // consume operator

                const right = try self.parseOperatorExpression(prec);

                var bin_op_data: NodeData = undefined;
                bin_op_data.as(.two).a = left;
                bin_op_data.as(.four).c = @truncate(self.relativeOffset(right));
                bin_op_data.as(.four).d = @truncate(@intFromEnum(op.?.kind));

                left = try self.createNode(Node{
                    .kind = .binary_lr_operator,
                    .data = bin_op_data,
                });
            } else if (unary_prec_right > prec and unary_prec_right != 0) {
                _ = self.nextToken();

                var unary_data: NodeData = undefined;
                switch (op.?.kind) {
                    .open_paren => {
                        var these_nodes = std.ArrayList(NodeIndex).init(self.allocator);
                        defer these_nodes.deinit();

                        var ptok = self.peekToken();
                        while (ptok) |p| : (ptok = self.peekToken()) {
                            switch (p.kind) {
                                .close_paren => {
                                    _ = self.nextToken();
                                    break;
                                },
                                else => try these_nodes.append(try self.parseOperatorExpression(20)),
                            }

                            ptok = self.peekToken();
                            if (ptok == null) @panic("Unexpected EOF");
                            switch (ptok.?.kind) {
                                .comma => {
                                    _ = self.nextToken();
                                },
                                .close_paren => {
                                    _ = self.nextToken();
                                    break;
                                },
                                else => std.debug.panic("TODO: Expected clsoe paren or comma found {}", .{ptok.?.kind}),
                            }
                        }

                        if (these_nodes.items.len == 0) {
                            unary_data.as(.two).a = left;

                            left = try self.createNode(Node{
                                .kind = .invoke,
                                .data = unary_data,
                            });
                        } else if (these_nodes.items.len == 0) {
                            unary_data.as(.two).a = left;
                            unary_data.as(.two).b = these_nodes.items[0];

                            left = try self.createNode(Node{
                                .kind = .invoke_one_arg,
                                .data = unary_data,
                            });
                        } else {
                            const starti = self.unit.node_ranges.items.len;
                            try self.unit.node_ranges.appendSlice(these_nodes.items);

                            std.log.info("{}", .{left});
                            unary_data.as(.four).a = @truncate(self.relativeOffset(left));
                            unary_data.as(.four).b = @truncate(these_nodes.items.len);
                            unary_data.as(.two).b = @truncate(starti);

                            left = try self.createNode(Node{
                                .kind = .invoke_args,
                                .data = unary_data,
                            });
                        }
                    },
                    .open_bracket => {
                        unary_data.as(.two).a = left;
                        unary_data.as(.two).b = try self.parseOperatorExpression(0);
                        _ = try self.expect(.close_bracket);

                        left = try self.createNode(Node{
                            .kind = .index,
                            .data = unary_data,
                        });
                    },
                    else => {
                        unary_data.as(.two).a = left;
                        unary_data.as(.four).c = @truncate(@intFromEnum(op.?.kind));
                        left = try self.createNode(Node{
                            .kind = .unary_suffix_operator,
                            .data = unary_data,
                        });
                    },
                }
            } else {
                break;
            }
        }

        return left;
    }

    pub fn parsePrimaryExpression(self: *Self) !NodeIndex {
        const ptok = self.peekToken() orelse @panic("EOF");
        const tidx = self.peekTokenIndex();

        const node = switch (ptok.kind) {
            .open_paren => {
                _ = self.nextToken();
                const result = try self.parseExpression();
                _ = try self.expect(.close_paren);
                return result;
            },
            .int_literal => Node{
                .kind = .int_literal,
                .data = .{
                    .long = self.unit.ivalue(tidx),
                },
            },
            .float_literal => Node{
                .kind = .float_literal,
                .data = .{
                    .double = self.unit.fvalue(tidx),
                },
            },
            .string_literal => Node{
                .kind = .string_literal,
                .data = .{
                    .two = .{ .a = @bitCast(tidx), .b = 0 },
                },
            },
            .char_literal => blk: {
                var node_data: NodeData = undefined;
                node_data.as(.eight).a = self.unit.charAt(tidx);
                break :blk Node{
                    .kind = .char_literal,
                    .data = node_data,
                };
            },
            .identifier => {
                _ = self.nextToken();
                const ntok = self.peekToken();
                if (ntok != null and ntok.?.kind == .colon) {
                    _ = self.nextToken();
                    const stmt = try self.parseStatement();
                    return try self.createNode(Node{
                        .kind = .label,
                        .data = .{
                            .two = .{ .a = @bitCast(tidx), .b = stmt },
                        },
                    });
                }

                return self.createNode(Node{
                    .kind = .identifier,
                    .data = .{
                        .two = .{ .a = @bitCast(tidx), .b = 0 },
                    },
                });
            },
            else => std.debug.panic("Unexpected in primary {}", .{ptok.kind}),
        };

        return try self.createNodeAndNext(node);
    }

    fn nextToken(self: *Self) ?tok.Token {
        if (self.unit.virtual_token_next < self.unit.virtual_tokens.items.len) {
            const tidx = self.unit.virtual_tokens.items[self.unit.virtual_token_next];
            self.unit.virtual_token_next += 1;
            return self.unit.token(tidx);
        }
        const index = self.tokenizer.next();
        if (index == null) return null;

        return self.unit.token(index.?);
    }

    fn peekToken(self: *Self) ?tok.Token {
        if (!self.in_handle_pp) {
            self.handlePP() catch @panic("OOM");
        }
        if (self.unit.virtual_token_next < self.unit.virtual_tokens.items.len) {
            const tidx = self.unit.virtual_tokens.items[self.unit.virtual_token_next];
            return self.unit.token(tidx);
        }
        const index = self.tokenizer.peek();
        if (index == null) return null;

        return self.unit.token(index.?);
    }

    fn peekTokenIndex(self: *Self) tok.TokenIndex {
        if (self.unit.virtual_token_next < self.unit.virtual_tokens.items.len) {
            const token_index = self.unit.virtual_tokens.items[self.unit.virtual_token_next];
            return token_index;
        }
        const index = self.tokenizer.peek();
        return index.?;
    }

    inline fn hasNext(self: *Self) bool {
        return self.unit.virtual_token_next < self.unit.virtual_tokens.items.len or self.tokenizer.peek() != null;
    }

    fn rawPeekToken(self: *Self) ?tok.Token {
        const index = self.tokenizer.peek();
        if (index == null) return null;

        return self.unit.token(index.?);
    }

    fn rawPeekTokenIndex(self: *Self) tok.TokenIndex {
        const index = self.tokenizer.peek();
        return index.?;
    }

    fn rawPeekTokenEOL(self: *Self) ?tok.Token {
        const index = self.tokenizer.peekEOL();
        if (index == null) return null;

        return self.unit.token(index.?);
    }

    fn rawPeekTokenEOLIndex(self: *Self) tok.TokenIndex {
        const index = self.tokenizer.peekEOL();
        return index.?;
    }

    fn expect(self: *Self, kind: tok.TokenKind) !tok.Token {
        const token = self.peekToken();
        if (token == null) return error.OutOfTokens;

        if (token.?.kind == kind) {
            _ = self.nextToken();
            return token.?;
        } else {
            std.log.err("Found {}", .{token.?.kind});
            return error.UnexpectedToken;
        }
    }

    fn expectIndex(self: *Self, kind: tok.TokenKind) !tok.TokenIndex {
        const token = self.peekToken();
        const tidx = self.peekTokenIndex();
        if (token == null) return error.OutOfTokens;

        if (token.?.kind == kind) {
            _ = self.nextToken();
            return tidx;
        } else {
            std.log.err("Found {}", .{token.?.kind});
            return error.UnexpectedToken;
        }
    }

    fn rawExpect(self: *Self, kind: tok.TokenKind) !tok.Token {
        const index = self.tokenizer.peek();
        if (index == null) return error.OutOfTokens;

        const token = self.unit.tokens[0].items[index.?];
        if (token.kind == kind) {
            _ = self.tokenizer.next();
            return token;
        } else {
            return error.UnexpectedToken;
        }
    }

    fn rawExpectEOL(self: *Self, kind: tok.TokenKind) !tok.Token {
        const index = self.tokenizer.peekEOL();
        if (index == null) return error.OutOfTokens;

        const token = self.unit.token(index.?);
        if (token.kind == kind) {
            _ = self.tokenizer.nextEOL();
            return token;
        } else {
            return error.UnexpectedToken;
        }
    }

    fn rawExpectEOLIndex(self: *Self, kind: tok.TokenKind) !tok.TokenIndex {
        const index = self.tokenizer.peekEOL();
        if (index == null) return error.OutOfTokens;

        const token = self.unit.token(index.?);
        if (token.kind == kind) {
            _ = self.tokenizer.nextEOL();
            return index.?;
        } else {
            return error.UnexpectedToken;
        }
    }

    fn handlePP(self: *Self) (ParseError || std.mem.Allocator.Error)!void {
        const old_handle_pp = self.in_handle_pp;
        self.in_handle_pp = true;
        defer self.in_handle_pp = old_handle_pp;

        var token = self.peekToken() orelse return;
        var tidx = self.peekTokenIndex();
        switch (token.kind) {
            .pp_directive => {
                _ = self.nextToken();
                const directive = self.unit.ppDirective(tidx);
                if (std.mem.eql(u8, directive, "define")) {
                    const ident_idx = try self.rawExpectEOLIndex(.identifier);

                    var ptok = self.rawPeekTokenEOL();
                    if (ptok != null and ptok.?.kind == .open_paren) {
                        _ = self.tokenizer.next();

                        var parameter_map = std.StringArrayHashMap(void).init(self.allocator);

                        ptok = self.rawPeekTokenEOL();
                        blk: {
                            while (ptok) |p| : (ptok = self.rawPeekTokenEOL()) {
                                tidx = self.rawPeekTokenEOLIndex();
                                switch (p.kind) {
                                    .identifier => try parameter_map.put(self.unit.identifier(tidx), {}),
                                    .close_paren => {
                                        _ = self.tokenizer.nextEOL();
                                        break;
                                    },
                                    else => std.debug.panic("TODO: add error handling (expected comma or paren) {}", .{p.kind}),
                                }
                                _ = self.tokenizer.nextEOL();

                                ptok = self.rawPeekTokenEOL();
                                if (ptok == null) @panic("TODO: expected at least paren");

                                switch (ptok.?.kind) {
                                    .comma => _ = self.tokenizer.nextEOL(),
                                    .close_paren => {
                                        _ = self.tokenizer.nextEOL();
                                        break :blk;
                                    },
                                    else => std.debug.panic("TODO: add error handling (expected comma or paren) {}", .{p.kind}),
                                }
                            }
                        }

                        const start = self.tokenizer.tokenCount();
                        var count: u32 = 0;

                        while (self.tokenizer.nextEOL()) |_| {
                            count += 1;
                        }

                        try self.unit.define_fns.put(self.unit.identifier(ident_idx), .{
                            .token_start = .{
                                .index = @truncate(start),
                                .file_index = 0,
                            },
                            .token_count = count,
                            .parameters = parameter_map,
                        });
                    } else {
                        const start = self.tokenizer.tokenCount();
                        var count: u32 = 0;

                        while (self.tokenizer.nextEOL()) |_| {
                            count += 1;
                        }
                        std.log.info("Define start is {} {}", .{ start, count });

                        try self.unit.defines.put(self.unit.identifier(ident_idx), .{
                            .token_start = .{
                                .index = @truncate(start),
                                .file_index = 0,
                            },
                            .token_count = count,
                        });
                    }
                } else if (std.mem.eql(u8, directive, "include")) {
                    const ptok = self.rawPeekTokenEOL();
                    if (ptok != null and ptok.?.kind == .string_literal) {
                        tidx = self.peekTokenIndex();
                        _ = self.tokenizer.nextEOL();
                        const file_path = self.unit.stringAt(tidx);
                        const this_file_dir_path = std.fs.path.dirname(self.unit.files.items[0].file_path).?;
                        const full_file_path = try std.fs.path.resolve(self.allocator, &.{
                            this_file_dir_path, file_path,
                        });

                        std.log.info("file is {s} {s}", .{ full_file_path, file_path });
                        var file = std.fs.openFileAbsolute(full_file_path, .{}) catch @panic("dang");
                        const file_contents = file.readToEndAlloc(self.allocator, std.math.maxInt(usize)) catch |e| std.debug.panic("foo {}", .{e});
                        defer self.allocator.free(file_contents);

                        var file_tokenizer = Tokenizer.initVirtual(self.allocator, self.unit, @truncate(self.unit.files.items.len));
                        while (file_tokenizer.next()) |pt| {
                            std.log.debug("Token: {}", .{pt});
                        }
                    } else {
                        @panic("todo");
                    }
                }
            },
            .identifier => {
                try self.handlePPIdentifier(tidx);
            },
            else => {},
        }

        token = self.peekToken() orelse return;
        tidx = self.peekTokenIndex();
        switch (token.kind) {
            .identifier => {
                try self.handlePPIdentifier(tidx);
            },
            else => return,
        }
    }

    fn handlePPIdentifier(self: *Self, tidx: tok.TokenIndex) !void {
        const ident_str = self.unit.identifier(tidx);

        if (self.unit.defines.get(ident_str)) |value| {
            _ = self.nextToken();

            const token_slice = self.unit.tokenSliceCount(value.token_start, value.token_count);
            if (self.unit.virtual_tokens.items.len == 0 or (self.unit.virtual_token_next >= self.unit.virtual_tokens.items.len)) {
                self.unit.virtual_tokens.items.len = 0;
                self.unit.virtual_token_next = 0;
            }
            try self.unit.virtual_tokens.insertSlice(self.unit.virtual_token_next, token_slice);

            try self.handlePP();
        } else if (self.unit.define_fns.get(ident_str)) |fn_value| {
            _ = self.nextToken();
            _ = try self.expect(.open_paren);
            var ptok = self.rawPeekToken();

            var argument_map = std.StringHashMap(DefineValue).init(self.allocator);
            var argument_start_token_index = self.tokenizer.tokenCount();
            var parameter_iter = fn_value.parameters.iterator();

            var indent: u32 = 0;
            blk: {
                var i: u32 = 0;
                while (ptok) |p| : (ptok = self.rawPeekToken()) {
                    switch (p.kind) {
                        .open_paren, .open_brace, .open_bracket => indent += 1,
                        .close_paren => {
                            if (indent == 0) {
                                if (i > 0) {
                                    const param = parameter_iter.next() orelse @panic("TODO: parameter arg mismatch");
                                    try argument_map.put(param.key_ptr.*, DefineValue{
                                        .token_start = argument_start_token_index,
                                        .token_count = self.tokenizer.tokenCount() - argument_start_token_index,
                                    });
                                }
                                break :blk;
                            }
                        },
                        .close_brace, .close_bracket => indent -= 1,
                        .comma => {
                            if (indent == 0) {
                                const param = parameter_iter.next() orelse @panic("TODO: parameter arg mismatch");
                                try argument_map.put(param.key_ptr.*, DefineValue{
                                    .token_start = argument_start_token_index,
                                    .token_count = self.tokenizer.tokenCount() - argument_start_token_index,
                                });
                                argument_start_token_index = self.tokenizer.tokenCount();
                            }
                        },
                        else => {},
                    }
                    i += 1;
                    _ = self.tokenizer.next();
                }
            }
            _ = try self.expect(.close_paren);
            if (argument_map.count() != fn_value.parameters.count()) {
                std.debug.panic("TODO: error (expected {} arg found {})", .{ fn_value.parameters.count(), argument_map.count() });
            }

            self.expansion_argument_map = argument_map;

            const token_slice = self.unit.tokens[0].items[fn_value.token_start .. fn_value.token_start + fn_value.token_count];
            if (self.unit.virtual_tokens.items.len == 0 or (self.unit.virtual_token_next >= self.unit.virtual_tokens.items.len)) {
                self.unit.virtual_tokens.items.len = 0;
                self.unit.virtual_token_next = 0;
            }
            try self.unit.virtual_tokens.insertSlice(self.unit.virtual_token_next, token_slice);

            try self.handlePP();
        } else if (self.expansion_argument_map.get(ident_str)) |value| {
            _ = self.nextToken();
            const token_slice = self.unit.tokens[0].items[value.token_start .. value.token_start + value.token_count];
            if (self.unit.virtual_tokens.items.len == 0 or (self.unit.virtual_token_next >= self.unit.virtual_tokens.items.len)) {
                self.unit.virtual_tokens.items.len = 0;
                self.unit.virtual_token_next = 0;
            }
            try self.unit.virtual_tokens.insertSlice(self.unit.virtual_token_next, token_slice);

            try self.handlePP();
        }
    }

    inline fn relativeOffset(self: *Self, offset: u32) u32 {
        return @as(u32, @truncate(self.unit.nodes.items.len)) - offset;
    }

    fn createNode(self: *Self, node: Node) !NodeIndex {
        const index = self.unit.nodes.items.len;
        try self.unit.nodes.append(node);

        return @truncate(index);
    }

    fn createNodeAndNext(self: *Self, node: Node) !NodeIndex {
        const result = try self.createNode(node);
        _ = self.nextToken();
        return result;
    }
};
