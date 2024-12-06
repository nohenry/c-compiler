const std = @import("std");
const Unit = @import("unit.zig").Unit;
const DefineValue = @import("unit.zig").DefineValue;
const tok = @import("tokenizer.zig");
const Tokenizer = tok.Tokenizer;
const TokenIndex = tok.TokenIndex;

pub const ParseError = error{ OutOfTokens, UnexpectedToken };

pub const NodeKind = enum(u32) {
    empty,
    float_literal,
    double_literal,
    int_literal,
    unsigned_int_literal,
    long_literal,
    unsigned_long_literal,
    long_long_literal,
    unsigned_long_long_literal,
    size_literal,
    unsigned_size_literal,
    string_literal,
    string_literal_join,
    stringified_literal,
    char_literal,

    /// node_data: a(u32) = index of lhs, b(u16) = relative index of rhs, c(u16) = operator
    binary_lr_operator,
    /// node_data: a(u32) = index of expr, c(u16) = operator
    unary_prefix_operator,
    /// node_data: a(u32) = index of expr, c(u16) = operator
    unary_suffix_operator,
    /// node_data: a(u32) = index of invokee
    invoke,
    /// node_data: a(u32) = index of invokee, b(u32) = arg_index
    invoke_one_arg,
    /// node_data: a(u16) = relative index of invokee, b(u16) = count, c(u32) = start index of parameters
    invoke_args,
    /// node_data: a(u32) = index of array/ptr, b(u32) = index of index expr
    index,
    /// node_data: a(u32) = index of type, c(u16) relative offset of expr, h(u8) = storage specifiers
    cast,

    /// node_data: a(u32) = token start attribute data, c(u16) = token count, d(u16) relative index of item
    attribute,
    /// node_data: a(u32) = token start label data, c(u16) = token count, d(u16) relative index of item
    asm_label,
    /// variable declaration
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
    /// node_data: a(u32) = base type index, b(u16) = relative index of size, h(u8) = type_qualifiers
    array_type_static,

    /// node_data: a(u16) = relative index of return type
    function_type,
    /// node_data: a(u16) = relative index of return type,
    ///            b(u16) = relative index of parameter type
    function_type_one_parameter,
    /// node_data: a(u16) = relative index of return type,
    ///            b(u16)  = parameter type count
    ///            b(u32) = index of parameter type start,
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

    pub fn writePretty(type_qualifier: Type, writer: anytype) !void {
        if ((type_qualifier & @"const") > 0) {
            _ = try writer.write("const");
            if ((type_qualifier & ~@"const") > 0) {
                try writer.writeByte(' ');
            }
        }
        if ((type_qualifier & restrict) > 0) {
            _ = try writer.write("restrict");
            if ((type_qualifier & ~restrict) > 0) {
                try writer.writeByte(' ');
            }
        }
        if ((type_qualifier & @"volatile") > 0) {
            _ = try writer.write("volatile");
            if ((type_qualifier & ~@"volatile") > 0) {
                try writer.writeByte(' ');
            }
        }
        if ((type_qualifier & atomic) > 0) {
            _ = try writer.write("atomic");
            if ((type_qualifier & ~atomic) > 0) {
                try writer.writeByte(' ');
            }
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

    pub fn formatNode(index: NodeIndex, unit: *Unit, writer: anytype) !NodeRangeOrNode {
        const self = &unit.nodes.items[index];
        var result = NodeRangeOrNode{ .none = {} };
        switch (self.kind) {
            .float_literal => try writer.print("\x1b[1;35mFloatLiteral\x1b[0m \x1b[1;33m{}\x1b[0m", .{self.data.double}),
            .double_literal => try writer.print("\x1b[1;35mDoubleLiteral\x1b[0m \x1b[1;33m{}\x1b[0m", .{self.data.double}),
            .int_literal => try writer.print("\x1b[1;35mIntLiteral\x1b[0m \x1b[32m(int)\x1b[0m \x1b[1;33m{}\x1b[0m", .{self.data.long}),
            .unsigned_int_literal => try writer.print("\x1b[1;35mIntLiteral\x1b[0m \x1b[32m(unsigned int)\x1b[0m \x1b[1;33m{}\x1b[0m", .{self.data.long}),
            .long_literal => try writer.print("\x1b[1;35mIntLiteral\x1b[0m \x1b[32m(long)\x1b[0m \x1b[1;33m{}\x1b[0m", .{self.data.long}),
            .unsigned_long_literal => try writer.print("\x1b[1;35mIntLiteral\x1b[0m \x1b[32m(unsigned long)\x1b[0m \x1b[1;33m{}\x1b[0m", .{self.data.long}),
            .long_long_literal => try writer.print("\x1b[1;35mIntLiteral\x1b[0m \x1b[32m(long long)\x1b[0m \x1b[1;33m{}\x1b[0m", .{self.data.long}),
            .unsigned_long_long_literal => try writer.print("\x1b[1;35mIntLiteral\x1b[0m \x1b[32m(unsigned long long)\x1b[0m \x1b[1;33m{}\x1b[0m", .{self.data.long}),
            .size_literal => try writer.print("\x1b[1;35mIntLiteral\x1b[0m \x1b[32m(ssize_t)\x1b[0m \x1b[1;33m{}\x1b[0m", .{self.data.long}),
            .unsigned_size_literal => try writer.print("\x1b[1;35mIntLiteral\x1b[0m \x1b[32m(size_t)\x1b[0m \x1b[1;33m{}\x1b[0m", .{self.data.long}),
            .string_literal => try writer.print("\x1b[1;35mStringLiteral\x1b[0m \x1b[1;33m\"{s}\"\x1b[0m", .{unit.stringAt(@bitCast(self.data.as(.two).a))}),
            .string_literal_join => {
                try writer.print("\x1b[1;35mStringLiteralJoin\x1b[0m \x1b[1;33m\"{s}\"\x1b[0m", .{unit.stringAt(@bitCast(self.data.as(.two).b))});
                result = .{ .node = self.data.as(.two).a };
            },
            .stringified_literal => try writer.print("\x1b[1;35mStringifiedLiteral\x1b[0m \x1b[1;33m\"{s}\"\x1b[0m", .{unit.stringifiedAt(@bitCast(self.data.as(.two).a))}),
            .char_literal => try writer.print("\x1b[1;35mCharLiteral\x1b[0m \x1b[1;33m'{c}'\x1b[0m", .{@as(u8, @truncate(self.data.two.a))}),
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
                const parameter_start_index = self.data.as(.two).b;
                const parameter_count = self.data.as(.four).b;
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
            .@"union" => {
                try writer.print("\x1b[1;35mUnion\x1b[0m", .{});
                const member_range_start = self.data.as(.two).a;
                const member_range_count = self.data.as(.two).b;

                result = .{
                    .node_range = .{
                        .start = member_range_start,
                        .count = member_range_count,
                    },
                };
            },
            .union_forward => {
                const ident_index: TokenIndex = @bitCast(self.data.as(.two).a);
                try writer.print("\x1b[1;35mUnion\x1b[0m \x1b[1;36m'{s}'\x1b[0m", .{unit.identifierAt(ident_index)});
            },
            .union_ident => {
                const ident_index: TokenIndex = @bitCast(self.data.as(.two).a);
                const member_range_index = absoluteIndex(index, 1);
                const member_range = unit.nodes.items[member_range_index];
                try writer.print("\x1b[1;35mUnion\x1b[0m \x1b[1;36m'{s}'\x1b[0m", .{unit.identifierAt(ident_index)});
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
                try writer.print("\x1b[1;35mEnumMember\x1b[0m \x1b[1;36m'{s}'\x1b[0m", .{unit.identifierAt(ident_index)});
            },
            .enum_member_value => {
                const ident_index: TokenIndex = @bitCast(self.data.as(.two).a);
                const value_index = self.data.as(.two).b;
                try writer.print("\x1b[1;35mEnumMember\x1b[0m \x1b[1;36m'{s}'\x1b[0m", .{unit.identifierAt(ident_index)});

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
                const ident_index: TokenIndex = @bitCast(self.data.as(.two).b);
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
                const arg = self.data.as(.two).b;

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
            .cast => {
                const type_index = self.data.as(.two).a;
                const expr_index = absoluteIndex(index, self.data.as(.four).c);
                // const storage_class: StorageClass.Type = @bitCast(self.data.as(.eight).h);

                try writer.print("\x1b[1;35mCast\x1b[0m", .{});

                result = NodeRangeOrNode.initNodes(&.{ type_index, expr_index });
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
            .attribute => {
                const token_start: TokenIndex = @bitCast(self.data.as(.two).a);
                const token_count = self.data.as(.four).c;
                const declaration = absoluteIndex(index, self.data.as(.four).d);
                try writer.print("\x1b[1;35mAttribute\x1b[0m TokenIndex: {}:{}, Count: {}", .{ token_start.file_index, token_start.index, token_count });
                result = NodeRangeOrNode.initNodes(&.{declaration});
            },
            .asm_label => {
                const token_start: TokenIndex = @bitCast(self.data.as(.two).a);
                const token_count = self.data.as(.four).c;
                const declaration = absoluteIndex(index, self.data.as(.four).d);
                try writer.print("\x1b[1;35mAsmLabel\x1b[0m TokenIndex: {}:{}, Count: {}", .{ token_start.file_index, token_start.index, token_count });
                result = NodeRangeOrNode.initNodes(&.{declaration});
            },
            .empty_statement => {
                try writer.print("\x1b[1;35mEmptyStatement\x1b[0m", .{});
            },
            else => try writer.print("tbd {}", .{self.kind}),
        }

        return result;
    }

    pub fn writeTree(index: NodeIndex, unit: *Unit, indent: u32, last: bool, with_types: bool, writer: anytype) !void {
        for (0..indent) |_| {
            _ = try writer.write("    ");
        }
        if (last) {
            _ = try writer.write("\x1b[34m└── \x1b[0m");
        } else {
            _ = try writer.write("\x1b[34m├── \x1b[0m");
        }

        const result = try formatNode(index, unit, writer);
        if (with_types) {
            if (unit.node_to_type.get(index)) |ty| {
                try writer.print(" : \x1b[1m", .{});
                try unit.interner.printTyWriter(ty, false, writer);
                try writer.print("\x1b[0m", .{});
            }
            if (unit.declared_type.get(index)) |ty| {
                try writer.print(" : \x1b[31;1m", .{});
                try unit.interner.printTyWriter(ty, false, writer);
                try writer.print("\x1b[0m", .{});
            }
        }
        try writer.print(" ({})", .{index});
        try writer.writeByte('\n');

        switch (result) {
            .none => {},
            .node => |idx| {
                try writeTree(idx, unit, indent + 1, true, with_types, writer);
            },
            .node_range => |rng| {
                for (rng.start..rng.start + rng.count) |i| {
                    const node_index = unit.node_ranges.items[i];

                    const is_last = i == rng.start + rng.count - 1;
                    try writeTree(node_index, unit, indent + 1, is_last, with_types, writer);
                }
            },
            .nodes => |rng| {
                for (0..rng.count) |i| {
                    const node_index = rng.indicies[i];

                    const is_last = i == rng.count - 1;
                    try writeTree(node_index, unit, indent + 1, is_last, with_types, writer);
                }
            },
            .node_and_range => |rng| {
                try writeTree(rng.node, unit, indent + 1, rng.range.count == 0, with_types, writer);

                for (rng.range.start..rng.range.start + rng.range.count) |i| {
                    const node_index = unit.node_ranges.items[i];

                    const is_last = i == rng.range.start + rng.range.count - 1;
                    try writeTree(node_index, unit, indent + 1, is_last, with_types, writer);
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
        return try self.parseDeclaration(true);
    }

    pub fn parseAttributeData(self: *Self) !NodeData {
        var ptok: ?TokenResult = undefined;
        self.nextToken();
        _ = try self.expect(.open_paren);
        _ = try self.expect(.open_paren);

        ptok = self.peekToken();
        const start_token = if (ptok != null)
            ptok.?.index
        else
            @panic("Unexpcted end");

        var indent: u32 = 0;
        while (ptok) |p| : (ptok = self.peekToken()) {
            switch (p.token.kind) {
                .close_paren => {
                    if (indent == 0) break else indent -= 1;
                },
                .open_paren => {
                    indent += 1;
                },
                else => {},
            }
            self.nextToken();
        }
        const end_token = ptok.?.index;

        var node_data: NodeData = undefined;
        node_data.as(.two).a = @bitCast(start_token);
        node_data.as(.four).c = @truncate(end_token.index - start_token.index);

        _ = try self.expect(.close_paren);
        _ = try self.expect(.close_paren);

        return node_data;
    }

    pub fn parseAsmLabel(self: *Self) !NodeData {
        var ptok: ?TokenResult = undefined;
        self.nextToken();
        _ = try self.expect(.open_paren);

        ptok = self.peekToken();
        const start_token = if (ptok != null)
            ptok.?.index
        else
            @panic("Unexpcted end");

        var indent: u32 = 0;
        while (ptok) |p| : (ptok = self.peekToken()) {
            switch (p.token.kind) {
                .close_paren => {
                    if (indent == 0) break else indent -= 1;
                },
                .open_paren => {
                    indent += 1;
                },
                else => {},
            }
            self.nextToken();
        }
        const end_token = ptok.?.index;

        var node_data: NodeData = undefined;
        node_data.as(.two).a = @bitCast(start_token);
        node_data.as(.four).c = @truncate(end_token.index - start_token.index);

        _ = try self.expect(.close_paren);

        return node_data;
    }

    pub fn parseDeclaration(self: *Self, toplevel: bool) !NodeIndex {
        var storage_class: StorageClass.Type = 0;

        var ptok = self.peekToken();
        if (ptok != null) {
            switch (ptok.?.token.kind) {
                .static_assert => {
                    const result = try self.parseStaticAssert();
                    _ = try self.expect(.semicolon);
                    return result;
                },
                // .identifier => {
                //     const ident_str = self.unit.identifierAt(ptok.?.index);
                //     if (std.mem.startsWith(u8, ident_str, "__attribute")) {
                //         var attribute_data = try self.parseAttributeData();

                //         const declaration = try self.parseDeclaration(toplevel);
                //         attribute_data.as(.four).d = @truncate(self.relativeOffset(declaration));

                //         return try self.createNode(Node{
                //             .kind = .attribute,
                //             .data = attribute_data,
                //         });
                //     }
                // },
                else => {},
            }
        }

        var attribute_data: ?NodeData = null;
        const type_node = try self.parseDeclarationSpecifiers(&storage_class, &attribute_data);
        ptok = self.peekToken();

        var these_nodes = std.ArrayList(NodeIndex).init(self.allocator);
        defer these_nodes.deinit();
        while (ptok) |p| : (ptok = self.peekToken()) {
            var front_attribute: ?NodeData = null;
            switch (p.token.kind) {
                .identifier => {
                    const ident_str = self.unit.identifierAt(p.index);
                    if (std.mem.startsWith(u8, ident_str, "__attribute")) {
                        if (these_nodes.items.len == 0) {
                            attribute_data = try self.parseAttributeData();
                        } else {
                            front_attribute = try self.parseAttributeData();
                        }

                        ptok = self.peekToken();

                        // continue;
                    }
                },
                else => {},
            }
            var node_to_append: NodeIndex = undefined;
            switch (p.token.kind) {
                .semicolon => {
                    self.nextToken();
                    if (these_nodes.items.len == 0) {
                        return type_node;
                    }
                    break;
                },
                else => {
                    var this_ident: ?TokenIndex = null;
                    const this_type = try self.parseDeclarator(type_node, &this_ident);

                    var this_attribute_data: ?NodeData = null;
                    var this_asm_label: ?NodeData = null;
                    ptok = self.peekToken();
                    if (ptok != null and ptok.?.token.kind == .identifier) {
                        const ident_str = self.unit.identifierAt(ptok.?.index);
                        if (std.mem.startsWith(u8, ident_str, "__attribute")) {
                            this_attribute_data = try self.parseAttributeData();
                        } else if (std.mem.startsWith(u8, ident_str, "__asm")) {
                            this_asm_label = try self.parseAsmLabel();
                        }
                    }

                    if (this_type.is_function and !this_type.is_function_ptr) {
                        var decl_node_data: NodeData = undefined;
                        decl_node_data.as(.two).a = @bitCast(this_ident orelse @panic("TODO: expected identifier in variable decl"));

                        ptok = self.peekToken();
                        var actual_function = false;
                        if (toplevel and these_nodes.items.len == 0 and ptok != null and ptok.?.token.kind == .open_brace) {
                            const body = try self.parseCompoundStatement();
                            decl_node_data.as(.four).c = @truncate(self.relativeOffset(this_type.node.?));
                            decl_node_data.as(.four).d = @truncate(self.relativeOffset(body));

                            const function_decl = try self.createNode(Node{
                                .kind = .function_declaration_body,
                                .data = decl_node_data,
                            });
                            node_to_append = function_decl;
                            actual_function = true;
                        } else {
                            decl_node_data.as(.four).c = @truncate(self.relativeOffset(this_type.node.?));

                            const function_decl = try self.createNode(Node{
                                .kind = .function_declaration,
                                .data = decl_node_data,
                            });
                            node_to_append = function_decl;
                        }

                        if (this_asm_label) |*tal| {
                            tal.as(.four).d = self.relativeOffset(node_to_append);

                            node_to_append = try self.createNode(Node{
                                .kind = .asm_label,
                                .data = tal.*,
                            });
                        }

                        if (this_attribute_data) |*tad| {
                            attribute_data = tad.*;
                        }

                        if (front_attribute) |*fa| {
                            attribute_data = fa.*;
                        }

                        if (actual_function) {
                            if (attribute_data) |*data| {
                                data.as(.four).d = @truncate(self.relativeOffset(node_to_append));
                                return try self.createNode(Node{
                                    .kind = .attribute,
                                    .data = data.*,
                                });
                            } else {
                                return node_to_append;
                            }
                        } else {
                            try these_nodes.append(node_to_append);
                        }
                    } else {
                        if (this_ident == null) {
                            try these_nodes.append(this_type.node.?);
                            continue;
                        }

                        var kind = NodeKind.var_declaration;
                        var decl_node_data: NodeData = undefined;
                        decl_node_data.as(.two).a = @bitCast(this_ident orelse @panic("TODO: expected identifier in variable decl"));

                        ptok = self.peekToken();
                        if (ptok != null and ptok.?.token.kind == .assignment) {
                            self.nextToken();

                            const decl_init = try self.parseInitializer();
                            decl_node_data.as(.four).d = @truncate(self.relativeOffset(decl_init));
                            kind = .var_declaration_init;
                        } else if ((storage_class & StorageClass.typedef) > 0) {
                            try self.unit.type_names.put(self.unit.identifierAt(@bitCast(this_ident.?)), {});
                        }

                        decl_node_data.as(.four).c = @truncate(self.relativeOffset(this_type.node.?));

                        const decl = try self.createNode(Node{
                            .kind = kind,
                            .data = decl_node_data,
                        });
                        node_to_append = decl;

                        if (this_asm_label) |*tal| {
                            tal.as(.four).d = self.relativeOffset(node_to_append);

                            node_to_append = try self.createNode(Node{
                                .kind = .asm_label,
                                .data = tal.*,
                            });
                        }

                        if (this_attribute_data) |*tad| {
                            tad.as(.four).d = self.relativeOffset(node_to_append);
                            node_to_append = try self.createNode(Node{
                                .kind = .attribute,
                                .data = tad.*,
                            });
                        }

                        if (front_attribute) |*fa| {
                            fa.as(.four).d = self.relativeOffset(node_to_append);
                            node_to_append = try self.createNode(Node{
                                .kind = .attribute,
                                .data = fa.*,
                            });
                        }

                        try these_nodes.append(node_to_append);
                    }
                },
            }

            ptok = self.peekToken();
            if (ptok == null) continue;
            switch (ptok.?.token.kind) {
                .comma => {
                    self.nextToken();
                },
                .semicolon => {
                    self.nextToken();
                    break;
                },
                else => {
                    var writer = std.io.getStdOut().writer();
                    Node.writeTree(type_node, self.unit, 0, true, false, &writer) catch @panic("");
                    std.log.err("{s}, pos: {}", .{ self.tokenizer.unit.filePos(ptok.?.index)[0], self.tokenizer.unit.filePos(ptok.?.index)[1] });
                    std.debug.panic("TODO: unexpected token {}", .{ptok.?.token.kind});
                },
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

        if (attribute_data) |*data| {
            data.as(.four).d = @truncate(self.relativeOffset(decl_node));
            return try self.createNode(Node{
                .kind = .attribute,
                .data = data.*,
            });
        } else {
            return decl_node;
        }
    }

    pub fn parseDeclarationSpecifiers(self: *Self, storage_class: *StorageClass.Type, attribute_data: *?NodeData) (ParseError || std.mem.Allocator.Error)!NodeIndex {
        var ptok = self.peekToken();
        var type_qualifier: TypeQualifier.Type = 0;
        var type_kind: ?NodeKind = null;
        var type_name_token: tok.TokenIndex = undefined;

        if (ptok != null) {
            switch (ptok.?.token.kind) {
                .identifier => {
                    const ident_str = self.unit.identifierAt(ptok.?.index);
                    if (std.mem.startsWith(u8, ident_str, "__attribute")) {
                        attribute_data.* = try self.parseAttributeData();
                    }
                    ptok = self.peekToken();
                },
                else => {},
            }
        }

        while (ptok) |p| : (ptok = self.peekToken()) {
            switch (p.token.kind) {
                .atomic => {
                    self.nextToken();
                    ptok = self.peekToken();
                    if (ptok != null and ptok.?.token.kind == .open_paren) {
                        self.nextToken();
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
                .@"inline" => storage_class.* |= StorageClass.@"inline",
                .noreturn => storage_class.* |= StorageClass.noreturn,

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
                .void => type_kind = .void_type,
                .@"struct" => return try self.parseStructOrUnion(true),
                .@"union" => return try self.parseStructOrUnion(false),
                .@"enum" => return try self.parseEnum(),

                .type_name => {
                    type_kind = .type_name;
                    type_name_token = ptok.?.index;
                },

                else => break,
            }
            self.nextToken();
        }

        var type_node_data: NodeData = undefined;
        if (type_kind == null) {
            var it = self.unit.type_names.iterator();
            while (it.next()) |value| {
                std.log.info("TypeName: \x1b[1;36m'{s}'\x1b[0m", .{value.key_ptr.*});
            }
            std.log.info("File: {s}", .{self.unit.files.items[ptok.?.index.file_index].file_path});
            std.log.info("Position: {}", .{self.unit.token(ptok.?.index)});
        }
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
        switch (ptok.token.kind) {
            .star => {
                self.nextToken();
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

        switch (ptok.token.kind) {
            .identifier => {
                self.nextToken();
                identifier.* = ptok.index;
                left = .{ .node = base_type };
            },
            .open_paren => {
                self.nextToken();
                const ntok = self.peekToken();
                // if (ntok != null and ntok.?.token.kind != .open_paren and ntok.?.token.kind != .open_bracket and ntok.?.token.kind != ) {
                // }
                if (ntok != null and (ntok.?.token.kind == .star or ntok.?.token.kind == .identifier)) {
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
            switch (ptok.token.kind) {
                .open_bracket => {
                    self.nextToken();
                    var type_qualifiers = self.parseAnyTypeQualifiers();
                    ptok = self.peekToken() orelse @panic("unexpecteed EOF");

                    var node_data: NodeData = undefined;
                    node_data.as(.two).a = left.node.?;
                    node_data.as(.eight).h = type_qualifiers;

                    switch (ptok.token.kind) {
                        .close_bracket => {
                            self.nextToken();
                            // node_data.as(.two).a = (try self.parseDirectDeclarator(base_type, base_ident, identifier)) orelse base_ident.*.?;

                            left.node = try self.createNode(Node{
                                .kind = .array_type,
                                .data = node_data,
                            });
                        },
                        .star => {
                            self.nextToken();
                            _ = try self.expect(.close_bracket);
                            // node_data.as(.two).a = (try self.parseDirectDeclarator(base_type, base_ident, identifier)) orelse base_ident.*.?;

                            left.node = try self.createNode(Node{
                                .kind = .array_type_variable,
                                .data = node_data,
                            });
                        },
                        .static => {
                            self.nextToken();
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
                    self.nextToken();
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
                node_data.as(.four).b = @truncate(rng.count);
                node_data.as(.two).b = rng.start;
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
            switch (p.token.kind) {
                .comma => {
                    self.nextToken();
                    continue;
                },
                .close_paren => {
                    self.nextToken();
                    break;
                },
                .ellipsis => {
                    self.nextToken();
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
            var attribute_data: ?NodeData = null;
            const type_node = try self.parseDeclarationSpecifiers(&storage_class, &attribute_data);

            const full_type = try self.parseDeclarator(type_node, &identifier);

            var node_data: NodeData = undefined;
            node_data.as(.four).a = @truncate(self.relativeOffset(full_type.node.?));
            node_data.as(.eight).c = storage_class;

            var node_to_append: NodeIndex = undefined;
            if (identifier) |ident| {
                node_data.as(.two).b = @bitCast(ident);
                node_to_append = try self.createNode(Node{
                    .kind = .parameter_ident,
                    .data = node_data,
                });
            } else {
                node_to_append = try self.createNode(Node{
                    .kind = .parameter,
                    .data = node_data,
                });
            }

            if (attribute_data) |*tad| {
                tad.as(.four).d = @truncate(self.relativeOffset(node_to_append));
                node_to_append = try self.createNode(Node{
                    .kind = .attribute,
                    .data = tad.*,
                });
            }

            try these_nodes.append(node_to_append);
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

    pub fn parseStructOrUnion(self: *Self, is_struct: bool) !NodeIndex {
        self.nextToken();
        const ptok = self.peekToken() orelse @panic("unexpcted eof");
        const ident = if (ptok.token.kind == .identifier or ptok.token.kind == .type_name) blk: {
            self.nextToken();
            break :blk ptok.index;
        } else null;

        const ntok = self.peekToken();
        if (ntok != null and ntok.?.token.kind == .open_brace) {
            self.nextToken();

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
                            .a = @bitCast(i),
                            .b = undefined,
                        },
                    },
                });
            } else {
                return try self.createNode(Node{
                    .kind = if (is_struct) .@"struct" else .@"union",
                    .data = .{
                        .two = .{ .a = members_range.start, .b = members_range.count },
                    },
                });
            }
        } else {
            return try self.createNode(Node{
                .kind = if (is_struct) .struct_forward else .union_forward,
                .data = .{
                    .two = .{ .a = @bitCast(ident.?), .b = undefined },
                },
            });
        }
    }

    pub fn parseStructDeclarations(self: *Self) !NodeRange {
        var these_nodes = std.ArrayList(NodeIndex).init(self.allocator);
        defer these_nodes.deinit();
        var storage_class: StorageClass.Type = 0;
        var attribute_data: ?NodeData = undefined;

        var ptok = self.peekToken();
        while (ptok) |p| : (ptok = self.peekToken()) {
            if (p.token.kind == .close_brace) {
                break;
            } else if (p.token.kind == .static_assert) {
                try these_nodes.append(try self.parseStaticAssert());
                _ = try self.expect(.semicolon);
                continue;
            }
            attribute_data = null;
            var member_type = try self.parseDeclarationSpecifiers(&storage_class, &attribute_data);
            if (attribute_data) |*tad| {
                tad.as(.four).d = self.relativeOffset(member_type);
                member_type = try self.createNode(Node{
                    .kind = .attribute,
                    .data = tad.*,
                });
            }

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
        if (ptok != null and ptok.?.token.kind == .semicolon) {
            // if there's a semicolon, it means this is an empty field.
            var node_data: NodeData = undefined;

            node_data.as(.four).c = @truncate(self.relativeOffset(member_type));
            try nodes.append(try self.createNode(Node{
                .kind = .member,
                .data = node_data,
            }));
            return;
        }

        ptok = self.peekToken();
        while (ptok) |p| : (ptok = self.peekToken()) {
            switch (p.token.kind) {
                .semicolon => {
                    break;
                },
                .comma => {
                    self.nextToken();
                },
                .colon => {
                    var node_data: NodeData = undefined;

                    self.nextToken();
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
                    if (ntok != null and ntok.?.token.kind == .colon) {
                        self.nextToken();
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
        self.nextToken();
        const ptok = self.peekToken() orelse @panic("unexpcted eof");
        const ident = if (ptok.token.kind == .identifier) blk: {
            self.nextToken();
            break :blk ptok.index;
        } else null;

        const ntok = self.peekToken();
        if (ntok != null and ntok.?.token.kind == .open_brace) {
            self.nextToken();

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
                        .two = .{ .a = @bitCast(i), .b = undefined },
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
                    .two = .{ .a = @bitCast(ident.?), .b = undefined },
                },
            });
        }
    }

    pub fn parseEnumMembers(self: *Self) !NodeRange {
        var these_nodes = std.ArrayList(NodeIndex).init(self.allocator);
        var ptok = self.peekToken();
        while (ptok) |p| : (ptok = self.peekToken()) {
            switch (p.token.kind) {
                .close_brace => {
                    break;
                },
                .identifier => {
                    self.nextToken();
                    const ident_index = ptok.?.index;

                    ptok = self.peekToken();
                    if (ptok != null and ptok.?.token.kind == .assignment) {
                        self.nextToken();
                        const expr = try self.parseOperatorExpression(30);

                        try these_nodes.append(try self.createNode(Node{
                            .kind = .enum_member_value,
                            .data = .{
                                .two = .{
                                    .a = @bitCast(ident_index),
                                    .b = expr,
                                },
                            },
                        }));
                    } else {
                        try these_nodes.append(try self.createNode(Node{
                            .kind = .enum_member,
                            .data = .{
                                .two = .{
                                    .a = @bitCast(ident_index),
                                    .b = undefined,
                                },
                            },
                        }));
                    }

                    ptok = self.peekToken();
                    if (ptok != null and ptok.?.token.kind == .comma) {
                        self.nextToken();
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
            switch (p.token.kind) {
                .@"const" => result |= TypeQualifier.@"const",
                .@"volatile" => result |= TypeQualifier.@"volatile",
                .restrict => result |= TypeQualifier.restrict,
                .atomic => result |= TypeQualifier.atomic,
                else => break,
            }
            self.nextToken();
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
        switch (ptok.token.kind) {
            .open_brace => return self.parseInitializerList(),
            else => return try self.parseOperatorExpression(20),
        }
    }

    pub fn parseInitializerList(self: *Self) !NodeIndex {
        self.nextToken();
        var these_nodes = std.ArrayList(NodeIndex).init(self.allocator);
        defer these_nodes.deinit();

        var ptok = self.peekToken();
        while (ptok) |p| : (ptok = self.peekToken()) {
            switch (p.token.kind) {
                .close_brace => {
                    self.nextToken();
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
                    if (ptok != null and ptok.?.token.kind == .comma) {
                        self.nextToken();
                    } else {
                        _ = try self.expect(.close_brace);
                        break;
                    }
                },
                .comma => {
                    self.nextToken();
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
        switch (ptok.?.token.kind) {
            .dot => {
                self.nextToken();
                const ident = try self.expect(.identifier);
                left = try self.createNode(Node{
                    .kind = .designator_field_terminal,
                    .data = .{
                        .two = .{ .a = @bitCast(ident.index), .b = undefined },
                    },
                });
            },
            .open_bracket => {
                self.nextToken();
                const expr = try self.parseOperatorExpression(30);
                _ = try self.expect(.close_bracket);
                left = try self.createNode(Node{
                    .kind = .designator_index_terminal,
                    .data = .{
                        .two = .{ .a = expr, .b = undefined },
                    },
                });
            },
            else => std.debug.panic("Unexpected token, expected . or [ found {}", .{ptok.?.token.kind}),
        }

        ptok = self.peekToken();
        while (ptok) |p| : (ptok = self.peekToken()) {
            switch (p.token.kind) {
                .dot => {
                    self.nextToken();
                    const ident = try self.expect(.identifier);
                    left = try self.createNode(Node{
                        .kind = .designator_field,
                        .data = .{
                            .two = .{ .a = @bitCast(ident.index), .b = left },
                        },
                    });
                },
                .open_bracket => {
                    self.nextToken();
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
        self.nextToken();
        _ = try self.expect(.open_paren);
        defer {
            _ = self.expect(.close_paren) catch @panic("failed to expect!!!");
        }
        const test_expr = try self.parseOperatorExpression(30);

        const ptok = self.peekToken();
        if (ptok != null and ptok.?.token.kind == .comma) {
            self.nextToken();
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
        return switch (ptok.token.kind) {
            .open_brace => try self.parseCompoundStatement(),
            .@"while" => {
                self.nextToken();
                _ = try self.expect(.open_paren);
                const condition = try self.parseExpression();
                _ = try self.expect(.close_paren);

                const ntok = self.peekToken();
                if (ntok != null and ntok.?.token.kind == .semicolon) {
                    self.nextToken();
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
                self.nextToken();
                const body = try self.parseStatement();

                _ = try self.expect(.@"while");
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
                self.nextToken();

                _ = try self.expect(.open_paren);

                const init_expr = try self.parseDeclOrExprStmt();

                var ntok = self.peekToken();
                const condition = if (ntok != null and ntok.?.token.kind == .semicolon)
                    try self.createNode(Node{ .kind = .empty_statement, .data = undefined })
                else
                    try self.parseExpression();

                _ = try self.expect(.semicolon);

                ntok = self.peekToken();
                const increment = if (ntok != null and ntok.?.token.kind != .close_paren)
                    try self.parseExpression()
                else
                    null;

                _ = try self.expect(.close_paren);

                var node_data: NodeData = undefined;

                ntok = self.peekToken();
                if (ntok != null and ntok.?.token.kind == .semicolon) {
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
                self.nextToken();
                _ = try self.expect(.open_paren);
                const condition = try self.parseExpression();
                _ = try self.expect(.close_paren);

                const body = try self.parseStatement();
                var node_data: NodeData = undefined;
                node_data.as(.two).a = condition;

                const ntok = self.peekToken();
                if (ntok != null and ntok.?.token.kind == .@"else") {
                    self.nextToken();
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
                self.nextToken();
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
                self.nextToken();
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
                self.nextToken();
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
                self.nextToken();
                const ident = try self.expect(.identifier);
                _ = try self.expect(.semicolon);

                return try self.createNode(Node{
                    .kind = .continue_statement,
                    .data = .{
                        .two = .{ .a = @bitCast(ident.index), .b = undefined },
                    },
                });
            },
            .@"continue" => {
                self.nextToken();
                _ = try self.expect(.semicolon);

                return try self.createNode(Node{
                    .kind = .continue_statement,
                    .data = undefined,
                });
            },
            .@"break" => {
                self.nextToken();
                _ = try self.expect(.semicolon);

                return try self.createNode(Node{
                    .kind = .break_statement,
                    .data = undefined,
                });
            },
            .@"return" => {
                self.nextToken();
                const ntok = self.peekToken();
                if (ntok != null and ntok.?.token.kind == .semicolon) {
                    self.nextToken();
                    return try self.createNode(Node{
                        .kind = .return_statement,
                        .data = undefined,
                    });
                }
                const expr = try self.parseExpression();
                _ = try self.expect(.semicolon);

                return try self.createNode(Node{
                    .kind = .return_statement_value,
                    .data = .{
                        .two = .{
                            .a = expr,
                            .b = undefined,
                        },
                    },
                });
            },
            .semicolon => {
                self.nextToken();
                return try self.createNode(Node{
                    .kind = .empty_statement,
                    .data = undefined,
                });
            },
            .identifier => {
                self.nextToken();
                const ntok = self.peekToken();

                if (ntok != null and ntok.?.token.kind == .colon) {
                    self.nextToken();
                    const stmt = try self.parseStatement();
                    return try self.createNode(Node{
                        .kind = .label,
                        .data = .{
                            .two = .{ .a = @bitCast(ptok.index), .b = stmt },
                        },
                    });
                } else {
                    const left = try self.createNode(Node{
                        .kind = .identifier,
                        .data = .{
                            .two = .{ .a = @bitCast(ptok.index), .b = 0 },
                        },
                    });
                    return try self.parseOperatorExpressionWithLeft(0, left);
                }
            },
            else => {
                const result = try self.parseExpression();
                const node = self.unit.nodes.items[result];
                if (node.kind == .label) return result;

                _ = try self.expect(.semicolon);
                return result;
            },
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
        switch (ptok.token.kind) {
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
                self.nextToken();
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
        self.nextToken(); // consume {
        var ptok = self.peekToken();
        if (ptok != null and ptok.?.token.kind == .close_brace) {
            return try self.createNodeAndNext(Node{
                .kind = .compound_empty,
                .data = undefined,
            });
        }

        const first_node = if (ptok != null) blk: {
            const result = try self.parseBlockItem(&ptok.?.token);
            ptok = self.peekToken();
            if (ptok != null and ptok.?.token.kind == .close_brace) {
                self.nextToken();
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
            switch (p.token.kind) {
                .close_brace => {
                    self.nextToken();
                    break;
                },
                else => try these_nodes.append(try self.parseBlockItem(&p.token)),
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

    pub fn unaryPrecedence(token: *const tok.Token) u16 {
        return switch (token.kind) {
            .plusplus,
            .minusminus,
            .open_paren,
            .open_bracket,
            => 150,
            else => 0,
        };
    }

    pub fn unaryPrecedenceRight(token: *const tok.Token) u16 {
        return switch (token.kind) {
            .plusplus,
            .minusminus,
            .plus,
            .minus,
            .exclamation,
            .tilde,
            .star,
            .ampersand,
            .sizeof,
            => 140,
            else => 0,
        };
    }

    pub fn binaryPrecedence(token: *const tok.Token) u16 {
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

            // .assignment,
            // .plus_eq,
            // .minus_eq,
            // .star_eq,
            // .slash_eq,
            // .percent_eq,
            // .left_shift_eq,
            // .right_shift_eq,
            // .ampersand_eq,
            // .carot_eq,
            // .pipe_eq,
            // => 20,
            .comma => 10,
            else => 0,
        };
    }

    pub fn binaryPrecedenceRight(token: *const tok.Token) u16 {
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
            .question => 30,
            else => 0,
        };
    }

    pub fn parseTypeOrOperatorExpression(self: *Self, last_prec: u16) !NodeIndex {
        const ptok = self.peekToken();
        switch (ptok.?.token.kind) {
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
            => {
                var storage_class: StorageClass.Type = 0;
                var identifier: ?TokenIndex = null;
                var attribute_data: ?NodeData = null;
                const type_node = try self.parseDeclarationSpecifiers(&storage_class, &attribute_data);
                if (attribute_data != null) @panic("what do we do?");
                return (try self.parseDeclarator(type_node, &identifier)).node.?;
            },
            else => return try self.parseOperatorExpression(last_prec),
        }
    }

    pub fn parseOperatorExpression(self: *Self, last_prec: u16) (ParseError || std.mem.Allocator.Error)!NodeIndex {
        var op = self.peekToken();
        var left: NodeIndex = undefined;
        blk: {
            if (op != null) {
                const unary_prec_right = unaryPrecedenceRight(&op.?.token);

                if (unary_prec_right != 0 and unary_prec_right >= last_prec) {
                    self.nextToken();
                    const op_kind = op.?.token.kind;
                    if (op.?.token.kind == .sizeof) {
                        op = self.peekToken();
                        if (op != null and op.?.token.kind == .open_paren) {
                            self.nextToken();

                            left = try self.parseTypeOrOperatorExpression(140);

                            _ = try self.expect(.close_paren);
                        } else {
                            left = try self.parseTypeOrOperatorExpression(140);
                        }
                    } else {
                        left = try self.parseOperatorExpression(unary_prec_right);
                    }

                    var unary_data: NodeData = undefined;
                    unary_data.as(.two).a = left;
                    unary_data.as(.four).c = @truncate(@intFromEnum(op_kind));
                    left = try self.createNode(Node{
                        .kind = .unary_prefix_operator,
                        .data = unary_data,
                    });

                    break :blk;
                }
            }

            left = try self.parsePrimaryExpression();
        }

        return try self.parseOperatorExpressionWithLeft(last_prec, left);
    }

    pub fn parseOperatorExpressionWithLeft(self: *Self, last_prec: u16, lleft: NodeIndex) (ParseError || std.mem.Allocator.Error)!NodeIndex {
        var left = lleft;

        while (true) {
            const op = self.peekToken();
            if (op == null) break;
            const prec = binaryPrecedence(&op.?.token);
            const prec_right = binaryPrecedenceRight(&op.?.token);
            const unary_prec_right = unaryPrecedence(&op.?.token);

            if ((prec > last_prec and prec != 0) or (prec_right >= last_prec and prec_right != 0)) {
                self.nextToken(); // consume operator

                if (op.?.token.kind == .question) {
                    const body = try self.parseOperatorExpression(prec);
                    _ = try self.expect(.colon);
                    const else_body = try self.parseOperatorExpression(prec);

                    var node_data: NodeData = undefined;
                    node_data.as(.two).a = left;
                    node_data.as(.four).c = self.relativeOffset(body);
                    node_data.as(.four).d = self.relativeOffset(else_body);

                    left = try self.createNode(Node{
                        .kind = .if_statement_else,
                        .data = node_data,
                    });
                    continue;
                }
                const right = try self.parseOperatorExpression(prec);

                var bin_op_data: NodeData = undefined;
                bin_op_data.as(.two).a = left;
                bin_op_data.as(.four).c = @truncate(self.relativeOffset(right));
                bin_op_data.as(.four).d = @truncate(@intFromEnum(op.?.token.kind));

                left = try self.createNode(Node{
                    .kind = .binary_lr_operator,
                    .data = bin_op_data,
                });
            } else if (unary_prec_right > last_prec and unary_prec_right != 0) {
                self.nextToken();

                var unary_data: NodeData = undefined;
                switch (op.?.token.kind) {
                    .open_paren => {
                        var these_nodes = std.ArrayList(NodeIndex).init(self.allocator);
                        defer these_nodes.deinit();

                        var ptok = self.peekToken();
                        while (ptok) |p| : (ptok = self.peekToken()) {
                            switch (p.token.kind) {
                                .close_paren => {
                                    self.nextToken();
                                    break;
                                },
                                else => try these_nodes.append(try self.parseOperatorExpression(20)),
                            }

                            ptok = self.peekToken();
                            if (ptok == null) @panic("Unexpected EOF");
                            switch (ptok.?.token.kind) {
                                .comma => {
                                    self.nextToken();
                                },
                                .close_paren => {
                                    self.nextToken();
                                    break;
                                },
                                else => {
                                    var stdout = std.io.getStdOut().writer();
                                    Node.writeTree(left, self.unit, 0, true, false, &stdout) catch @panic("err");
                                    std.debug.panic("TODO: Expected clsoe paren or comma found {}", .{ptok.?.token.kind});
                                },
                            }
                        }

                        if (these_nodes.items.len == 0) {
                            unary_data.as(.two).a = left;

                            left = try self.createNode(Node{
                                .kind = .invoke,
                                .data = unary_data,
                            });
                        } else if (these_nodes.items.len == 1) {
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
                        unary_data.as(.four).c = @truncate(@intFromEnum(op.?.token.kind));
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
        var ptok = self.peekToken() orelse @panic("EOF");

        // float_literal,
        // double_literal,
        // int_literal,
        // unsigned_int_literal,
        // long_literal,
        // unsigned_long_literal,
        // long_long_literal,
        // unsigned_long_long_literal,
        // size_literal,
        // unsigned_size_literal,

        const node = switch (ptok.token.kind) {
            .open_paren => {
                self.nextToken();
                ptok = self.peekToken() orelse @panic("EOF");

                switch (ptok.token.kind) {
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
                    => {
                        var storage_class: StorageClass.Type = 0;
                        var identifier: ?TokenIndex = null;
                        var attribute_data: ?NodeData = null;
                        const type_node = try self.parseDeclarationSpecifiers(&storage_class, &attribute_data);

                        const full_type = try self.parseDeclarator(type_node, &identifier);

                        _ = try self.expect(.close_paren);

                        ptok = self.peekToken() orelse @panic("eof");
                        const expr = if (ptok.token.kind == .open_brace)
                            try self.parseInitializerList()
                        else
                            try self.parseOperatorExpression(140);

                        var node_data: NodeData = undefined;
                        node_data.as(.two).a = full_type.node.?;
                        node_data.as(.four).c = self.relativeOffset(expr);
                        node_data.as(.eight).h = @bitCast(storage_class);
                        return try self.createNode(Node{
                            .kind = .cast,
                            .data = node_data,
                        });
                    },
                    else => {},
                }

                const result = try self.parseExpression();
                _ = try self.expect(.close_paren);
                return result;
            },
            .float_literal => Node{
                .kind = .float_literal,
                .data = .{
                    .double = self.unit.fvalue(ptok.index),
                },
            },
            .double_literal => Node{
                .kind = .float_literal,
                .data = .{
                    .double = self.unit.fvalue(ptok.index),
                },
            },
            .int_literal => Node{
                .kind = .int_literal,
                .data = .{
                    .long = self.unit.ivalue(ptok.index),
                },
            },
            .unsigned_int_literal => Node{
                .kind = .unsigned_int_literal,
                .data = .{
                    .long = self.unit.ivalue(ptok.index),
                },
            },
            .long_literal => Node{
                .kind = .long_literal,
                .data = .{
                    .long = self.unit.ivalue(ptok.index),
                },
            },
            .unsigned_long_literal => Node{
                .kind = .unsigned_long_literal,
                .data = .{
                    .long = self.unit.ivalue(ptok.index),
                },
            },
            .long_long_literal => Node{
                .kind = .long_long_literal,
                .data = .{
                    .long = self.unit.ivalue(ptok.index),
                },
            },
            .unsigned_long_long_literal => Node{
                .kind = .unsigned_long_long_literal,
                .data = .{
                    .long = self.unit.ivalue(ptok.index),
                },
            },
            .size_literal => Node{
                .kind = .size_literal,
                .data = .{
                    .long = self.unit.ivalue(ptok.index),
                },
            },
            .unsigned_size_literal => Node{
                .kind = .unsigned_size_literal,
                .data = .{
                    .long = self.unit.ivalue(ptok.index),
                },
            },
            .string_literal => {
                self.nextToken();
                var return_node = try self.createNode(Node{
                    .kind = .string_literal,
                    .data = .{
                        .two = .{ .a = @bitCast(ptok.index), .b = 0 },
                    },
                });
                var ntok = self.peekToken();
                while (ntok != null and ntok.?.token.kind == .string_literal) : (ntok = self.peekToken()) {
                    self.nextToken();
                    return_node = try self.createNode(Node{
                        .kind = .string_literal_join,
                        .data = .{
                            .two = .{
                                .a = return_node,
                                .b = @bitCast(ntok.?.index),
                            },
                        },
                    });
                }
                return return_node;
            },
            .stringified_literal => Node{
                .kind = .stringified_literal,
                .data = .{
                    .two = .{ .a = @bitCast(ptok.index), .b = 0 },
                },
            },
            .char_literal => Node{
                .kind = .char_literal,
                .data = .{
                    .two = .{ .a = self.unit.charAt(@bitCast(ptok.index)), .b = 0 },
                },
            },
            .identifier => {
                self.nextToken();
                // const ntok = self.peekToken();
                // if (ntok != null and ntok.?.token.kind == .colon) {
                //     self.nextToken();
                //     const stmt = try self.parseStatement();
                //     return try self.createNode(Node{
                //         .kind = .label,
                //         .data = .{
                //             .two = .{ .a = @bitCast(ptok.index), .b = stmt },
                //         },
                //     });
                // }

                return self.createNode(Node{
                    .kind = .identifier,
                    .data = .{
                        .two = .{ .a = @bitCast(ptok.index), .b = 0 },
                    },
                });
            },
            .type_name => {
                self.nextToken();
                // const ntok = self.peekToken();
                // if (ntok != null and ntok.?.token.kind == .colon) {
                //     self.nextToken();
                //     const stmt = try self.parseStatement();
                //     return try self.createNode(Node{
                //         .kind = .label,
                //         .data = .{
                //             .two = .{ .a = @bitCast(ptok.index), .b = stmt },
                //         },
                //     });
                // }

                return self.createNode(Node{
                    .kind = .type_name,
                    .data = .{
                        .two = .{ .a = @bitCast(ptok.index), .b = 0 },
                    },
                });
            },
            else => std.debug.panic("Unexpected in primary {}", .{ptok.token.kind}),
        };

        return try self.createNodeAndNext(node);
    }

    const TokenResult = struct {
        index: tok.TokenIndex,
        token: tok.Token,
    };

    inline fn nextToken(self: *Self) void {
        const index = self.tokenizer.next(false);
        _ = index;
    }

    fn peekToken(self: *Self) ?TokenResult {
        const index = self.tokenizer.peek(false);
        if (index == null) return null;

        return .{
            .token = self.unit.token(index.?),
            .index = index.?,
        };
    }

    inline fn hasNext(self: *Self) bool {
        return self.unit.virtual_token_next < self.unit.virtual_tokens.items.len or self.tokenizer.peek(false) != null;
    }

    fn rawPeekToken(self: *Self) ?tok.Token {
        const index = self.tokenizer.peek(false);
        if (index == null) return null;

        return self.unit.token(index.?);
    }

    fn rawPeekTokenIndex(self: *Self) tok.TokenIndex {
        const index = self.tokenizer.peek(false);
        return index.?;
    }

    fn rawPeekTokenEOL(self: *Self) ?tok.Token {
        const index = self.tokenizer.peek(true);
        if (index == null) return null;

        return self.unit.token(index.?);
    }

    fn rawPeekTokenEOLIndex(self: *Self) tok.TokenIndex {
        const index = self.tokenizer.peek(true);
        return index.?;
    }

    fn expect(self: *Self, kind: tok.TokenKind) !TokenResult {
        const token = self.peekToken();
        if (token == null) return error.OutOfTokens;

        if (token.?.token.kind == kind) {
            self.nextToken();
            return token.?;
        } else {
            std.log.err("Found {}", .{token.?.token.kind});
            return error.UnexpectedToken;
        }
    }

    fn expectIndex(self: *Self, kind: tok.TokenKind) !tok.TokenIndex {
        const token = self.peekToken();
        if (token == null) return error.OutOfTokens;

        if (token.?.token.kind == kind) {
            self.nextToken();
            return token.?.index;
        } else {
            std.log.err("Found {}", .{token.?.token.kind});
            return error.UnexpectedToken;
        }
    }

    fn rawExpect(self: *Self, kind: tok.TokenKind) !tok.Token {
        const index = self.tokenizer.peek(false);
        if (index == null) return error.OutOfTokens;

        const token = self.unit.tokens[0].items[index.?];
        if (token.kind == kind) {
            _ = self.tokenizer.next(false);
            return token;
        } else {
            return error.UnexpectedToken;
        }
    }

    fn rawExpectEOL(self: *Self, kind: tok.TokenKind) !tok.Token {
        const index = self.tokenizer.peek(true);
        if (index == null) return error.OutOfTokens;

        const token = self.unit.token(index.?);
        if (token.kind == kind) {
            _ = self.tokenizer.next(true);
            return token;
        } else {
            return error.UnexpectedToken;
        }
    }

    fn rawExpectEOLIndex(self: *Self, kind: tok.TokenKind) !tok.TokenIndex {
        const index = self.tokenizer.peek(true);
        if (index == null) return error.OutOfTokens;

        const token = self.unit.token(index.?);
        if (token.kind == kind) {
            _ = self.tokenizer.next(true);
            return index.?;
        } else {
            return error.UnexpectedToken;
        }
    }

    //             } else if (std.mem.eql(u8, directive, "include")) {
    //                 const ptok = self.rawPeekTokenEOL();
    //                 if (ptok != null and ptok.?.token.kind == .string_literal) {
    //                     tidx = self.peekTokenIndex();
    //                     _ = self.tokenizer.nextEOL();
    //                     const file_path = self.unit.stringAt(tidx);
    //                     const this_file_dir_path = std.fs.path.dirname(self.unit.files.items[0].file_path).?;
    //                     const full_file_path = try std.fs.path.resolve(self.allocator, &.{
    //                         this_file_dir_path, file_path,
    //                     });

    //                     std.log.info("file is {s} {s}", .{ full_file_path, file_path });
    //                     var file = std.fs.openFileAbsolute(full_file_path, .{}) catch @panic("dang");
    //                     const file_contents = file.readToEndAlloc(self.allocator, std.math.maxInt(usize)) catch |e| std.debug.panic("foo {}", .{e});
    //                     defer self.allocator.free(file_contents);

    //                     var file_tokenizer = Tokenizer.initVirtual(self.allocator, self.unit, @truncate(self.unit.files.items.len));
    //                     while (file_tokenizer.next()) |pt| {
    //                         std.log.debug("Token: {}", .{pt});
    //                     }
    //                 } else {
    //                     @panic("todo");
    //                 }
    //             }
    //         },

    inline fn relativeOffset(self: *Self, offset: u32) u16 {
        return @as(u16, @truncate(self.unit.nodes.items.len - offset));
    }

    fn createNode(self: *Self, node: Node) !NodeIndex {
        const index = self.unit.nodes.items.len;
        try self.unit.nodes.append(node);

        return @truncate(index);
    }

    fn createNodeAndNext(self: *Self, node: Node) !NodeIndex {
        const result = try self.createNode(node);
        self.nextToken();
        return result;
    }
};
