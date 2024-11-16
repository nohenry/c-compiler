const std = @import("std");
const Unit = @import("unit.zig").Unit;
const DefineValue = @import("unit.zig").DefineValue;
const tok = @import("tokenizer.zig");
const Tokenizer = tok.Tokenizer;

pub const ParseError = error{ OutOfTokens, UnexpectedToken };

pub const NodeKind = enum(u32) {
    int_literal,
    float_literal,
    string_literal,

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

    /// node_data: a(u32) = index of identifier, b(u16) = relative index of type, c(u16) = relative index of init
    var_declaration_init,

    /// node_data: a(u32) = index of element type, b(u16) = relative start of type_qualifier, c(u16) = count of type qualifiers
    /// node_data: a(u32) = index of base type, b(u32) = start of type_qualifier
    pointer,

    /// For all types:
    /// node_data: a(u8) = TypeQualifier.Type
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

    /// node_data: a(u32) = token index of type name identifier
    type_name,
    /// node_data: a(u32) = token index of identifier
    identifier,

    /// Represents Node Range
    /// node_data: a = start, b = end
    range,
};

pub const NodeData = extern union {
    long: u64,
    // range: NodeRange,
    two: extern struct {
        a: u32,
        b: u32,
    },
    four: extern struct {
        a: u16,
        b: u16,
        c: u16,
        d: u16,
    },
    eight: extern struct {
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
    float: extern struct {
        a: f32,
        b: f32,
    },

    pub inline fn as(self: *@This(), comptime field: std.meta.FieldEnum(@This())) *std.meta.FieldType(@This(), field) {
        return &@field(self, @tagName(field));
    }
};

comptime {
    std.debug.assert(@sizeOf(NodeData) == 8);
}

pub const StorageClass = struct {
    pub const Type = u8;
    pub const typedef: Type = (1 << 0);
    pub const @"extern": Type = (1 << 1);
    pub const static: Type = (1 << 2);
    pub const thread_local: Type = (1 << 3);
    pub const auto: Type = (1 << 4);
    pub const register: Type = (1 << 5);

    pub fn write(storage_class: Type, writer: anytype) !void {
        if ((storage_class & typedef) > 0) {
            _ = try writer.write("TYPEDEF");
        }
        if ((storage_class & @"extern") > 0) {
            _ = try writer.write("EXTERN");
        }
        if ((storage_class & static) > 0) {
            _ = try writer.write("STATIC");
        }
        if ((storage_class & thread_local) > 0) {
            _ = try writer.write("THREAD_LOCAL");
        }
        if ((storage_class & auto) > 0) {
            _ = try writer.write("auto");
        }
        if ((storage_class & register) > 0) {
            _ = try writer.write("register");
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
            _ = try writer.write("CONST");
        }
        if ((type_qualifier & restrict) > 0) {
            _ = try writer.write("RESTRICT");
        }
        if ((type_qualifier & @"volatile") > 0) {
            _ = try writer.write("VOLATILE");
        }
        if ((type_qualifier & atomic) > 0) {
            _ = try writer.write("ATOMIC");
        }
    }
};

pub const NodeIndex = u32;
pub const Node = struct {
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
                const ident_index = self.data.as(.two).a;
                const type_index = absoluteIndex(index, self.data.as(.four).c);

                try writer.print("\x1b[1;35mVarDeclaration\x1b[0m \x1b[1;36m'{s}'\x1b[0m", .{unit.identifierAt(ident_index)});

                result = .{
                    .node = type_index,
                };
            },
            .var_declaration_init => {
                const ident_index = self.data.as(.two).a;
                const type_index = absoluteIndex(index, self.data.as(.four).c);
                const init_index = absoluteIndex(index, self.data.as(.four).d);

                try writer.print("\x1b[1;35mVarDeclaration\x1b[0m \x1b[1;36m'{s}'\x1b[0m", .{unit.identifierAt(ident_index)});

                result = NodeRangeOrNode.initNodes(&.{
                    type_index,
                    init_index,
                });
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
                const qualifier = self.data.as(.eight).a;
                try writer.print("\x1b[1;35mType\x1b[0m ", .{});
                try TypeQualifier.write(qualifier, writer);
                try writer.print("\x1b[32m{s}\x1b[0m", .{typeToStr(self.kind)});
            },
            .type_name => {
                const ident_index = self.data.as(.two).a;

                try writer.print("\x1b[1;35mType\x1b[0m \x1b[32m{s}\x1b[0m", .{unit.identifierAt(ident_index)});
            },
            .identifier => {
                const ident_index = self.data.as(.two).a;

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
    virtual_tokens: std.ArrayList(tok.Token),
    virtual_token_next: u32 = 0,
    in_handle_pp: bool = false,

    expansion_argument_map: std.StringHashMap(DefineValue),

    const Self = @This();
    pub fn init(unit: *Unit, tokenizer: *Tokenizer) Self {
        return .{
            .allocator = unit.allocator,
            .unit = unit,
            .tokenizer = tokenizer,
            .virtual_tokens = std.ArrayList(tok.Token).init(unit.allocator),
            .expansion_argument_map = std.StringHashMap(DefineValue).init(unit.allocator),
        };
    }

    pub fn parseUnit(self: *Self) !NodeRange {
        var these_nodes = std.ArrayList(NodeIndex).init(self.allocator);
        try self.handlePP();

        while (self.hasNext()) {
            const node_index = try self.parseExternalDeclaration();
            try these_nodes.append(node_index);
            // if (node_range.count == 0) break;
            // for (node_range.start..node_range.start + node_range.count) |index| {
            //     try these_nodes.append(self.unit.node_ranges.items[index]);
            // }
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
        return try self.parseDeclaration();
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

    pub fn parseDeclaration(self: *Self) !NodeIndex {
        var ptok = self.peekToken();
        var storage_class: StorageClass.Type = 0;
        var type_qualifier: TypeQualifier.Type = 0;
        var type_kind: ?NodeKind = null;
        var type_name_token: tok.TokenIndex = undefined;

        while (ptok) |p| : (ptok = self.peekToken()) {
            switch (p.kind) {
                .atomic => type_qualifier |= TypeQualifier.atomic,
                .auto => storage_class |= StorageClass.auto,
                .@"const" => type_qualifier |= TypeQualifier.@"const",
                .@"extern" => storage_class |= StorageClass.@"extern",
                .register => storage_class |= StorageClass.register,
                .restrict => type_qualifier |= TypeQualifier.restrict,
                .static => storage_class |= StorageClass.static,
                .thread_local => storage_class |= StorageClass.thread_local,
                .typedef => storage_class |= StorageClass.typedef,
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

                .type_name => {
                    type_kind = .type_name;
                    type_name_token = p.start;
                },
                .identifier => break,

                .semicolon => {
                    break;
                },

                else => std.debug.panic("TODO: unexpected token {}", .{p.kind}),
            }
            _ = self.tokenizer.next();
        }

        var type_node_data: NodeData = undefined;
        switch (type_kind.?) {
            .type_name => {
                type_node_data.two.a = type_name_token;
            },
            else => {
                type_node_data.eight.a = type_qualifier;
            },
        }

        const type_node = try self.createNode(Node{
            .kind = type_kind.?,
            .data = type_node_data,
        });

        ptok = self.peekToken();
        var these_nodes = std.ArrayList(NodeIndex).init(self.allocator);
        defer these_nodes.deinit();
        while (ptok) |p| : (ptok = self.peekToken()) {
            switch (p.kind) {
                .semicolon => {
                    _ = self.tokenizer.next();
                    break;
                },
                else => {
                    var this_ident: u32 = undefined;
                    const this_type = (try self.parseDeclarator(type_node, &this_ident)).?;

                    var kind = NodeKind.var_declaration;
                    var decl_node_data: NodeData = undefined;
                    decl_node_data.as(.two).a = this_ident;

                    ptok = self.peekToken();
                    if (ptok != null and ptok.?.kind == .assignment) {
                        _ = self.tokenizer.next();

                        const decl_init = try self.parseInitializer();
                        decl_node_data.as(.four).d = @truncate(self.relativeOffset(decl_init));
                        kind = .var_declaration_init;
                    }

                    decl_node_data.as(.four).c = @truncate(self.relativeOffset(this_type));

                    try these_nodes.append(try self.createNode(Node{
                        .kind = kind,
                        .data = decl_node_data,
                    }));
                },
            }

            ptok = self.peekToken();
            if (ptok == null) continue;
            switch (ptok.?.kind) {
                .comma => {
                    _ = self.tokenizer.next();
                },
                .semicolon => {
                    _ = self.tokenizer.next();
                    break;
                },
                else => @panic("TODO: unexpected token"),
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

    pub fn parseDeclarator(self: *Self, base_type: NodeIndex, identifier: *u32) !?NodeIndex {
        const ptok = self.peekToken() orelse @panic("ran out of tokens");
        switch (ptok.kind) {
            .identifier => {
                _ = self.nextToken();
                identifier.* = ptok.start;
                return base_type;
                // return try self.createNodeAndNext(Node{
                //     .kind = .declaration,
                //     .data = .{ .two = .{
                //         .a = ptok.start,
                //         .b = base_type,
                //     } },
                // });
            },
            .star => {
                _ = self.nextToken();
                const type_qualifier = self.parseAnyTypeQualifiers();
                const element_type = (try self.parseDeclarator(base_type, identifier)).?;

                return try self.createNodeAndNext(Node{
                    .kind = .pointer,
                    .data = .{
                        .two = .{
                            .a = element_type,
                            .b = type_qualifier,
                        },
                    },
                });
            },
            else => return null,
        }
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
                else => {},
            }
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

    pub fn parseInitializer(self: *Self) !NodeIndex {
        return try self.parseExpression();
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

    pub fn parseOperatorExpression(self: *Self, last_prec: u16) !NodeIndex {
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
        var ptok = self.peekToken() orelse @panic("EOF");

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
                    .long = ptok.ivalue(self.unit),
                },
            },
            .float_literal => Node{
                .kind = .float_literal,
                .data = .{
                    .double = ptok.fvalue(self.unit),
                },
            },
            .identifier => Node{
                .kind = .identifier,
                .data = .{
                    .two = .{ .a = ptok.start, .b = 0 },
                },
            },
            else => std.debug.panic("Unexpected in primary {}", .{ptok.kind}),
        };

        return try self.createNodeAndNext(node);
    }

    fn nextToken(self: *Self) ?tok.Token {
        if (self.virtual_token_next < self.virtual_tokens.items.len) {
            const token = self.virtual_tokens.items[self.virtual_token_next];
            self.virtual_token_next += 1;
            return token;
        }
        const index = self.tokenizer.next();
        if (index == null) return null;

        return self.unit.tokens.items[index.?];
    }

    fn peekToken(self: *Self) ?tok.Token {
        if (!self.in_handle_pp) {
            self.handlePP() catch @panic("OOM");
        }
        if (self.virtual_token_next < self.virtual_tokens.items.len) {
            const token = self.virtual_tokens.items[self.virtual_token_next];
            return token;
        }
        const index = self.tokenizer.peek();
        if (index == null) return null;

        return self.unit.tokens.items[index.?];
    }

    inline fn hasNext(self: *Self) bool {
        return self.virtual_token_next < self.virtual_tokens.items.len or self.tokenizer.peek() != null;
    }

    fn rawPeekToken(self: *Self) ?tok.Token {
        const index = self.tokenizer.peek();
        if (index == null) return null;

        return self.unit.tokens.items[index.?];
    }

    fn rawPeekTokenEOL(self: *Self) ?tok.Token {
        const index = self.tokenizer.peekEOL();
        if (index == null) return null;

        return self.unit.tokens.items[index.?];
    }

    fn expect(self: *Self, kind: tok.TokenKind) !tok.Token {
        const token = self.peekToken();
        if (token == null) return error.OutOfTokens;

        if (token.?.kind == kind) {
            _ = self.nextToken();
            return token.?;
        } else {
            return error.UnexpectedToken;
        }
    }

    fn rawExpect(self: *Self, kind: tok.TokenKind) !tok.Token {
        const index = self.tokenizer.peek();
        if (index == null) return error.OutOfTokens;

        const token = self.unit.tokens.items[index.?];
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

        const token = self.unit.tokens.items[index.?];
        if (token.kind == kind) {
            _ = self.tokenizer.nextEOL();
            return token;
        } else {
            return error.UnexpectedToken;
        }
    }

    fn handlePP(self: *Self) (ParseError || std.mem.Allocator.Error)!void {
        var token = self.rawPeekToken() orelse return;
        const old_handle_pp = self.in_handle_pp;
        self.in_handle_pp = true;
        defer self.in_handle_pp = old_handle_pp;

        switch (token.kind) {
            .pp_directive => {
                _ = self.tokenizer.nextEOL();

                const directive = token.ppDirective(self.unit);
                if (std.mem.eql(u8, directive, "define")) {
                    const ident = try self.rawExpectEOL(.identifier);
                    var ptok = self.rawPeekTokenEOL();
                    if (ptok != null and ptok.?.kind == .open_paren) {
                        _ = self.tokenizer.next();

                        var parameter_map = std.StringArrayHashMap(void).init(self.allocator);

                        ptok = self.rawPeekTokenEOL();
                        blk: {
                            while (ptok) |p| : (ptok = self.rawPeekTokenEOL()) {
                                switch (p.kind) {
                                    .identifier => try parameter_map.put(p.identifier(self.unit), {}),
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

                        try self.unit.define_fns.put(ident.identifier(self.unit), .{
                            .token_start = @truncate(start),
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

                        try self.unit.defines.put(ident.identifier(self.unit), .{
                            .token_start = @truncate(start),
                            .token_count = count,
                        });
                    }
                }
            },
            .identifier => {
                try self.handlePPIdentifier(&token);
            },
            else => {},
        }

        token = self.peekToken() orelse return;
        switch (token.kind) {
            .identifier => {
                try self.handlePPIdentifier(&token);
            },
            else => return,
        }
    }

    fn handlePPIdentifier(self: *Self, token: *const tok.Token) !void {
        const ident_str = token.identifier(self.unit);

        if (self.unit.defines.get(ident_str)) |value| {
            _ = self.nextToken();

            const token_slice = self.unit.tokens.items[value.token_start .. value.token_start + value.token_count];
            if (self.virtual_tokens.items.len == 0 or (self.virtual_token_next >= self.virtual_tokens.items.len)) {
                self.virtual_tokens.items.len = 0;
                self.virtual_token_next = 0;
            }
            try self.virtual_tokens.insertSlice(self.virtual_token_next, token_slice);

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

            const token_slice = self.unit.tokens.items[fn_value.token_start .. fn_value.token_start + fn_value.token_count];
            if (self.virtual_tokens.items.len == 0 or (self.virtual_token_next >= self.virtual_tokens.items.len)) {
                self.virtual_tokens.items.len = 0;
                self.virtual_token_next = 0;
            }
            try self.virtual_tokens.insertSlice(self.virtual_token_next, token_slice);

            try self.handlePP();
        } else if (self.expansion_argument_map.get(ident_str)) |value| {
            _ = self.nextToken();
            const token_slice = self.unit.tokens.items[value.token_start .. value.token_start + value.token_count];
            if (self.virtual_tokens.items.len == 0 or (self.virtual_token_next >= self.virtual_tokens.items.len)) {
                self.virtual_tokens.items.len = 0;
                self.virtual_token_next = 0;
            }
            try self.virtual_tokens.insertSlice(self.virtual_token_next, token_slice);

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
