const std = @import("std");
const Unit = @import("unit.zig").Unit;
const NodeIndex = @import("parser.zig").NodeIndex;
const Node = @import("parser.zig").Node;
const Type = @import("types.zig").Type;
const TypeFlags = @import("types.zig").TypeFlags;
const TokenKind = @import("tokenizer.zig").TokenKind;
const TokenIndex = @import("tokenizer.zig").TokenIndex;
const TypeQualifier = @import("parser.zig").TypeQualifier;
const StorageClass = @import("parser.zig").StorageClass;

pub const BuiltinsType = struct {
    pub const map = std.StaticStringMap(*const fn (*Unit) (std.mem.Allocator.Error)!Type).initComptime(blk: {
        const decls = @typeInfo(BuiltinsType).@"struct".decls;
        var result: [decls.len - 1]struct { []const u8, (*const fn (*Unit) (std.mem.Allocator.Error)!Type) } = undefined;
        for (decls, 0..) |decl, i| {
            if (std.mem.eql(u8, decl.name, "map")) {
                continue;
            }
            result[i - 1][0] = decl.name;
            result[i - 1][1] = @field(BuiltinsType, decl.name);
        }
        break :blk &result;
    });

    pub fn __builtin_va_list(unit: *Unit) !Type {
        const value = "__builtin_va_list";
        return unit.interner.builtinStructTy(@intFromPtr(value), &.{
            unit.interner.pointerTy(unit.interner.intTy(false, 0), 0),
        }, 0);
    }

    pub fn __builtin_va_start(unit: *Unit) !Type {
        return unit.interner.funcTy(&.{
            try __builtin_va_list(unit),
            unit.interner.anyTy(),
        }, unit.interner.voidTy(), false);
    }

    pub fn __builtin_va_arg(unit: *Unit) !Type {
        return unit.interner.funcTy(&.{
            try __builtin_va_list(unit),
            unit.interner.anyKindTy(.type),
        }, unit.interner.anyTy(), false);
    }

    pub fn __builtin_va_end(unit: *Unit) !Type {
        return unit.interner.funcTy(&.{
            try __builtin_va_list(unit),
        }, unit.interner.voidTy(), false);
    }

    pub fn __int128_t(unit: *Unit) !Type {
        return unit.interner.int128Ty(true, 0);
    }

    pub fn __uint128_t(unit: *Unit) !Type {
        return unit.interner.int128Ty(true, 0);
    }
};

pub const TypeChecker = struct {
    unit: *Unit,
    storage_class: ?StorageClass.Type = null,

    const Self = @This();

    pub fn init(unit: *Unit) Self {
        return .{
            .unit = unit,
        };
    }

    /// nidx should be index to string_literal_join or string_literal
    pub fn getStringJoinLength(self: *Self, nidx: NodeIndex, len: u32) u32 {
        const node = &self.unit.nodes.items[nidx];

        switch (node.kind) {
            .string_literal, .stringified_literal => {
                return self.unit.stringLength(@bitCast(node.data.two.a)) + len;
            },
            .string_literal_join => {
                const length = self.unit.stringLength(@bitCast(node.data.two.b));
                return try @call(.always_tail, getStringJoinLength, .{ self, node.data.two.a, length + len });
            },
            else => unreachable,
        }
    }

    pub fn checkNode(self: *Self, nidx: NodeIndex, expected_type: ?Type) !Type {
        const node = &self.unit.nodes.items[nidx];
        const result: Type = switch (node.kind) {
            .empty => return self.unit.interner.voidTy(),
            .char_literal => self.unit.interner.charTy(true, 0),
            .float_literal => self.unit.interner.floatTy(0),
            .double_literal => self.unit.interner.doubleTy(0),
            .int_literal => self.unit.interner.intTy(true, 0),
            .unsigned_int_literal => self.unit.interner.intTy(false, 0),
            .long_literal => self.unit.interner.longTy(true, 0),
            .unsigned_long_literal => self.unit.interner.longTy(false, 0),
            .long_long_literal => self.unit.interner.longlongTy(true, 0),
            .unsigned_long_long_literal => self.unit.interner.longlongTy(false, 0),
            .string_literal => self.unit.interner.arrayTy(
                self.unit.interner.charTy(false, 0),
                self.unit.stringLength(@bitCast(node.data.two.a)) + 1,
                0,
            ),
            .string_literal_join => blk: {
                break :blk self.unit.interner.arrayTy(
                    self.unit.interner.charTy(false, 0),
                    self.getStringJoinLength(nidx, 1),
                    0,
                );
            },
            .stringified_literal => blk: {
                break :blk self.unit.interner.arrayTy(
                    self.unit.interner.charTy(false, 0),
                    self.getStringJoinLength(nidx, 1),
                    0,
                );
            },
            .initializer_list => expected_type orelse std.debug.panic("Unable to infer initializer type", .{}),
            .initializer_list_one => blk: {
                const designation = &self.unit.nodes.items[node.data.two.a];
                switch (designation.kind) {
                    .designation => @panic("TODO"),
                    else => {
                        const element = switch (expected_type.?.kind) {
                            .array => |arr| arr.base,
                            .array_unsized => |arr| arr.base,
                            else => blk1: {
                                if (expected_type.?.isScalar()) {
                                    std.log.warn("Excess braces around initializer", .{});
                                    break :blk1 expected_type.?;
                                }

                                std.debug.panic("Unable to get elemetn type", .{});
                            },
                        };
                        _ = try self.checkNode(node.data.two.a, element);
                    },
                }

                break :blk expected_type.?;
            },
            .initializer_list_many => blk: {
                switch (expected_type.?.kind) {
                    .array => |arr| {
                        var index = node.data.two.a;
                        const end_index = index + node.data.two.b;
                        while (index < end_index) : (index += 1) {
                            const desig_index = self.unit.node_ranges.items[index];
                            const designation = &self.unit.nodes.items[desig_index];
                            switch (designation.kind) {
                                .designation => @panic("TODO"),
                                else => {
                                    _ = try self.checkNode(desig_index, arr.base);
                                },
                            }
                        }
                        if (node.data.two.b > arr.size) {
                            std.log.warn("Too many values in initizlier (will be ignored)", .{});
                        }
                        break :blk expected_type.?;
                    },
                    .array_unsized => |arr| {
                        var index = node.data.two.a;
                        const end_index = index + node.data.two.b;
                        while (index < end_index) : (index += 1) {
                            const desig_index = self.unit.node_ranges.items[index];
                            const designation = &self.unit.nodes.items[desig_index];
                            switch (designation.kind) {
                                .designation => @panic("TODO"),
                                else => {
                                    _ = try self.checkNode(desig_index, arr.base);
                                },
                            }
                        }
                        const sized_type = self.unit.interner.arrayUnsizedToSized(expected_type.?, node.data.two.b);
                        break :blk sized_type;
                    },
                    else => {
                        if (expected_type.?.isScalar()) {
                            std.log.warn("Excess braces around initializer", .{});
                            break :blk expected_type.?;
                        }

                        std.debug.panic("Unable to get elemetn type", .{});
                    },
                }
            },
            .binary_lr_operator => blk: {
                const op: TokenKind = @enumFromInt(node.data.four.d);
                // switch (op) {
                //     .assignment,
                //     .plus_eq,
                //     .minus_eq,
                //     .star_eq,
                //     .slash_eq,
                //     .percent_eq,
                //     .left_shift_eq,
                //     .right_shift_eq,
                //     .ampersand_eq,
                //     .carot_eq,
                //     .pipe_eq,
                //     => {

                //     },
                //     else => {},
                // }
                const left = try self.checkNode(node.data.two.a, null);
                if (op == .dot) {
                    if (!left.isStructured()) {
                        std.debug.panic("Left side is not a structure type!!!!", .{});
                    }

                    const right_node = &self.unit.nodes.items[Node.absoluteIndex(nidx, node.data.four.c)];
                    if (right_node.kind != .identifier) { // TODO: what if field is typename??
                        std.debug.panic("Right side is not a identifier!!!!", .{});
                    }

                    const field_str = self.unit.identifierAt(@bitCast(right_node.data.two.a));

                    switch (left.kind) {
                        .@"struct", .@"union", .unnamed_struct, .unnamed_union => {
                            const st_nidx, const fields_multi_type = switch (left.kind) {
                                .@"struct" => |s| .{ s.nidx, s.fields },
                                .unnamed_struct => |s| .{ s.nidx, s.fields },
                                .@"union" => |s| .{ s.nidx, s.variants },
                                .unnamed_union => |s| .{ s.nidx, s.variants },
                                else => unreachable,
                            };
                            const field_map = self.unit.field_map.getPtr(st_nidx).?;
                            const field_index = field_map.get(field_str) orelse {
                                std.debug.panic("Field \x1b[1m'{s}'\x1b[0m does not exist in type \x1b[32m{s}\x1b[0m", .{
                                    field_str,
                                    self.unit.interner.printTyToStr(left, self.unit.allocator),
                                });
                            };
                            const refed_nidx = switch (left.kind) {
                                .@"struct", .@"union" => field_node: {
                                    const str_node = &self.unit.nodes.items[st_nidx - 1];
                                    break :field_node self.unit.node_ranges.items[str_node.data.two.a + field_index];
                                },
                                .unnamed_struct, .unnamed_union => field_node: {
                                    const str_node = &self.unit.nodes.items[st_nidx];
                                    break :field_node self.unit.node_ranges.items[str_node.data.two.a + field_index];
                                },
                                else => unreachable,
                            };
                            try self.unit.node_to_node.put(Node.absoluteIndex(nidx, node.data.four.c), refed_nidx);

                            const field_tys = self.unit.interner.getMultiTypes(fields_multi_type);
                            const field = field_tys[field_index];
                            switch (field.kind) {
                                // .field => {
                                //     try self.unit.node_to_node.put(right_node.data.two.a, field.kind.field.nidx);
                                //     break :blk field.kind.field.base;
                                // },
                                .bitfield => {
                                    break :blk field.kind.bitfield.base;
                                },
                                .bitfield_named => {
                                    break :blk field.kind.bitfield_named.base;
                                },
                                else => break :blk field,
                            }
                        },
                        else => {},
                    }

                    std.debug.panic("Structured type does not contain field \x1b[1m{s}\x1b[0m", .{field_str});

                    break :blk;
                } else if (op == .arrow) {
                    if (left.kind != .pointer) {
                        std.debug.panic("Left side is not a pointer to structure type!!!!", .{});
                    }

                    const struct_type = left.kind.pointer.base;
                    if (!struct_type.isStructured()) {
                        std.debug.panic("Left side is not a pointer to structure type!!!!", .{});
                    }

                    const right_node = &self.unit.nodes.items[Node.absoluteIndex(nidx, node.data.four.c)];
                    if (right_node.kind != .identifier) { // TODO: what if field is typename??
                        std.debug.panic("Right side is not a identifier!!!!", .{});
                    }

                    const field_str = self.unit.identifierAt(@bitCast(right_node.data.two.a));

                    switch (struct_type.kind) {
                        .@"struct", .@"union", .unnamed_struct, .unnamed_union => {
                            const st_nidx, const fields_multi_type = switch (struct_type.kind) {
                                .@"struct" => |s| .{ s.nidx, s.fields },
                                .unnamed_struct => |s| .{ s.nidx, s.fields },
                                .@"union" => |s| .{ s.nidx, s.variants },
                                .unnamed_union => |s| .{ s.nidx, s.variants },
                                else => unreachable,
                            };
                            const field_map = self.unit.field_map.getPtr(st_nidx).?;
                            const field_index = field_map.get(field_str) orelse {
                                std.debug.panic("Field \x1b[1m'{s}'\x1b[0m does not exist in type \x1b[32m{s}\x1b[0m", .{
                                    field_str,
                                    self.unit.interner.printTyToStr(struct_type, self.unit.allocator),
                                });
                            };
                            const field_tys = self.unit.interner.getMultiTypes(fields_multi_type);
                            const field = field_tys[field_index];
                            switch (field.kind) {
                                .bitfield => {
                                    break :blk field.kind.bitfield.base;
                                },
                                .bitfield_named => {
                                    break :blk field.kind.bitfield_named.base;
                                },
                                else => break :blk field,
                            }
                        },
                        else => {},
                    }

                    std.debug.panic("Structured type does not contain field \x1b[1m{s}\x1b[0m", .{field_str});

                    break :blk;
                }
                const right = try self.checkNode(Node.absoluteIndex(nidx, node.data.four.c), null);

                switch (op) {
                    .plus,
                    .minus,
                    .star,
                    .slash,
                    .percent,
                    .ampersand,
                    .pipe,
                    .carot,
                    .bit_left_shift,
                    .bit_right_shift,
                    => {
                        if (left == right) break :blk left;

                        if (left.kind == .longdouble or right.kind == .longdouble) {
                            break :blk self.unit.interner.longdoubleTy(left.qualifiers);
                        } else if (left.kind == .double or right.kind == .double) {
                            break :blk self.unit.interner.doubleTy(left.qualifiers);
                        } else if (left.kind == .float or right.kind == .float) {
                            break :blk self.unit.interner.floatTy(left.qualifiers);
                        } else if (left.isIntegral() and right.isIntegral()) {
                            if (left.isSigned() == right.isSigned()) {
                                if (left.rank() <= right.rank()) {
                                    break :blk right;
                                } else {
                                    break :blk left;
                                }
                            } else if (!left.isSigned() and left.rank() >= right.rank()) {
                                break :blk left;
                            } else if (!right.isSigned() and right.rank() >= left.rank()) {
                                break :blk right;
                            } else if (left.isSigned()) {
                                const new_type = self.unit.interner.createOrGetTyKind(left.toSigned(false));
                                break :blk new_type;
                            } else if (right.isSigned()) {
                                const new_type = self.unit.interner.createOrGetTyKind(right.toSigned(false));
                                break :blk new_type;
                            }
                        }
                        std.debug.panic("unahnadled type left: {s}, right: {s}", .{
                            self.unit.interner.printTyToStr(left, self.unit.allocator),
                            self.unit.interner.printTyToStr(right, self.unit.allocator),
                        });
                    },
                    .gt,
                    .gte,
                    .lt,
                    .lte,
                    .equality,
                    .nequality,
                    => {
                        if (left == right) break :blk self.unit.interner.boolTy(0);

                        if (left.kind == .longdouble or right.kind == .longdouble) {
                            break :blk self.unit.interner.boolTy(0);
                        } else if (left.kind == .double or right.kind == .double) {
                            break :blk self.unit.interner.boolTy(0);
                        } else if (left.kind == .float or right.kind == .float) {
                            break :blk self.unit.interner.boolTy(0);
                        } else if (left.isIntegral() and right.isIntegral()) {
                            if (left.isSigned() == right.isSigned()) {
                                if (left.rank() <= right.rank()) {
                                    break :blk self.unit.interner.boolTy(0);
                                } else {
                                    break :blk self.unit.interner.boolTy(0);
                                }
                            } else if (!left.isSigned() and left.rank() >= right.rank()) {
                                break :blk self.unit.interner.boolTy(0);
                            } else if (!right.isSigned() and right.rank() >= left.rank()) {
                                break :blk self.unit.interner.boolTy(0);
                            } else if (left.isSigned()) {
                                break :blk self.unit.interner.boolTy(0);
                            } else if (right.isSigned()) {
                                break :blk self.unit.interner.boolTy(0);
                            }
                        } else if (left.kind == .bool and right.kind == .bool) {
                            break :blk self.unit.interner.boolTy(0);
                        }

                        std.debug.panic("unahnadled type left: {s}, right: {s}", .{
                            self.unit.interner.printTyToStr(left, self.unit.allocator),
                            self.unit.interner.printTyToStr(right, self.unit.allocator),
                        });
                    },
                    .double_ampersand, .double_pipe => {
                        if (left == right)
                            break :blk self.unit.interner.boolTy(0)
                        else if (left.isScalar() and right.isScalar())
                            break :blk self.unit.interner.boolTy(0);

                        std.debug.panic("unahnadled type left: {s}, right: {s}", .{
                            self.unit.interner.printTyToStr(left, self.unit.allocator),
                            self.unit.interner.printTyToStr(right, self.unit.allocator),
                        });
                    },
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
                    => {
                        break :blk left;
                    },
                    else => std.debug.panic("Unexpected op {}", .{op}),
                }
            },
            .unary_prefix_operator => blk: {
                const expr_type = try self.checkNode(node.data.two.a, null);
                const op: TokenKind = @enumFromInt(node.data.four.c);
                switch (op) {
                    .plusplus,
                    .minusminus,
                    .plus,
                    .minus,
                    .exclamation,
                    .tilde,
                    => {
                        if (expr_type.isScalar()) break :blk expr_type;

                        std.debug.panic("Can't apply operator to type \x1b[1m{s}\x1b[0m", .{
                            self.unit.interner.printTyToStr(expr_type, self.unit.allocator),
                        });
                    },
                    .star => {
                        if (expr_type.kind != .pointer) {
                            std.debug.panic("Tried to dereference a non-pointer type: \x1b[1m{s}\x1b[0m", .{
                                self.unit.interner.printTyToStr(expr_type, self.unit.allocator),
                            });
                        }

                        break :blk expr_type.kind.pointer.base;
                    },
                    .ampersand => {
                        break :blk self.unit.interner.pointerTy(expr_type, 0);
                    },
                    .sizeof => {
                        break :blk self.unit.interner.longlongTy(true, 0);
                    },
                    else => unreachable,
                }
            },
            .unary_suffix_operator => blk: {
                const expr_type = try self.checkNode(node.data.two.a, null);
                const op: TokenKind = @enumFromInt(node.data.four.d);
                switch (op) {
                    .plusplus,
                    .minusminus,
                    => {
                        if (expr_type.isScalar()) break :blk expr_type;

                        std.debug.panic("Can't apply operator to type \x1b[1m{s}\x1b[0m", .{
                            self.unit.interner.printTyToStr(expr_type, self.unit.allocator),
                        });
                    },
                    else => unreachable,
                }
            },
            .invoke => blk: {
                const expr_type = try self.checkNode(node.data.two.a, null);
                if (expr_type.kind != .func) {
                    std.debug.panic("Tried to call a value that's not a function (type \x1b[31m{s}\x1b[0m)", .{
                        self.unit.interner.printTyToStr(expr_type, self.unit.allocator),
                    });
                }
                break :blk expr_type.kind.func.ret_ty;
            },
            .invoke_one_arg => blk: {
                const expr_type = try self.checkNode(node.data.two.a, null);
                const arg_type = try self.checkNode(node.data.two.b, null);
                if (expr_type.kind != .func) {
                    std.debug.panic("Tried to call a value that's not a function (type \x1b[31m{s}\x1b[0m)", .{
                        self.unit.interner.printTyToStr(expr_type, self.unit.allocator),
                    });
                }
                const param_types = self.unit.interner.getMultiTypes(expr_type.kind.func.params);

                if (param_types.len == 1) {
                    if (self.implicitlyCast(arg_type, param_types[0])) |ty| {
                        _ = ty;
                    } else {
                        std.debug.panic("Function expected type \x1b[32m{s}\x1b[0m for first argument but got \x1b[32m{s}\x1b[0m instead", .{
                            self.unit.interner.printTyToStr(param_types[0], self.unit.allocator),
                            self.unit.interner.printTyToStr(arg_type, self.unit.allocator),
                        });
                    }
                } else if (param_types.len == 0 and (expr_type.flags & TypeFlags.Variadic) > 0) {} else {
                    std.debug.panic("Function expected {} arguments but received 1", .{param_types.len});
                }

                break :blk expr_type.kind.func.ret_ty;
            },
            .invoke_args => blk: {
                const expr_type = try self.checkNode(Node.absoluteIndex(nidx, node.data.four.a), null);
                const arg_start = node.data.two.b;
                const arg_count = node.data.four.b;

                if (expr_type.kind != .func) {
                    std.debug.panic("Tried to call a value that's not a function (type \x1b[31m{s}\x1b[0m)", .{
                        self.unit.interner.printTyToStr(expr_type, self.unit.allocator),
                    });
                }
                const param_types = self.unit.interner.getMultiTypes(expr_type.kind.func.params);

                // ex == pr !var 1
                // ex == pr var  1
                // ex < pr !var  0
                // ex < pr var   1
                // ex > pr !var  0
                // ex > pr var   0

                var index = arg_start;
                const end_index = index + param_types.len;
                const arg_end_index = index + arg_count;
                var count: u32 = 0;

                while (index < end_index) : ({
                    index += 1;
                    count += 1;
                }) {
                    const node_index = self.unit.node_ranges.items[index];
                    const provided_type = try self.checkNode(node_index, param_types[count]);

                    if (self.implicitlyCast(provided_type, param_types[count])) |ty| {
                        _ = ty;
                    } else {
                        std.debug.panic("Function expected type \x1b[32m{s}\x1b[0m for argument {} but got \x1b[32m{s}\x1b[0m instead", .{
                            self.unit.interner.printTyToStr(param_types[count], self.unit.allocator),
                            count,
                            self.unit.interner.printTyToStr(provided_type, self.unit.allocator),
                        });
                    }
                }

                if (param_types.len == arg_count) {} else if (param_types.len < arg_count and (expr_type.flags & TypeFlags.Variadic) > 0) {} else {
                    std.debug.panic("Function expected {} arguments but received {}", .{ param_types.len, arg_count });
                }

                // va arg types
                while (index < arg_end_index) : ({
                    index += 1;
                    count += 1;
                }) {
                    const node_index = self.unit.node_ranges.items[index];
                    const provided_type = try self.checkNode(node_index, null);
                    _ = provided_type;
                }

                break :blk expr_type.kind.func.ret_ty;
            },
            .index => blk: {
                const ptr_type = try self.checkNode(node.data.two.a, null);
                const expr_type = try self.checkNode(node.data.two.b, null);

                if (self.implicitlyCast(expr_type, self.unit.interner.longlongTy(true, 0))) |_| {} else {
                    std.debug.panic("Expected integral type for pointer index but found \x1b[32m{s}\x1b[0m", .{
                        self.unit.interner.printTyToStr(expr_type, self.unit.allocator),
                    });
                }

                switch (ptr_type.kind) {
                    .pointer => |ptr| {
                        break :blk ptr.base;
                    },
                    .array => |ptr| {
                        break :blk ptr.base;
                    },
                    .array_unsized => |ptr| {
                        break :blk ptr.base;
                    },
                    else => {
                        std.debug.panic("Expected indexable type such as pointer or array but found \x1b[32m{s}\x1b[0m", .{
                            self.unit.interner.printTyToStr(ptr_type, self.unit.allocator),
                        });
                    },
                }
            },
            .cast => blk: {
                const to_type = try self.checkNodeType(node.data.two.a);
                const expr_type = try self.checkNode(Node.absoluteIndex(nidx, node.data.four.c), to_type);

                if (self.implicitlyCast(expr_type, to_type)) |ty| {
                    break :blk ty;
                }

                if (expr_type.kind == .pointer and to_type.kind == .pointer) {
                    break :blk to_type;
                } else if (expr_type.isIntegral() and to_type.kind == .pointer) {
                    break :blk to_type;
                } else if (expr_type.kind == .pointer and to_type.isIntegral()) {
                    break :blk to_type;
                }

                std.debug.panic("Can't cast value of type \x1b[1m{s}\x1b[0m to type \x1b[1m{s}\x1b[0m", .{
                    self.unit.interner.printTyToStr(expr_type, self.unit.allocator),
                    self.unit.interner.printTyToStr(to_type, self.unit.allocator),
                });
            },
            .if_statement => {
                const condition_type = try self.checkNode(node.data.two.a, null);
                if (!condition_type.isScalar()) {
                    std.debug.panic("Expected integral type in if condition!", .{});
                }

                _ = try self.checkNode(node.data.two.b, null);

                return self.unit.interner.voidTy();
            },
            .if_statement_no_body => {
                const condition_type = try self.checkNode(node.data.two.a, null);
                if (!condition_type.isScalar()) {
                    std.debug.panic("Expected integral type in if condition!", .{});
                }

                return self.unit.interner.voidTy();
            },
            .if_statement_else => {
                const condition_type = try self.checkNode(node.data.two.a, null);
                if (!condition_type.isScalar()) {
                    std.debug.panic("Expected integral type in if condition!", .{});
                }

                _ = try self.checkNode(Node.absoluteIndex(nidx, node.data.four.c), null);
                _ = try self.checkNode(Node.absoluteIndex(nidx, node.data.four.d), null);

                return self.unit.interner.voidTy();
            },
            .if_statement_no_body_else => {
                const condition_type = try self.checkNode(node.data.two.a, null);
                if (!condition_type.isScalar()) {
                    std.debug.panic("Expected integral type in if condition!", .{});
                }
                _ = try self.checkNode(node.data.two.b, null);

                return self.unit.interner.voidTy();
            },
            .while_loop => {
                const condition_type = try self.checkNode(node.data.two.a, null);
                if (!condition_type.isScalar()) {
                    std.debug.panic("Expected integral type in while condition!", .{});
                }

                _ = try self.checkNode(node.data.two.b, null);

                return self.unit.interner.voidTy();
            },
            .while_loop_empty => {
                const condition_type = try self.checkNode(node.data.two.a, null);
                if (!condition_type.isScalar()) {
                    std.debug.panic("Expected integral type in while condition!", .{});
                }

                return self.unit.interner.voidTy();
            },
            .do_while_loop => {
                const condition_type = try self.checkNode(node.data.two.a, null);
                if (!condition_type.isScalar()) {
                    std.debug.panic("Expected integral type in do while condition!", .{});
                }

                _ = try self.checkNode(node.data.two.b, null);

                return self.unit.interner.voidTy();
            },
            .do_while_loop_empty => {
                const condition_type = try self.checkNode(node.data.two.a, null);
                if (!condition_type.isScalar()) {
                    std.debug.panic("Expected integral type in do while condition!", .{});
                }

                return self.unit.interner.voidTy();
            },
            .for_loop => {
                _ = try self.checkNode(Node.absoluteIndex(nidx, node.data.four.a), null);

                const condition_type = try self.checkNode(Node.absoluteIndex(nidx, node.data.four.b), null);
                if (!condition_type.isScalar()) {
                    std.debug.panic("Expected integral type in for condition!", .{});
                }
                _ = try self.checkNode(Node.absoluteIndex(nidx, node.data.four.c), null);
                _ = try self.checkNode(Node.absoluteIndex(nidx, node.data.four.d), null);

                return self.unit.interner.voidTy();
            },
            .for_loop_inc => {
                _ = try self.checkNode(Node.absoluteIndex(nidx, node.data.four.a), null);

                const condition_type = try self.checkNode(Node.absoluteIndex(nidx, node.data.four.b), null);
                if (!condition_type.isScalar()) {
                    std.debug.panic("Expected integral type in for condition!", .{});
                }
                _ = try self.checkNode(Node.absoluteIndex(nidx, node.data.four.d), null);

                return self.unit.interner.voidTy();
            },
            .for_loop_empty => {
                _ = try self.checkNode(Node.absoluteIndex(nidx, node.data.four.a), null);

                const condition_type = try self.checkNode(Node.absoluteIndex(nidx, node.data.four.b), null);
                if (!condition_type.isScalar()) {
                    std.debug.panic("Expected integral type in for condition!", .{});
                }
                _ = try self.checkNode(Node.absoluteIndex(nidx, node.data.four.c), null);

                return self.unit.interner.voidTy();
            },
            .for_loop_empty_inc => {
                _ = try self.checkNode(Node.absoluteIndex(nidx, node.data.four.a), null);

                const condition_type = try self.checkNode(Node.absoluteIndex(nidx, node.data.four.b), null);
                if (!condition_type.isScalar()) {
                    std.debug.panic("Expected integral type in for condition!", .{});
                }
                _ = try self.checkNode(Node.absoluteIndex(nidx, node.data.four.d), null);

                return self.unit.interner.voidTy();
            },
            .switch_case => {
                const condition_type = try self.checkNode(node.data.two.a, null);
                if (!condition_type.isIntegral()) {
                    std.debug.panic("Expected integral type or enumeration in switch!", .{});
                }
                _ = try self.checkNode(node.data.two.b, null);

                return self.unit.interner.voidTy();
            },
            .case => {
                const condition_type = try self.checkNode(node.data.two.a, null);
                if (!condition_type.isIntegral()) {
                    std.debug.panic("Expected integral type or enumeration in switch!", .{});
                }
                _ = try self.checkNode(node.data.two.b, null);

                return self.unit.interner.voidTy();
            },
            .default => {
                _ = try self.checkNode(node.data.two.a, null);
                return self.unit.interner.voidTy();
            },
            .label => {
                _ = try self.checkNode(node.data.two.b, null);
                return self.unit.interner.voidTy();
            },
            .continue_statement,
            .break_statement,
            .return_statement,
            .empty_statement,
            => return self.unit.interner.voidTy(),
            .return_statement_value => {
                _ = try self.checkNode(node.data.as(.two).a, null);
                return self.unit.interner.voidTy();
            },
            .identifier => blk: {
                const ident_index = node.data.two.a;
                const ident_str = self.unit.identifierAt(@bitCast(ident_index));
                if (BuiltinsType.map.get(ident_str)) |ty| {
                    break :blk try ty(self.unit);
                }
                const sym = self.unit.symbol_table.searchSymbol(ident_str) orelse {
                    std.debug.panic("Undefined identifier \x1b[1m{s}\x1b[0m", .{ident_str});
                };

                self.unit.node_to_node.put(nidx, sym.nidx) catch @panic("OOM");

                const declared_ty = self.unit.declared_type.get(sym.nidx) orelse {
                    std.debug.panic("Symbol \x1b[1m{s}\x1b[0m does not have type declared with it (this is probably compiler bug)", .{ident_str});
                };

                break :blk declared_ty;
            },
            .declaration => {
                var next_index = node.data.as(.two).a;
                const count = node.data.as(.four).c;
                const storage = node.data.as(.eight).g;

                const old_sc = self.storage_class;
                defer self.storage_class = old_sc;
                self.storage_class = storage;

                const end_index = next_index + count;
                while (next_index != end_index) : (next_index += 1) {
                    const node_index = self.unit.node_ranges.items[next_index];
                    _ = try self.checkNode(node_index, null);
                }

                return self.unit.interner.voidTy();
            },
            .var_declaration_init => {
                const ident_index = node.data.as(.two).a;
                const type_index = Node.absoluteIndex(nidx, node.data.as(.four).c);
                const init_index = Node.absoluteIndex(nidx, node.data.as(.four).d);
                const ident_str = self.unit.identifierAt(@bitCast(ident_index));

                self.unit.symbol_table.putSymbol(ident_str, .{ .nidx = nidx });
                const var_type = try self.checkNodeType(type_index);
                const init_type = try self.checkNode(init_index, var_type);
                if (init_type.isIncomplete()) {
                    std.debug.panic("Variable has incomplete type! (Attempted to use struct or union before it was declared)", .{});
                }

                if (self.implicitlyCast(init_type, var_type)) |new_type| {
                    self.unit.declared_type.put(nidx, new_type) catch @panic("OOM");

                    return self.unit.interner.voidTy();
                }

                std.debug.panic("Can't assign type \x1b[32m{s}\x1b[0m to variable type \x1b[32m{s}\x1b[0m", .{
                    self.unit.interner.printTyToStr(init_type, self.unit.allocator),
                    self.unit.interner.printTyToStr(var_type, self.unit.allocator),
                });
            },
            .var_declaration => {
                const ident_index = node.data.as(.two).a;
                const type_index = Node.absoluteIndex(nidx, node.data.as(.four).c);
                const ident_str = self.unit.identifierAt(@bitCast(ident_index));

                self.unit.symbol_table.putSymbol(ident_str, .{ .nidx = nidx });
                const var_type = try self.checkNodeType(type_index);
                self.unit.declared_type.put(nidx, var_type) catch @panic("OOM");

                if ((self.storage_class.? & StorageClass.typedef) == 0 and var_type.isIncomplete()) {
                    std.debug.panic("Variable has incomplete type! (Attempted to use struct or union before it was declared) {s}: {s}", .{
                        ident_str,
                        self.unit.interner.printTyToStr(var_type, self.unit.allocator),
                    });
                }

                return self.unit.interner.voidTy();
            },
            .compound_empty => self.unit.interner.voidTy(),
            .compound_one => {
                const item_index = node.data.as(.two).a;
                _ = try self.checkNode(item_index, null);
                return self.unit.interner.voidTy();
            },
            .compound => {
                var range_start = node.data.as(.two).a;
                const range_end = range_start + node.data.as(.two).b;
                while (range_start < range_end) : (range_start += 1) {
                    _ = try self.checkNode(self.unit.node_ranges.items[range_start], null);
                }
                return self.unit.interner.voidTy();
            },
            .function_declaration => {
                const ident_index = node.data.as(.two).a;
                const ident_str = self.unit.identifierAt(@bitCast(ident_index));
                self.unit.symbol_table.putSymbol(ident_str, .{ .nidx = nidx });
                const type_index = Node.absoluteIndex(nidx, node.data.as(.four).c);
                const fn_type = try self.checkNodeType(type_index);
                self.unit.declared_type.put(nidx, fn_type) catch @panic("OOM");

                return self.unit.interner.voidTy();
            },
            .function_declaration_body => {
                const ident_index = node.data.as(.two).a;
                const ident_str = self.unit.identifierAt(@bitCast(ident_index));

                self.unit.symbol_table.putSymbol(ident_str, .{ .nidx = nidx });
                self.unit.symbol_table.pushScope();
                const type_index = Node.absoluteIndex(nidx, node.data.as(.four).c);
                const fn_type = try self.checkNodeType(type_index);

                const body_index = Node.absoluteIndex(nidx, node.data.as(.four).d);
                _ = try self.checkNode(body_index, null);
                self.unit.symbol_table.popScope();

                self.unit.declared_type.put(nidx, fn_type) catch @panic("OOM");

                return self.unit.interner.voidTy();
            },
            .char_type, .signed_char_type, .unsigned_char_type, .short_type, .signed_short_type, .unsigned_short_type, .int_type, .signed_int_type, .unsigned_int_type, .long_type, .signed_long_type, .unsigned_long_type, .long_long_type, .signed_long_long_type, .unsigned_long_long_type, .float_type, .double_type, .long_double_type, .bool_type, .unsigned, .signed, .void_type, .pointer, .array_type, .array_type_fixed, .function_type, .function_type_one_parameter, .function_type_parameter, .parameter, .parameter_ident, .@"struct", .@"union", .struct_ident, .union_ident, .@"enum", .enum_ident, .struct_forward, .union_forward, .enum_forward, .type_name => blk: {
                const ty = try self.checkNodeType(nidx);
                break :blk self.unit.interner.typeTy(ty, 0);
            },
            else => {
                std.log.warn("Skipping node {} of kind: {}", .{ nidx, self.unit.nodes.items[nidx].kind });
                return self.unit.interner.voidTy();
            },
        };

        try self.put(nidx, result);

        return result;
    }

    pub fn checkNodeType(self: *Self, nidx: NodeIndex) !Type {
        return try self.checkNodeTypeImpl(nidx);
    }

    pub fn checkNodeTypeImpl(self: *Self, nidx: NodeIndex) std.mem.Allocator.Error!Type {
        const node = &self.unit.nodes.items[nidx];
        const result: Type = switch (node.kind) {
            .char_type => self.unit.interner.charTy(false, node.data.eight.h),
            .signed_char_type => self.unit.interner.charTy(true, node.data.eight.h),
            .unsigned_char_type => self.unit.interner.charTy(false, node.data.eight.h),
            .short_type => self.unit.interner.shortTy(true, node.data.eight.h),
            .signed_short_type => self.unit.interner.shortTy(true, node.data.eight.h),
            .unsigned_short_type => self.unit.interner.shortTy(false, node.data.eight.h),
            .int_type => self.unit.interner.intTy(true, node.data.eight.h),
            .signed_int_type => self.unit.interner.intTy(true, node.data.eight.h),
            .unsigned_int_type => self.unit.interner.intTy(false, node.data.eight.h),
            .long_type => self.unit.interner.longTy(true, node.data.eight.h),
            .signed_long_type => self.unit.interner.longTy(true, node.data.eight.h),
            .unsigned_long_type => self.unit.interner.longTy(false, node.data.eight.h),
            .long_long_type => self.unit.interner.longTy(true, node.data.eight.h),
            .signed_long_long_type => self.unit.interner.longlongTy(true, node.data.eight.h),
            .unsigned_long_long_type => self.unit.interner.longlongTy(false, node.data.eight.h),
            .float_type => self.unit.interner.floatTy(node.data.eight.h),
            .double_type => self.unit.interner.doubleTy(node.data.eight.h),
            .long_double_type => self.unit.interner.longdoubleTy(node.data.eight.h),
            .bool_type => self.unit.interner.boolTy(node.data.eight.h),
            .unsigned => self.unit.interner.intTy(false, node.data.eight.h),
            .signed => self.unit.interner.intTy(true, node.data.eight.h),

            .void_type => self.unit.interner.voidTy(),

            .pointer => blk: {
                const qual: TypeQualifier.Type = @intCast(node.data.two.b);
                const base = try self.checkNodeTypeImpl(node.data.two.a);
                break :blk self.unit.interner.pointerTy(base, qual);
            },

            .array_type => blk: {
                const base = try self.checkNodeTypeImpl(node.data.two.a);
                if (base.isIncomplete()) {
                    std.debug.panic("Array has incomplete type! (tried to reference undefined struct or union)", .{});
                }
                const ty = self.unit.interner.arrayUnsizedTy(base, node.data.eight.h);
                break :blk ty;
            },
            .array_type_fixed => blk: {
                const size_index = Node.absoluteIndex(nidx, node.data.four.c);
                var evaluator = SimpleEvaluator.init(self.unit);
                const size_value = try evaluator.evalNode(size_index);
                if (size_value != .int_value) {
                    std.debug.panic("Expected an integer value for array size!", .{});
                }

                const base = try self.checkNodeTypeImpl(node.data.two.a);
                if (base.isIncomplete()) {
                    std.debug.panic("Array has incomplete type! (tried to reference undefined struct or union)", .{});
                }

                const ty = self.unit.interner.arrayTy(base, size_value.int_value, node.data.eight.h);

                break :blk ty;
            },

            .function_type => blk: {
                const ret_ty_index = Node.absoluteIndex(nidx, node.data.four.a);
                const ret_ty = try self.checkNodeTypeImpl(ret_ty_index);
                if (ret_ty.isIncomplete()) {
                    std.debug.panic("Function has incomplete return type! (tried to reference undefined struct or union)", .{});
                }
                const fun_ty = self.unit.interner.funcTyNoParams(ret_ty, false);
                break :blk fun_ty;
            },
            .function_type_one_parameter => blk: {
                const ret_ty_index = Node.absoluteIndex(nidx, node.data.four.a);
                const param_index = Node.absoluteIndex(nidx, node.data.four.b);
                const ret_ty = try self.checkNodeTypeImpl(ret_ty_index);
                if (ret_ty.isIncomplete()) {
                    std.debug.panic("Function has incomplete return type! (tried to reference undefined struct or union)", .{});
                }

                var param_buf = [1]Type{undefined};
                const param_tys, const variadic = if (self.unit.nodes.items[param_index].kind == .parameter_ellipsis) .{
                    param_buf[0..0],
                    true,
                } else .{
                    blk1: {
                        param_buf[0] = try self.checkNodeTypeImpl(param_index);
                        if (param_buf[0].isIncomplete()) {
                            std.debug.panic("Function has incomplete type for argument 0! (tried to reference undefined struct or union)", .{});
                        }
                        break :blk1 param_buf[0..1];
                    },
                    false,
                };

                const fun_ty = self.unit.interner.funcTy(param_tys, ret_ty, variadic);
                break :blk fun_ty;
            },
            .function_type_parameter => blk: {
                const ret_ty_index = Node.absoluteIndex(nidx, node.data.four.a);
                var param_index = node.data.two.b;
                const param_count = node.data.four.b;
                const ret_ty = try self.checkNodeTypeImpl(ret_ty_index);
                const end_index = param_index + param_count;
                if (ret_ty.isIncomplete()) {
                    std.debug.panic("Function has incomplete return type! (tried to reference undefined struct or union)", .{});
                }

                var variadic = false;
                var params = std.ArrayList(Type).init(self.unit.allocator);
                while (param_index < end_index) : (param_index += 1) {
                    const node_index = self.unit.node_ranges.items[param_index];
                    if (self.unit.nodes.items[node_index].kind == .parameter_ellipsis) {
                        variadic = true;
                        break;
                    }
                    const param_ty = try self.checkNodeTypeImpl(node_index);
                    if (param_ty.isIncomplete()) {
                        std.debug.panic("Function has incomplete type for argument {}! (tried to reference undefined struct or union)", .{params.items.len});
                    }
                    try params.append(param_ty);
                }

                const fun_ty = self.unit.interner.funcTy(params.items, ret_ty, variadic);
                break :blk fun_ty;
            },
            .parameter => blk: {
                const ty_index = Node.absoluteIndex(nidx, node.data.four.a);
                break :blk try self.checkNodeTypeImpl(ty_index);
            },
            .parameter_ident => blk: {
                const ty_index = Node.absoluteIndex(nidx, node.data.four.a);
                const ident_index = node.data.two.b;
                const ident_str = self.unit.identifierAt(@bitCast(ident_index));
                self.unit.symbol_table.putSymbol(ident_str, .{ .nidx = nidx });
                const param_ty = try self.checkNodeTypeImpl(ty_index);
                try self.unit.declared_type.put(nidx, param_ty);
                // std.heap.MemoryPool(Type).
                break :blk param_ty;
            },

            .@"struct", .@"union" => blk: {
                const member_range = node.data;
                const members = try self.checkStructured(nidx, member_range.two.a, member_range.two.a + member_range.two.b);
                defer members.deinit();

                const result_ty = if (node.kind == .@"struct")
                    self.unit.interner.unnamedStructTy(nidx, members.items, 0)
                else
                    self.unit.interner.unnamedUnionTy(nidx, members.items, 0);
                try self.unit.declared_type.put(nidx, result_ty);
                break :blk result_ty;
            },

            .struct_ident, .union_ident => blk: {
                // insert into sym table first for self references
                const ident_str = self.unit.identifierAt(@bitCast(node.data.two.a));
                self.unit.symbol_table.putTypeSymbol(ident_str, .{ .nidx = nidx });

                std.debug.assert(self.unit.nodes.items[nidx - 1].kind == .range);
                const member_range = self.unit.nodes.items[nidx - 1].data;

                const members = try self.checkStructured(nidx, member_range.two.a, member_range.two.a + member_range.two.b);
                defer members.deinit();

                const result_ty = if (node.kind == .struct_ident)
                    self.unit.interner.structTy(nidx, members.items, 0)
                else
                    self.unit.interner.unionTy(nidx, members.items, 0);

                try self.unit.declared_type.put(nidx, result_ty);

                break :blk result_ty;
            },
            .@"enum" => blk: {
                var index = node.data.two.a;
                const end_index = index + node.data.two.b;

                var index_value: u64 = 0;
                while (index < end_index) : (index += 1) {
                    const member_index = self.unit.node_ranges.items[index];
                    const member = &self.unit.nodes.items[member_index];
                    switch (member.kind) {
                        .enum_member => {
                            const ident_str = self.unit.identifierAt(@bitCast(member.data.two.a));
                            self.unit.enum_constants.put(ident_str, index_value) catch @panic("OOM");
                        },
                        .enum_member_value => {
                            const ident_str = self.unit.identifierAt(@bitCast(member.data.two.a));
                            var eval = SimpleEvaluator.init(self.unit);
                            const this_index_value = try eval.evalNode(member.data.two.b);
                            self.unit.enum_constants.put(ident_str, this_index_value.int_value) catch @panic("OOM");
                            index_value = this_index_value.int_value;
                        },
                        else => @panic("compiler bug"),
                    }

                    index_value += 1;
                }

                const result_ty = self.unit.interner.unnamedEnumTy(nidx, 0);
                try self.unit.declared_type.put(nidx, result_ty);

                break :blk result_ty;
            },
            .enum_ident => blk: {
                std.debug.assert(self.unit.nodes.items[nidx - 1].kind == .range);
                const member_range = self.unit.nodes.items[nidx - 1].data;
                var index = member_range.two.a;
                const end_index = index + member_range.two.b;

                var index_value: u64 = 0;
                while (index < end_index) : (index += 1) {
                    const member_index = self.unit.node_ranges.items[index];
                    const member = &self.unit.nodes.items[member_index];
                    switch (member.kind) {
                        .enum_member => {
                            const ident_str = self.unit.identifierAt(@bitCast(node.data.two.a));
                            self.unit.enum_constants.put(ident_str, index_value) catch @panic("OOM");
                        },
                        .enum_member_value => {
                            const ident_str = self.unit.identifierAt(@bitCast(node.data.two.a));
                            var eval = SimpleEvaluator.init(self.unit);
                            const this_index_value = try eval.evalNode(node.data.two.b);
                            self.unit.enum_constants.put(ident_str, this_index_value.int_value) catch @panic("OOM");
                            index_value = this_index_value.int_value;
                        },
                        else => @panic("compiler bug"),
                    }

                    index_value += 1;
                }

                const result_ty = self.unit.interner.enumTy(nidx, 0);

                try self.unit.declared_type.put(nidx, result_ty);
                const ident_str = self.unit.identifierAt(@bitCast(node.data.two.a));
                self.unit.symbol_table.putTypeSymbol(ident_str, .{ .nidx = nidx });

                break :blk result_ty;
            },
            .struct_forward => blk: {
                const ident_str = self.unit.identifierAt(@bitCast(node.data.two.a));
                const sym = self.unit.symbol_table.searchTypeSymbol(ident_str) orelse {
                    // std.debug.panic("Struct \x1b[1m'{s}'\x1b[0m is not defined", .{ident_str});
                    break :blk self.unit.interner.tbdNidx(@bitCast(node.data.two.a));
                };

                try self.unit.node_to_node.put(nidx, sym.nidx);
                return self.unit.declared_type.get(sym.nidx) orelse self.unit.interner.tbdNidx(@bitCast(node.data.two.a));
            },
            .union_forward => blk: {
                const ident_str = self.unit.identifierAt(@bitCast(node.data.two.a));
                const sym = self.unit.symbol_table.searchTypeSymbol(ident_str) orelse {
                    // std.debug.panic("Union \x1b[1m'{s}'\x1b[0m is not defined", .{ident_str});
                    break :blk self.unit.interner.tbdNidx(@bitCast(node.data.two.a));
                };

                try self.unit.node_to_node.put(nidx, sym.nidx);
                return self.unit.declared_type.get(sym.nidx) orelse self.unit.interner.tbdNidx(@bitCast(node.data.two.a));
            },
            .enum_forward => {
                const ident_str = self.unit.identifierAt(@bitCast(node.data.two.a));
                const sym = self.unit.symbol_table.searchTypeSymbol(ident_str) orelse {
                    std.debug.panic("Union \x1b[1m'{s}'\x1b[0m is not defined", .{ident_str});
                };

                try self.unit.node_to_node.put(nidx, sym.nidx);
                return self.unit.declared_type.get(sym.nidx).?;
            },
            .type_name => blk: {
                const ident_index = node.data.two.a;
                const ident_str = self.unit.identifierAt(@bitCast(ident_index));
                if (BuiltinsType.map.get(ident_str)) |ty| {
                    break :blk try ty(self.unit);
                }
                const sym = self.unit.symbol_table.searchSymbol(ident_str) orelse {
                    self.unit.printTokenInfo(@bitCast(ident_index));
                    std.debug.panic("Undefined identifier \x1b[1m{s}\x1b[0m", .{ident_str});
                };

                self.unit.node_to_node.put(nidx, sym.nidx) catch @panic("OOM");

                const declared_ty = self.unit.declared_type.get(sym.nidx) orelse {
                    std.debug.panic("Symbol \x1b[1m{s}\x1b[0m does not have type declared with it (this is probably compiler bug)", .{ident_str});
                };

                break :blk declared_ty;
            },

            else => {
                std.log.warn("Skipping node {} of type {}", .{ nidx, self.unit.nodes.items[nidx].kind });
                return self.unit.interner.voidTy();
            },
        };

        self.unit.node_to_type.put(nidx, result) catch @panic("OOM");

        return result;
    }

    pub fn checkStructured(self: *Self, nidx: NodeIndex, start_index: NodeIndex, end_index: NodeIndex) !std.ArrayList(Type) {
        var index = start_index;

        var members = std.ArrayList(Type).init(self.unit.allocator);
        var field_mapping = std.StringHashMap(u32).init(self.unit.allocator);

        var field_count: u32 = 0;
        while (index < end_index) : ({
            index += 1;
            field_count += 1;
        }) {
            const member_node_index = self.unit.node_ranges.items[index];
            const member_node = &self.unit.nodes.items[member_node_index];
            const member_ty = try self.checkNodeType(Node.absoluteIndex(member_node_index, member_node.data.four.c));
            if (member_ty.isIncomplete()) {
                std.debug.panic("Struct/Union member has incomplete type! (tried to reference undefined struct or union)", .{});
            }

            switch (member_node.kind) {
                .member => {
                    try members.append(member_ty);
                },
                .member_bitfield => {
                    const bitfield_index = Node.absoluteIndex(member_node_index, member_node.data.four.d);
                    var eval = SimpleEvaluator.init(self.unit);
                    const bitfield_value = try eval.evalNode(bitfield_index);

                    try members.append(self.unit.interner.createOrGetTy(.{
                        .bitfield = .{
                            .base = member_ty,
                            .bits = @truncate(bitfield_value.int_value),
                        },
                    }, 0));
                },
                .member_ident => {
                    const member_ident_str = self.unit.identifierAt(@bitCast(member_node.data.two.a));
                    try members.append(member_ty);
                    try field_mapping.put(member_ident_str, field_count);
                },
                .member_ident_bitfield => {
                    const member_ident_str = self.unit.identifierAt(@bitCast(member_node.data.two.a));
                    const bitfield_index = Node.absoluteIndex(member_node_index, member_node.data.four.d);
                    var eval = SimpleEvaluator.init(self.unit);
                    const bitfield_value = try eval.evalNode(bitfield_index);

                    try members.append(self.unit.interner.createOrGetTy(.{
                        .bitfield = .{
                            .base = member_ty,
                            .bits = @truncate(bitfield_value.int_value),
                        },
                    }, 0));
                    try field_mapping.put(member_ident_str, field_count);
                },
                else => unreachable,
            }
        }

        try self.unit.field_map.put(nidx, field_mapping);

        return members;
    }

    pub fn implicitlyCast(self: *Self, from: Type, to: Type) ?Type {
        if (from == to)
            return from
        else if (to == self.unit.interner.anyTy())
            return from
        else if (from == self.unit.interner.anyTy())
            return to
        else if (to == self.unit.interner.anyKindTy(std.meta.activeTag(from.kind)))
            return from
        else if (from == self.unit.interner.anyKindTy(std.meta.activeTag(to.kind)))
            return to
        else if (from.kind == .pointer and to.kind == .pointer) {
            if (from.kind.pointer.base == to.kind.pointer.base) {
                // f 0b0000 0b0000 0b0001 0b0001
                // c 0b0000 0b0001 0b0000 0b0001
                //   ---------------------------
                // f 0b0000 0b0000 0b0001 0b0001
                // c 0b1111 0b1110 0b1111 0b1110
                //   ---------------------------
                //   0b0000 0b0000 0b0001 0b0000
                if (from.qualifiers & ~to.qualifiers == 0) {
                    return to;
                }
            }
            std.log.warn("Tried to implicitly cast from \x1b[32m{s}\x1b[0m to \x1b[32m{s}\x1b[0m", .{
                self.unit.interner.printTyToStr(from, self.unit.allocator),
                self.unit.interner.printTyToStr(to, self.unit.allocator),
            });
            return to;
        } else if (from.kind == .array and to.kind == .pointer) {
            if (self.implicitlyCast(from.kind.array.base, to.kind.pointer.base) != null) {
                if (from.qualifiers & ~to.qualifiers == 0) {
                    return to;
                }
            }
        } else if (from.kind == .array_unsized and to.kind == .pointer) {
            if (self.implicitlyCast(from.kind.array_unsized.base, to.kind.pointer.base) != null) {
                if (from.qualifiers & ~to.qualifiers == 0) {
                    return to;
                }
            }
        } else if (from.isArithmetic() and to.isArithmetic())
            return to
        else if (from.kind == .@"struct" and to.kind == .@"struct") {
            return if (@as(u32, @bitCast(from.kind.@"struct".nidx)) == @as(u32, @bitCast(to.kind.@"struct".nidx))) from else null;
        } else if (from.kind == .@"union" and to.kind == .@"union") {
            return if (@as(u32, @bitCast(from.kind.@"union".nidx)) == @as(u32, @bitCast(to.kind.@"union".nidx))) from else null;
        } else if (from.kind == .unnamed_struct and to.kind == .unnamed_struct) {
            return if (from.kind.unnamed_struct.nidx == to.kind.unnamed_struct.nidx) from else null;
        } else if (from.kind == .unnamed_union and to.kind == .unnamed_union) {
            return if (from.kind.unnamed_union.nidx == to.kind.unnamed_union.nidx) from else null;
        } else if (from.kind == .array and to.kind == .array_unsized) {
            return if (from.kind.array.base == to.kind.array_unsized.base) from else null;
        }

        return null;
    }

    // pub fn intRank(a: Type, b: Type) u32 {
    //     std.debug.assert(a.isIntegral() and b.isIntegral());
    //     const ai: u32 = @intFromEnum(a.*);
    //     const bi: u32 = @intFromEnum(a.*);
    //     return ai - bi;
    // }

    inline fn put(self: *Self, nidx: NodeIndex, ty: Type) !void {
        try self.unit.node_to_type.put(nidx, ty);
    }
};

pub const ConstValue = union(enum) {
    int_value: u64,

    pub fn boolValue(self: @This()) bool {
        return switch (self) {
            .int_value => |i| i > 0,
        };
    }
};

pub const SimpleEvaluator = struct {
    unit: *Unit,

    const Self = @This();
    pub fn init(unit: *Unit) Self {
        return .{
            .unit = unit,
        };
    }

    pub fn commonValue(self: *Self, left: ConstValue, right: ConstValue) struct { ConstValue, ConstValue } {
        _ = self;
        return .{ left, right };
    }

    pub fn evalNode(self: *Self, nidx: NodeIndex) !ConstValue {
        const node = self.unit.nodes.items[nidx];
        switch (node.kind) {
            .int_literal => return .{ .int_value = node.data.long },
            .unsigned_int_literal => return .{ .int_value = node.data.long },
            .long_literal => return .{ .int_value = node.data.long },
            .unsigned_long_literal => return .{ .int_value = node.data.long },
            .long_long_literal => return .{ .int_value = node.data.long },
            .unsigned_long_long_literal => return .{ .int_value = node.data.long },
            .size_literal => return .{ .int_value = node.data.long },
            .unsigned_size_literal => return .{ .int_value = node.data.long },
            .identifier => {
                const ident_str = self.unit.identifierAt(@bitCast(node.data.two.a));
                if (self.unit.enum_constants.get(ident_str)) |cv| {
                    return .{ .int_value = cv };
                }

                std.debug.panic("Invalid constant value {}", .{nidx});
            },

            .if_statement_else => {
                const condition = try self.evalNode(node.data.two.a);
                if (condition.boolValue()) {
                    return try self.evalNode(Node.absoluteIndex(nidx, node.data.four.c));
                } else {
                    return try self.evalNode(Node.absoluteIndex(nidx, node.data.four.d));
                }
            },

            .binary_lr_operator => {
                const op: TokenKind = @enumFromInt(node.data.four.d);
                const left = try self.evalNode(node.data.two.a);
                switch (op) {
                    .plus,
                    .minus,
                    .star,
                    .slash,
                    .percent,
                    .ampersand,
                    .pipe,
                    .carot,
                    .bit_left_shift,
                    .bit_right_shift,
                    .equality,
                    .nequality,
                    => {
                        const right = try self.evalNode(node.data.two.a);
                        const lval, const rval = self.commonValue(left, right);
                        const result: ConstValue = switch (lval) {
                            .int_value => .{
                                .int_value = switch (op) {
                                    .plus => lval.int_value + rval.int_value,
                                    .minus => lval.int_value - rval.int_value,
                                    .star => lval.int_value * rval.int_value,
                                    .slash => lval.int_value / rval.int_value,
                                    .percent => lval.int_value % rval.int_value,
                                    .ampersand => lval.int_value & rval.int_value,
                                    .pipe => lval.int_value | rval.int_value,
                                    .carot => lval.int_value ^ rval.int_value,
                                    .bit_left_shift => @shlWithOverflow(lval.int_value, @as(u6, @intCast(rval.int_value)))[0],
                                    .bit_right_shift => @shrExact(lval.int_value, @as(u6, @intCast(rval.int_value))),
                                    .equality => if (lval.int_value == rval.int_value) 1 else 0,
                                    .nequality => if (lval.int_value != rval.int_value) 1 else 0,
                                    else => @panic("invalid operator"),
                                },
                            },
                        };

                        return result;
                    },
                    else => {
                        std.debug.panic("Invalid operator for constant value {}", .{nidx});
                    },
                }
            },
            else => std.debug.panic("Invalid constant value {}", .{nidx}),
        }
    }
};
