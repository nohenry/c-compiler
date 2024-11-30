const std = @import("std");
const Unit = @import("unit.zig").Unit;
const NodeIndex = @import("parser.zig").NodeIndex;
const Node = @import("parser.zig").Node;
const Type = @import("types.zig").Type;
const TokenKind = @import("tokenizer.zig").TokenKind;
const TokenIndex = @import("tokenizer.zig").TokenIndex;
const TypeQualifier = @import("parser.zig").TypeQualifier;

pub const TypeChecker = struct {
    unit: *Unit,

    const Self = @This();

    pub fn init(unit: *Unit) Self {
        return .{
            .unit = unit,
        };
    }

    pub fn checkNode(self: *Self, nidx: NodeIndex) !Type {
        const node = &self.unit.nodes.items[nidx];
        const result: Type = switch (node.kind) {
            .empty => return self.unit.interner.voidTy(),
            .char_literal => return self.unit.interner.charTy(true, 0),
            .float_literal => return self.unit.interner.floatTy(0),
            .double_literal => return self.unit.interner.doubleTy(0),
            .int_literal => return self.unit.interner.intTy(true, 0),
            .unsigned_int_literal => return self.unit.interner.intTy(false, 0),
            .long_literal => return self.unit.interner.longTy(true, 0),
            .unsigned_long_literal => return self.unit.interner.longTy(false, 0),
            .long_long_literal => return self.unit.interner.longlongTy(true, 0),
            .unsigned_long_long_literal => return self.unit.interner.longlongTy(false, 0),
            .binary_lr_operator => blk: {
                const op: TokenKind = @enumFromInt(node.data.four.d);
                const left = try self.checkNode(node.data.two.a);
                if (op == .dot) {
                    if (!left.isStructured()) {
                        std.debug.panic("Left side is not a structure type!!!!", .{});
                    }

                    const right_node = &self.unit.nodes.items[Node.absoluteIndex(nidx, node.data.four.c)];
                    if (right_node.kind != .identifier) { // TODO: what if field is typename??
                        std.debug.panic("Right side is not a identifier!!!!", .{});
                    }

                    const field_str = self.unit.identifierAt(@bitCast(right_node.data.two.a));

                    if (left.getStructureField(field_str)) |field_ty| {
                        break :blk field_ty;
                    }

                    std.debug.panic("Structured type does not contain field \x1b[1m{s}\x1b[0m", .{field_str});

                    break :blk;
                }
                const right = try self.checkNode(Node.absoluteIndex(nidx, node.data.four.c));

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
                    else => std.debug.panic("Unexpected op {}", .{op}),
                }
            },
            .return_statement_value => blk: {
                _ = try self.checkNode(node.data.as(.two).a);
                break :blk self.unit.interner.voidTy();
            },
            .identifier => blk: {
                const ident_index = node.data.two.a;
                const ident_str = self.unit.identifierAt(@bitCast(ident_index));
                const sym = self.unit.symbol_table.searchSymbol(ident_str) orelse {
                    std.debug.panic("Undefined identifier \x1b[1m{s}\x1b[0m", .{ident_str});
                };

                self.unit.node_to_node.put(nidx, sym.nidx) catch @panic("OOM");

                const declared_ty = self.unit.declared_type.get(sym.nidx) orelse {
                    std.debug.panic("Symbol \x1b[1m{s}\x1b0m does not have type declared with it (this is probably compiler bug)", .{ident_str});
                };

                break :blk declared_ty;
            },
            .declaration => blk: {
                var next_index = node.data.as(.two).a;
                const count = node.data.as(.four).c;
                const storage = node.data.as(.eight).g;
                _ = storage;

                const end_index = next_index + count;
                while (next_index != end_index) : (next_index += 1) {
                    const node_index = self.unit.node_ranges.items[next_index];
                    _ = try self.checkNode(node_index);
                }

                break :blk self.unit.interner.voidTy();
            },
            .var_declaration_init => blk: {
                const ident_index = node.data.as(.two).a;
                const type_index = Node.absoluteIndex(nidx, node.data.as(.four).c);
                const init_index = Node.absoluteIndex(nidx, node.data.as(.four).d);
                const ident_str = self.unit.identifierAt(@bitCast(ident_index));

                self.unit.symbol_table.putSymbol(ident_str, .{ .nidx = nidx });
                const var_type = try self.checkNodeType(type_index);
                self.unit.declared_type.put(nidx, var_type) catch @panic("OOM");
                const init_type = try self.checkNode(init_index);
                if (canImplicitlyCast(init_type, var_type)) {
                    break :blk self.unit.interner.voidTy();
                }

                std.debug.panic("Can't assign type {s} to variable type {s}", .{
                    self.unit.interner.printTyToStr(init_type, self.unit.allocator),
                    self.unit.interner.printTyToStr(var_type, self.unit.allocator),
                });
            },
            .var_declaration => blk: {
                const ident_index = node.data.as(.two).a;
                const type_index = Node.absoluteIndex(nidx, node.data.as(.four).c);
                const ident_str = self.unit.identifierAt(@bitCast(ident_index));

                self.unit.symbol_table.putSymbol(ident_str, .{ .nidx = nidx });
                const var_type = try self.checkNodeType(type_index);
                self.unit.declared_type.put(nidx, var_type) catch @panic("OOM");

                break :blk self.unit.interner.voidTy();
            },
            .compound_empty => self.unit.interner.voidTy(),
            .compound_one => blk: {
                const item_index = node.data.as(.two).a;
                _ = try self.checkNode(item_index);
                break :blk self.unit.interner.voidTy();
            },
            .compound => blk: {
                var range_start = node.data.as(.two).a;
                const range_end = range_start + node.data.as(.two).b;
                while (range_start < range_end) : (range_start += 1) {
                    _ = try self.checkNode(self.unit.node_ranges.items[range_start]);
                }
                break :blk self.unit.interner.voidTy();
            },
            .function_declaration => blk: {
                const ident_index = node.data.as(.two).a;
                const ident_str = self.unit.identifierAt(@bitCast(ident_index));
                self.unit.symbol_table.putSymbol(ident_str, .{ .nidx = nidx });
                const type_index = Node.absoluteIndex(nidx, node.data.as(.four).c);
                const fn_type = try self.checkNodeType(type_index);
                self.unit.declared_type.put(nidx, fn_type) catch @panic("OOM");

                break :blk self.unit.interner.voidTy();
            },
            .function_declaration_body => blk: {
                const ident_index = node.data.as(.two).a;
                const ident_str = self.unit.identifierAt(@bitCast(ident_index));

                self.unit.symbol_table.putSymbol(ident_str, .{ .nidx = nidx });
                self.unit.symbol_table.pushScope();
                const type_index = Node.absoluteIndex(nidx, node.data.as(.four).c);
                const fn_type = try self.checkNodeType(type_index);

                const body_index = Node.absoluteIndex(nidx, node.data.as(.four).d);
                _ = try self.checkNode(body_index);
                self.unit.symbol_table.popScope();

                self.unit.declared_type.put(nidx, fn_type) catch @panic("OOM");

                break :blk self.unit.interner.voidTy();
            },
            else => {
                std.log.warn("Skipping node {}", .{nidx});
                return self.unit.interner.voidTy();
            },
        };

        try self.put(nidx, result);

        return result;
    }

    pub fn checkNodeType(self: *Self, nidx: NodeIndex) !Type {
        return try self.checkNodeTypeImpl(nidx, null);
    }

    pub fn checkNodeTypeImpl(self: *Self, nidx: NodeIndex, last_type: ?*Type) !Type {
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
                var ty = self.unit.interner.pointerTy(self.unit.interner.voidTy(), qual);
                if (last_type) |last| {
                    ty = self.unit.interner.rebasePointer(ty, last.*);
                }
                const base = try self.checkNodeTypeImpl(node.data.two.a, &ty);
                if (base.kind == .pointer or base.kind == .func) {
                    break :blk base;
                } else {
                    ty, _ = self.unit.interner.rebasePointerRecursive(ty, base);
                    break :blk ty;
                }
            },

            .function_type => blk: {
                const ret_ty_index = Node.absoluteIndex(nidx, node.data.four.a);
                const ret_ty = try self.checkNodeTypeImpl(ret_ty_index, null);
                var fun_ty = self.unit.interner.funcTyNoParams(ret_ty);
                if (last_type) |last| {
                    const new_type, const ret_ty_base = self.unit.interner.rebasePointerRecursive(ret_ty, self.unit.interner.voidTy());

                    fun_ty = self.unit.interner.funcTyNoParams(
                        self.unit.interner.rebasePointerRecursive(last.*, ret_ty_base)[0],
                    );

                    break :blk self.unit.interner.rebasePointerRecursive(new_type, fun_ty)[0];
                } else if (ret_ty.kind == .pointer) {
                    const new_type, const ret_ty_base = self.unit.interner.rebasePointerRecursive(ret_ty, self.unit.interner.voidTy());

                    fun_ty = self.unit.interner.funcTyNoParams(ret_ty_base);
                    break :blk self.unit.interner.rebasePointerRecursive(new_type, fun_ty)[0];
                }

                break :blk fun_ty;
            },
            .function_type_one_parameter => blk: {
                const ret_ty_index = Node.absoluteIndex(nidx, node.data.four.a);
                const param_index = Node.absoluteIndex(nidx, node.data.four.b);
                const ret_ty = try self.checkNodeTypeImpl(ret_ty_index, null);
                const param_ty = try self.checkNodeTypeImpl(param_index, null);
                var fun_ty = self.unit.interner.funcTy(&.{param_ty}, ret_ty);

                if (last_type) |last| {
                    const new_type, const ret_ty_base = self.unit.interner.rebasePointerRecursive(ret_ty, self.unit.interner.voidTy());

                    fun_ty = self.unit.interner.funcTy(
                        &.{param_ty},
                        self.unit.interner.rebasePointerRecursive(last.*, ret_ty_base)[0],
                    );

                    break :blk self.unit.interner.rebasePointerRecursive(new_type, fun_ty)[0];
                } else if (ret_ty.kind == .pointer) {
                    const new_type, const ret_ty_base = self.unit.interner.rebasePointerRecursive(ret_ty, self.unit.interner.voidTy());

                    fun_ty = self.unit.interner.funcTy(
                        &.{param_ty},
                        ret_ty_base,
                    );
                    break :blk self.unit.interner.rebasePointerRecursive(new_type, fun_ty)[0];
                }

                break :blk fun_ty;
            },
            .function_type_parameter => blk: {
                const ret_ty_index = Node.absoluteIndex(nidx, node.data.four.a);
                var param_index = node.data.two.b;
                const param_count = node.data.four.b;
                const ret_ty = try self.checkNodeTypeImpl(ret_ty_index, null);
                const end_index = param_index + param_count;

                var params = std.ArrayList(Type).init(self.unit.allocator);
                while (param_index < end_index) : (param_index += 1) {
                    const node_index = self.unit.node_ranges.items[param_index];
                    try params.append(try self.checkNodeTypeImpl(node_index, null));
                }

                var fun_ty = self.unit.interner.funcTy(params.items, ret_ty);

                if (last_type) |last| {
                    const new_type, const ret_ty_base = self.unit.interner.rebasePointerRecursive(ret_ty, self.unit.interner.voidTy());

                    fun_ty = self.unit.interner.funcTy(
                        params.items,
                        self.unit.interner.rebasePointerRecursive(last.*, ret_ty_base)[0],
                    );

                    break :blk self.unit.interner.rebasePointerRecursive(new_type, fun_ty)[0];
                } else if (ret_ty.kind == .pointer) {
                    const new_type, const ret_ty_base = self.unit.interner.rebasePointerRecursive(ret_ty, self.unit.interner.voidTy());

                    fun_ty = self.unit.interner.funcTy(
                        params.items,
                        ret_ty_base,
                    );
                    break :blk self.unit.interner.rebasePointerRecursive(new_type, fun_ty)[0];
                }

                break :blk fun_ty;
            },
            .parameter => blk: {
                const ty_index = Node.absoluteIndex(nidx, node.data.four.a);
                break :blk try self.checkNodeTypeImpl(ty_index, null);
            },
            .parameter_ident => blk: {
                const ty_index = Node.absoluteIndex(nidx, node.data.four.a);
                const ident_index = node.data.two.b;
                const ident_str = self.unit.identifierAt(@bitCast(ident_index));
                self.unit.symbol_table.putSymbol(ident_str, .{ .nidx = nidx });
                break :blk try self.checkNodeTypeImpl(ty_index, null);
            },

            else => {
                std.log.warn("Skipping node {}", .{nidx});
                return self.unit.interner.voidTy();
            },
        };

        self.unit.node_to_type.put(nidx, result) catch @panic("OOM");

        return result;
    }

    pub fn canImplicitlyCast(from: Type, to: Type) bool {
        if (from.isScalar() and to.isScalar()) return true else if (from.kind == .@"struct" and to.kind == .@"struct") {
            return @as(u32, @bitCast(from.kind.@"struct".name)) == @as(u32, @bitCast(to.kind.@"struct".name));
        } else if (from.kind == .@"union" and to.kind == .@"union") {
            return @as(u32, @bitCast(from.kind.@"union".name)) == @as(u32, @bitCast(to.kind.@"union".name));
        } else if (from.kind == .unnamed_struct and to.kind == .unnamed_struct) {
            return from.kind.unnamed_struct.nidx == to.kind.unnamed_struct.nidx;
        } else if (from.kind == .unnamed_union and to.kind == .unnamed_union) {
            return from.kind.unnamed_union.nidx == to.kind.unnamed_union.nidx;
        }
        return false;
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
