const std = @import("std");
const Unit = @import("unit.zig").Unit;
const NodeIndex = @import("parser.zig").NodeIndex;
const Node = @import("parser.zig").Node;
const Type = @import("types.zig").Type;
const TokenKind = @import("tokenizer.zig").TokenKind;

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
            .char_literal => return self.unit.interner.charTy(true),
            .float_literal => return self.unit.interner.floatTy(),
            .double_literal => return self.unit.interner.doubleTy(),
            .int_literal => return self.unit.interner.intTy(true),
            .unsigned_int_literal => return self.unit.interner.intTy(false),
            .long_literal => return self.unit.interner.longTy(true),
            .unsigned_long_literal => return self.unit.interner.longTy(false),
            .long_long_literal => return self.unit.interner.longlongTy(true),
            .unsigned_long_long_literal => return self.unit.interner.longlongTy(false),
            .binary_lr_operator => blk: {
                const left = try self.checkNode(node.data.two.a);
                const right = try self.checkNode(Node.absoluteIndex(nidx, node.data.four.c));
                const op: TokenKind = @enumFromInt(node.data.four.d);

                switch (op) {
                    .plus,
                    .minus,
                    .star,
                    .slash,
                    .gt,
                    .gte,
                    .lt,
                    .lte,
                    .equality,
                    .nequality,
                    .ampersand,
                    .pipe,
                    .carot,
                    => {
                        std.log.info("Types: left: {s}, right: {s}", .{
                            self.unit.interner.printTyToStr(left, self.unit.allocator),
                            self.unit.interner.printTyToStr(right, self.unit.allocator),
                        });
                        if (left == right) break :blk left;

                        if (left.* == .longdouble or right.* == .longdouble) {
                            break :blk self.unit.interner.longdoubleTy();
                        } else if (left.* == .double or right.* == .double) {
                            break :blk self.unit.interner.doubleTy();
                        } else if (left.* == .float or right.* == .float) {
                            break :blk self.unit.interner.floatTy();
                        } else if (left.isIntegral() and right.isIntegral()) {
                            if (left.isSigned() == right.isSigned()) {
                                // const rank = intRank(left, right);
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
                                const new_type = self.unit.interner.createOrGetTy(left.toSigned(false));
                                break :blk new_type;
                            } else if (right.isSigned()) {
                                const new_type = self.unit.interner.createOrGetTy(right.toSigned(false));
                                break :blk new_type;
                            }
                        }
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
            .function_declaration_body => blk: {
                const body_index = Node.absoluteIndex(nidx, node.data.as(.four).d);
                _ = try self.checkNode(body_index);
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
