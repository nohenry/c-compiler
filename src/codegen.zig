const std = @import("std");
const Unit = @import("unit.zig").Unit;
const NodeIndex = @import("parser.zig").NodeIndex;
const Node = @import("parser.zig").Node;
const TokenKind = @import("tokenizer.zig").TokenKind;
const Type = @import("types.zig").Type;

const Symbol = struct {
    value: []const u8,
    prefix: bool = true,
    address: u32,
    ty: MachoSymbolType,
};

const Value = union(enum) {
    inst: u32,
    immediate: u64,
    register: u5,
    memory: u32,
    memory_offset: struct {
        offset: u32,
        register: u5,
    },

    pub fn getValue(self: @This(), cg: *CodeGenerator, ty: Type, result_ty: ?Type, imm: bool, mem: bool) !Value {
        switch (self) {
            .immediate => |x| {
                if (imm) {
                    return self;
                } else if (cg.availableRegister()) |reg| {
                    const layout = cg.computeLayout(ty);
                    const inst = CodeGenerator.dataProcImm(.move, .{
                        .sfopchwimm16rd = .{
                            .rd = reg,
                            .imm = @truncate(x),
                            .hw = 0,
                            .opc = 0b10,
                            .sf = layout.size >= 8,
                        },
                    });
                    _ = try cg.writeInst(inst);
                    cg.useRegister(reg);
                    return .{ .register = reg };
                }
                _ = mem;

                @panic("shouldbnt be here");
            },
            .memory_offset => |mo| {
                if (cg.availableRegister()) |reg| {
                    if (result_ty) |rty| {
                        _ = try cg.writeCastLoad(ty, rty, reg, mo.register, mo.offset);
                    } else {
                        _ = try cg.writeInst(CodeGenerator.formLoad(ty, reg, mo.register, mo.offset));
                    }
                    // _ = try cg.writeInst(CodeGenerator.loadStore(.load_store_register, 0, 0b10, 0, 0, .{
                    //     .szVopcimm12rnrt = .{
                    //         .rt = reg,
                    //         .rn = mo.register,
                    //         .imm = @truncate(mo.offset),
                    //         .opc = 0b01,
                    //         .V = false,
                    //         .sz = 0b11,
                    //     },
                    // }));
                    cg.useRegister(reg);
                    return .{ .register = reg };
                }
                @panic("shouldbnt be here");
            },
            .register => |reg| {
                if (result_ty) |rty| {
                    _ = try cg.writeCast(ty, rty, reg, reg);
                }
                return self;
            },
            else => return self,
        }
    }
};

pub const Layout = struct {
    size: u32,
    alignment: u32,

    pub fn max(self: @This()) u32 {
        return @max(self.size, self.alignment);
    }
};

pub const FunctionContext = struct {
    local_offsets: std.AutoHashMap(NodeIndex, u32),
    next_offset: u32 = 0,
    return_type: Type,
};

pub const StructLayout = struct {
    offsets: std.AutoHashMap(u32, u32),
};

pub const CodeGenerator = struct {
    allocator: std.mem.Allocator,
    unit: *Unit,

    builder: MachoBuilder,

    segment_offset: u32,
    symtab_offset: u32,
    text_section_descriptor_offset: u32,
    inst_count: u32 = 0,

    symbols: std.MultiArrayList(Symbol),
    structs: std.AutoHashMap(NodeIndex, StructLayout),
    expects_reg: ?u5 = null,

    fctx: ?FunctionContext = null,

    registers: [32]bool = [1]bool{false} ** 32,

    const Self = @This();

    pub fn init(unit: *Unit) !Self {
        var builder = try MachoBuilder.init(unit.allocator);

        const load_segment = try builder.addLoadSegment(1);
        const symtab_segment = try builder.addLoadSymTab();
        const section = builder.ptrToSection(load_segment, 0);
        @memset(&section.sectname, 0);
        @memset(&section.segname, 0);
        @memcpy(section.sectname[0.."__text".len], "__text");
        @memcpy(section.segname[0.."__TEXT".len], "__TEXT");

        section.addr = 0;
        section.size = 5;
        section.@"align" = 2;
        section.reloff = 0;
        section.nreloc = 0;
        section.flags = std.macho.S_REGULAR | std.macho.S_ATTR_PURE_INSTRUCTIONS | std.macho.S_ATTR_SOME_INSTRUCTIONS;
        section.reserved1 = 0;
        section.reserved2 = 0;
        section.reserved3 = 0;

        // const offset = try builder.writeSlice(&[_]u8{1, 2, 3, 4, 5});
        // section.offset = offset;
        // builder.ptrTo(std.macho.segment_command_64, load_segment).fileoff = offset;
        // builder.ptrTo(std.macho.segment_command_64, load_segment).filesize = 5;

        const sym_entry_offset = builder.currentOffset();
        builder.ptrTo(std.macho.symtab_command, symtab_segment).symoff = sym_entry_offset;
        // _ = try builder.writeSymbolEntry(symtab_segment, 0x6969, .{});
        const text_section_data = builder.currentOffset();
        builder.ptrTo(std.macho.segment_command_64, load_segment).fileoff = text_section_data;
        section.offset = text_section_data;
        try builder.alignForward(@alignOf(u32));

        return .{
            .allocator = unit.allocator,
            .unit = unit,
            .builder = builder,

            .structs = .init(unit.allocator),
            .segment_offset = load_segment,
            .symtab_offset = symtab_segment,
            .text_section_descriptor_offset = 0,

            .symbols = .{},
        };
    }

    inline fn nodeptr(self: *const Self, nidx: NodeIndex) *Node {
        return &self.unit.nodes.items[nidx];
    }

    pub fn availableRegister(self: *const Self) ?u5 {
        var result: u5 = 0;
        while (result < self.registers.len) : (result += 1) {
            if (!self.registers[result]) return @truncate(result);
        }
        return null;
    }

    pub fn useRegister(self: *Self, register: u5) void {
        self.registers[register] = true;
    }

    pub fn genCompound(self: *Self, nidx: NodeIndex) !void {
        const node = self.nodeptr(nidx);
        switch (node.kind) {
            .compound_empty => {},
            .compound_one => {
                const item_index = node.data.as(.two).a;
                _ = try self.genNode(item_index);
            },
            .compound => {
                var range_start = node.data.as(.two).a;
                const range_end = range_start + node.data.as(.two).b;
                while (range_start < range_end) : (range_start += 1) {
                    _ = try self.genNode(self.unit.node_ranges.items[range_start]);
                }
            },
            else => @panic(""),
        }
    }

    pub fn genNode(self: *Self, nidx: NodeIndex) (std.mem.Allocator.Error)!Value {
        const node = self.nodeptr(nidx);

        switch (node.kind) {
            .declaration => {
                var next_index = node.data.as(.two).a;
                const count = node.data.as(.four).c;
                const storage = node.data.as(.eight).g;
                _ = storage;

                const end_index = next_index + count;
                while (next_index != end_index) : (next_index += 1) {
                    const node_index = self.unit.node_ranges.items[next_index];
                    _ = try self.genNode(node_index);
                }
            },
            .var_declaration_init => {
                const decl_ty = self.unit.declared_type.get(nidx).?;
                const layout = self.computeLayout(decl_ty);

                self.fctx.?.next_offset = std.mem.alignForward(u32, self.fctx.?.next_offset, layout.alignment);
                const offset = self.fctx.?.next_offset;
                self.fctx.?.next_offset += layout.max();
                try self.fctx.?.local_offsets.put(nidx, offset);
                std.log.info("Var layout: {}", .{layout});

                const init_index = Node.absoluteIndex(nidx, node.data.as(.four).d);
                const init_value = try self.genNodeExpr(init_index, decl_ty);
                const value = try init_value.getValue(self, decl_ty, decl_ty, false, false);

                _ = try self.writeInst(formStore(decl_ty, value.register, SP, offset));
            },
            .var_declaration => {
                const decl_ty = self.unit.declared_type.get(nidx).?;
                const layout = self.computeLayout(decl_ty);

                self.fctx.?.next_offset = std.mem.alignForward(u32, self.fctx.?.next_offset, layout.alignment);
                const offset = self.fctx.?.next_offset;
                self.fctx.?.next_offset += layout.max();
                try self.fctx.?.local_offsets.put(nidx, offset);
                std.log.info("Var layout: {}", .{layout});
            },
            .return_statement => {
                _ = try self.writeInst(brExcSys(.un_cond_br_reg, .{
                    .opcop23Rnop4 = .{
                        .rn = LR,
                        .op3 = 0,
                        .opc = 0b10,
                    },
                }));
            },
            .return_statement_value => {
                self.expects_reg = X0;
                defer self.expects_reg = null;
                self.useRegister(X0);
                const value = try self.genNodeExpr(node.data.as(.two).a, self.fctx.?.return_type);
                const value_ty = self.unit.node_to_type.get(node.data.as(.two).a).?;

                const rrvalue = try value.getValue(self, value_ty, self.fctx.?.return_type, true, false);
                if (rrvalue == .immediate) {
                    _ = try self.writeInst(CodeGenerator.dataProcImm(.move, .{
                        .sfopchwimm16rd = .{
                            .rd = X0,
                            .imm = @truncate(rrvalue.immediate),
                            .hw = 0,
                            .opc = 0b10,
                            .sf = true,
                        },
                    }));
                } else if (rrvalue == .register and rrvalue.register != X0) {
                    _ = try self.writeInst(dataProcReg(.logical_shift, .{
                        .sfopcshNRmimmRnRd = .{
                            .rd = X0,
                            .rn = X31,
                            .imm = 0,
                            .rm = rrvalue.register,
                            .N = false,
                            .sh = 0,
                            .opc = 0b01,
                        },
                    }));
                }

                // _ = try self.writeInst(brExcSys(.un_cond_br_reg, .{
                //     .opcop23Rnop4 = .{
                //         .rn = LR,
                //         .op3 = 0,
                //         .opc = 0b10,
                //     },
                // }));
            },
            .function_declaration_body => {
                const ident_index = node.data.as(.two).a;
                try self.symbols.append(self.unit.allocator, Symbol{
                    .value = self.unit.identifierAt(@bitCast(ident_index)),
                    .address = self.inst_count * 4,
                    .ty = MachoSymbolType{
                        .external = true,
                        .ty = .section,
                    },
                });

                const old_registers = self.registers;
                defer self.registers = old_registers;

                const fn_type = self.unit.declared_type.get(nidx).?;
                const old_fctx = self.fctx;
                defer self.fctx = old_fctx;
                self.fctx = FunctionContext{
                    .local_offsets = .init(self.allocator),
                    .return_type = fn_type.kind.func.ret_ty,
                };

                // Subtract from SP for stack frame (placeholder)
                const sub_sp_index = try self.writeInst(0);

                const fn_param_types = self.unit.interner.getMultiTypes(fn_type.kind.func.params);

                const fn_type_nidx = Node.absoluteIndex(nidx, node.data.four.c);
                const fn_type_node = &self.unit.nodes.items[fn_type_nidx];
                var i: u32 = 0;
                switch (fn_type_node.kind) {
                    .function_type => {},
                    .function_type_one_parameter => {
                        const param_nidx = Node.absoluteIndex(fn_type_nidx, fn_type_node.data.four.b);
                        const param = &self.unit.nodes.items[param_nidx];
                        switch (param.kind) {
                            .parameter => {},
                            .parameter_ident => {
                                const layout = self.computeLayout(fn_param_types[i]);
                                try self.fctx.?.local_offsets.put(param_nidx, 0);
                                _ = try self.writeInst(formStore(
                                    fn_param_types[i],
                                    0,
                                    SP,
                                    self.fctx.?.next_offset,
                                ));
                                self.fctx.?.next_offset += layout.max();
                            },
                            .parameter_ellipsis => {},
                            else => @panic("compielr bug"),
                        }
                    },
                    .function_type_parameter => {
                        var index = fn_type_node.data.two.b;
                        const end_index = index + fn_type_node.data.four.b;

                        while (index < end_index) : (index += 1) {
                            const param_nidx = self.unit.node_ranges.items[index];
                            const param = &self.unit.nodes.items[param_nidx];
                            switch (param.kind) {
                                .parameter => {},
                                .parameter_ident => {
                                    const layout = self.computeLayout(fn_param_types[i]);
                                    self.fctx.?.next_offset = std.mem.alignForward(u32, self.fctx.?.next_offset, layout.alignment);
                                    try self.fctx.?.local_offsets.put(param_nidx, self.fctx.?.next_offset);

                                    // Parameters after w7 are already on stack
                                    if (i < X8) {
                                        _ = try self.writeInst(formStore(
                                            fn_param_types[i],
                                            @truncate(i),
                                            SP,
                                            self.fctx.?.next_offset,
                                        ));
                                    }

                                    self.fctx.?.next_offset += layout.max();
                                },
                                .parameter_ellipsis => {},
                                else => @panic("compielr bug"),
                            }

                            i += 1;
                        }
                    },
                    else => @panic("compiler bug"),
                }

                // for (fn_param_types) |param_type| {
                // }

                const body_index = Node.absoluteIndex(nidx, node.data.as(.four).d);
                try self.genCompound(body_index);

                const stack_frame_size = std.mem.alignForward(u32, self.fctx.?.next_offset, 16);
                if (stack_frame_size == 0) {
                    // repurpose this for ret
                    _ = try self.writeInstAt(brExcSys(.un_cond_br_reg, .{
                        .opcop23Rnop4 = .{
                            .rn = LR,
                            .op3 = 0,
                            .opc = 0b10,
                        },
                    }), sub_sp_index);
                } else {
                    _ = try self.writeInstAt(dataProcImm(.add_sub, .{
                        .sfopSshimm12rnrd = .{
                            .op = 1,
                            .S = false,
                            .imm = @truncate(stack_frame_size),
                            .rn = SP,
                            .rd = SP,
                        },
                    }), sub_sp_index);

                    _ = try self.writeInst(dataProcImm(.add_sub, .{
                        .sfopSshimm12rnrd = .{
                            .op = 0,
                            .S = false,
                            .imm = @truncate(stack_frame_size),
                            .rn = SP,
                            .rd = SP,
                        },
                    }));

                    _ = try self.writeInst(brExcSys(.un_cond_br_reg, .{
                        .opcop23Rnop4 = .{
                            .rn = LR,
                            .op3 = 0,
                            .opc = 0b10,
                        },
                    }));
                }
            },
            else => {
                const old_registers = self.registers;
                const result = try self.genNodeExpr(nidx, null);
                self.registers = old_registers;
                return result;
            },
        }

        return .{ .inst = 0 };

        // var stack_index: u32 = 0;

        // const sub_inst_index = try self.writeInst(dataProcImm(.add_sub, .{
        //     .sfopSshimm12rnrd = .{
        //         .op = 1,
        //         .S = false,
        //         .imm = undefined,
        //         .rn = SP,
        //         .rd = SP,
        //     },
        // }));

        // stack_index += 1;
        // self.builder.ptrTo(OPS, sub_inst_index).sfopSshimm12rnrd.imm = @truncate(stack_index);

        // _ = try self.writeInst(dataProcImm(.add_sub, .{
        //     .sfopSshimm12rnrd = .{
        //         .op = 0,
        //         .S = false,
        //         .imm = @truncate(stack_index),
        //         .rn = SP,
        //         .rd = SP,
        //     },
        // }));

        // _ = try self.writeInst(brExcSys(.un_cond_br_reg, .{
        //     .opcop23Rnop4 = .{
        //         .rn = LR,
        //         .op3 = 0,
        //         .opc = 0b10,
        //     },
        // }));

        // _ = try self.writeInst(0xD503201F);
        // try self.symbols.append(self.unit.allocator, Symbol{
        //     .value = "_main",
        //     .address = 0,
        //     .ty = MachoSymbolType{
        //         .external = true,
        //         .ty = .section,
        //     },
        // });
    }

    pub fn genNodeExpr(self: *Self, nidx: NodeIndex, result_ty: ?Type) !Value {
        _ = result_ty;
        const node = &self.unit.nodes.items[nidx];
        switch (node.kind) {
            .char_literal => return .{ .immediate = node.data.two.a },
            .int_literal => return .{ .immediate = node.data.two.a },
            .unsigned_int_literal => return .{ .immediate = node.data.two.a },
            .long_literal => return .{ .immediate = node.data.two.a },
            .unsigned_long_literal => return .{ .immediate = node.data.two.a },
            .long_long_literal => return .{ .immediate = node.data.two.a },
            .unsigned_long_long_literal => return .{ .immediate = node.data.two.a },
            .size_literal => return .{ .immediate = node.data.two.a },
            .unsigned_size_literal => return .{ .immediate = node.data.two.a },
            .identifier => {
                // const refed_nidx = self.unit.node_to_node.get(nidx).?;
                // const local = self.fctx.?.local_offsets.get(refed_nidx).?;

                // const ident_ty = self.unit.node_to_type.get(nidx).?;
                // const out_reg = self.availableRegister().?;
                // self.useRegister(out_reg);

                const referred_nidx = self.unit.node_to_node.get(nidx).?;
                if (self.fctx.?.local_offsets.get(referred_nidx)) |vr| {
                    return .{
                        .memory_offset = .{
                            .offset = vr,
                            .register = SP,
                        },
                    };
                }

                @panic("unhandled lvalue");

                // if (result_ty) |rty| {
                //     try self.writeCastLoad(ident_ty, rty, out_reg, SP, local);
                // } else {
                //     _ = try self.writeInst(formLoad(ident_ty, out_reg, SP, local));
                // }
                // return .{ .register = out_reg };
            },
            .binary_lr_operator => blk: {
                const old_expects_reg = self.expects_reg;
                defer self.expects_reg = old_expects_reg;
                self.expects_reg = null;

                const op: TokenKind = @enumFromInt(node.data.as(.four).d);

                switch (op) {
                    .dot => {
                        const ltype = self.unit.node_to_type.get(node.data.two.a).?;
                        _ = self.computeLayout(ltype);
                        const lvalue = try self.genNodeLvalue(node.data.two.a);
                        // const refed_node = self.unit.node_to_node.get(Node.absoluteIndex(nidx, node.data.four.c)).?;

                        const right_node = &self.unit.nodes.items[Node.absoluteIndex(nidx, node.data.four.c)];
                        const field_str = self.unit.identifierAt(@bitCast(right_node.data.two.a));

                        const strc_nidx = ltype.getStructureIndex();
                        const field_offset = switch (ltype.kind) {
                            .@"struct", .unnamed_struct => blk2: {
                                const field_map = self.unit.field_map.getPtr(strc_nidx).?;
                                const field_index = field_map.get(field_str).?;
                                const strct = self.structs.getPtr(strc_nidx).?;
                                const field_offset = strct.offsets.get(field_index).?;

                                break :blk2 field_offset;
                            },
                            .@"union", .unnamed_union => blk2: {
                                break :blk2 0;
                            },
                            else => unreachable,
                        };

                        // const out_reg = self.availableRegister().?;
                        // self.useRegister(out_reg);
                        // _ = try self.writeInst(formLoad(ident_ty, out_reg, SP, local));

                        // return .{ .register = out_reg };

                        switch (lvalue) {
                            .memory_offset => |mo| {
                                return .{
                                    .memory_offset = .{
                                        .offset = mo.offset + field_offset,
                                        .register = mo.register,
                                    },
                                };
                            },
                            else => @panic(""),
                        }
                    },
                    .assignment => {
                        const rvalue = try self.genNode(Node.absoluteIndex(nidx, node.data.as(.four).c));
                        const lvalue = try self.genNodeLvalue(node.data.as(.two).a);
                        switch (lvalue) {
                            .memory_offset => |mo| {
                                const ty = self.unit.node_to_type.get(node.data.two.a).?;
                                const rrvalue = try rvalue.getValue(self, ty, null, false, false);
                                std.log.info("Assign {} {} {}", .{ mo, mo.offset / 4, rvalue });
                                if (ty.isIntegral()) {
                                    _ = try self.writeInst(formStore(ty, rrvalue.register, mo.register, mo.offset));
                                } else {
                                    @panic("todo");
                                }
                            },
                            else => @panic("Unhandled lavelu assignment"),
                        }

                        break :blk;
                    },
                    else => {},
                }

                const ty = self.unit.node_to_type.get(nidx).?;
                const layout = self.computeLayout(ty);
                const rvalue = try self.genNodeExpr(Node.absoluteIndex(nidx, node.data.as(.four).c), ty);
                const lvalue = try self.genNodeExpr(node.data.as(.two).a, ty);

                const rty = self.unit.node_to_type.get(Node.absoluteIndex(nidx, node.data.four.c)).?;
                const lty = self.unit.node_to_type.get(node.data.two.a).?;

                switch (op) {
                    .plus, .minus => {
                        const rrvalue = try rvalue.getValue(self, rty, ty, true, false);
                        const llvalue = try lvalue.getValue(self, lty, ty, rrvalue != .immediate, false);

                        const out_reg = old_expects_reg orelse self.availableRegister().?;
                        if (llvalue == .register and rrvalue == .register) {
                            _ = try self.writeInst(CodeGenerator.dataProcReg(.add_sub_shift, .{
                                .sfopSshRmimmRnRd = .{
                                    .rd = out_reg,
                                    .rn = llvalue.register,
                                    .rm = rrvalue.register,
                                    .S = false,
                                    .op = @bitCast(op == .minus),
                                },
                            }));
                        } else if (llvalue == .register and rrvalue == .immediate) {
                            _ = try self.writeInst(dataProcImm(.add_sub, .{
                                .sfopSshimm12rnrd = .{
                                    .op = @bitCast(op == .minus),
                                    .S = false,
                                    .imm = @truncate(rrvalue.immediate),
                                    .rn = llvalue.register,
                                    .rd = out_reg,
                                },
                            }));
                        } else if (llvalue == .immediate and rrvalue == .register) {
                            @panic("mmm no");
                        }

                        self.useRegister(out_reg);
                        return .{ .register = out_reg };
                    },
                    .star => {
                        const rrvalue = try rvalue.getValue(self, rty, ty, false, false);
                        const llvalue = try lvalue.getValue(self, lty, ty, false, false);

                        const out_reg = old_expects_reg orelse self.availableRegister().?;
                        _ = try self.writeInst(CodeGenerator.dataProcReg(.data_3reg, .{
                            .sfop54op31Rmop0RaRnRd = .{
                                .rd = out_reg,
                                .rn = llvalue.register,
                                .ra = XZR,
                                .op0 = 0,
                                .rm = rrvalue.register,
                                .op31 = 0,
                                .op54 = 0,
                                .sf = layout.size >= 8,
                            },
                        }));

                        self.useRegister(out_reg);
                        return .{ .register = out_reg };
                    },
                    else => {},
                }
            },
            .unary_prefix_operator => {
                const op: TokenKind = @enumFromInt(node.data.four.c);
                const expr_val = try self.genNodeExpr(node.data.two.a, null);
                switch (op) {
                    .star => {
                        const out_reg = self.availableRegister().?;
                        self.useRegister(out_reg);
                        const expr_ty = self.unit.node_to_type.get(node.data.two.a).?;
                        _ = try self.writeInst(formLoad(expr_ty, out_reg, expr_val.register, 0));
                        return .{ .register = out_reg };
                    },
                    else => {},
                }
            },
            .index => {
                const ptr_ty = self.unit.node_to_type.get(node.data.two.a).?;
                const offset_ty = self.unit.node_to_type.get(node.data.two.b).?;

                const pre_ptr_val = try self.genNodeExpr(node.data.two.a, null);
                const ptr_val = try pre_ptr_val.getValue(self, ptr_ty, null, false, false);
                const offset_val = try self.genNodeExpr(node.data.two.b, null);
                const f_offset_val = try offset_val.getValue(self, offset_ty, null, true, false);

                const base_off: u32, const elem_ty = switch (ptr_ty.kind) {
                    .pointer => |ptr| blk: {
                        // const out_reg = self.availableRegister();
                        // self.useRegister(out_reg);

                        break :blk .{ 0, ptr.base };
                        // _ = try self.writeInst(formLoad(ptr.base, out_reg, ptr, offset: u32))
                    },
                    .array => |arr| blk: {
                        break :blk .{ 0, arr.base };
                    },
                    .array_unsized => |arr| blk: {
                        break :blk .{ 0, arr.base };
                    },
                    else => unreachable,
                };
                _ = base_off;

                const elem_layout = self.computeLayout(elem_ty);
                switch (f_offset_val) {
                    .immediate => |val| {
                        // const out_reg = self.availableRegister().?;
                        // self.useRegister(out_reg);
                        // _ = try self.writeInst(formLoad(elem_ty, out_reg, ptr_val.register, @as(u32, @truncate(val)) * elem_layout.size));
                        //
                        return .{
                            .memory_offset = .{
                                .register = ptr_val.register,
                                .offset = @as(u32, @truncate(val)) * elem_layout.size,
                            },
                        };

                        // return .{ .register = out_reg };
                    },
                    .register => |reg| {
                        const offset_reg = self.availableRegister().?;
                        self.useRegister(offset_reg);
                        _ = try self.writeInst(CodeGenerator.dataProcImm(.move, .{
                            .sfopchwimm16rd = .{
                                .rd = offset_reg,
                                .imm = @truncate(elem_layout.size),
                                .hw = 0,
                                .opc = 0b10,
                                .sf = true,
                            },
                        }));

                        _ = try self.writeInst(CodeGenerator.dataProcReg(.data_3reg, .{
                            .sfop54op31Rmop0RaRnRd = .{
                                .rd = offset_reg,
                                .rn = reg,
                                .ra = XZR,
                                .op0 = 0,
                                .rm = offset_reg,
                                .op31 = 0,
                                .op54 = 0,
                                .sf = true,
                            },
                        }));

                        const out_reg = self.availableRegister().?;
                        self.useRegister(out_reg);

                        switch (ptr_val) {
                            .register => |ptr_reg| {
                                _ = try self.writeInst(CodeGenerator.dataProcReg(.add_sub_shift, .{
                                    .sfopSshRmimmRnRd = .{
                                        .rd = offset_reg,
                                        .rn = ptr_reg,
                                        .rm = offset_reg,
                                        .S = false,
                                        .op = 0,
                                    },
                                }));

                                return .{
                                    .memory_offset = .{
                                        .register = offset_reg,
                                        .offset = 0,
                                    },
                                };
                                // _ = try self.writeInst(formLoad(elem_ty, out_reg, offset_reg, 0));
                            },
                            .memory_offset => |mo| {
                                const ptr_reg = self.availableRegister().?;
                                self.useRegister(ptr_reg);
                                _ = try self.writeInst(formLoad(elem_ty, ptr_reg, mo.register, mo.offset));

                                _ = try self.writeInst(CodeGenerator.dataProcReg(.add_sub_shift, .{
                                    .sfopSshRmimmRnRd = .{
                                        .rd = offset_reg,
                                        .rn = ptr_reg,
                                        .rm = offset_reg,
                                        .S = false,
                                        .op = 0,
                                    },
                                }));

                                return .{
                                    .memory_offset = .{
                                        .register = offset_reg,
                                        .offset = 0,
                                    },
                                };
                                // _ = try self.writeInst(formLoad(elem_ty, out_reg, offset_reg, 0));
                            },
                            else => @panic("not supported"),
                        }

                        return .{ .register = out_reg };
                    },
                    else => @panic("Unsupported index"),
                }
            },
            .cast => {
                const to_type = self.unit.node_to_type.get(node.data.two.a).?;
                const expr_nidx = try self.genNodeExpr(Node.absoluteIndex(nidx, node.data.four.c), to_type);
                const expr_type = self.unit.node_to_type.get(Node.absoluteIndex(nidx, node.data.four.c)).?;

                return expr_nidx.getValue(self, expr_type, to_type, false, false);

                // const out_reg = switch (expr_nidx) {
                //     .memory_offset => |mo| blk: {
                //         const out_reg = self.availableRegister().?;
                //         self.useRegister(out_reg);
                //         try self.writeCastLoad(expr_type, to_type, out_reg, mo.register, mo.offset);
                //         break :blk out_reg;
                //     },
                //     .register => |reg| blk: {
                //         _ = try self.writeCast(expr_type, to_type, reg, reg);
                //         break :blk reg;
                //     },
                //     else => @panic("todo"),
                // };

                // if (result_ty) |rty| {
                //     _ = try self.writeCast(to_type, rty, out_reg, out_reg);
                // }
                // return .{ .register = out_reg };
            },
            else => std.log.warn("Skipping node {}", .{nidx}),
        }

        return .{ .inst = 0 };
    }

    pub fn genNodeLvalue(self: *Self, nidx: NodeIndex) !Value {
        const node = &self.unit.nodes.items[nidx];
        switch (node.kind) {
            .identifier => {
                const referred_nidx = self.unit.node_to_node.get(nidx).?;
                if (self.fctx.?.local_offsets.get(referred_nidx)) |vr| {
                    return .{
                        .memory_offset = .{
                            .offset = vr,
                            .register = SP,
                        },
                    };
                }

                @panic("unhandled lvalue");
            },
            .binary_lr_operator => {
                const old_expects_reg = self.expects_reg;
                defer self.expects_reg = old_expects_reg;
                self.expects_reg = null;

                const op: TokenKind = @enumFromInt(node.data.as(.four).d);
                switch (op) {
                    .dot => {
                        const ltype = self.unit.node_to_type.get(node.data.two.a).?;
                        _ = self.computeLayout(ltype);
                        const lvalue = try self.genNodeLvalue(node.data.two.a);
                        // const refed_node = self.unit.node_to_node.get(Node.absoluteIndex(nidx, node.data.four.c)).?;

                        const right_node = &self.unit.nodes.items[Node.absoluteIndex(nidx, node.data.four.c)];
                        const field_str = self.unit.identifierAt(@bitCast(right_node.data.two.a));

                        const strc_nidx = ltype.getStructureIndex();
                        const field_offset = switch (ltype.kind) {
                            .@"struct", .unnamed_struct => blk2: {
                                const field_map = self.unit.field_map.getPtr(strc_nidx).?;
                                const field_index = field_map.get(field_str).?;
                                const strct = self.structs.getPtr(strc_nidx).?;
                                const field_offset = strct.offsets.get(field_index).?;

                                break :blk2 field_offset;
                            },
                            .@"union", .unnamed_union => blk2: {
                                break :blk2 0;
                            },
                            else => unreachable,
                        };

                        switch (lvalue) {
                            .memory_offset => |mo| {
                                return .{
                                    .memory_offset = .{
                                        .offset = mo.offset + field_offset,
                                        .register = mo.register,
                                    },
                                };
                            },
                            else => @panic(""),
                        }
                    },
                    else => {},
                }
                const rvalue = try self.genNode(Node.absoluteIndex(nidx, node.data.as(.four).c));
                _ = rvalue;
                @panic("foo");
            },
            else => @panic("unhandled node"),
        }
    }

    // pub fn genImplicitCastIfNeeded()

    pub fn computeLayout(self: *Self, ty: Type) Layout {
        return switch (ty.kind) {
            .char => .{ .size = 1, .alignment = 1 },
            .short => .{ .size = 2, .alignment = 2 },
            .int => .{ .size = 4, .alignment = 4 },
            .long => .{ .size = 8, .alignment = 8 },
            .longlong => .{ .size = 8, .alignment = 8 },
            .float => .{ .size = 4, .alignment = 4 },
            .double => .{ .size = 8, .alignment = 8 },
            .longdouble => .{ .size = 16, .alignment = 16 },
            .pointer => .{ .size = 8, .alignment = 8 },
            .array => |arr| blk: {
                const base_layout = self.computeLayout(arr.base);
                break :blk .{ .size = base_layout.size * @as(u32, @truncate(arr.size)), .alignment = base_layout.alignment };
            },
            .@"struct", .unnamed_struct => blk: {
                const fields = switch (ty.kind) {
                    .@"struct" => |st| self.unit.interner.getMultiTypes(st.fields),
                    .unnamed_struct => |st| self.unit.interner.getMultiTypes(st.fields),
                    else => unreachable,
                };
                const nidx = switch (ty.kind) {
                    .@"struct" => |st| st.nidx,
                    .unnamed_struct => |st| st.nidx,
                    else => unreachable,
                };
                var field_offsets = std.AutoHashMap(u32, u32).init(self.unit.allocator);
                var offset: u32 = 0;
                var largest_alignment: u32 = 0;
                for (fields, 0..) |field, i| {
                    switch (field.kind) {
                        .field => |f| {
                            const layout = self.computeLayout(f.ty);
                            if (layout.alignment > largest_alignment) {
                                largest_alignment = layout.alignment;
                            }
                            offset = std.mem.alignForward(u32, offset, layout.alignment);
                            field_offsets.put(@truncate(i), offset) catch @panic("OOM");
                            offset += layout.size;
                        },
                        else => {
                            const layout = self.computeLayout(field);
                            if (layout.alignment > largest_alignment) {
                                largest_alignment = layout.alignment;
                            }

                            offset = std.mem.alignForward(u32, offset, layout.alignment);
                            field_offsets.put(@truncate(i), offset) catch @panic("OOM");
                            offset += layout.size;
                        },
                    }
                }

                self.structs.put(nidx, .{ .offsets = field_offsets }) catch @panic("oom");
                offset = std.mem.alignForward(u32, offset, largest_alignment);
                break :blk .{ .size = offset, .alignment = largest_alignment };
            },
            .@"union", .unnamed_union => blk: {
                const fields = switch (ty.kind) {
                    .@"union" => |st| self.unit.interner.getMultiTypes(st.variants),
                    .unnamed_union => |st| self.unit.interner.getMultiTypes(st.variants),
                    else => unreachable,
                };
                var largest_alignment: u32 = 0;
                var largest_size: u32 = 0;
                for (fields) |field| {
                    switch (field.kind) {
                        .field => |f| {
                            const layout = self.computeLayout(f.ty);
                            if (layout.alignment > largest_alignment) {
                                largest_alignment = layout.alignment;
                            }
                            if (layout.size > largest_size) {
                                largest_size = layout.size;
                            }
                        },
                        else => {
                            const layout = self.computeLayout(field);
                            if (layout.alignment > largest_alignment) {
                                largest_alignment = layout.alignment;
                            }
                            if (layout.size > largest_size) {
                                largest_size = layout.size;
                            }
                        },
                    }
                }

                largest_size = std.mem.alignForward(u32, largest_size, largest_alignment);
                break :blk .{ .size = largest_size, .alignment = largest_alignment };
            },
            else => @panic("umm"),
        };
    }

    pub fn writeCast(self: *Self, ty: Type, result_ty: Type, in_reg: u5, out_reg: u5) !bool {
        if (ty == result_ty) {
            return false;
        }

        if (ty.isIntegral() and result_ty.isIntegral()) {
            if (ty.isSigned() and result_ty.rank() > ty.rank()) {
                const imms: u6 = switch (ty.kind) {
                    .char => 0b000111,
                    .short => 0b001111,
                    .int => 0b011111,
                    else => unreachable,
                };

                _ = try self.writeInst(dataProcImm(.bitfield, .{
                    .sfopcNimmrimmsRnRd = .{
                        .rd = out_reg,
                        .rn = in_reg,
                        .imms = imms,
                        .immr = 0b000000,
                        .N = if (ty.rank() > self.unit.interner.intTy(true, 0).rank()) 1 else 0,
                        .opc = 0,
                        .sf = ty.rank() > self.unit.interner.intTy(true, 0).rank(),
                    },
                }));

                return true;
            } else if (result_ty.rank() != ty.rank()) {
                const imms: u6 = switch (ty.kind) {
                    .char => 0b000111,
                    .short => 0b001111,
                    else => unreachable,
                };
                _ = try self.writeInst(dataProcImm(.logical, .{
                    .sfopcNimmrimmsRnRd = .{
                        .rd = out_reg,
                        .rn = in_reg,
                        .imms = imms,
                        .immr = 0,
                        .N = 1,
                        .opc = 0b00,
                        .sf = true,
                    },
                }));

                return true;
            } else {
                @panic("todo: do we need more cases??");
            }

            return false;
        }
        unreachable;
    }

    pub fn writeCastLoad(self: *Self, ty: Type, result_ty: Type, out_reg: u5, offset_reg: u5, offset: u32) !void {
        if (ty == result_ty) {
            _ = try self.writeInst(formLoad(ty, out_reg, offset_reg, offset));
            return;
        }
        if (ty.isIntegral() and result_ty.isIntegral()) {
            var sz: u2 = switch (ty.kind) {
                .char => 0b00,
                .short => 0b01,
                .int => 0b10,
                .long => 0b11,
                .longlong => 0b11,
                else => unreachable,
            };
            var opc: u2 = 0b01;
            const div = @as(u32, 1) << sz;
            if (ty.isSigned() and result_ty.rank() > ty.rank()) {
                if (result_ty.rank() > self.unit.interner.intTy(false, 0).rank()) {
                    opc = 0b10;
                } else {
                    opc = 0b11;
                }
            } else if (ty.rank() > self.unit.interner.intTy(false, 0).rank()) {
                sz = 0b11;
            }

            _ = try self.writeInst(loadStore(.load_store_register, 0, 0b10, 0, 0, .{
                .szVopcimm12rnrt = .{
                    .rt = out_reg,
                    .rn = offset_reg,
                    .imm = @truncate(offset / div),
                    .opc = opc,
                    .V = false,
                    .sz = sz,
                },
            }));

            return;
        }
        unreachable;
    }

    pub fn writeCastLoad2(self: *Self, ty: Type, result_ty: Type, out_reg: u5, offset_reg: u5, offset: u32) !u32 {
        if (ty == result_ty) return formLoad(ty, out_reg, offset_reg, offset);
        if (ty.isIntegral() and result_ty.isIntegral()) {
            var sz: u2 = 0;
            var div: u32 = 1;
            var opc: u2 = undefined;
            if (ty.isSigned() == result_ty.isSigned()) {
                if (ty.rank() < result_ty.rank()) {
                    switch (ty.kind) {
                        .char => {
                            sz = 0b00;
                            if (ty.kind.char) {
                                if (result_ty.rank() > self.unit.interner.intTy(false, 0).rank()) {
                                    opc = 0b10; // 64-bit
                                } else {
                                    opc = 0b11; // 32-bit
                                }
                            } else {
                                opc = 0b01;
                            }
                        },
                        .short => {
                            sz = 0b01;
                            div = 2;
                            if (ty.kind.short) {
                                if (result_ty.rank() > self.unit.interner.intTy(false, 0).rank()) {
                                    opc = 0b10; // 64-bit
                                } else {
                                    opc = 0b11; // 32-bit
                                }
                            } else {
                                opc = 0b01;
                            }
                        },
                        .int => {
                            sz = 0b10;
                            div = 4;
                            if (ty.kind.int and result_ty.rank() > self.unit.interner.intTy(false, 0).rank()) {
                                opc = 0b10; // 64-bit ldrsw
                            } else {
                                opc = 0b01;
                            }
                        },
                        .long => {
                            sz = 0b10;
                            div = 8;
                        },
                        .longlong => {
                            sz = 0b10;
                            div = 8;
                        },
                        else => unreachable,
                    }
                    return try self.writeInst(loadStore(.load_store_register, 0, 0b10, 0, 0, .{
                        .szVopcimm12rnrt = .{
                            .rt = out_reg,
                            .rn = offset_reg,
                            .imm = @truncate(offset / div),
                            .opc = opc,
                            .V = false,
                            .sz = sz,
                        },
                    }));
                } else {
                    opc = 0b01;
                    switch (ty.kind) {
                        .char => {
                            sz = 0b00;
                        },
                        .short => {
                            sz = 0b01;
                            div = 2;
                        },
                        .int => {
                            sz = 0b10;
                            div = 4;
                        },
                        .long => {
                            sz = 0b10;
                            div = 8;
                        },
                        .longlong => {
                            sz = 0b10;
                            div = 8;
                        },
                        else => unreachable,
                    }

                    _ = try self.writeInst(loadStore(.load_store_register, 0, 0b10, 0, 0, .{
                        .szVopcimm12rnrt = .{
                            .rt = out_reg,
                            .rn = offset_reg,
                            .imm = @truncate(offset / div),
                            .opc = opc,
                            .V = false,
                            .sz = sz,
                        },
                    }));

                    if (result_ty.rank() < self.unit.interner.intTy(true, 0).rank()) {
                        _ = try self.writeInst(dataProcImm(.bitfield, .{
                            .sfopcNimmrimmsRnRd = .{
                                .rd = out_reg,
                                .rn = out_reg,
                                .imms = 0b000111,
                                .immr = 0b000000,
                                .N = 1,
                                .opc = 0,
                                .sf = ty.rank() > self.unit.interner.intTy(true, 0).rank(),
                            },
                        }));
                    }
                    return 0;
                }
            } else if (!ty.isSigned() and ty.rank() >= result_ty.rank()) {
                // break :blk ty;
                unreachable;
            } else if (!result_ty.isSigned() and result_ty.rank() >= ty.rank()) {
                // break :blk result_ty;
                unreachable;
            } else if (ty.isSigned()) {
                // const new_type = self.unit.interner.createOrGetTyKind(ty.toSigned(false));
                // break :blk new_type;
                unreachable;
            } else if (result_ty.isSigned()) {
                // const new_type = self.unit.interner.createOrGetTyKind(result_ty.toSigned(false));
                // break :blk new_type;
                unreachable;
            }
        }
        unreachable;
    }

    pub fn formLoad(ty: Type, out_reg: u5, offset_reg: u5, offset: u32) u32 {
        // const out_reg = self.availableRegister().?;
        // self.useRegister(out_reg);

        const sz: u2, const divide: u32 = switch (ty.kind) {
            .char => .{ 0b00, 1 },
            .short => .{ 0b01, 2 },
            .int => .{ 0b10, 4 },
            .long => .{ 0b11, 8 },
            .longlong => .{ 0b11, 8 },
            .pointer => .{ 0b11, 8 },
            else => unreachable,
        };

        return loadStore(.load_store_register, 0, 0b10, 0, 0, .{
            .szVopcimm12rnrt = .{
                .rt = out_reg,
                .rn = offset_reg,
                .imm = @truncate(offset / divide),
                .opc = 0b01,
                .V = false,
                .sz = sz,
            },
        });
    }

    pub fn formStore(ty: Type, in_reg: u5, offset_reg: u5, offset: u32) u32 {
        const sz: u2, const divide: u32 = switch (ty.kind) {
            .char => .{ 0b00, 1 },
            .short => .{ 0b01, 2 },
            .int => .{ 0b10, 4 },
            .long => .{ 0b11, 8 },
            .longlong => .{ 0b11, 8 },
            .pointer => .{ 0b11, 8 },
            else => unreachable,
        };

        return loadStore(.load_store_register, 0, 0b10, 0, 0, .{
            .szVopcimm12rnrt = .{
                .rt = in_reg,
                .rn = offset_reg,
                .imm = @truncate(offset / divide),
                .opc = 0b00,
                .V = false,
                .sz = sz,
            },
        });
    }

    pub fn loadStore(op0: enum(u4) {
        load_store_register = 0b0011,
    }, op1: u1, op2: u2, op3: u6, op4: u2, data: OPS) u32 {
        return @as(u32, 0x08000000) | (@as(u32, @intFromEnum(op0)) << 28) | (@as(u32, op1) << 26) | (@as(u32, op2) << 23) | (@as(u32, op3) << 16) | (@as(u32, op4) << 10) | @as(u32, @bitCast(data));
    }

    pub fn dataProcImm(op: enum(u32) {
        add_sub = 0x01000000,
        logical = 0x02000000,
        move = 0x02800000,
        bitfield = 0x03000000,
    }, data: OPS) u32 {
        return @as(u32, 0x10000000) | @intFromEnum(op) | @as(u32, @bitCast(data));
    }

    pub fn dataProcReg(op: enum(u32) {
        logical_shift = 0x00000000,
        add_sub_shift = 0x01000000,
        data_3reg = 0x11000000,
    }, data: OPS) u32 {
        return @as(u32, 0x0a000000) | @intFromEnum(op) | @as(u32, @bitCast(data));
    }

    pub fn brExcSys(op: enum(u32) {
        un_cond_br_reg = 0xD2000000,
        un_cond_br_imm = 0x00000000,
    }, data: OPS) u32 {
        return @as(u32, 0x14000000) | @intFromEnum(op) | @as(u32, @bitCast(data));
    }

    pub const OPS = packed union {
        szVopcimm12rnrt: packed struct(u32) {
            rt: u5,
            rn: u5,
            imm: u12,
            opc: u2,
            _1: u2 = 0,
            V: bool,
            _2: u3 = 0,
            sz: u2,
        },
        sfopSshimm12rnrd: packed struct(u32) {
            rd: u5,
            rn: u5,
            imm: u12,
            sh: bool = false,
            _1: u6 = 0,
            S: bool,
            op: u1,
            sf: bool = true,
        },
        sfopcshNRmimmRnRd: packed struct(u32) {
            rd: u5,
            rn: u5,
            imm: u6,
            rm: u5,
            N: bool,
            sh: u2,
            _1: u5 = 0,
            opc: u2,
            sf: bool = true,
        },
        sfopchwimm16rd: packed struct(u32) {
            rd: u5,
            imm: u16,
            hw: u2,
            _1: u6 = 0,
            opc: u2,
            sf: bool,
        },
        opcop23Rnop4: packed struct(u32) {
            op4: u5 = 0,
            rn: u5,
            op3: u6,
            op2: u5 = 0b11111,
            opc: u4,
            _1: u7 = 0,
        },
        sfopSshRmimmRnRd: packed struct(u32) {
            rd: u5,
            rn: u5,
            imm: u6 = 0,
            rm: u5,
            _1: u1 = 0,
            shift: u2 = 0,
            _2: u5 = 0,
            S: bool,
            op: u1,
            sf: bool = true,
        },
        sfop54op31Rmop0RaRnRd: packed struct(u32) {
            rd: u5,
            rn: u5,
            ra: u5,
            op0: u1,
            rm: u5,
            op31: u3,
            _1: u5 = 0,
            op54: u2,
            sf: bool,
        },
        sfSRmopRnRd: packed struct(u32) {
            rd: u5,
            rn: u5,
            op: u6,
            rm: u5,
            _1: u8 = 0,
            S: bool = false,
            _2: u1 = 0,
            sf: bool,
        },
        szopcimm9RnRt: packed struct(u32) {
            rt: u5,
            rn: u5,
            _1: u2 = 0,
            imm9: u9,
            _2: u1 = 0,
            opc: u2,
            _3: u6 = 0,
            sz: u2,
        },
        sfopcNimmrimmsRnRd: packed struct(u32) {
            rd: u5,
            rn: u5,
            imms: u6,
            immr: u6,
            N: u1,
            _1: u6 = 0,
            opc: u2,
            sf: bool,
        },
        // szVRmoptSRnRt: packed struct(u32) {
        //     rt: u5,
        //     rn: u5,
        //     _1: u2 = 0,
        //     S: u1 = 0,
        //     opt: u3,
        //     rm: u5,
        //     opc: u2,
        //     V: bool,
        //     size: u2,
        // },
    };

    pub fn writeInst(self: *Self, inst: u32) !u32 {
        self.inst_count += 1;
        return try self.builder.writeInt(inst);
    }

    pub fn writeInstAt(self: *Self, inst: u32, offset: u32) !u32 {
        return try self.builder.writeIntAt(inst, offset);
    }

    pub fn writeToFile(self: *Self, path: []const u8) !void {
        self.builder.ptrTo(std.macho.segment_command_64, self.segment_offset).filesize = self.inst_count * @sizeOf(u32);
        self.builder.ptrTo(std.macho.segment_command_64, self.segment_offset).vmsize = self.inst_count * @sizeOf(u32);
        self.builder.ptrToSection(self.segment_offset, 0).size = self.inst_count * @sizeOf(u32);

        // const syms_len = self.builder.currentOffset() - syms_offset;

        const str_offset = self.builder.currentOffset();
        for (self.symbols.items(.value)) |str| {
            _ = try self.builder.writeInt(@as(u8, '_'));
            _ = try self.builder.writeSlice(str);
            _ = try self.builder.writeInt(@as(u8, 0));
        }
        const str_len = self.builder.currentOffset() - str_offset;

        var next_offset: u32 = 0;
        const syms_offset = self.builder.currentOffset();
        for (self.symbols.items(.ty), self.symbols.items(.address), self.symbols.items(.value)) |ty, addr, value| {
            const offset = try self.builder.writeSymbolEntry(self.symtab_offset, next_offset, ty);
            self.builder.ptrTo(std.macho.nlist_64, offset).n_sect = 1;
            self.builder.ptrTo(std.macho.nlist_64, offset).n_value = addr;
            next_offset += @truncate(value.len + 2);
        }

        self.builder.ptrTo(std.macho.symtab_command, self.symtab_offset).symoff = syms_offset;
        self.builder.ptrTo(std.macho.symtab_command, self.symtab_offset).nsyms = @truncate(self.symbols.len);
        self.builder.ptrTo(std.macho.symtab_command, self.symtab_offset).stroff = str_offset;
        self.builder.ptrTo(std.macho.symtab_command, self.symtab_offset).strsize = str_len;

        try self.builder.writeToFile(path);
    }
};

pub const MachoSymbolType = packed struct(u8) {
    external: bool = true,
    ty: enum(u3) {
        undef = std.macho.N_UNDF >> 1,
        absolute = std.macho.N_ABS >> 1,
        section = std.macho.N_SECT >> 1,
        prebound = std.macho.N_PBUD >> 1,
        indirect = std.macho.N_INDR >> 1,
    } = .undef,
    private: bool = false,
    stab: u3 = 0,
};

pub const MachoBuilder = struct {
    buffer: std.ArrayList(u8),

    load_index: u32 = 0,
    command_count: u32 = 0,

    const Self = @This();

    pub fn init(allocator: std.mem.Allocator) !Self {
        var result = Self{
            .buffer = std.ArrayList(u8).init(allocator),
        };

        const header = std.macho.mach_header_64{
            .cputype = std.macho.CPU_TYPE_ARM64,
            .cpusubtype = std.macho.CPU_SUBTYPE_ARM_ALL,
            .filetype = std.macho.MH_OBJECT,
            .ncmds = 0,
            .sizeofcmds = 0,
            .flags = std.macho.MH_SUBSECTIONS_VIA_SYMBOLS,
        };

        _ = try result.writeValue(&header);
        _ = result.ptrTo(std.macho.mach_header_64, 0);

        return result;
    }

    pub fn writeToFile(self: *Self, path: []const u8) !void {
        var out_file = try std.fs.cwd().createFile(path, .{});
        try out_file.writeAll(self.buffer.items);
    }

    pub fn addLoadSegment(self: *Self, num_sections: u32) !u32 {
        const segment_command = std.macho.segment_command_64{
            .cmdsize = @sizeOf(std.macho.segment_command_64) + @sizeOf(std.macho.section_64) * num_sections,
            .fileoff = undefined,
            .filesize = undefined,
            .nsects = num_sections,
            .segname = [1]u8{0} ** 16,
        };
        const offset = self.buffer.items.len;
        self.load_index += segment_command.cmdsize;
        self.ptrTo(std.macho.mach_header_64, 0).ncmds += 1;
        self.ptrTo(std.macho.mach_header_64, 0).sizeofcmds += segment_command.cmdsize;
        // self.command_count += 1;
        try self.buffer.ensureTotalCapacity(self.buffer.items.len + segment_command.cmdsize);
        _ = try self.writeValue(&segment_command);
        self.buffer.items.len += @sizeOf(std.macho.section_64) * num_sections;
        return @truncate(offset);
    }

    pub fn addLoadSymTab(self: *Self) !u32 {
        const segment_command = std.macho.symtab_command{
            .cmdsize = @sizeOf(std.macho.symtab_command),
            .symoff = undefined,
            .nsyms = undefined,
            .stroff = undefined,
            .strsize = undefined,
        };
        const offset = self.buffer.items.len;
        self.load_index += segment_command.cmdsize;
        self.ptrTo(std.macho.mach_header_64, 0).ncmds += 1;
        self.ptrTo(std.macho.mach_header_64, 0).sizeofcmds += segment_command.cmdsize;

        _ = try self.writeValue(&segment_command);
        return @truncate(offset);
    }

    pub fn ptrTo(self: *Self, comptime T: type, index: u32) *align(1) T {
        const ptr = self.buffer.items.ptr + index;
        const value_ptr: [*]align(1) T = @ptrCast(ptr);
        return &value_ptr[0];
    }

    pub fn ptrToSection(self: *Self, load_command_offset: u32, section_index: u32) *align(1) std.macho.section_64 {
        const ptr = self.buffer.items.ptr + load_command_offset + @sizeOf(std.macho.segment_command_64) + @sizeOf(std.macho.section_64) * section_index;
        const value_ptr: [*]align(1) std.macho.section_64 = @ptrCast(ptr);
        return &value_ptr[0];
    }

    pub fn writeValue(self: *Self, value: anytype) !u32 {
        const offset = self.currentOffset();
        const value_ptr: [*]const std.meta.Child(@TypeOf(value)) = @ptrCast(value);
        const byte_ptr: [*]const u8 = @ptrCast(value_ptr);
        try self.buffer.appendSlice(byte_ptr[0..@sizeOf(std.meta.Child(@TypeOf(value)))]);
        return offset;
    }

    pub fn alignForward(self: *Self, alignment: usize) !void {
        const new_len = std.mem.alignForward(usize, self.buffer.items.len, alignment);
        try self.buffer.ensureTotalCapacity(new_len);
        self.buffer.items.len = new_len;
    }

    pub fn writeInt(self: *Self, value: anytype) !u32 {
        const ty_info = @typeInfo(@TypeOf(value));
        std.debug.assert(ty_info == .int);
        std.debug.assert(std.mem.isAligned(self.buffer.items.len, @alignOf(@TypeOf(value))));

        const offset = self.currentOffset();
        const value_ptr: *const @TypeOf(value) = &value;
        try self.buffer.ensureUnusedCapacity(@sizeOf(@TypeOf(value)));
        const byte_ptr: *@TypeOf(value) = @alignCast(@ptrCast(&self.buffer.items.ptr[self.buffer.items.len]));
        self.buffer.items.len += @sizeOf(@TypeOf(value));
        byte_ptr.* = value_ptr.*;
        // try self.buffer.appendSlice(byte_ptr[0..@sizeOf(std.meta.Child(@TypeOf(value)))]);
        return offset;
    }

    pub fn writeIntAt(self: *Self, value: anytype, offset: u32) !u32 {
        const ty_info = @typeInfo(@TypeOf(value));
        std.debug.assert(ty_info == .int);
        std.debug.assert(std.mem.isAligned(offset, @alignOf(@TypeOf(value))));
        std.debug.assert(offset < self.buffer.items.len);
        std.debug.assert(offset + @sizeOf(@TypeOf(value)) <= self.buffer.items.len);

        const value_ptr: *const @TypeOf(value) = &value;
        const byte_ptr: *@TypeOf(value) = @alignCast(@ptrCast(&self.buffer.items.ptr[offset]));
        byte_ptr.* = value_ptr.*;
        // try self.buffer.appendSlice(byte_ptr[0..@sizeOf(std.meta.Child(@TypeOf(value)))]);
        return offset;
    }

    pub fn writeSlice(self: *Self, slice: anytype) !u32 {
        const offset = self.currentOffset();
        // const value_ptr: [*]const std.meta.Child(@TypeOf(value)) = value.ptr;
        const byte_ptr: [*]const u8 = @ptrCast(slice.ptr);
        try self.buffer.appendSlice(byte_ptr[0 .. slice.len * @sizeOf(std.meta.Elem(@TypeOf(slice)))]);
        return offset;
    }

    pub fn writeSymbolEntry(self: *Self, symtab_offset: u32, str_idx: u32, ty: MachoSymbolType) !u32 {
        const offset = self.currentOffset();

        const value = std.macho.nlist_64{
            .n_strx = str_idx,
            .n_type = @bitCast(ty),
            .n_sect = 0,
            .n_desc = 0,
            .n_value = 0,
        };

        const value_ptr: [*]const std.macho.nlist_64 = @ptrCast(&value);
        const byte_ptr: [*]const u8 = @ptrCast(value_ptr);
        try self.buffer.appendSlice(byte_ptr[0..@sizeOf(std.macho.nlist_64)]);

        self.ptrTo(std.macho.symtab_command, symtab_offset).nsyms += 1;
        // self.ptrTo(std.macho.symtab_command, symtab_offset). += 1;

        return offset;
    }

    pub inline fn currentOffset(self: *const Self) u32 {
        return @truncate(self.buffer.items.len);
    }
};

const SP: u5 = 31;
const XZR: u5 = 31;
const LR: u5 = 30;
const X0: u5 = 0;
const X1: u5 = 1;
const X2: u5 = 2;
const X3: u5 = 3;
const X4: u5 = 4;
const X5: u5 = 5;
const X6: u5 = 6;
const X7: u5 = 7;
const X8: u5 = 8;
const X9: u5 = 9;
const X10: u5 = 10;
const X11: u5 = 11;
const X12: u5 = 12;
const X13: u5 = 13;
const X14: u5 = 14;
const X15: u5 = 15;
const X16: u5 = 16;
const X17: u5 = 17;
const X18: u5 = 18;
const X19: u5 = 19;
const X20: u5 = 20;
const X21: u5 = 21;
const X22: u5 = 22;
const X23: u5 = 23;
const X24: u5 = 24;
const X25: u5 = 25;
const X26: u5 = 26;
const X27: u5 = 27;
const X28: u5 = 28;
const X29: u5 = 29;
const X30: u5 = 30;
const X31: u5 = 31;
