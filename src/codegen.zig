const std = @import("std");
const Unit = @import("unit.zig").Unit;
const NodeIndex = @import("parser.zig").NodeIndex;
const Node = @import("parser.zig").Node;
const TokenKind = @import("tokenizer.zig").TokenKind;

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

    pub fn getValue(self: @This(), cg: *CodeGenerator, imm: bool, mem: bool) !Value {
        switch (self) {
            .immediate => |x| {
                if (imm) {
                    return self;
                } else if (cg.availableRegister()) |reg| {
                    _ = try cg.writeInst(CodeGenerator.dataProcImm(.move, .{
                        .sfopchwimm16rd = .{
                            .rd = reg,
                            .imm = @truncate(x),
                            .hw = 0,
                            .opc = 0b10,
                            .sf = 1,
                        },
                    }));
                    cg.useRegister(reg);
                    return .{ .register = reg };
                }
                _ = mem;
                
                @panic("shouldbnt be here");
            },
            else => return self,
        }
    }
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
    expects_reg: ?u5 = null,

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
            .compound_empty => {
                _ = try self.writeInst(brExcSys(.un_cond_br_reg, .{
                    .opcop23Rnop4 = .{
                        .rn = LR,
                        .op3 = 0,
                        .opc = 0b10,
                    },
                }));
            },
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
            .int_literal => {
                const ivalue = node.data.as(.two).a;
                return .{ .immediate = ivalue };
            },
            .binary_lr_operator => {
                const old_expects_reg = self.expects_reg;
                defer self.expects_reg = old_expects_reg;
                self.expects_reg = null;

                const lvalue = try self.genNode(node.data.as(.two).a);
                const rvalue = try self.genNode(Node.absoluteIndex(nidx, node.data.as(.four).c));
                const rrvalue = try rvalue.getValue(self, true, false);
                const llvalue = try lvalue.getValue(self, rrvalue != .immediate, false);
                const out_reg = old_expects_reg orelse self.availableRegister().?;
                const op: TokenKind = @enumFromInt(node.data.as(.four).d);

                switch (op) {
                    .plus, .minus => {
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
                    },
                    else => {},
                }

                self.useRegister(out_reg);
                return .{ .register = out_reg };
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
                const value = try self.genNode(node.data.as(.two).a);
                const rrvalue = try value.getValue(self, true, false);
                if (rrvalue == .immediate) {
                    _ = try self.writeInst(CodeGenerator.dataProcImm(.move, .{
                        .sfopchwimm16rd = .{
                            .rd = X0,
                            .imm = @truncate(rrvalue.immediate),
                            .hw = 0,
                            .opc = 0b10,
                            .sf = 1,
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

                _ = try self.writeInst(brExcSys(.un_cond_br_reg, .{
                    .opcop23Rnop4 = .{
                        .rn = LR,
                        .op3 = 0,
                        .opc = 0b10,
                    },
                }));
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

                const body_index = Node.absoluteIndex(nidx, node.data.as(.four).d);
                try self.genCompound(body_index);
            },
            else => std.log.warn("Skipping node {}", .{nidx}),
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

    pub fn loadStore(op0: enum(u4) {
        load_store_register = 0b0011,
    }, op1: u1, op2: u2, op3: u6, op4: u2, data: OPS) u32 {
        return @as(u32, 0x08000000) | (@as(u32, @intFromEnum(op0) << 28)) | (@as(u32, op1) << 26) | (@as(u32, op2) << 23) | (@as(u32, op3) << 16) | (@as(u32, op4) << 10) | @as(u32, @bitCast(data));
    }

    pub fn dataProcImm(op: enum(u32) {
        add_sub = 0x01000000,
        move = 0x02800000,
    }, data: OPS) u32 {
        return @as(u32, 0x10000000) | @intFromEnum(op) | @as(u32, @bitCast(data));
    }

    pub fn dataProcReg(op: enum(u32) {
        logical_shift = 0x00000000,
        add_sub_shift = 0x01000000,
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
            sf: u1 = 1,
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
            _2: u5= 0,
            S: bool,
            op: u1,
            sf: bool = true,
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

    pub fn writeToFile(self: *Self, path: []const u8) !void {
        self.builder.ptrTo(std.macho.segment_command_64, self.segment_offset).filesize = self.inst_count * @sizeOf(u32);
        self.builder.ptrTo(std.macho.segment_command_64, self.segment_offset).vmsize = self.inst_count * @sizeOf(u32);
        self.builder.ptrToSection(self.segment_offset, 0).size = self.inst_count * @sizeOf(u32);

        // const syms_len = self.builder.currentOffset() - syms_offset;

        const str_offset = self.builder.currentOffset();
        for (self.symbols.items(.value)) |str| {
            _ = try self.builder.writeInt(@as(u8, '_'));
            _ = try self.builder.writeSlice(str);
        }
        const str_len = self.builder.currentOffset() - str_offset;

        var next_offset: u32 = 0;
        const syms_offset = self.builder.currentOffset();
        for (self.symbols.items(.ty),self.symbols.items(.address), self.symbols.items(.value)) |ty, addr, value| {
            const offset = try self.builder.writeSymbolEntry(self.symtab_offset, next_offset, ty);
            self.builder.ptrTo(std.macho.nlist_64, offset).n_sect = 1;
            self.builder.ptrTo(std.macho.nlist_64, offset).n_value = addr;
            next_offset += @truncate(value.len);
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
        const byte_ptr: *@TypeOf(value) = @alignCast(@ptrCast(&self.buffer.items.ptr[self.buffer.items.len]));
        try self.buffer.ensureUnusedCapacity(@sizeOf(@TypeOf(value)));
        self.buffer.items.len += @sizeOf(@TypeOf(value));
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