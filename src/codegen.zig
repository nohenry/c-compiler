const std = @import("std");
const Unit = @import("unit.zig").Unit;
const NodeIndex = @import("parser.zig").NodeIndex;
const Node = @import("parser.zig").Node;
const StorageClass = @import("parser.zig").StorageClass;
const SimpleEvaluator = @import("typecheck.zig").SimpleEvaluator;
const TokenKind = @import("tokenizer.zig").TokenKind;
const Type = @import("types.zig").Type;

const RelocType = enum {
    variable,
    function,
};

const Symbol = struct {
    address: u32,
    section: u32,
    prefix: bool = true,
    macho_ty: MachoSymbolType,
    reloc_ty: RelocType,
    uses: u32 = 0,
};

const MemoryOffset = struct {
    offset: i32,
    register: u5,
};

const Value = union(enum) {
    inst: u32,
    immediate: u64,
    register: u5,
    memory: u32,
    reloc_offset: MemoryOffset,
    reloc_label: void,
    memory_offset: MemoryOffset,

    builtin: BuiltinFunction,

    pub fn getValue(self: @This(), cg: *CodeGenerator, ty: Type, result_ty: ?Type, imm: bool, mem: bool) !Value {
        var s = self;
        switch (s) {
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
            .memory_offset, .reloc_offset => |*mo| {
                if (self == .reloc_offset) {
                    try cg.reloc_buffer.append(std.macho.relocation_info{
                        .r_symbolnum = @intCast(mo.offset),
                        .r_address = @intCast(cg.builder.currentOffsetInSection(cg.segment_offset, 0)),
                        .r_extern = 1,
                        .r_length = 2,
                        .r_pcrel = 0,
                        .r_type = @intFromEnum(std.macho.reloc_type_arm64.ARM64_RELOC_PAGEOFF12),
                    });
                    mo.offset = 0;
                }
                if (result_ty) |rty| {
                    _ = try cg.writeCastLoad(ty, rty, mo.register, mo.register, mo.offset);
                } else {
                    _ = try cg.writeInst(CodeGenerator.formLoad(ty, mo.register, mo.register, mo.offset));
                }
                return .{ .register = mo.register };
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
    local_offsets: std.AutoHashMap(NodeIndex, i32),
    next_offset: u32 = 0,
    max_additional_stack: u32 = 0,
    sub_invocations: u32 = 0,
    return_type: Type,
};

pub const StructLayout = struct {
    offsets: std.AutoHashMap(u32, u32),
};

pub const BuiltinFunction = *const fn (*CodeGenerator, []const NodeIndex) (std.mem.Allocator.Error)!Value;
pub const BuiltinsCodeGen = struct {
    pub const map = std.StaticStringMap(BuiltinFunction).initComptime(blk: {
        const decls = @typeInfo(@This()).@"struct".decls;
        var result: [decls.len - 1]struct { []const u8, BuiltinFunction } = undefined;
        for (decls, 0..) |decl, i| {
            if (std.mem.eql(u8, decl.name, "map")) {
                continue;
            }
            result[i - 1][0] = decl.name;
            result[i - 1][1] = @field(@This(), decl.name);
        }
        break :blk &result;
    });

    pub fn __builtin_va_start(cg: *CodeGenerator, args: []const NodeIndex) !Value {
        _ = cg;
        _ = args;
        return .{ .inst = 0 };
    }

    pub fn __builtin_va_arg(cg: *CodeGenerator, args: []const NodeIndex) !Value {
        _ = cg;
        _ = args;
        return .{ .inst = 0 };
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

    data_section_buffer: std.ArrayList(u8),
    common_section_buffer: std.ArrayList(u8),
    cstring_section_buffer: std.ArrayList(u8),
    reloc_buffer: std.ArrayList(std.macho.relocation_info),

    symbols: std.StringArrayHashMap(Symbol),
    zero_use_symbol_index: u32 = 0,
    structs: std.AutoHashMap(NodeIndex, StructLayout),
    expects_reg: ?u5 = null,
    storage_class: ?StorageClass.Type = null,

    fctx: ?FunctionContext = null,

    registers: [32]bool = [1]bool{false} ** 32,

    const Self = @This();

    pub fn init(unit: *Unit) !Self {
        var builder = try MachoBuilder.init(unit.allocator);

        const load_segment = try builder.addLoadSegment(4);
        const symtab_segment = try builder.addLoadSymTab();
        const text_section = builder.ptrToSection(load_segment, 0);
        @memset(&text_section.sectname, 0);
        @memset(&text_section.segname, 0);
        @memcpy(text_section.sectname[0.."__text".len], "__text");
        @memcpy(text_section.segname[0.."__TEXT".len], "__TEXT");

        text_section.addr = 0;
        text_section.size = 0;
        text_section.@"align" = 2;
        text_section.reloff = 0;
        text_section.nreloc = 0;
        text_section.flags = std.macho.S_REGULAR | std.macho.S_ATTR_PURE_INSTRUCTIONS | std.macho.S_ATTR_SOME_INSTRUCTIONS;
        text_section.reserved1 = 0;
        text_section.reserved2 = 0;
        text_section.reserved3 = 0;

        const data_section = builder.ptrToSection(load_segment, 1);
        @memset(&data_section.sectname, 0);
        @memset(&data_section.segname, 0);
        @memcpy(data_section.sectname[0.."__data".len], "__data");
        @memcpy(data_section.segname[0.."__DATA".len], "__DATA");

        data_section.addr = 0;
        data_section.size = 0;
        data_section.@"align" = 2;
        data_section.reloff = 0;
        data_section.nreloc = 0;
        data_section.flags = std.macho.S_REGULAR;
        data_section.reserved1 = 0;
        data_section.reserved2 = 0;
        data_section.reserved3 = 0;

        const common_section = builder.ptrToSection(load_segment, 2);
        @memset(&common_section.sectname, 0);
        @memset(&common_section.segname, 0);
        @memcpy(common_section.sectname[0.."__common".len], "__common");
        @memcpy(common_section.segname[0.."__DATA".len], "__DATA");

        common_section.addr = 0;
        common_section.size = 0;
        common_section.@"align" = 2;
        common_section.reloff = 0;
        common_section.nreloc = 0;
        common_section.flags = std.macho.S_REGULAR;
        common_section.reserved1 = 0;
        common_section.reserved2 = 0;
        common_section.reserved3 = 0;

        const cstring_section = builder.ptrToSection(load_segment, 3);
        @memset(&cstring_section.sectname, 0);
        @memset(&cstring_section.segname, 0);
        @memcpy(cstring_section.sectname[0.."__cstring".len], "__cstring");
        @memcpy(cstring_section.segname[0.."__TEXT".len], "__TEXT");

        cstring_section.addr = 0;
        cstring_section.size = 0;
        cstring_section.@"align" = 2;
        cstring_section.reloff = 0;
        cstring_section.nreloc = 0;
        cstring_section.flags = std.macho.S_REGULAR;
        cstring_section.reserved1 = 0;
        cstring_section.reserved2 = 0;
        cstring_section.reserved3 = 0;

        // const offset = try builder.writeSlice(&[_]u8{1, 2, 3, 4, 5});
        // section.offset = offset;
        // builder.ptrTo(std.macho.segment_command_64, load_segment).fileoff = offset;
        // builder.ptrTo(std.macho.segment_command_64, load_segment).filesize = 5;

        const sym_entry_offset = builder.currentOffset();
        builder.ptrTo(std.macho.symtab_command, symtab_segment).symoff = sym_entry_offset;
        // _ = try builder.writeSymbolEntry(symtab_segment, 0x6969, .{});
        const text_section_data = builder.currentOffset();
        builder.ptrTo(std.macho.segment_command_64, load_segment).fileoff = text_section_data;
        text_section.offset = text_section_data;
        try builder.alignForward(@alignOf(u32));

        return .{
            .allocator = unit.allocator,
            .unit = unit,
            .builder = builder,

            .data_section_buffer = .init(unit.allocator),
            .common_section_buffer = .init(unit.allocator),
            .cstring_section_buffer = .init(unit.allocator),
            .reloc_buffer = .init(unit.allocator),
            .structs = .init(unit.allocator),
            .segment_offset = load_segment,
            .symtab_offset = symtab_segment,
            .text_section_descriptor_offset = 0,

            .symbols = .init(unit.allocator),
        };
    }

    /// Inserts a symbol key/value in the symbol map.
    /// Must use this function as it preserves order of used symbols first, then unused
    /// I.e. if the sym is used more than zero times, it is in the first half of the array hash map
    /// If the symbol is unused it is in the later half.
    ///
    /// The purpose of this is to preserve symbol indicies for existing relocations.
    /// Also can early break out of symbol dumping loop
    pub fn insertSymbol(self: *Self, str: []const u8, sym: Symbol) !void {
        if (sym.uses == 0) {
            try self.symbols.put(str, sym);
        } else if (self.symbols.count() == 0) {
            try self.symbols.put(str, sym);
            self.zero_use_symbol_index = 1;
        } else if (self.zero_use_symbol_index == self.symbols.count()) {
            try self.symbols.put(str, sym);
            self.zero_use_symbol_index += 1;
        } else {
            try self.symbols.put(str, sym);
            const key = self.symbols.keys()[self.zero_use_symbol_index];
            const entry = self.symbols.fetchSwapRemove(key).?;
            try self.symbols.put(entry.key, entry.value);
            self.zero_use_symbol_index += 1;
        }
    }

    /// Increments symbol usage counter by 1.
    /// Must use this function as it preserves order of used symbols first, then unused
    /// Returns new index of the symbol (it can stay the same)
    pub fn incUsage(self: *Self, sym_index: usize) !usize {
        self.symbols.values()[sym_index].uses += 1;
        if (sym_index < self.zero_use_symbol_index) {
            return sym_index;
        } else {
            // c = sym_index
            // |0|1|2|3|4|5|6| index
            //        ^        zero_use_symbol_index
            //    a   b   c d  symbols
            // next:
            // |0|1|2|3|4|5|6|
            //        ^
            //    a   b   d c
            // |0|1|2|3|4|5|6|
            //        ^
            //    a   c   d b
            const key = self.symbols.keys()[sym_index];
            const sym_entry = self.symbols.fetchSwapRemove(key).?;
            try self.symbols.put(sym_entry.key, sym_entry.value);

            const zi_key = self.symbols.keys()[self.zero_use_symbol_index];
            const entry = self.symbols.fetchSwapRemove(zi_key).?;
            try self.symbols.put(entry.key, entry.value);

            self.zero_use_symbol_index += 1;
            return self.zero_use_symbol_index - 1;
        }
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
        const old_registers = self.registers;
        defer self.registers = old_registers;

        switch (node.kind) {
            .declaration => blk: {
                const old_sc = self.storage_class;
                defer self.storage_class = old_sc;

                var next_index = node.data.as(.two).a;
                const count = node.data.as(.four).c;
                const storage = node.data.as(.eight).g;
                if ((storage & StorageClass.typedef) > 0) break :blk;
                self.storage_class = storage;

                const end_index = next_index + count;
                while (next_index != end_index) : (next_index += 1) {
                    const node_index = self.unit.node_ranges.items[next_index];
                    _ = try self.genNode(node_index);
                }
            },
            .var_declaration_init => {
                const ident_index1 = node.data.two.a;
                const ident_str = self.unit.identifierAt(@bitCast(ident_index1));
                std.log.debug("vardeclinit {s}", .{ident_str});
                const decl_ty = self.unit.declared_type.get(nidx).?;
                const layout = self.computeLayout(decl_ty);

                const init_index = Node.absoluteIndex(nidx, node.data.as(.four).d);
                if (self.fctx == null) {
                    var eval = SimpleEvaluator.init(self.unit);
                    const init_value = try eval.evalNode(init_index);
                    const offset = self.data_section_buffer.items.len;

                    const aligned_slice = try self.data_section_buffer.addManyAsSlice(layout.max());
                    const size_slice = aligned_slice[0..layout.size];

                    if (decl_ty.isIntegral()) {
                        if (layout.size >= @sizeOf(u64)) {
                            std.mem.writeInt(u64, @ptrCast(size_slice.ptr), init_value.int_value, .little);
                        } else if (layout.size >= @sizeOf(u32)) {
                            std.mem.writeInt(u32, @ptrCast(size_slice.ptr), @intCast(init_value.int_value), .little);
                        } else if (layout.size >= @sizeOf(u16)) {
                            std.mem.writeInt(u16, @ptrCast(size_slice.ptr), @intCast(init_value.int_value), .little);
                        } else if (layout.size >= @sizeOf(u8)) {
                            std.mem.writeInt(u8, @ptrCast(size_slice.ptr), @intCast(init_value.int_value), .little);
                        }
                    }

                    const ident_index = node.data.two.a;
                    try self.insertSymbol(self.unit.identifierAt(@bitCast(ident_index)), Symbol{
                        // .address = undefined,
                        // .address = self.builder.currentOffsetInSection(self.segment_offset, 1),
                        .reloc_ty = .variable,
                        .address = @truncate(offset),
                        .section = 2,
                        .macho_ty = MachoSymbolType{
                            .external = true,
                            .ty = .section,
                        },
                        .uses = 1,
                    });
                } else {
                    self.fctx.?.next_offset = std.mem.alignForward(u32, self.fctx.?.next_offset, layout.alignment);
                    self.fctx.?.next_offset += layout.max();
                    const offset = -@as(i32, @intCast(self.fctx.?.next_offset));
                    try self.fctx.?.local_offsets.put(nidx, offset);
                    std.log.info("foo {}", .{offset});

                    const init_value = try self.genNodeExpr(init_index, decl_ty);
                    const value = try init_value.getValue(self, decl_ty, decl_ty, false, false);

                    if (value == .register) {
                        _ = try self.writeInst(try self.formStore(decl_ty, value.register, LOCAL_OFFSET_REGISTER, offset));
                    } else {
                        std.log.err("Invalid init vallue (todo probabyl)", .{});
                    }
                }
            },
            .var_declaration => {
                const ident_index = node.data.two.a;
                const ident_str = self.unit.identifierAt(@bitCast(ident_index));
                std.log.debug("vardecl {s}", .{ident_str});
                const decl_ty = self.unit.declared_type.get(nidx).?;
                const layout = self.computeLayout(decl_ty);

                if ((self.storage_class.? & StorageClass.@"extern") > 0) {
                    try self.insertSymbol(self.unit.identifierAt(@bitCast(ident_index)), Symbol{
                        .reloc_ty = .variable,
                        .address = 0,
                        .section = 0,
                        .macho_ty = MachoSymbolType{
                            .external = true,
                            .ty = .undef,
                        },
                    });
                } else if (self.fctx == null) {
                    const offset = self.common_section_buffer.items.len;
                    _ = try self.common_section_buffer.addManyAsSlice(layout.max());

                    try self.insertSymbol(self.unit.identifierAt(@bitCast(ident_index)), Symbol{
                        // .address = undefined,
                        .reloc_ty = .variable,
                        .address = @truncate(offset),
                        .section = 3,
                        .macho_ty = MachoSymbolType{
                            .external = true,
                            .ty = .section,
                        },
                        .uses = 1,
                    });
                } else {
                    self.fctx.?.next_offset = std.mem.alignForward(u32, self.fctx.?.next_offset, layout.alignment);
                    const offset = -@as(i32, @intCast(self.fctx.?.next_offset));
                    self.fctx.?.next_offset += layout.max();
                    try self.fctx.?.local_offsets.put(nidx, offset);
                }
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
            .function_declaration => {
                const ident_index = node.data.as(.two).a;
                try self.insertSymbol(self.unit.identifierAt(@bitCast(ident_index)), Symbol{
                    .reloc_ty = .function,
                    .address = 0,
                    .section = 0,
                    .macho_ty = MachoSymbolType{
                        .external = true,
                        .ty = .undef,
                    },
                });

                return .{ .inst = 0 };
            },
            .function_declaration_body => {
                const ident_index = node.data.as(.two).a;
                try self.insertSymbol(self.unit.identifierAt(@bitCast(ident_index)), Symbol{
                    .reloc_ty = .function,
                    .address = self.inst_count * 4,
                    .section = 1,
                    .macho_ty = MachoSymbolType{
                        .external = true,
                        .ty = .section,
                    },
                    .uses = 1,
                });

                // const old_registers = self.registers;
                // defer self.registers = old_registers;

                const fn_type = self.unit.declared_type.get(nidx).?;
                const old_fctx = self.fctx;
                defer self.fctx = old_fctx;
                self.fctx = FunctionContext{
                    .local_offsets = .init(self.allocator),
                    .return_type = fn_type.kind.func.ret_ty,
                    .next_offset = 0,
                };

                // prologue-epilogue layout
                // sub sp, sp, <size of local variables + 16 for x29 and x30 + size of additional stack size>
                // stp x29, x30, [sp, <size of additional stack size>]
                // add x29, sp, <size of local variables + 16 for x29 and x30 + size of additional stack size>
                //
                // ... body
                //
                // ldp x29, x30, [sp, <size of additional stack size>]
                // add sp, sp, <size of local variables + 16 + size of additional stack size>
                //
                // stackframe layout
                // | ----- Local Variables ----- | nl bytes
                // | -----     X29, X30    ----- | 16 bytes
                // | ----  Additional Stack ---- | na bytes
                // total stack size = aligned(nl + 16) + aligned(na)
                // alignment is 16 bytes for aarch64

                const sub_sp_index_1 = try self.writeInst(0);
                const stp_x29_x30 = try self.writeInst(0);
                const mov_x29_sp = try self.writeInst(0);

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
                                self.fctx.?.next_offset += layout.max();
                                const offset = -@as(i32, @intCast(self.fctx.?.next_offset));
                                try self.fctx.?.local_offsets.put(param_nidx, 0);
                                _ = try self.writeInst(try self.formStore(
                                    fn_param_types[i],
                                    0,
                                    SP,
                                    offset,
                                ));
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

                                    // Parameters after w7 are already on stack
                                    if (i < X8) {
                                        self.fctx.?.next_offset = std.mem.alignForward(u32, self.fctx.?.next_offset, layout.alignment);
                                        const offset = -@as(i32, @intCast(self.fctx.?.next_offset));
                                        try self.fctx.?.local_offsets.put(param_nidx, offset);
                                        _ = try self.writeInst(try self.formStore(
                                            fn_param_types[i],
                                            @truncate(i),
                                            SP,
                                            offset,
                                        ));

                                        self.fctx.?.next_offset += layout.max();
                                    } else {
                                        // try self.fctx.?.local_offsets.put(param_nidx, self.fctx.?.next_offset);
                                        @panic("todo: handle this");
                                    }
                                },
                                .parameter_ellipsis => {},
                                else => @panic("compielr bug"),
                            }

                            i += 1;
                        }
                    },
                    else => @panic("compiler bug"),
                }

                const body_index = Node.absoluteIndex(nidx, node.data.as(.four).d);
                try self.genCompound(body_index);

                const stack_frame_size = std.mem.alignForward(u32, self.fctx.?.next_offset + 16, 16);
                const additional_max_size = std.mem.alignForward(u32, self.fctx.?.max_additional_stack, 16);
                // const total_stack_size = std.mem.alignForward(u32, self.fctx.?.next_offset + self.fctx.?.max_additional_stack, 16);
                const total_stack_size = stack_frame_size + additional_max_size;

                _ = try self.writeInstAt(dataProcImm(.add_sub, .{
                    .sfopSshimm12rnrd = .{
                        .op = 1,
                        .S = false,
                        .imm = @truncate(total_stack_size),
                        .rn = SP,
                        .rd = SP,
                    },
                }), sub_sp_index_1);

                _ = try self.writeInstAt(loadStore(.load_store_pair_offset, 0, 0b10, 0, 0, .{
                    .opcVLimmRt2RnRt = .{
                        .rt = X29,
                        .rn = SP,
                        .rt2 = X30,
                        .imm = @intCast(additional_max_size / 8),
                        .L = 0,
                        .V = 0,
                        .opc = 0b10,
                    },
                }), stp_x29_x30);

                _ = try self.writeInstAt(dataProcImm(.add_sub, .{
                    .sfopSshimm12rnrd = .{
                        .rd = LOCAL_OFFSET_REGISTER,
                        .rn = SP,
                        .imm = @truncate(total_stack_size), // +16 for x29 and x30
                        .S = false,
                        .op = 0,
                    },
                }), mov_x29_sp);

                _ = try self.writeInst(loadStore(.load_store_pair_offset, 0, 0b10, 0, 0, .{
                    .opcVLimmRt2RnRt = .{
                        .rt = X29,
                        .rn = SP,
                        .rt2 = X30,
                        .imm = @intCast(additional_max_size / 8),
                        .L = 1,
                        .V = 0,
                        .opc = 0b10,
                    },
                }));

                _ = try self.writeInst(dataProcImm(.add_sub, .{
                    .sfopSshimm12rnrd = .{
                        .op = 0,
                        .S = false,
                        .imm = @truncate(total_stack_size),
                        .rn = SP,
                        .rd = SP,
                    },
                }));

                if (false) {
                    if (self.builder.currentOffset() == sub_sp_index_1 + 4) {
                        // repurpose this for ret
                        _ = try self.writeInstAt(brExcSys(.un_cond_br_reg, .{
                            .opcop23Rnop4 = .{
                                .rn = LR,
                                .op3 = 0,
                                .opc = 0b10,
                            },
                        }), sub_sp_index_1);
                    } else {
                        _ = try self.writeInstAt(brExcSys(.hints, .{
                            .crmop2 = .{
                                .op2 = 0,
                                .crm = 0,
                            },
                        }), sub_sp_index_1);
                        _ = try self.writeInst(brExcSys(.un_cond_br_reg, .{
                            .opcop23Rnop4 = .{
                                .rn = LR,
                                .op3 = 0,
                                .opc = 0b10,
                            },
                        }));
                    }
                } else {
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
                const start_reloc = self.reloc_buffer.items.len;
                const start_index = self.builder.currentOffset();
                const result = try self.genNodeExpr(nidx, null);
                // Must remove last relocation
                switch (result) {
                    .reloc_offset => {
                        self.reloc_buffer.items.len = start_reloc;
                    },
                    .reloc_label => {
                        self.reloc_buffer.items.len = start_reloc;
                        self.builder.restoreOffset(start_index);
                    },
                    else => {},
                }
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

    /// nidx should be index to string_literal_join or string_literal
    pub fn genStringJoin(self: *Self, nidx: NodeIndex) !void {
        const node = &self.unit.nodes.items[nidx];

        switch (node.kind) {
            .string_literal => {
                _ = try self.unit.writeStringToBuffer(@bitCast(node.data.two.a), self.cstring_section_buffer.writer());
            },
            .stringified_literal => {
                _ = try self.unit.writeStringifiedToBuffer(@bitCast(node.data.two.a), self.cstring_section_buffer.writer());
            },
            .string_literal_join => {
                try self.genStringJoin(node.data.two.a);

                const this_node = self.unit.token(@bitCast(node.data.two.b));
                switch (this_node.kind) {
                    .string_literal => _ = try self.unit.writeStringToBuffer(@bitCast(node.data.two.b), self.cstring_section_buffer.writer()),
                    .stringified_literal => _ = try self.unit.writeStringifiedToBuffer(@bitCast(node.data.two.b), self.cstring_section_buffer.writer()),
                    else => unreachable,
                }
            },
            else => unreachable,
        }
    }

    pub fn isInvokeeBuiltin(self: *Self, nidx: NodeIndex) bool {
        const node = &self.unit.nodes.items[nidx];
        if (node.kind == .identifier) {
            const ident_str = self.unit.identifierAt(@bitCast(node.data.two.a));
            return BuiltinsCodeGen.map.has(ident_str);
        }
        return false;
    }

    pub fn genNodeExpr(self: *Self, nidx: NodeIndex, result_ty: ?Type) !Value {
        // _ = result_ty;
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
            .string_literal, .string_literal_join, .stringified_literal => {
                const offset = self.cstring_section_buffer.items.len;
                try self.genStringJoin(nidx);
                try self.cstring_section_buffer.append(0);

                var symbol_value_buffer = std.ArrayList(u8).init(self.unit.allocator);
                try std.fmt.format(symbol_value_buffer.writer(), ".cstr{}", .{self.symbols.count()});
                const sym_index = self.symbols.count();
                try self.insertSymbol(symbol_value_buffer.items, Symbol{
                    .reloc_ty = .variable,
                    .address = @truncate(offset),
                    .section = 4,
                    .macho_ty = MachoSymbolType{
                        .external = true,
                        .ty = .section,
                    },
                    .uses = 1,
                });

                try self.reloc_buffer.append(std.macho.relocation_info{
                    .r_symbolnum = @truncate(sym_index),
                    .r_address = @intCast(self.builder.currentOffsetInSection(self.segment_offset, 0)),
                    .r_extern = 1,
                    .r_length = 2,
                    .r_pcrel = 1,
                    .r_type = @intFromEnum(std.macho.reloc_type_arm64.ARM64_RELOC_PAGE21),
                });

                const out_reg = self.availableRegister().?;
                self.useRegister(out_reg);

                _ = try self.writeInst(dataProcImm(.pc_rel, .{
                    .opimmloimmhird = .{
                        .rd = out_reg,
                        .immlo = 0,
                        .immhi = 0,
                        .op = 1,
                    },
                }));

                return .{
                    .reloc_offset = .{
                        .offset = @intCast(sym_index),
                        .register = out_reg,
                    },
                };
            },
            .identifier => {
                // const refed_nidx = self.unit.node_to_node.get(nidx).?;
                // const local = self.fctx.?.local_offsets.get(refed_nidx).?;

                // const ident_ty = self.unit.node_to_type.get(nidx).?;
                // const out_reg = self.availableRegister().?;
                // self.useRegister(out_reg);

                const str = self.unit.identifierAt(@bitCast(node.data.two.a));
                if (BuiltinsCodeGen.map.get(str)) |fptr| {
                    return .{ .builtin = fptr };
                }
                const referred_nidx = self.unit.node_to_node.get(nidx).?;

                if (self.fctx.?.local_offsets.get(referred_nidx)) |vr| {
                    return .{
                        .memory_offset = .{
                            .offset = vr,
                            .register = LOCAL_OFFSET_REGISTER,
                        },
                    };
                } else if (self.symbols.getIndex(str)) |sym_index| {
                    const sym = self.symbols.values()[sym_index];
                    const new_sym_index = try self.incUsage(sym_index);
                    try self.reloc_buffer.append(std.macho.relocation_info{
                        .r_symbolnum = @truncate(new_sym_index),
                        .r_address = @intCast(self.builder.currentOffsetInSection(self.segment_offset, 0)),
                        .r_extern = 1,
                        .r_length = 2,
                        .r_pcrel = 1,
                        // .r_type = @intFromEnum(std.macho.reloc_type_arm64.ARM64_RELOC_PAGE21),
                        .r_type = switch (sym.reloc_ty) {
                            .variable => @intFromEnum(std.macho.reloc_type_arm64.ARM64_RELOC_PAGE21),
                            .function => if (result_ty == null)
                                @intFromEnum(std.macho.reloc_type_arm64.ARM64_RELOC_BRANCH26)
                            else
                                @intFromEnum(std.macho.reloc_type_arm64.ARM64_RELOC_PAGE21),
                        },
                    });
                    switch (sym.reloc_ty) {
                        .variable => {
                            const out_reg = self.availableRegister().?;
                            self.useRegister(out_reg);
                            // const addr: u31 =
                            _ = try self.writeInst(dataProcImm(.pc_rel, .{
                                .opimmloimmhird = .{
                                    .rd = out_reg,
                                    .immlo = 0,
                                    .immhi = 0,
                                    .op = 1,
                                },
                            }));
                            return .{
                                .reloc_offset = .{
                                    .offset = @intCast(new_sym_index),
                                    .register = out_reg,
                                },
                            };
                        },
                        .function => {
                            // const out_reg = self.availableRegister().?;
                            // self.useRegister(out_reg);
                            // if (result_ty == null) {
                            //     _ = try self.writeInst(dataProcImm(.pc_rel, .{
                            //         .opimmloimmhird = .{
                            //             .rd = out_reg,
                            //             .immlo = 0,
                            //             .immhi = 0,
                            //             .op = 1,
                            //         },
                            //     }));
                            //     return .{
                            //         .reloc_offset = .{
                            //             .offset = @truncate(sym_index),
                            //             .register = out_reg,
                            //         },
                            //     };
                            // }

                            return .reloc_label;
                        },
                    }
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
                        const lvalue = try self.genNodeExpr(node.data.two.a, null);
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
                                        .offset = mo.offset + @as(i32, @intCast(field_offset)),
                                        .register = mo.register,
                                    },
                                };
                            },
                            else => @panic(""),
                        }
                    },
                    .assignment => {
                        const rvalue = try self.genNode(Node.absoluteIndex(nidx, node.data.as(.four).c));
                        var lvalue = try self.genNodeExpr(node.data.as(.two).a, null);
                        switch (lvalue) {
                            .memory_offset, .reloc_offset => |*mo| {
                                const ty = self.unit.node_to_type.get(node.data.two.a).?;
                                const rrvalue = try rvalue.getValue(self, ty, null, false, false);

                                if (lvalue == .reloc_offset) {
                                    try self.reloc_buffer.append(std.macho.relocation_info{
                                        .r_symbolnum = @intCast(mo.offset),
                                        .r_address = @intCast(self.builder.currentOffsetInSection(self.segment_offset, 0)),
                                        .r_extern = 1,
                                        .r_length = 2,
                                        .r_pcrel = 0,
                                        .r_type = @intFromEnum(std.macho.reloc_type_arm64.ARM64_RELOC_PAGEOFF12),
                                    });
                                    mo.offset = 0;
                                }

                                if (ty.isIntegral()) {
                                    _ = try self.writeInst(try self.formStore(ty, rrvalue.register, mo.register, mo.offset));
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
            .invoke => {
                const invokee_val = try self.genNodeExpr(node.data.two.a, null);
                if (invokee_val == .builtin) {
                    return invokee_val.builtin(self, &.{});
                }
                self.fctx.?.sub_invocations += 1;

                switch (invokee_val) {
                    .reloc_label => {
                        _ = try self.writeInst(brExcSys(.un_cond_br_imm, .{
                            .opimm26 = .{
                                .imm = 0,
                                .op = 1,
                            },
                        }));
                    },
                    else => @panic("todo"),
                }

                self.useRegister(X0);
                return .{ .register = X0 };
            },
            .invoke_one_arg => {
                if (self.isInvokeeBuiltin(node.data.two.a)) {
                    const invokee_val = try self.genNodeExpr(node.data.two.a, null);
                    return invokee_val.builtin(self, &.{node.data.two.b});
                }
                self.fctx.?.sub_invocations += 1;
                const fn_type = self.unit.node_to_type.get(node.data.two.a).?;
                const param_types = self.unit.interner.getMultiTypes(fn_type.kind.func.params);

                const arg_ty = self.unit.node_to_type.get(node.data.two.b).?;

                const old_ex_reg = self.expects_reg;
                self.expects_reg = X0;
                defer self.expects_reg = old_ex_reg;

                const pre_arg_val = try self.genNodeExpr(node.data.two.b, param_types[0]);
                const arg_val = try pre_arg_val.getValue(self, arg_ty, param_types[0], true, false);

                try self.writeLoadArg(arg_val, arg_ty, X0);

                // must do this after args for correct relocation
                const invokee_val = try self.genNodeExpr(node.data.two.a, null);
                switch (invokee_val) {
                    .reloc_label => {
                        _ = try self.writeInst(brExcSys(.un_cond_br_imm, .{
                            .opimm26 = .{
                                .imm = 0,
                                .op = 1,
                            },
                        }));
                    },
                    else => @panic("todo"),
                }

                self.useRegister(X0);
                return .{ .register = X0 };
            },
            .invoke_args => {
                const invokee_index = Node.absoluteIndex(nidx, node.data.four.a);
                if (self.isInvokeeBuiltin(invokee_index)) {
                    const invokee_val = try self.genNodeExpr(invokee_index, null);
                    return invokee_val.builtin(self, self.unit.node_ranges.items[node.data.two.b..node.data.two.b+node.data.four.b]);
                }
                self.fctx.?.sub_invocations += 1;
                const fn_type = self.unit.node_to_type.get(invokee_index).?;
                const param_types = self.unit.interner.getMultiTypes(fn_type.kind.func.params);

                var index = node.data.two.b;
                const end_index = index + param_types.len;
                const arg_end_index = index + node.data.four.b;

                var count: u32 = 0;

                const old_ex_reg = self.expects_reg;
                defer self.expects_reg = old_ex_reg;
                while (index < end_index) : ({
                    index += 1;
                    count += 1;
                }) {
                    const node_index = self.unit.node_ranges.items[index];
                    const arg_ty = self.unit.node_to_type.get(node_index).?;

                    if (count < X8) {
                        self.expects_reg = @truncate(count);
                        self.useRegister(@truncate(count));

                        const old_registers = self.registers;
                        defer self.registers = old_registers;
                        const pre_arg_val = try self.genNodeExpr(node_index, param_types[count]);
                        const arg_val = try pre_arg_val.getValue(self, arg_ty, param_types[count], true, false);

                        try self.writeLoadArg(arg_val, arg_ty, @truncate(count));
                    } else {
                        @panic("todo");
                    }
                }

                const temp_reg = self.availableRegister().?;
                self.useRegister(temp_reg);
                var stack_offset: u32 = 0;
                // va arg values
                while (index < arg_end_index) : ({
                    index += 1;
                    count += 1;
                }) {
                    const node_index = self.unit.node_ranges.items[index];
                    const arg_ty = self.unit.node_to_type.get(node_index).?;

                    const layout = self.computeLayout(arg_ty);
                    stack_offset = std.mem.alignForward(u32, stack_offset, layout.alignment);

                    const pre_arg_val = try self.genNodeExpr(node_index, null);
                    const arg_val = try pre_arg_val.getValue(self, arg_ty, null, true, false);
                    try self.writeLoadArg(arg_val, arg_ty, temp_reg);

                    _ = try self.writeInst(try self.formStore(arg_ty, temp_reg, SP, @intCast(stack_offset)));

                    stack_offset += @intCast(layout.max());
                }
                self.fctx.?.max_additional_stack = @max(self.fctx.?.max_additional_stack, stack_offset);

                // must do this after args for correct relocation
                const invokee_val = try self.genNodeExpr(invokee_index, null);
                switch (invokee_val) {
                    .reloc_label => {
                        _ = try self.writeInst(brExcSys(.un_cond_br_imm, .{
                            .opimm26 = .{
                                .imm = 0,
                                .op = 1,
                            },
                        }));
                    },
                    else => @panic("todo"),
                }

                self.useRegister(X0);
                return .{ .register = X0 };
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
                                .offset = @as(i32, @intCast(val)) * @as(i32, @intCast(elem_layout.size)),
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
            else => std.log.warn("Skipping node {} kind: {}", .{ nidx, self.unit.nodes.items[nidx].kind }),
        }

        return .{ .inst = 0 };
    }

    pub fn writeLoadArg(self: *Self, arg_val: Value, arg_ty: Type, arg_reg: ?u5) !void {
        switch (arg_val) {
            .immediate => |val| {
                const layout = self.computeLayout(arg_ty);
                const inst = CodeGenerator.dataProcImm(.move, .{
                    .sfopchwimm16rd = .{
                        .rd = arg_reg.?,
                        .imm = @truncate(val),
                        .hw = 0,
                        .opc = 0b10,
                        .sf = layout.size >= 8,
                    },
                });
                _ = try self.writeInst(inst);
            },
            .register => |reg| {
                if (arg_val.register != arg_reg.?) {
                    _ = try self.writeInst(dataProcReg(.logical_shift, .{
                        .sfopcshNRmimmRnRd = .{
                            .rd = X0,
                            .rn = X31,
                            .imm = 0,
                            .rm = reg,
                            .N = false,
                            .sh = 0,
                            .opc = 0b01,
                        },
                    }));
                }
            },
            else => @panic("todo"),
        }
    }

    // pub fn genNodeLvalue(self: *Self, nidx: NodeIndex) !Value {
    //     const node = &self.unit.nodes.items[nidx];
    //     switch (node.kind) {
    //         .identifier => {
    //             const referred_nidx = self.unit.node_to_node.get(nidx).?;
    //             const str = self.unit.identifierAt(@bitCast(node.data.two.a));
    //             if (self.fctx.?.local_offsets.get(referred_nidx)) |vr| {
    //                 return .{
    //                     .memory_offset = .{
    //                         .offset = vr,
    //                         .register = SP,
    //                     },
    //                 };
    //             } else if (self.symbols.getIndex(str)) |sym| {
    //                 try self.reloc_buffer.append(std.macho.relocation_info{
    //                     .r_symbolnum = @truncate(sym),
    //                     .r_address = @intCast(self.builder.currentOffsetInSection(self.segment_offset, 0)),
    //                     .r_extern = 1,
    //                     .r_length = 2,
    //                     .r_pcrel = 0,
    //                     .r_type = @intFromEnum(std.macho.reloc_type_arm64.ARM64_RELOC_PAGE21),
    //                 });
    //                 return .{
    //                     .memory_offset = .{
    //                         .offset = 0,
    //                         .register = 0,
    //                     },
    //                 };
    //             }

    //             @panic("unhandled lvalue");
    //         },
    //         .binary_lr_operator => {
    //             const old_expects_reg = self.expects_reg;
    //             defer self.expects_reg = old_expects_reg;
    //             self.expects_reg = null;

    //             const op: TokenKind = @enumFromInt(node.data.as(.four).d);
    //             switch (op) {
    //                 .dot => {
    //                     const ltype = self.unit.node_to_type.get(node.data.two.a).?;
    //                     _ = self.computeLayout(ltype);
    //                     const lvalue = try self.genNodeLvalue(node.data.two.a);
    //                     // const refed_node = self.unit.node_to_node.get(Node.absoluteIndex(nidx, node.data.four.c)).?;

    //                     const right_node = &self.unit.nodes.items[Node.absoluteIndex(nidx, node.data.four.c)];
    //                     const field_str = self.unit.identifierAt(@bitCast(right_node.data.two.a));

    //                     const strc_nidx = ltype.getStructureIndex();
    //                     const field_offset = switch (ltype.kind) {
    //                         .@"struct", .unnamed_struct => blk2: {
    //                             const field_map = self.unit.field_map.getPtr(strc_nidx).?;
    //                             const field_index = field_map.get(field_str).?;
    //                             const strct = self.structs.getPtr(strc_nidx).?;
    //                             const field_offset = strct.offsets.get(field_index).?;

    //                             break :blk2 field_offset;
    //                         },
    //                         .@"union", .unnamed_union => blk2: {
    //                             break :blk2 0;
    //                         },
    //                         else => unreachable,
    //                     };

    //                     switch (lvalue) {
    //                         .memory_offset => |mo| {
    //                             return .{
    //                                 .memory_offset = .{
    //                                     .offset = mo.offset + field_offset,
    //                                     .register = mo.register,
    //                                 },
    //                             };
    //                         },
    //                         else => @panic(""),
    //                     }
    //                 },
    //                 else => {},
    //             }
    //             const rvalue = try self.genNode(Node.absoluteIndex(nidx, node.data.as(.four).c));
    //             _ = rvalue;
    //             @panic("foo");
    //         },
    //         else => @panic("unhandled node"),
    //     }
    // }

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
            .array_unsized => .{ .size = 8, .alignment = 8 },
            .@"struct", .unnamed_struct, .builtin_struct => blk: {
                const fields = switch (ty.kind) {
                    .@"struct" => |st| self.unit.interner.getMultiTypes(st.fields),
                    .unnamed_struct => |st| self.unit.interner.getMultiTypes(st.fields),
                    .builtin_struct => |st| self.unit.interner.getMultiTypes(st.fields),
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

                switch (ty.kind) {
                    .@"struct" => |st| {
                        self.structs.put(st.nidx, .{ .offsets = field_offsets }) catch @panic("oom");
                    },
                    .unnamed_struct => |st| {
                        self.structs.put(st.nidx, .{ .offsets = field_offsets }) catch @panic("oom");
                    },
                    .builtin_struct => {},
                    else => unreachable,
                }

                offset = std.mem.alignForward(u32, offset, largest_alignment);
                break :blk .{ .size = offset, .alignment = largest_alignment };
            },
            .@"union", .unnamed_union, .builtin_union => blk: {
                const fields = switch (ty.kind) {
                    .@"union" => |st| self.unit.interner.getMultiTypes(st.variants),
                    .unnamed_union => |st| self.unit.interner.getMultiTypes(st.variants),
                    .builtin_union => |st| self.unit.interner.getMultiTypes(st.variants),
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
            else => std.debug.panic("unable to calculate memory layout of type {s}", .{
                self.unit.interner.printTyToStr(ty, self.allocator),
            }),
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

    pub fn writeCastLoad(self: *Self, ty: Type, result_ty: Type, out_reg: u5, offset_reg: u5, offset: i32) !void {
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
            const div = @as(i32, 1) << sz;
            if (ty.isSigned() and result_ty.rank() > ty.rank()) {
                if (result_ty.rank() > self.unit.interner.intTy(false, 0).rank()) {
                    opc = 0b10;
                } else {
                    opc = 0b11;
                }
            } else if (ty.rank() > self.unit.interner.intTy(false, 0).rank()) {
                sz = 0b11;
            }

            if (OPS.fitsIn(.szVopcimm9RnRt, .imm, offset)) {
                // store unsacled immediate
                _ = try self.writeInst(loadStore(.load_store_register, 0, 0b00, 0, 0, .{
                    .szVopcimm9RnRt = .{
                        .rt = out_reg,
                        .rn = offset_reg,
                        .imm = @truncate(offset),
                        .opc = opc,
                        .V = false,
                        .sz = sz,
                    },
                }));
            } else if (OPS.fitsIn(.szVopcimm12rnrt, .imm, @divTrunc(offset, div))) {
                // store unsigned immediate
                _ = try self.writeInst(loadStore(.load_store_register, 0, 0b10, 0, 0, .{
                    .szVopcimm12rnrt = .{
                        .rt = out_reg,
                        .rn = offset_reg,
                        .imm = @intCast(@divTrunc(offset, div)),
                        .opc = opc,
                        .V = false,
                        .sz = sz,
                    },
                }));
            } else @panic("todo");

            return;
        }

        if (ty.kind == .array and result_ty.kind == .pointer) {
            _ = try self.writeInst(dataProcImm(.add_sub, .{
                .sfopSshimm12rnrd = .{
                    .rd = out_reg,
                    .rn = out_reg,
                    .imm = 0,
                    .S = false,
                    .op = 0,
                },
            }));
            return;
        }

        unreachable;
    }

    pub fn formLoad(ty: Type, out_reg: u5, offset_reg: u5, offset: i32) u32 {
        const sz: u2, const divide: i32 = switch (ty.kind) {
            .char => .{ 0b00, 1 },
            .short => .{ 0b01, 2 },
            .int => .{ 0b10, 4 },
            .long => .{ 0b11, 8 },
            .longlong => .{ 0b11, 8 },
            .pointer => .{ 0b11, 8 },
            else => unreachable,
        };

        if (OPS.fitsIn(.szVopcimm9RnRt, .imm, offset)) {
            // store unsacled immediate
            return loadStore(.load_store_register, 0, 0b00, 0, 0, .{
                .szVopcimm9RnRt = .{
                    .rt = out_reg,
                    .rn = offset_reg,
                    .imm = @truncate(offset),
                    .opc = 0b01,
                    .V = false,
                    .sz = sz,
                },
            });
        } else if (OPS.fitsIn(.szVopcimm12rnrt, .imm, @divTrunc(offset, divide))) {
            // store unsigned immediate
            return loadStore(.load_store_register, 0, 0b10, 0, 0, .{
                .szVopcimm12rnrt = .{
                    .rt = out_reg,
                    .rn = offset_reg,
                    .imm = @intCast(@divTrunc(offset, divide)),
                    .opc = 0b01,
                    .V = false,
                    .sz = sz,
                },
            });
        } else @panic("todo");
    }

    pub fn formStore(self: *Self, ty: Type, in_reg: u5, offset_reg: u5, offset: i32) !u32 {
        const sz: u2, const divide: i32 = switch (ty.kind) {
            .char => .{ 0b00, 1 },
            .short => .{ 0b01, 2 },
            .int => .{ 0b10, 4 },
            .long => .{ 0b11, 8 },
            .longlong => .{ 0b11, 8 },
            .pointer => .{ 0b11, 8 },
            else => unreachable,
        };

        if (OPS.fitsIn(.szVopcimm9RnRt, .imm, offset)) {
            // store unsacled immediate
            return loadStore(.load_store_register, 0, 0b00, 0, 0, .{
                .szVopcimm9RnRt = .{
                    .rt = in_reg,
                    .rn = offset_reg,
                    .imm = @truncate(offset),
                    .opc = 0b00,
                    .V = false,
                    .sz = sz,
                },
            });
        } else if (OPS.fitsIn(.szVopcimm12rnrt, .imm, offset)) {
            // store unsigned immediate
            return loadStore(.load_store_register, 0, 0b10, 0, 0, .{
                .szVopcimm12rnrt = .{
                    .rt = in_reg,
                    .rn = offset_reg,
                    .imm = @intCast(@divTrunc(offset, divide)),
                    .opc = 0b00,
                    .V = false,
                    .sz = sz,
                },
            });
        } else {
            const new_offset_reg = self.availableRegister().?;
            self.useRegister(new_offset_reg);

            if (offset < 0) {
                const add_sub_max: i32 = OPS.maxValue(.sfopSshimm12rnrd, .imm);

                var additional_offset = -offset;
                _ = try self.writeInst(dataProcImm(.add_sub, .{
                    .sfopSshimm12rnrd = .{
                        .rd = new_offset_reg,
                        .rn = offset_reg,
                        .imm = @intCast(@min(add_sub_max, additional_offset)),
                        .S = false,
                        .op = 1,
                    },
                }));
                additional_offset -= @min(add_sub_max, additional_offset);

                while (additional_offset > 0) {
                    _ = try self.writeInst(dataProcImm(.add_sub, .{
                        .sfopSshimm12rnrd = .{
                            .rd = new_offset_reg,
                            .rn = new_offset_reg,
                            .imm = @intCast(@min(add_sub_max, additional_offset)),
                            .S = false,
                            .op = 1,
                        },
                    }));
                    additional_offset -= @min(add_sub_max, additional_offset);
                }
            } else {
                @panic("todo");
            }

            // store unsigned immediate
            return loadStore(.load_store_register, 0, 0b10, 0, 0, .{
                .szVopcimm12rnrt = .{
                    .rt = in_reg,
                    .rn = new_offset_reg,
                    .imm = 0,
                    .opc = 0b00,
                    .V = false,
                    .sz = sz,
                },
            });
        }
    }

    pub fn loadStore(op0: enum(u4) {
        load_store_register = 0b0011,
        load_store_pair_offset = 0b0010,
    }, op1: u1, op2: u2, op3: u6, op4: u2, data: OPS) u32 {
        return @as(u32, 0x08000000) | (@as(u32, @intFromEnum(op0)) << 28) | (@as(u32, op1) << 26) | (@as(u32, op2) << 23) | (@as(u32, op3) << 16) | (@as(u32, op4) << 10) | @as(u32, @bitCast(data));
    }

    pub fn dataProcImm(op: enum(u32) {
        pc_rel = 0x00000000,
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
        hints = 0xD503201F,
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
        szVopcimm9RnRt: packed struct(u32) {
            rt: u5,
            rn: u5,
            _1: u2 = 0,
            imm: i9,
            _2: u1 = 0,
            opc: u2,
            _3: u2 = 0,
            V: bool,
            _4: u3 = 0,
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
        opimmloimmhird: packed struct(u32) {
            rd: u5,
            immhi: u19,
            _1: u5 = 0,
            immlo: u2,
            op: u1,
        },
        crmop2: packed struct(u32) {
            _1: u5 = 0,
            op2: u3,
            crm: u4,
            _2: u20 = 0,
        },
        opimm26: packed struct(u32) {
            imm: u26,
            _1: u5 = 0,
            op: u1,
        },
        opcVLimmRt2RnRt: packed struct(u32) {
            rt: u5,
            rn: u5,
            rt2: u5,
            imm: i7,
            L: u1,
            _1: u3 = 0,
            V: u1,
            _2: u3 = 0,
            opc: u2,
        },

        pub fn maxValue(
            comptime field: std.meta.FieldEnum(OPS),
            comptime sub_field: std.meta.FieldEnum(std.meta.FieldType(OPS, field)),
        ) comptime_int {
            const FieldType = std.meta.FieldType(std.meta.FieldType(OPS, field), sub_field);
            return std.math.maxInt(FieldType);
        }

        pub fn fitsIn(
            comptime field: std.meta.FieldEnum(OPS),
            comptime sub_field: std.meta.FieldEnum(std.meta.FieldType(OPS, field)),
            value: anytype,
        ) bool {
            const FieldType = std.meta.FieldType(std.meta.FieldType(OPS, field), sub_field);
            const field_info = @typeInfo(FieldType);
            const ValueType = @TypeOf(value);
            const value_info = @typeInfo(ValueType);
            if (value_info != .int) @compileError("Expected value to be integer!");
            if (field_info != .int) @compileError("Expected field to be integer!");

            if (ValueType == FieldType) return true
            else if (value_info.int.signedness == field_info.int.signedness) {
                std.log.info(" value {}", .{value});
                if (value_info.int.bits < field_info.int.bits) {
                    return true;
                } else if (value_info.int.signedness == .signed) {
                    if (value > 0) {
                        return value < std.math.maxInt(FieldType);
                    } else {
                        return value > std.math.minInt(FieldType);
                    }
                } else {
                    return value < std.math.maxInt(FieldType);
                }
            } else if (value_info.int.signedness == .unsigned and field_info.int.signedness == .signed) {
                std.log.info(" value {}", .{value});
                if (value_info.int.bits < field_info.int.bits) {
                    return true;
                } else {
                    return (value >> (value_info.int.bits - 1) == 0);
                }
            } else {
                std.log.info("signed value {}", .{value});
                if (value < 0) return false
                else return value < std.math.maxInt(FieldType);
            }
        }
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
        const total_length = self.inst_count * @sizeOf(u32) + self.data_section_buffer.items.len + self.common_section_buffer.items.len + self.cstring_section_buffer.items.len;
        try self.builder.buffer.ensureUnusedCapacity(
            total_length,
        );
        self.builder.ptrTo(std.macho.segment_command_64, self.segment_offset).filesize = total_length;
        self.builder.ptrTo(std.macho.segment_command_64, self.segment_offset).vmsize = total_length;

        const text_section = self.builder.ptrToSection(self.segment_offset, 0);
        text_section.size = self.inst_count * @sizeOf(u32);

        if (self.reloc_buffer.items.len > 0) {
            text_section.reloff = self.builder.currentOffset();
            text_section.nreloc = @truncate(self.reloc_buffer.items.len);
            _ = try self.builder.writeSlice(self.reloc_buffer.items);
        }

        const data_section = self.builder.ptrToSection(self.segment_offset, 1);
        data_section.addr = text_section.addr + text_section.size;
        data_section.offset = self.builder.currentOffset();
        data_section.size = self.data_section_buffer.items.len;
        try self.builder.buffer.appendSlice(self.data_section_buffer.items);

        const common_section = self.builder.ptrToSection(self.segment_offset, 2);
        common_section.addr = data_section.addr + data_section.size;
        common_section.offset = self.builder.currentOffset();
        common_section.size = self.common_section_buffer.items.len;
        try self.builder.buffer.appendSlice(self.common_section_buffer.items);

        const cstring_section = self.builder.ptrToSection(self.segment_offset, 3);
        cstring_section.addr = common_section.addr + common_section.size;
        cstring_section.offset = self.builder.currentOffset();
        cstring_section.size = self.cstring_section_buffer.items.len;
        try self.builder.buffer.appendSlice(self.cstring_section_buffer.items);

        const sections = [_]*align(1) std.macho.section_64{ text_section, data_section, common_section, cstring_section };
        // const sections = [_]*align(1) std.macho.section_64{ text_section };

        // const syms_len = self.builder.currentOffset() - syms_offset;

        const str_offset = self.builder.currentOffset();
        var sym_it = self.symbols.iterator();
        _ = try self.builder.writeInt(@as(u8, 0));
        while (sym_it.next()) |sym| {
            // Since unsued symbols are all at the end of the array map, we can early-exit
            // as we don't need to write unused symbols
            if (sym.value_ptr.uses == 0) break;
            _ = try self.builder.writeInt(@as(u8, '_'));
            _ = try self.builder.writeSlice(sym.key_ptr.*);
            _ = try self.builder.writeInt(@as(u8, 0));
        }
        const str_len = self.builder.currentOffset() - str_offset;

        sym_it = self.symbols.iterator();
        var next_offset: u32 = 1;
        const syms_offset = self.builder.currentOffset();
        while (sym_it.next()) |sym| {
            // Since unsued symbols are all at the end of the array map, we can early-exit
            // as we don't need to write unused symbols
            if (sym.value_ptr.uses == 0) break;
            const offset = try self.builder.writeSymbolEntry(self.symtab_offset, next_offset, sym.value_ptr.macho_ty);
            self.builder.ptrTo(std.macho.nlist_64, offset).n_sect = @truncate(sym.value_ptr.section);
            if (sym.value_ptr.section > 0) {
                self.builder.ptrTo(std.macho.nlist_64, offset).n_value = sections[sym.value_ptr.section - 1].addr + sym.value_ptr.address;
            }
            next_offset += @truncate(sym.key_ptr.len + 2);
        }

        self.builder.ptrTo(std.macho.symtab_command, self.symtab_offset).symoff = syms_offset;
        self.builder.ptrTo(std.macho.symtab_command, self.symtab_offset).nsyms = self.zero_use_symbol_index;
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
        // try out_file.chmod(0o777);
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

    pub inline fn restoreOffset(self: *Self, offset: u32) void {
        self.buffer.items.len = offset;
    }

    pub inline fn currentOffsetInSection(self: *Self, segment_offset: u32, sect: u32) u32 {
        return @as(u32, @truncate(self.buffer.items.len)) - self.ptrToSection(segment_offset, sect).offset;
    }
};

const SP: u5 = 31;
const XZR: u5 = 31;
const LOCAL_OFFSET_REGISTER: u5 = X29;
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
