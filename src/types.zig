const std = @import("std");
const Unit = @import("unit.zig").Unit;
const TokenIndex = @import("tokenizer.zig").TokenIndex;
const NodeIndex = @import("parser.zig").NodeIndex;
const TypeQualifier = @import("parser.zig").TypeQualifier;

pub const TypeFlags = struct {
    pub const Type = u8;
    pub const Variadic: TypeFlags.Type = (1 << 0);
    pub const Incomplete: TypeFlags.Type = (1 << 1);
};

pub const TypeKind = struct {
    qualifiers: TypeQualifier.Type,
    flags: TypeFlags.Type = 0,
    kind: union(enum(u32)) {
        void: void,
        char: bool,
        short: bool,
        int: bool,
        long: bool,
        longlong: bool,
        int128: bool,
        float: void,
        double: void,
        longdouble: void,
        float_complex: void,
        double_complex: void,
        longdouble_complex: void,
        bool: void,

        pointer: struct { base: Type },
        type: Type,

        array: struct { base: Type, size: usize },
        array_unsized: struct { base: Type },

        builtin_struct: struct {
            fields: Type,
            index: u64,
        },
        unnamed_struct: struct {
            fields: Type,
            nidx: NodeIndex,
        },
        @"struct": struct {
            fields: Type,
            nidx: NodeIndex,
        },
        unnamed_union: struct {
            variants: Type,
            nidx: NodeIndex,
        },
        builtin_union: struct {
            variants: Type,
            index: u64,
        },
        @"union": struct {
            variants: Type,
            nidx: NodeIndex,
        },
        unnamed_enum: struct {
            nidx: NodeIndex,
        },
        @"enum": struct {
            nidx: NodeIndex,
        },
        tbd_nidx: TokenIndex,

        field: struct {
            name: StringInterner.Index,
            ty: Type,
        },

        bitfield_named: struct {
            name: StringInterner.Index,
            bits: u32,
            base: Type,
        },

        bitfield: struct {
            base: Type,
            bits: u32,
        },
        lvalue_ref: struct {
            base: Type,
        },

        /// Internal use only. Represents any possible type.
        /// Used in builtin functions
        any: void,
        any_kind: u32,

        multi_type: []const Type,
        multi_type_impl: MultiType,
        // multi_type_keyed: std.StringArrayHashMap(Type),
        // multi_type_keyed_impl: usize,

        func: struct {
            params: Type,
            ret_ty: Type,
        },
    },

    pub fn isIntegral(self: @This()) bool {
        return switch (self.kind) {
            .char, .short, .int, .long, .longlong => true,
            else => false,
        };
    }

    pub fn rank(self: @This()) u32 {
        return @intFromEnum(self.kind);
    }

    pub fn isStructured(self: *const @This()) bool {
        return switch (self.kind) {
            .@"struct", .unnamed_struct, .@"union", .unnamed_union => true,
            else => false,
        };
    }

    pub fn getStructureField(self: *const @This(), field_name: []const u8) ?Type {
        _ = field_name;
        return switch (self.kind) {
            // .@"struct" => |st| {
            //     return st.fields.kind.multi_type_keyed.get(field_name);
            // },
            // .unnamed_struct => |st| {
            //     return st.fields.kind.multi_type_keyed.get(field_name);
            // },
            // .@"union" => |st| {
            //     return st.variants.kind.multi_type_keyed.get(field_name);
            // },
            // .unnamed_union => |st| {
            //     return st.variants.kind.multi_type_keyed.get(field_name);
            // },
            // .@"struct", .unnamed_struct, .@"union", .unnamed_union => true,
            else => unreachable,
        };
    }

    pub fn getStructureIndex(self: *const @This()) NodeIndex {
        return switch (self.kind) {
            .@"struct" => |st| st.nidx,
            .unnamed_struct => |st| st.nidx,
            .@"union" => |st| st.nidx,
            .unnamed_union => |st| st.nidx,
            else => unreachable,
        };
    }

    pub fn isArithmetic(self: *const @This()) bool {
        return switch (self.kind) {
            .char,
            .short,
            .int,
            .long,
            .longlong,
            .float,
            .double,
            .longdouble,
            => true,
            else => false,
        };
    }

    pub fn isScalar(self: *const @This()) bool {
        return switch (self.kind) {
            .char,
            .short,
            .int,
            .long,
            .longlong,
            .float,
            .double,
            .longdouble,
            .bool,
            .pointer,
            => true,
            else => false,
        };
    }

    pub fn isSigned(self: @This()) bool {
        return switch (self.kind) {
            .char, .short, .int, .long, .longlong => |x| x,
            else => false,
        };
    }

    pub fn isIncomplete(self: @This()) bool {
        return switch (self.kind) {
            .tbd_nidx => true,
            else => (self.qualifiers & TypeFlags.Incomplete) > 0,
        };
    }

    pub fn toSigned(self: @This(), signed: bool) @This() {
        var s = self.kind;
        switch (s) {
            .char, .short, .int, .long, .longlong => |*x| x.* = signed,
            else => @panic("Invalid"),
        }
        return .{ .kind = s, .qualifiers = self.qualifiers };
    }
};

const TypeKindKind = std.meta.FieldType(TypeKind, .kind);

// comptime {
//     @compileLog(@sizeOf(TypeKind));
//     @compileLog(@sizeOf(std.meta.FieldType(TypeKind, .kind)));
// }

pub const MultiType = struct { start: u32, len: u32 };
pub const MultiTypeKeyed = struct { name: []const u8, ty: Type };

pub const TypeMapContext = struct {
    interner: *const TypeInterner,

    pub fn hash(ctx: @This(), key: TypeKind) u64 {
        var hasher = std.hash.Wyhash.init(0);
        switch (key.kind) {
            .multi_type => |vals| {
                for (vals) |val| {
                    std.hash.autoHashStrat(&hasher, val, .Shallow);
                }
            },
            .multi_type_impl => |vals| {
                var i: u32 = 0;
                while (i < vals.len) : (i += 1) {
                    std.hash.autoHashStrat(&hasher, ctx.interner.multi_types.items[vals.start + i], .Shallow);
                }
            },
            else => std.hash.autoHashStrat(&hasher, key, .Shallow),
        }
        return hasher.final();
    }

    pub fn eql(ctx: @This(), a: TypeKind, b: TypeKind) bool {
        switch (a.kind) {
            .multi_type => |vals| {
                if (b.kind != .multi_type_impl) return false;
                const ind = b.kind.multi_type_impl;

                const tys = ctx.interner.multi_types.items[ind.start .. ind.start + ind.len];
                return std.mem.eql(Type, vals, tys);
            },
            // .multi_type_keyed => |vals| {
            //     if (b.kind != .multi_type_keyed_impl) return false;

            //     const bmap = &ctx.interner.multi_types_keyed.items[b.kind.multi_type_keyed_impl];
            //     if (vals.count() != bmap.count()) return false;

            //     var ait = vals.iterator();
            //     var bit = bmap.iterator();
            //     var aval = ait.next();
            //     var bval = bit.next();
            //     while (aval != null and bval != null) {
            //         if (aval.?.value_ptr.* != bval.?.value_ptr.*) return false;
            //         if (!std.mem.eql(u8, aval.?.key_ptr.*, bval.?.key_ptr.*)) return false;

            //         aval = ait.next();
            //         bval = bit.next();
            //     }

            //     return true;
            // },
            else => return std.meta.eql(a, b),
        }
    }
};

pub const TypeMap = std.HashMap(
    TypeKind,
    Type,
    TypeMapContext,
    std.hash_map.default_max_load_percentage,
);

pub const Type = *TypeKind;

pub const TypeInterner = struct {
    unit: *Unit,
    allocator: std.heap.MemoryPool(TypeKind),
    type_map: TypeMap,
    multi_types: std.ArrayList(Type),
    str_interner: StringInterner,

    const Self = @This();

    pub fn init(unit: *Unit) Self {
        return .{
            .allocator = .init(std.heap.page_allocator),
            .unit = unit,
            .type_map = undefined,
            .multi_types = std.ArrayList(Type).init(unit.allocator),
            .str_interner = StringInterner.init(unit.allocator),
        };
    }

    pub fn setup(self: *Self) void {
        const types = TypeMap.initContext(self.unit.allocator, .{ .interner = self });
        self.type_map = types;
    }

    pub fn voidTy(self: *Self) Type {
        return self.createOrGetTy(.void, 0);
    }

    pub fn charTy(self: *Self, signed: bool, qualifiers: TypeQualifier.Type) Type {
        return self.createOrGetTy(.{ .char = signed }, qualifiers);
    }

    pub fn shortTy(self: *Self, signed: bool, qualifiers: TypeQualifier.Type) Type {
        return self.createOrGetTy(.{ .short = signed }, qualifiers);
    }

    pub fn intTy(self: *Self, signed: bool, qualifiers: TypeQualifier.Type) Type {
        return self.createOrGetTy(.{ .int = signed }, qualifiers);
    }

    pub fn longTy(self: *Self, signed: bool, qualifiers: TypeQualifier.Type) Type {
        return self.createOrGetTy(.{ .long = signed }, qualifiers);
    }

    pub fn longlongTy(self: *Self, signed: bool, qualifiers: TypeQualifier.Type) Type {
        return self.createOrGetTy(.{ .longlong = signed }, qualifiers);
    }

    pub fn int128Ty(self: *Self, signed: bool, qualifiers: TypeQualifier.Type) Type {
        return self.createOrGetTy(.{ .int128 = signed }, qualifiers);
    }

    pub fn floatTy(self: *Self, qualifiers: TypeQualifier.Type) Type {
        return self.createOrGetTy(.float, qualifiers);
    }

    pub fn doubleTy(self: *Self, qualifiers: TypeQualifier.Type) Type {
        return self.createOrGetTy(.double, qualifiers);
    }

    pub fn longdoubleTy(self: *Self, qualifiers: TypeQualifier.Type) Type {
        return self.createOrGetTy(.longdouble, qualifiers);
    }

    pub fn floatComplexTy(self: *Self, qualifiers: TypeQualifier.Type) Type {
        return self.createOrGetTy(.float_complex, qualifiers);
    }

    pub fn doubleComplexTy(self: *Self, qualifiers: TypeQualifier.Type) Type {
        return self.createOrGetTy(.double_complex, qualifiers);
    }

    pub fn longdoubleComplexTy(self: *Self, qualifiers: TypeQualifier.Type) Type {
        return self.createOrGetTy(.longdouble_complex, qualifiers);
    }


    pub fn boolTy(self: *Self, qualifiers: TypeQualifier.Type) Type {
        return self.createOrGetTy(.bool, qualifiers);
    }

    pub fn pointerTy(self: *Self, base: Type, qualifier: TypeQualifier.Type) Type {
        return self.createOrGetTy(.{ .pointer = .{ .base = base } }, qualifier);
    }

    pub fn typeTy(self: *Self, base: Type, qualifier: TypeQualifier.Type) Type {
        return self.createOrGetTy(.{ .type = base }, qualifier);
    }

    /// Sets the base value of pointer, array type or function type (base is return type)
    pub fn rebasePointer(self: *Self, ptr_type: Type, new_base: Type) Type {
        switch (ptr_type.kind) {
            .pointer => return self.pointerTy(new_base, ptr_type.qualifiers),
            .array => return self.arrayTy(new_base, ptr_type.kind.array.size, ptr_type.qualifiers),
            .array_unsized => return self.arrayUnsizedTy(new_base, ptr_type.qualifiers),
            .func => |fun| return self.createOrGetTy(.{
                .func = .{
                    .params = fun.params,
                    .ret_ty = new_base,
                },
            }, ptr_type.qualifiers),
            else => unreachable,
        }
    }

    pub fn rebasePointerRecursive(self: *Self, ptr_type: Type, new_base: Type) struct {
        /// new type
        Type,
        /// old base
        Type,
    } {
        switch (ptr_type.kind) {
            .pointer => |ptr| {
                const new_type, const old_base = self.rebasePointerRecursive(ptr.base, new_base);

                return .{
                    self.pointerTy(
                        new_type,
                        ptr_type.qualifiers,
                    ),
                    old_base,
                };
            },
            .array => |arr| {
                const new_type, const old_base = self.rebasePointerRecursive(arr.base, new_base);

                return .{
                    self.arrayTy(
                        new_type,
                        arr.size,
                        ptr_type.qualifiers,
                    ),
                    old_base,
                };
            },
            .array_unsized => |arr| {
                const new_type, const old_base = self.rebasePointerRecursive(arr.base, new_base);

                return .{
                    self.arrayUnsizedTy(
                        new_type,
                        ptr_type.qualifiers,
                    ),
                    old_base,
                };
            },
            .func => |fun| {
                const new_type, const old_base = self.rebasePointerRecursive(fun.ret_ty, new_base);
                return .{
                    self.createOrGetTy(.{
                        .func = .{
                            .params = fun.params,
                            .ret_ty = new_type,
                        },
                    }, ptr_type.qualifiers),
                    old_base,
                };
            },
            else => return .{ new_base, ptr_type },
        }
    }

    pub fn arrayTy(self: *Self, base: Type, size: usize, qualifiers: TypeQualifier.Type) Type {
        return self.createOrGetTy(.{ .array = .{ .base = base, .size = size } }, qualifiers);
    }

    pub fn arrayUnsizedTy(self: *Self, base: Type, qualifiers: TypeQualifier.Type) Type {
        return self.createOrGetTy(.{ .array_unsized = .{ .base = base } }, qualifiers);
    }

    pub fn arrayUnsizedToSized(self: *Self, unsized: Type, size: usize) Type {
        return self.createOrGetTy(.{
            .array = .{
                .base = unsized.kind.array_unsized.base,
                .size = size,
            },
        }, unsized.qualifiers);
    }

    pub fn builtinStructTy(self: *Self, index: u64, fields: []const Type, qualifiers: TypeQualifier.Type) Type {
        const field_tys = self.multiTy(fields);
        return self.createOrGetTy(.{
            .builtin_struct = .{
                .index = index,
                .fields = field_tys,
            },
        }, qualifiers);
    }

    pub fn unnamedStructTy(self: *Self, nidx: NodeIndex, fields: []const Type, qualifiers: TypeQualifier.Type) Type {
        const field_tys = self.multiTy(fields);
        return self.createOrGetTy(.{
            .unnamed_struct = .{
                .nidx = nidx,
                .fields = field_tys,
            },
        }, qualifiers);
    }

    pub fn structTy(self: *Self, nidx: NodeIndex, fields: []const Type, qualifiers: TypeQualifier.Type) Type {
        const field_tys = self.multiTy(fields);
        return self.createOrGetTy(.{
            .@"struct" = .{
                .nidx = nidx,
                .fields = field_tys,
            },
        }, qualifiers);
    }

    pub fn builtinUnionTy(self: *Self, index: u32, variants: []const Type, qualifiers: TypeQualifier.Type) Type {
        const variant_tys = self.multiTy(variants);
        return self.createOrGetTy(.{
            .builtin_union = .{
                .index = index,
                .variants = variant_tys,
            },
        }, qualifiers);
    }

    pub fn unnamedUnionTy(self: *Self, nidx: NodeIndex, variants: []const Type, qualifiers: TypeQualifier.Type) Type {
        const variant_tys = self.multiTy(variants);
        return self.createOrGetTy(.{
            .unnamed_union = .{
                .nidx = nidx,
                .variants = variant_tys,
            },
        }, qualifiers);
    }

    pub fn unionTy(self: *Self, nidx: NodeIndex, variants: []const Type, qualifiers: TypeQualifier.Type) Type {
        const variant_tys = self.multiTy(variants);
        return self.createOrGetTy(.{
            .@"union" = .{
                .nidx = nidx,
                .variants = variant_tys,
            },
        }, qualifiers);
    }

    pub fn enumTy(self: *Self, nidx: NodeIndex, qualifiers: TypeQualifier.Type) Type {
        return self.createOrGetTy(.{
            .@"enum" = .{
                .nidx = nidx,
            },
        }, qualifiers);
    }

    pub fn unnamedEnumTy(self: *Self, nidx: NodeIndex, qualifiers: TypeQualifier.Type) Type {
        return self.createOrGetTy(.{
            .unnamed_enum = .{
                .nidx = nidx,
            },
        }, qualifiers);
    }

    pub fn tbdNidx(self: *Self, nidx: TokenIndex) Type {
        return self.createOrGetTy(.{
            .tbd_nidx = nidx,
        }, 0);
    }

    pub fn anyTy(self: *Self) Type {
        return self.createOrGetTy(.{
            .any = {},
        }, 0);
    }

    pub fn anyKindTy(self: *Self, kind: std.meta.Tag(TypeKindKind)) Type {
        return self.createOrGetTy(.{
            .any_kind = @intFromEnum(kind),
        }, 0);
    }

    pub fn multiTy(self: *Self, types: []const Type) Type {
        const ty = self.type_map.get(.{
            .kind = .{ .multi_type = types },
            .qualifiers = 0,
        });
        if (ty) |val| {
            return val;
        }

        const starti = self.multi_types.items.len;
        self.multi_types.appendSlice(types) catch unreachable;
        const endi = self.multi_types.items.len;

        const val = self.allocator.create() catch unreachable;
        val.kind = .{
            .multi_type_impl = .{
                .start = @truncate(starti),
                .len = @truncate(endi - starti),
            },
        };
        val.qualifiers = 0;

        self.type_map.put(val.*, val) catch unreachable;

        return val;
    }

    pub fn multiTyKeyed(self: *Self, values: std.StringArrayHashMap(Type)) Type {
        // const ty = self.type_map.getOrPut(.{ .kind = .{ .multi_type_keyed = values }, .qualifiers = 0 }) catch unreachable;
        // if (ty.found_existing) {
        //     return ty.value_ptr.*;
        // }

        const index = self.multi_types_keyed.items.len;
        self.multi_types_keyed.append(values) catch unreachable;
        _ = index;

        const val = self.allocator.create() catch unreachable;
        // val.kind = .{
        //     .void_ty
        //     // .multi_type_keyed_impl = index,
        // };
        val.qualifiers = 0;

        // ty.value_ptr.* = val;
        // return ty.value_ptr.*;
        return undefined;
    }

    /// Expects multi_type to be .multi_type_impl
    pub fn getMultiTypes(self: *Self, multi_type: Type) []const Type {
        return self.multi_types.items[multi_type.kind.multi_type_impl.start .. multi_type.kind.multi_type_impl.start + multi_type.kind.multi_type_impl.len];
    }

    pub fn lvalueRefTy(self: *Self, base: Type) Type {
        return self.createOrGetTy(.{ .lvalue_ref = .{ .base = base } }, 0);
    }

    pub fn funcTyNoParams(self: *Self, ret_ty: Type, variadic: bool) Type {
        return self.funcTy(&.{}, ret_ty, variadic);
    }
    pub fn funcTy(self: *Self, param_tys: []const Type, ret_ty: Type, variadic: bool) Type {
        const param_multi_ty = self.multiTy(param_tys);

        return self.createOrGetTyKind(.{
            .kind = .{
                .func = .{
                    .params = param_multi_ty,
                    .ret_ty = ret_ty,
                },
            },
            .qualifiers = 0,
            .flags = if (variadic) TypeFlags.Variadic else 0,
        });
    }

    pub fn printTyToStr(self: *const Self, ty: Type, allocator: std.mem.Allocator) []const u8 {
        var buf = std.ArrayList(u8).init(allocator);
        const buf_writer = buf.writer();
        self.printTyWriter(ty, false, buf_writer) catch @panic("Printing type failed");
        return buf.items;
    }

    pub fn printTyWriter(self: *const Self, ty: *const TypeKind, multi_struct: bool, writer: anytype) !void {
        switch (ty.kind) {
            .pointer, .array, .array_unsized => {},
            else => {
                if (ty.qualifiers > 0) {
                    try TypeQualifier.writePretty(ty.qualifiers, writer);
                    try writer.writeByte(' ');
                }
            },
        }
        switch (ty.kind) {
            .void => try writer.print("void", .{}),
            .char => |sign| if (sign)
                try writer.print("char", .{})
            else
                try writer.print("unsigned char", .{}),
            .short => |sign| if (sign)
                try writer.print("short", .{})
            else
                try writer.print("unsigned short", .{}),
            .int => |sign| if (sign)
                try writer.print("int", .{})
            else
                try writer.print("unsigned int", .{}),
            .long => |sign| if (sign)
                try writer.print("long", .{})
            else
                try writer.print("unsigned long", .{}),
            .longlong => |sign| if (sign)
                try writer.print("long long", .{})
            else
                try writer.print("unsigned long long", .{}),

            .int128 => |sign| if (sign)
                try writer.print("__int128_t", .{})
            else
                try writer.print("__uint128_t", .{}),

            .float => try writer.print("float", .{}),
            .double => try writer.print("double", .{}),
            .longdouble => try writer.print("long double", .{}),
            .float_complex => try writer.print("float complex", .{}),
            .double_complex => try writer.print("double complex", .{}),
            .longdouble_complex => try writer.print("long double complex", .{}),
            .bool => try writer.print("bool", .{}),

            .pointer => |ptr| {
                try self.printTyWriter(ptr.base, false, writer);
                try writer.print(" *", .{});
                try TypeQualifier.writePretty(ty.qualifiers, writer);
            },

            .array => |val| {
                try self.printTyWriter(val.base, false, writer);
                try writer.writeByte('[');
                if (ty.qualifiers > 0) {
                    try TypeQualifier.writePretty(ty.qualifiers, writer);
                    try writer.writeByte(' ');
                }
                try writer.print("{}]", .{val.size});
            },
            .array_unsized => |val| {
                try self.printTyWriter(val.base, false, writer);
                try writer.writeByte('[');
                if (ty.qualifiers > 0) {
                    try TypeQualifier.writePretty(ty.qualifiers, writer);
                }
                try writer.print("]", .{});
            },
            .builtin_struct => |rec| {
                const str: [*:0]u8 = @ptrFromInt(rec.index);
                try writer.print("struct ({s}) {{ ", .{str});
                try self.printTyWriter(rec.fields, true, writer);
                try writer.print("}}", .{});
            },
            .unnamed_struct => |rec| {
                try writer.print("struct ({}) {{ ", .{rec.nidx});
                try self.printTyWriter(rec.fields, true, writer);
                try writer.print("}}", .{});
            },
            .@"struct" => |rec| {
                try writer.print("struct ", .{});
                const tok_index = self.unit.nodes.items[rec.nidx].data.two.a;
                try writer.writeAll(self.unit.identifierAt(@bitCast(tok_index)));
                try writer.print(" {{ ", .{});
                try self.printTyWriter(rec.fields, true, writer);
                try writer.print("}}", .{});
            },
            .builtin_union => |uni| {
                try writer.print("union (builtin {}) {{ ", .{uni.index});
                try self.printTyWriter(uni.variants, true, writer);
                try writer.print("}}", .{});
            },
            .unnamed_union => |uni| {
                try writer.print("union ({}) {{ ", .{uni.nidx});
                try self.printTyWriter(uni.variants, true, writer);
                try writer.print("}}", .{});
            },
            .@"union" => |uni| {
                try writer.print("union ", .{});
                const tok_index = self.unit.nodes.items[uni.nidx].data.two.a;
                try writer.writeAll(self.unit.identifierAt(@bitCast(tok_index)));
                try writer.print(" {{ ", .{});
                try self.printTyWriter(uni.variants, true, writer);
                try writer.print("}}", .{});
            },
            .unnamed_enum => |enu| {
                try writer.print("enum ({})", .{enu.nidx});
            },
            .@"enum" => |enu| {
                try writer.print("enum ", .{});
                const tok_index = self.unit.nodes.items[enu.nidx].data.two.a;
                try writer.writeAll(self.unit.identifierAt(@bitCast(tok_index)));
            },
            .tbd_nidx => |tbd| {
                try writer.print("TBD ({})", .{tbd});
            },

            .field => |fld| {
                try self.printTyWriter(fld.ty, false, writer);
                try writer.print(" {s}", .{self.str_interner.get(fld.name)});
            },

            .bitfield_named => |bf| {
                try self.printTyWriter(bf.base, false, writer);
                try writer.print(" {s}", .{self.str_interner.get(bf.name)});
                try writer.print(" : {}", .{bf.bits});
            },

            .bitfield => |bf| {
                try self.printTyWriter(bf.base, false, writer);
                try writer.print(" : {}", .{bf.bits});
            },

            .lvalue_ref => |ref| {
                try self.printTyWriter(ref.base, false, writer);
                try writer.print("(ref)", .{});
            },
            .any => try writer.print("any", .{}),
            .any_kind => |kind| try writer.print("any_kind(.{s})", .{
                @tagName(@as(
                    std.meta.Tag(
                        std.meta.FieldType(TypeKind, .kind),
                    ),
                    @enumFromInt(kind),
                )),
            }),
            .type => |base| {
                try writer.print("type(", .{});
                try self.printTyWriter(base, false, writer);
                try writer.print(")", .{});
            },

            .multi_type => |tys| {
                if (multi_struct) {
                    if (tys.len > 0) {
                        try self.printTyWriter(tys[0], false, writer);
                        try writer.print("; ", .{});

                        for (tys[1..]) |mty| {
                            try self.printTyWriter(mty, false, writer);
                            try writer.print("; ", .{});
                        }
                    }
                } else {
                    if (tys.len > 0) {
                        try self.printTyWriter(tys[0], false, writer);

                        for (tys[1..]) |mty| {
                            try writer.print(", ", .{});
                            try self.printTyWriter(mty, false, writer);
                        }
                    }
                }
            },
            .multi_type_impl => |mty| {
                try self.printTyWriter(&.{
                    .qualifiers = 0,
                    .kind = .{
                        .multi_type = self.multi_types.items[mty.start .. mty.start + mty.len],
                    },
                }, multi_struct, writer);
            },
            // .multi_type_keyed => |kyd| {
            //     try writer.print("{{ ", .{});
            //     var it = kyd.iterator();

            //     if (it.next()) |val| {
            //         try self.printTyWriter(val.value_ptr.*, writer);
            //         try writer.print(" {s}; ", .{val.key_ptr.*});
            //     }
            //     while (it.next()) |val| {
            //         try self.printTyWriter(val.value_ptr.*, writer);
            //         try writer.print(" {s}; ", .{val.key_ptr.*});
            //     }
            //     try writer.print("}}", .{});
            // },
            // .multi_type_keyed_impl => |ind| {
            //     try self.printTyWriter(&.{
            //         .qualifiers = 0,
            //         .kind = .{
            //             .multi_type_keyed = self.multi_types_keyed.items[ind],
            //         },
            //     }, writer);
            // },
            .func => |func| {
                try self.printTyWriter(func.ret_ty, false, writer);
                try writer.print(" ", .{});
                try writer.print("(", .{});
                try self.printTyWriter(func.params, false, writer);
                if ((ty.flags & TypeFlags.Variadic) > 0) {
                    try writer.print(", ...", .{});
                }
                try writer.print(")", .{});
            },
        }
    }

    pub fn printTy(self: *const Self, ty: Type) void {
        const writer = std.io.getStdOut();
        self.printTyWriter(ty, writer.writer()) catch @panic("Error while printing");
    }

    pub fn replaceType(self: *Self, from: Type, to: Type) !Type {
        from.* = to.*;
        try self.type_map.put(to.*, from);
        return from;
    }

    pub fn createOrGetTy(self: *Self, value: std.meta.FieldType(TypeKind, .kind), qualifiers: TypeQualifier.Type) Type {
        const ty = self.type_map.getOrPut(.{ .kind = value, .qualifiers = qualifiers }) catch unreachable;
        if (ty.found_existing) {
            return ty.value_ptr.*;
        }

        const val = self.allocator.create() catch unreachable;
        val.kind = value;
        val.qualifiers = qualifiers;

        ty.value_ptr.* = val;
        return ty.value_ptr.*;
    }

    pub fn createOrGetTyKind(self: *Self, value: TypeKind) Type {
        const ty = self.type_map.getOrPut(value) catch unreachable;
        if (ty.found_existing) {
            return ty.value_ptr.*;
        }

        const val = self.allocator.create() catch unreachable;
        val.* = value;

        ty.value_ptr.* = val;
        return ty.value_ptr.*;
    }
};

pub const StringInterner = struct {
    pub const Index = u32;
    const StringBufferIndex = struct { start: u32, count: u32 };

    allocator: std.mem.Allocator,
    buffer: std.ArrayList(u8),
    map: std.StringHashMap(Index),
    list: std.ArrayList(StringBufferIndex),

    const Self = @This();

    pub fn init(allocator: std.mem.Allocator) Self {
        return .{
            .allocator = allocator,
            .buffer = std.ArrayList(u8).init(allocator),
            .map = std.StringHashMap(Index).init(allocator),
            .list = std.ArrayList(StringBufferIndex).init(allocator),
        };
    }

    pub fn getOrPut(self: *Self, string: []const u8) !Index {
        const entry = try self.map.getOrPut(string);
        if (entry.found_existing) {
            return entry.value_ptr.*;
        }

        const index = self.list.items.len;
        try self.buffer.appendSlice(string);
        try self.list.append(.{
            .start = @truncate(index),
            .count = @truncate(string.len),
        });
        entry.value_ptr.* = @truncate(index);

        return @truncate(index);
    }

    pub fn get(self: *const Self, index: Index) []const u8 {
        const buffer_index = self.list.items[index];
        return self.buffer.items[buffer_index.start .. buffer_index.start + buffer_index.count];
    }
};
