const std = @import("std");

pub const TypeKind = union(enum) {
    void: void,
    char: bool,
    // uchar: bool,
    short: bool,
    // ushort: bool,
    int: bool,
    // uint: bool,
    long: bool,
    // ulong: bool,
    longlong: bool,
    // ulonglong: bool,
    float: void,
    double: void,
    longdouble: void,
    bool: void,

    pointer: struct { base: Type },

    array: struct { base: Type, size: usize },

    @"struct": struct {
        backing_field: ?Type,
        fields: Type,
    },
    @"union": struct {
        backing_field: ?Type,
        variants: Type,
        indicies: Type,
    },

    multi_type: []const Type,
    multi_type_impl: MultiType,
    multi_type_keyed: std.StringArrayHashMap(Type),
    multi_type_keyed_impl: usize,

    func: struct {
        params: Type,
        ret_ty: ?Type,
    },

    pub fn isIntegral(self: @This()) bool {
        return switch (self) {
            .char, .short, .int, .long, .longlong => true,
            else => false,
        };
    }

    pub fn rank(self: @This()) u32 {
        return @intFromEnum(self);
    }

    pub fn isSigned(self: @This()) bool {
        return switch (self) {
            .char, .short, .int, .long, .longlong => |x| x,
            else => false,
        };
    }

    pub fn toSigned(self: @This(), signed: bool) @This() {
        var s = self;
        switch (s) {
            .char, .short, .int, .long, .longlong => |*x| x.* = signed,
            else => @panic("Invalid"),
        }
        return s;
    }
};

pub const MultiType = struct { start: u32, len: u32 };
pub const MultiTypeKeyed = struct { name: []const u8, ty: Type };

pub const TypeMapContext = struct {
    interner: *const TypeInterner,

    pub fn hash(ctx: @This(), key: TypeKind) u64 {
        _ = ctx;
        var hasher = std.hash.Wyhash.init(0);
        std.hash.autoHashStrat(&hasher, key, .Shallow);
        return hasher.final();
    }

    pub fn eql(ctx: @This(), a: TypeKind, b: TypeKind) bool {
        switch (a) {
            .multi_type => |vals| {
                if (b != .multi_type_impl) return false;
                const ind = b.multi_type_impl;

                return std.mem.eql(Type, vals, ctx.interner.multi_types.items[ind.start .. ind.start + ind.len]);
            },
            .multi_type_keyed => |vals| {
                if (b != .multi_type_keyed_impl) return false;

                const bmap = &ctx.interner.multi_types_keyed.items[b.multi_type_keyed_impl];
                if (vals.count() != bmap.count()) return false;

                var ait = vals.iterator();
                var bit = bmap.iterator();
                var aval = ait.next();
                var bval = bit.next();
                while (aval != null and bval != null) {
                    if (aval.?.value_ptr.* != bval.?.value_ptr.*) return false;
                    if (!std.mem.eql(u8, aval.?.key_ptr.*, bval.?.key_ptr.*)) return false;

                    aval = ait.next();
                    bval = bit.next();
                }

                return true;
            },
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

pub const Type = *const TypeKind;

pub const TypeInterner = struct {
    allocator: std.mem.Allocator,
    type_map: TypeMap,
    multi_types: std.ArrayList(Type),
    multi_types_keyed: std.ArrayList(std.StringArrayHashMap(Type)),

    const Self = @This();

    pub fn init(allocator: std.mem.Allocator) Self {
        return .{
            .allocator = allocator,
            .type_map = undefined,
            .multi_types = std.ArrayList(Type).init(allocator),
            .multi_types_keyed = std.ArrayList(std.StringArrayHashMap(Type)).init(allocator),
        };
    }

    pub fn setup(self: *Self) void {
        const types = TypeMap.initContext(self.allocator, .{ .interner = self });
        self.type_map = types;
    }

    pub fn voidTy(self: *Self) Type {
        return self.createOrGetTy(.void);
    }

    pub fn charTy(self: *Self, signed: bool) Type {
        return self.createOrGetTy(.{ .char = signed });
    }

    pub fn shortTy(self: *Self, signed: bool) Type {
        return self.createOrGetTy(.{ .short = signed });
    }

    pub fn intTy(self: *Self, signed: bool) Type {
        return self.createOrGetTy(.{ .int = signed });
    }

    pub fn longTy(self: *Self, signed: bool) Type {
        return self.createOrGetTy(.{ .long = signed });
    }

    pub fn longlongTy(self: *Self, signed: bool) Type {
        return self.createOrGetTy(.{ .longlong = signed });
    }

    pub fn floatTy(self: *Self) Type {
        return self.createOrGetTy(.float);
    }

    pub fn doubleTy(self: *Self) Type {
        return self.createOrGetTy(.double);
    }

    pub fn longdoubleTy(self: *Self) Type {
        return self.createOrGetTy(.longdouble);
    }

    pub fn boolTy(self: *Self) Type {
        return self.createOrGetTy(.bool);
    }

    pub fn arrayTy(self: *Self, base: Type, size: usize) Type {
        return self.createOrGetTy(.{ .array = .{ .base = base, .size = size } });
    }

    pub fn structTy(self: *Self, backing_field: ?Type, fields: std.StringArrayHashMap(Type)) Type {
        const field_tys = self.multiTyKeyed(fields);
        return self.createOrGetTy(.{
            .@"struct" = .{
                .backing_field = backing_field,
                .fields = field_tys,
            },
        });
    }

    pub fn unionTy(self: *Self, backing_field: ?Type, variants: std.StringArrayHashMap(Type), indicies: []const Type) Type {
        const variant_tys = self.multiTyKeyed(variants);
        const index_tys = self.multiTy(indicies);
        return self.createOrGetTy(.{
            .@"union" = .{
                .backing_field = backing_field,
                .variants = variant_tys,
                .indicies = index_tys,
            },
        });
    }

    pub fn multiTy(self: *Self, types: []const Type) Type {
        const ty = self.types.get(.{ .multi_type = types });
        if (ty) |val| {
            return val;
        }

        const starti = self.multi_types.items.len;
        self.multi_types.appendSlice(types) catch unreachable;
        const endi = self.multi_types.items.len;

        const val = self.allocator.create(TypeKind) catch unreachable;
        val.* = .{
            .multi_type_impl = .{
                .start = @truncate(starti),
                .len = @truncate(endi - starti),
            },
        };

        self.types.put(val.*, val) catch unreachable;

        return val;
    }

    pub fn multiTyKeyed(self: *Self, values: std.StringArrayHashMap(Type)) Type {
        const ty = self.types.getOrPut(.{ .multi_type_keyed = values }) catch unreachable;
        if (ty.found_existing) {
            return ty.value_ptr.*;
        }

        const index = self.multi_types_keyed.items.len;
        self.multi_types_keyed.append(values) catch unreachable;

        const val = self.allocator.create(TypeKind) catch unreachable;
        val.* = .{
            .multi_type_keyed_impl = index,
        };

        ty.value_ptr.* = val;
        return ty.value_ptr.*;
    }

    /// Expects multi_type to be .multi_type_impl
    pub fn getMultiTypes(self: *Self, multi_type: Type) []const Type {
        return self.multi_types.items[multi_type.multi_type_impl.start .. multi_type.multi_type_impl.start + multi_type.multi_type_impl.len];
    }

    pub fn funcTyNoParams(self: *Self, ret_ty: ?Type) Type {
        return self.funcTy(&.{}, ret_ty);
    }
    pub fn funcTy(self: *Self, param_tys: []const Type, ret_ty: ?Type) Type {
        const param_multi_ty = self.multiTy(param_tys);

        return self.createOrGetTy(.{
            .func = .{
                .params = param_multi_ty,
                .ret_ty = ret_ty,
            },
        });
    }

    pub fn printTyToStr(self: *const Self, ty: Type, allocator: std.mem.Allocator) []const u8 {
        var buf = std.ArrayList(u8).init(allocator);
        const buf_writer = buf.writer();
        self.printTyWriter(ty, buf_writer) catch @panic("Printing type failed");
        return buf.items;
    }

    pub fn printTyWriter(self: *const Self, ty: Type, writer: anytype) !void {
        switch (ty.*) {
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

            .float => try writer.print("float", .{}),
            .double => try writer.print("double", .{}),
            .longdouble =>  try writer.print("long double", .{}),
            .bool => try writer.print("bool", .{}),
            
            .pointer => |ptr| {
                try writer.print("*", .{});
                try self.printTyWriter(ptr.base, writer);
            },

            .array => |val| {
                try self.printTyWriter(val.base, writer);
                try writer.print("[{}]", .{val.size});
            },
            .@"struct" => |rec| {
                try writer.print("struct", .{});
                try self.printTyWriter(rec.fields, writer);
            },
            .@"union" => |uni| {
                try writer.print("union", .{});
                try self.printTyWriter(uni.variants, writer);
            },

            .multi_type => |tys| {
                try writer.print("(", .{});
                if (tys.len > 0) {
                    try self.printTyWriter(tys[0], writer);

                    for (tys[1..]) |mty| {
                        try writer.print(",", .{});
                        try self.printTyWriter(mty, writer);
                    }
                }
                try writer.print(")", .{});
            },
            .multi_type_impl => |mty| {
                try self.printTyWriter(&.{ .multi_type = self.multi_types.items[mty.start .. mty.start + mty.len] }, writer);
            },
            .multi_type_keyed => |kyd| {
                try writer.print("{{", .{});
                var it = kyd.iterator();

                if (it.next()) |val| {
                    try self.printTyWriter(val.value_ptr.*, writer);
                    try writer.print(" {s}", .{val.key_ptr.*});
                }
                while (it.next()) |val| {
                    try writer.print(", ", .{});
                    try self.printTyWriter(val.value_ptr.*, writer);
                    try writer.print(" {s}", .{val.key_ptr.*});
                }
                try writer.print("}}", .{});
            },
            .multi_type_keyed_impl => |ind| {
                try self.printTyWriter(&.{ .multi_type_keyed = self.multi_types_keyed.items[ind] }, writer);
            },
            .func => |func| {
                if (func.ret_ty) |ret| {
                    try self.printTyWriter(ret, writer);
                } else {
                    try writer.print("void", .{});
                }
                try writer.print(" ", .{});
                try self.printTyWriter(func.params, writer);
            },
        }
    }

    pub fn printTy(self: *const Self, ty: Type) void {
        const writer = std.io.getStdOut();
        self.printTyWriter(ty, writer.writer()) catch @panic("Error while printing");
    }

    pub fn createOrGetTy(self: *Self, value: TypeKind) Type {
        const ty = self.type_map.getOrPut(value) catch unreachable;
        if (ty.found_existing) {
            return ty.value_ptr.*;
        }

        const val = self.allocator.create(TypeKind) catch unreachable;
        val.* = value;

        ty.value_ptr.* = val;
        return ty.value_ptr.*;
    }
};
