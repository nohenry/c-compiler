//! Represents a Translation Unit

const std = @import("std");
const Token = @import("tokenizer.zig").Token;
const TokenIndex = @import("tokenizer.zig").TokenIndex;
const TokenRange = @import("tokenizer.zig").TokenRange;
const FileIndex = @import("tokenizer.zig").FileIndexType;
const TokenIndexType = @import("tokenizer.zig").TokenIndexType;
const Node = @import("parser.zig").Node;
const NodeIndex = @import("parser.zig").NodeIndex;

pub const DefineValue = struct {
    range: TokenRange,
};

pub const ArgumentMap = std.StringHashMap(DefineValue);

pub const DefineFunction = struct {
    range: TokenRange,

    parameters: std.StringArrayHashMap(void),
    var_arg: bool,
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

    pub fn get(self: *Self, index: Index) []const u8 {
        const buffer_index = self.list.items[index];
        return self.buffer[buffer_index.start .. buffer_index.start + buffer_index.count];
    }
};

pub const File = struct {
    file_path: []const u8,
    source: []u8,
    tokens: std.ArrayList(Token),
};

pub const Unit = struct {
    allocator: std.mem.Allocator,
    type_names: std.StringHashMap(void),
    files: std.ArrayList(File),

    token_end_range: std.AutoHashMap(TokenIndex, u32),
    nodes: std.ArrayList(Node),
    node_ranges: std.ArrayList(NodeIndex),

    defines: std.StringHashMap(DefineValue),
    define_fns: std.StringHashMap(DefineFunction),

    include_dirs: std.ArrayList([]const u8),

    const Self = @This();

    pub fn init(allocator: std.mem.Allocator) Self {
        const files = std.ArrayList(File).init(allocator);
        // files.append(.{
        //     .file_path = file_path,
        //     .source = source,
        //     .tokens = std.ArrayList(Token).init(allocator),
        // }) catch @panic("OOM");

        var include_dirs = std.ArrayList([]const u8).init(allocator);
        include_dirs.append("/opt/homebrew/Cellar/llvm/18.1.7/lib/clang/18/include") catch @panic("OOM");
        include_dirs.append("/Library/Developer/CommandLineTools/SDKs/MacOSX14.sdk/usr/include") catch @panic("OOM");

        return .{
            .allocator = allocator,
            .files = files,
            .type_names = std.StringHashMap(void).init(allocator),
            .token_end_range = std.AutoHashMap(TokenIndex, u32).init(allocator),

            .nodes = std.ArrayList(Node).init(allocator),
            .node_ranges = std.ArrayList(NodeIndex).init(allocator),
            .defines = std.StringHashMap(DefineValue).init(allocator),
            .define_fns = std.StringHashMap(DefineFunction).init(allocator),
            .include_dirs = include_dirs,
        };
    }

    pub inline fn filePos(self: *Unit, tidx: TokenIndex) struct { []const u8, u32 } {
        return .{
            self.files.items[tidx.file_index].file_path,
            self.files.items[tidx.file_index].tokens.items[tidx.index].start,
        };
    }

    pub fn define(self: *Unit, key: []const u8) void {
        self.defines.put(key, .{
            .range = .{
                .start = .{ .file_index = 0, .index = 0 },
                .end = .{ .file_index = 0, .index = 0 },
                .flags = 0,
            },
        }) catch @panic("OOM");
    }

    pub fn addFile(self: *Unit, file_path: []const u8, source: []u8) FileIndex {
        const index: FileIndex = @truncate(self.files.items.len);
        self.files.append(File{
            .file_path = self.allocator.dupe(u8, file_path) catch @panic("OOM"),
            .source = source,
            .tokens = std.ArrayList(Token).init(self.allocator),
        }) catch @panic("OOM");

        return index;
    }

    pub fn searchQuoteDirs(self: *Unit, this_file: FileIndex, file_path: []const u8) ?[]const u8 {
        var buffer: [512]u8 = undefined;
        var allocator = std.heap.FixedBufferAllocator.init(&buffer);

        const this_file_dir_path = std.fs.path.dirname(self.files.items[this_file].file_path).?;
        const full_file_path = std.fs.path.resolve(allocator.allocator(), &.{
            this_file_dir_path, file_path,
        }) catch @panic("Unable to resolve include filea");

        const file = std.fs.openFileAbsolute(full_file_path, .{}) catch {
            return self.searchIncludeDirs(file_path);
        };

        file.close();
        return self.allocator.dupe(u8, full_file_path) catch @panic("OOM");
    }

    pub fn searchIncludeDirs(self: *Unit, file_path: []const u8) ?[]const u8 {
        var buffer: [512]u8 = undefined;
        var allocator = std.heap.FixedBufferAllocator.init(&buffer);
        for (self.include_dirs.items) |include_dir| {
            const full_file_path = std.fs.path.resolve(allocator.allocator(), &.{
                include_dir, file_path,
            }) catch @panic("Unable to resolve include filea");

            const file = std.fs.openFileAbsolute(full_file_path, .{}) catch {
                defer allocator.reset();
                continue;
            };
            file.close();
            return self.allocator.dupe(u8, full_file_path) catch @panic("OOM");
        }

        return null;
    }

    pub fn writeToken(self: *Unit, preprocessed_writer: anytype, tidx: TokenIndex) !void {
        defer preprocessed_writer.writeByte(' ') catch @panic("oof");
        const tok = self.token(tidx);
        switch (tok.kind) {
            .identifier => {
                const token_source_slice = self.identifierAt(tidx);
                try preprocessed_writer.writeAll(token_source_slice);
            },
            .string_literal => {
                try preprocessed_writer.writeByte('"');
                const token_source_slice = self.stringAt(tidx);
                try preprocessed_writer.writeAll(token_source_slice);
                try preprocessed_writer.writeByte('"');
            },
            .stringified_literal => {
                try preprocessed_writer.writeByte('"');
                const token_source_slice = self.stringifiedAt(tidx);
                try preprocessed_writer.writeAll(token_source_slice);
                try preprocessed_writer.writeByte('"');
            },
            .type_name => {
                const token_source_slice = self.identifierAt(tidx);
                try preprocessed_writer.writeAll(token_source_slice);
            },
            .char_literal => {
                try preprocessed_writer.writeByte('\'');
                const token_source_slice = self.charAt(tidx);
                try preprocessed_writer.writeByte(token_source_slice);
                try preprocessed_writer.writeByte('\'');
            },
            else => {
                if (tok.kind.isIntLiteral()) {
                    const token_source_slice = self.tokenSourceSlice(tidx);
                    try preprocessed_writer.writeAll(token_source_slice);
                    return;
                }

                try preprocessed_writer.writeAll(tok.kind.toStr());
            },
        }
    }

    pub inline fn tokens(self: *Unit, file_index: FileIndex) *std.ArrayList(Token) {
        return &self.files.items[file_index].tokens;
    }

    pub fn token(self: *const Unit, tidx: TokenIndex) Token {
        return self.files.items[tidx.file_index].tokens.items[tidx.index];
    }

    pub inline fn tokenSlice(self: *Unit, start: TokenIndex, end: TokenIndex) []const Token {
        std.debug.assert(start.file_index == end.file_index);
        return self.files.items[start.file_index].tokens.items[start.index..end.index];
    }

    pub inline fn tokenSliceCount(self: *Unit, start: TokenIndex, count: u32) []const Token {
        return self.files.items[start.file_index].tokens.items[start.index .. start.index + count];
    }

    pub fn tokenSourceSlice(self: *Unit, tidx: TokenIndex) []const u8 {
        const source = self.files.items[tidx.file_index].source;
        const length = self.tokenLength(tidx);
        const tok = self.token(tidx);
        return source[tok.start .. tok.start + length];
    }

    pub fn tokenLength(self: *Unit, tidx: TokenIndex) u32 {
        const source = self.files.items[tidx.file_index].source;
        const tok = self.token(tidx);
        const start = tok.start;
        var index: u32 = tok.start;

        switch (tok.kind) {
            .float_literal,
            .double_literal,
            .int_literal,
            .unsigned_int_literal,
            .long_literal,
            .unsigned_long_literal,
            .long_long_literal,
            .unsigned_long_long_literal,
            .size_literal,
            .unsigned_size_literal,
            => {
                const IS_FLOAT: u8 = (1 << 0);
                const DID_DOT: u8 = (1 << 1);
                const DID_E: u8 = (1 << 2);
                var base: u8 = if (source[index] == '0') 8 else 10;
                var flags: u8 = 0;
                while (index < source.len) : (index += 1) doneLit: {
                    switch (source[index]) {
                        '\'', '_' => continue,
                        '.' => {
                            if ((flags & DID_DOT) > 0) break :doneLit;

                            flags |= DID_DOT;
                            flags |= IS_FLOAT;
                        },
                        'e' => {
                            if ((flags & DID_E) > 0) break :doneLit;

                            flags |= DID_E;
                            flags |= IS_FLOAT;
                        },
                        'b', 'B' => |x| {
                            if (base == 8 and index == start + 1) {
                                base = 2;
                            } else if (base > 10 and 1 < base - 10) {
                                continue;
                            } else {
                                std.debug.panic("Found \x1b[1;36m'{c}'\x1b[0m which is invalid for base \x1b[1;33m{}\x1b[0m", .{ x, base });
                            }
                        },
                        'x', 'X' => |x| {
                            if (base == 8 and index == start + 1) {
                                base = 16;
                            } else {
                                std.log.err("{s}, pos: {}, {} {}", .{ self.filePos(tidx)[0], self.filePos(tidx)[1], base, start });
                                std.debug.panic("Found \x1b[1;36m'{c}'\x1b[0m which is invalid for base \x1b[1;33m{}\x1b[0m", .{ x, base });
                            }
                        },
                        '0' => continue,
                        '1'...'9' => |x| if ((flags & IS_FLOAT) > 0) {
                            continue;
                        } else if (x - '0' < base) {
                            continue;
                        } else {
                            std.debug.panic("Found \x1b[1;36m'{c}'\x1b[0m which is invalid for base \x1b[1;33m{}\x1b[0m", .{ x, base });
                        },
                        'a', 'c', 'd', 'f'...'k' => |x| if (base > 10 and x - 'a' < base - 10) {
                            continue;
                        } else if (x == 'f') {
                            break;
                        } else {
                            std.debug.panic("Found \x1b[1;36m'{c}'\x1b[0m which is invalid for base \x1b[1;33m{}\x1b[0m", .{ x, base });
                        },
                        'A', 'C', 'D', 'F'...'K' => |x| if (base > 10 and x - 'A' < base - 10) {
                            continue;
                        } else if (x == 'F') {
                            break;
                        } else {
                            std.debug.panic("Found \x1b[1;36m'{c}'\x1b[0m which is invalid for base \x1b[1;33m{}\x1b[0m", .{ x, base });
                        },
                        else => break,
                    }
                }

                var unsigned = false;
                var long = false;
                while (index < source.len) : (index += 1) {
                    switch (source[index]) {
                        'f' => {
                            index += 1;
                            break;
                        },
                        'u' => {
                            if (long) break;
                            index += 1;
                            unsigned = true;
                        },
                        'z', 'Z' => {
                            if (long) break;
                            index += 1;
                            break;
                        },
                        'l', 'L' => {
                            if (long) {
                                index += 1;
                                break;
                            } else {
                                long = true;
                            }
                        },
                        else => break,
                    }
                }
            },
            .identifier => {
                while (index < source.len) : (index += 1) {
                    switch (source[index]) {
                        'a'...'z', 'A'...'Z', '_', '0'...'9' => continue,
                        else => break,
                    }
                }
            },
            else => {
                index += @truncate(tok.kind.toStr().len);
            },
        }

        // std.log.warn("{s}, pos: {}", .{self.filePos(tidx)[0], self.filePos(tidx)[1]});
        // std.log.warn("len: {}", .{index - tok.start});

        return index - tok.start;
    }

    pub fn ivalue(self: *const Unit, tidx: TokenIndex) u64 {
        const tok = self.token(tidx);
        const source = self.files.items[tidx.file_index].source;
        const include_to_index = self.token_end_range.get(tidx);
        var index = tok.start;
        var sum: u64 = 0;

        const start = index;
        var base: u8 = if (source[index] == '0') 8 else 10;
        while (index < source.len) : (index += 1) {
            switch (source[index]) {
                ' ', '\t', '#' => if (include_to_index == null or index >= include_to_index.?) break,
                '\'', '_' => continue,
                'b', 'B' => {
                    if (base == 8 and index == start + 1) {
                        base = 2;
                    } else if (base > 10 and 1 < base - 10) {
                        sum *= @as(u64, base);
                        sum += @as(u64, 11);
                        continue;
                    }
                },
                'x', 'X' => {
                    if (base == 8 and index == start + 1) {
                        base = 16;
                    }
                },
                '0' => {
                    sum *= @as(u64, base);
                },
                '1'...'9' => |x| if (x - '0' < base) {
                    sum *= @as(u64, base);
                    sum += @as(u64, source[index] - '0');
                    continue;
                },
                'a', 'c', 'd'...'k' => |x| if (base > 10 and x - 'a' < base - 10) {
                    sum *= @as(u64, base);
                    sum += @as(u64, source[index] - 'a' + 10);
                    continue;
                },
                'A', 'C', 'D'...'K' => |x| if (base > 10 and x - 'A' < base - 10) {
                    sum *= @as(u64, base);
                    sum += @as(u64, source[index] - 'A' + 10);
                    continue;
                },
                else => break,
            }
        }

        return sum;
    }

    pub fn fvalue(self: *const Unit, tidx: TokenIndex) f64 {
        const tok = self.token(tidx);
        const source = self.files.items[tidx.file_index].source;
        var index = tok.start;
        var sum: f64 = 0.0;
        var fract: f64 = 0.0;
        var mult: f64 = 0.0;
        var didDot = false;
        var didE = false;
        while (index < source.len) : (index += 1) {
            switch (source[index]) {
                '0'...'9' => |c| {
                    if (didDot) {
                        fract /= 10.0;
                        fract += (@as(f64, @floatFromInt(c - '0')) / 10.0);
                    } else if (didE) {
                        mult *= 10.0;
                        mult += @as(f64, @floatFromInt(c - '0'));
                    } else {
                        sum *= 10.0;
                        sum += @as(f64, @floatFromInt(c - '0'));
                    }
                },
                'e' => didE = true,
                '.' => didDot = true,
                else => break,
            }
        }

        if (didE)
            return (sum + fract) * std.math.pow(f64, 10.0, mult)
        else
            return (sum + fract);
    }

    pub fn identifierAt(self: *const Self, tidx: TokenIndex) []const u8 {
        const tok = self.token(tidx);
        const source = self.files.items[tidx.file_index].source;
        var index = tok.start;
        while (index < source.len) : (index += 1) {
            switch (source[index]) {
                'a'...'z', 'A'...'Z', '_', '0'...'9' => continue,
                else => break,
            }
        }

        return source[tok.start..index];
    }

    pub fn stringAt(self: *const Self, tidx: TokenIndex) []const u8 {
        const tok = self.token(tidx);
        const source = self.files.items[tidx.file_index].source;
        std.debug.assert(source[tok.start] == '"');
        var index = tok.start + 1;
        while (index < source.len) : (index += 1) {
            switch (source[index]) {
                '"' => break,
                else => {},
            }
        }

        return source[tok.start + 1 .. index];
    }

    pub fn stringifiedAt(self: *const Self, tidx: TokenIndex) []const u8 {
        const tok = self.token(tidx);
        const source = self.files.items[tidx.file_index].source;
        var index = tok.start;
        var indent: u32 = 0;
        while (index < source.len) : (index += 1) {
            switch (source[index]) {
                '(' => indent += 1,
                ')' => if (indent == 0) break else {
                    indent -= 1;
                },
                ',' => if (indent == 0) break,
                else => {},
            }
        }

        return source[tok.start..index];
    }

    pub fn charAt(self: *const Self, tidx: TokenIndex) u8 {
        const tok = self.token(tidx);
        const source = self.files.items[tidx.file_index].source;
        std.debug.assert(source[tok.start] == '\'');
        var index = tok.start + 1;
        var c: ?u8 = null;

        while (index < source.len) : (index += 1) {
            switch (source[index]) {
                '\\' => {
                    index += 1;

                    switch (source[index]) {
                        '\\' => {
                            c = '\\';
                        },
                        'n' => {
                            c = '\n';
                        },
                        't' => {
                            c = '\t';
                        },
                        'r' => {
                            c = '\r';
                        },
                        '0' => {
                            c = 0;
                        },
                        '\'' => c = '\'',
                        '"' => c = '"',
                        else => std.debug.panic("Invalid escape '{c}'", .{source[index]}),
                    }
                },
                '\'' => break,
                else => {
                    c = source[index];
                },
            }
        }

        return c.?;
    }

    // pub fn identifier(self: *const Unit, tidx: TokenIndex) []const u8 {
    //     const tok = self.token(tidx);
    //     const source = self.files.items[tidx.file_index].source;
    //     var index = tok.start;
    //     while (index < source.len) : (index += 1) {
    //         switch (source[index]) {
    //             'a'...'z', 'A'...'Z', '_', '0'...'9' => continue,
    //             else => break,
    //         }
    //     }

    //     return source[tok.start..index];
    // }

    pub fn ppDirective(self: *const Unit, tidx: TokenIndex) []const u8 {
        const tok = self.token(tidx);
        const source = self.files.items[tidx.file_index].source;
        var index = tok.start + 1;
        while (index < source.len) : (index += 1) {
            switch (source[index]) {
                'a'...'z', 'A'...'Z', '_' => continue,
                else => break,
            }
        }

        return source[tok.start + 1 .. index];
    }

    //     pub inline fn getOrPut(self: *Self, string: []const u8) !StringInterner.Index {
    //         return self.string_interner.getOrPut(string);
    //     }

    //     pub inline fn get(self: *Self, index: StringInterner.Index) []const u8 {
    //         return self.get(index);
    //     }
};
