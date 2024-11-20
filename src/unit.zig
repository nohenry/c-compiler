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
    source: []const u8,
    tokens: std.ArrayList(Token),
};

pub const Unit = struct {
    allocator: std.mem.Allocator,
    type_names: std.StringHashMap(void),
    files: std.ArrayList(File),

    nodes: std.ArrayList(Node),
    node_ranges: std.ArrayList(NodeIndex),

    defines: std.StringHashMap(DefineValue),
    define_fns: std.StringHashMap(DefineFunction),

    include_dirs: std.ArrayList([]const u8),

    const Self = @This();

    pub fn init(allocator: std.mem.Allocator, file_path: []const u8, source: []const u8) Self {
        var files = std.ArrayList(File).init(allocator);
        files.append(.{
            .file_path = file_path,
            .source = source,
            .tokens = std.ArrayList(Token).init(allocator),
        }) catch @panic("OOM");

        var include_dirs = std.ArrayList([]const u8).init(allocator);
        include_dirs.append("/opt/homebrew/Cellar/llvm/18.1.7/lib/clang/18/include") catch @panic("OOM");
        include_dirs.append("/Library/Developer/CommandLineTools/SDKs/MacOSX14.sdk/usr/include") catch @panic("OOM");

        return .{
            .allocator = allocator,
            .files = files,
            .type_names = std.StringHashMap(void).init(allocator),

            .nodes = std.ArrayList(Node).init(allocator),
            .node_ranges = std.ArrayList(NodeIndex).init(allocator),
            .defines = std.StringHashMap(DefineValue).init(allocator),
            .define_fns = std.StringHashMap(DefineFunction).init(allocator),
            .include_dirs = include_dirs,
        };
    }

    pub inline fn filePos(self: *Unit, tidx: TokenIndex) struct{[]const u8, u32} {
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

    pub fn addFile(self: *Unit, file_path: []const u8, source: []const u8) FileIndex {
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

    pub fn ivalue(self: *const Unit, tidx: TokenIndex) u64 {
        const tok = self.token(tidx);
        const source = self.files.items[tidx.file_index].source;
        var index = tok.start;
        var sum: u64 = 0;
        while (index < source.len) : (index += 1) {
            switch (source[index]) {
                '0'...'9' => |c| {
                    sum *= 10;
                    sum += @as(u64, c - '0');
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
                        else => {},
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

    pub fn identifier(self: *const Unit, tidx: TokenIndex) []const u8 {
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
