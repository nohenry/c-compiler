//! Represents a Translation Unit

const std = @import("std");
const Token = @import("tokenizer.zig").Token;
const TokenIndex = @import("tokenizer.zig").TokenIndex;
const Node = @import("parser.zig").Node;
const NodeIndex = @import("parser.zig").NodeIndex;

pub const DefineValue = struct {
    token_start: TokenIndex,
    token_count: u32,
};

pub const DefineFunction = struct {
    token_start: TokenIndex,
    token_count: u32,

    parameters: std.StringArrayHashMap(void),
};

pub const Unit = struct {
    allocator: std.mem.Allocator,
    source: []const u8,
    type_names: std.StringHashMap(void),
    tokens: std.ArrayList(Token),

    nodes: std.ArrayList(Node),
    node_ranges: std.ArrayList(NodeIndex),

    defines: std.StringHashMap(DefineValue),
    define_fns: std.StringHashMap(DefineFunction),

    const Self = @This();

    pub fn init(allocator: std.mem.Allocator, source: []const u8) Self {
        return .{
            .allocator = allocator,
            .source = source,
            .type_names = std.StringHashMap(void).init(allocator),
            .tokens = std.ArrayList(Token).init(allocator),
            .nodes = std.ArrayList(Node).init(allocator),
            .node_ranges = std.ArrayList(NodeIndex).init(allocator),
            .defines = std.StringHashMap(DefineValue).init(allocator),
            .define_fns = std.StringHashMap(DefineFunction).init(allocator),
        };
    }

    pub fn identifierAt(self: *const Self, start: u32) []const u8 {
        var index = start;
        while (index < self.source.len) : (index += 1) {
            switch (self.source[index]) {
                'a'...'z', 'A'...'Z', '_' => continue,
                else => break,
            }
        }

        return self.source[start..index];
    }
};
