const std = @import("std");
const Unit = @import("unit.zig").Unit;
const DefineValue = @import("unit.zig").DefineValue;
const Parser = @import("parser.zig").Parser;
const diag = @import("diagnostics.zig");

const TokenizeError = error{
    InvalidCharacter,
};

pub const TokenKind = enum(u32) {
    float_literal,
    double_literal,
    decimal32_literal,
    decimal64_literal,
    decimal128_literal,
    int_literal,
    unsigned_int_literal,
    long_literal,
    unsigned_long_literal,
    long_long_literal,
    unsigned_long_long_literal,
    size_literal,
    unsigned_size_literal,
    string_literal,
    stringified_literal,
    char_literal,

    type_name,
    enum_constant,
    identifier,

    open_paren,
    close_paren,
    open_brace,
    close_brace,
    open_bracket,
    close_bracket,
    comma,
    semicolon,
    star,
    slash,
    percent,
    plus,
    plusplus,
    minus,
    minusminus,
    bit_left_shift,
    bit_right_shift,
    lt,
    lte,
    gt,
    gte,
    equality,
    nequality,
    ampersand,
    carot,
    pipe,
    double_ampersand,
    double_pipe,
    exclamation,
    tilde,
    ellipsis,
    colon,
    hash,
    hashhash,

    assignment,
    plus_eq,
    minus_eq,
    star_eq,
    slash_eq,
    percent_eq,
    left_shift_eq,
    right_shift_eq,
    ampersand_eq,
    carot_eq,
    pipe_eq,
    question,
    at,

    dot,
    arrow,

    auto,
    atomic,
    @"break",
    @"const",
    case,
    @"continue",
    default,
    do,
    @"else",
    @"enum",
    @"extern",
    @"for",
    goto,
    @"if",
    @"inline",
    noreturn,
    register,
    restrict,
    @"return",
    sizeof,
    static,
    static_assert,
    @"struct",
    @"switch",
    thread_local,
    typedef,
    @"union",
    void,
    @"volatile",
    @"while",

    unsigned,
    signed,
    char,
    short,
    int,
    long,
    float,
    double,
    bool,

    pp_directive,

    pub inline fn isIntLiteral(self: TokenKind) bool {
        return switch (self) {
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
            .string_literal,
            .stringified_literal,
            .char_literal,
            => true,
            else => false,
        };
    }

    pub fn toStr(self: TokenKind) []const u8 {
        return switch (self) {
            .open_paren => "(",
            .close_paren => ")",
            .open_brace => "{",
            .close_brace => "}",
            .open_bracket => "[",
            .close_bracket => "]",
            .comma => ",",
            .semicolon => ";",
            .star => "*",
            .slash => "/",
            .percent => "%",
            .plus => "+",
            .plusplus => "++",
            .minus => "-",
            .minusminus => "--",
            .bit_left_shift => "<<",
            .bit_right_shift => ">>",
            .lt => "<",
            .lte => "<=",
            .gt => ">",
            .gte => ">=",
            .equality => "==",
            .nequality => "!=",
            .ampersand => "&",
            .carot => "^",
            .pipe => "|",
            .double_ampersand => "&&",
            .double_pipe => "||",
            .exclamation => "!",
            .tilde => "~",
            .colon => ":",
            .question => "?",
            .ellipsis => "...",
            .at => "@",

            .assignment => "=",
            .plus_eq => "+=",
            .minus_eq => "-=",
            .star_eq => "*=",
            .slash_eq => "/=",
            .percent_eq => "%=",
            .left_shift_eq => "<<=",
            .right_shift_eq => ">>=",
            .ampersand_eq => "&=",
            .carot_eq => "^=",
            .pipe_eq => "|=",

            .dot => ".",
            .arrow => "->",

            .auto => "auto",
            .atomic => "atomic",
            .@"break" => "break",
            .@"const" => "const",
            .case => "case",
            .@"continue" => "continue",
            .default => "default",
            .do => "do",
            .@"else" => "else",
            .@"enum" => "enum",
            .@"extern" => "extern",
            .@"for" => "for",
            .goto => "goto",
            .@"if" => "if",
            .@"inline" => "inline",
            .noreturn => "noreturn",
            .register => "register",
            .restrict => "restrict",
            .@"return" => "return",
            .sizeof => "sizeof",
            .static => "static",
            .static_assert => "static_assert",
            .@"struct" => "struct",
            .@"switch" => "switch",
            .thread_local => "thread_local",
            .typedef => "typedef",
            .@"union" => "union",
            .void => "void",
            .@"volatile" => "volatile",
            .@"while" => "while",

            .unsigned => "unsigned",
            .signed => "signed",
            .char => "char",
            .short => "short",
            .int => "int",
            .long => "long",
            .float => "float",
            .double => "double",
            .bool => "bool",

            else => std.debug.panic("Couldn't convert to string: {}", .{self}),
        };
    }
};

pub const keyword_map = std.StaticStringMap(TokenKind).initComptime(&.{
    .{ "auto", .auto },
    .{ "atomic", .atomic },
    .{ "break", .@"break" },
    .{ "const", .@"const" },
    .{ "case", .case },
    .{ "continue", .@"continue" },
    .{ "default", .default },
    .{ "do", .do },
    .{ "else", .@"else" },
    .{ "enum", .@"enum" },
    .{ "extern", .@"extern" },
    .{ "for", .@"for" },
    .{ "goto", .goto },
    .{ "if", .@"if" },
    .{ "inline", .@"inline" },
    .{ "noreturn", .noreturn },
    .{ "register", .register },
    .{ "restrict", .restrict },
    .{ "return", .@"return" },
    .{ "sizeof", .sizeof },
    .{ "static", .static },
    .{ "static_assert", .static_assert },
    .{ "struct", .@"struct" },
    .{ "switch", .@"switch" },
    .{ "thread_local", .thread_local },
    .{ "typedef", .typedef },
    .{ "union", .@"union" },
    .{ "void", .void },
    .{ "volatile", .@"volatile" },
    .{ "while", .@"while" },

    .{ "unsigned", .unsigned },
    .{ "signed", .signed },
    .{ "char", .char },
    .{ "short", .short },
    .{ "int", .int },
    .{ "long", .long },
    .{ "float", .float },
    .{ "double", .double },
    .{ "bool", .bool },
});

pub const TokenIndexType = u20;
pub const FileIndexType = u12;
/// Specifies global token index.
pub const TokenIndex = packed struct(u32) {
    /// index of token in file
    index: TokenIndexType,
    /// file index in unit
    file_index: FileIndexType,
};

/// A range of tokens. End is exclusive
pub const TokenRange = struct {
    start: TokenIndex,
    end: TokenIndex,
    flags: Flags.Type,

    pub const Flags = struct {
        pub const Type = u8;
        pub const EXPANSION_ARGUMENTS: Type = (1 << 0);
        pub const EXPANSION_DEPTH: Type = (1 << 1);
        pub const ALREADY_EXPANDED: Type = (1 << 2);
        pub const EXPANSION_TOKEN: Type = (1 << 3);
    };

    /// Creates a range from an inclusive end token index and a count of tokens
    pub inline fn initEndCount(end: TokenIndex, cnt: TokenIndexType) @This() {
        return @This(){
            .start = .{
                .index = end.index + 1 - cnt,
                .file_index = end.file_index,
            },
            .end = .{
                .index = end.index + 1,
                .file_index = end.file_index,
            },
            .flags = 0,
        };
    }

    pub fn count(self: *@This()) u32 {
        return self.end.index - self.start.index;
    }
};

/// A token only needs a kind and start location in source
pub const Token = struct {
    kind: TokenKind,
    start: u32,
};

/// A macro function argument (tokens passed into macro invocation)
pub const Argument = struct {
    tokens: []TokenRange,
    expanded_tokens: []TokenRange,

    pub inline fn hasSingleToken(self: *const @This()) bool {
        return self.tokens.len == 1 and self.tokens[0].count() == 1;
    }

    pub inline fn hasSingleExpandedToken(self: *const @This()) bool {
        return self.expanded_tokens.len == 1 and self.expanded_tokens[0].count() == 1;
    }

    pub fn expandSingleIdentifier(self: *@This()) ?TokenIndex {
        if (self.tokens.len != 1) return null;
        if (self.tokens[0].count() != 1) return null;
        return self.tokens[0].start;
    }
};

pub const ArgumentMap = std.StringHashMap(Argument);

/// A tokenizer specific to a file.
pub const FileTokenizer = struct {
    tokenizer: *Tokenizer,
    pos: u32 = 0,
    source: []u8,
    source_len: u32, // used since source.len can change
    file_index: FileIndexType,
    included_file: bool = false,

    const Self = @This();

    inline fn isNot(self: *Self, offsetFromPos: u32, char: u8) bool {
        return (self.pos + offsetFromPos < self.source_len) and self.source[self.pos + offsetFromPos] != char;
    }

    pub inline fn tokenCount(self: *Self) u32 {
        return if (self.tokenizer.peeked_token == null)
            @truncate(self.tokenizer.unit.tokens(self.file_index).items.len)
        else
            @truncate(self.tokenizer.unit.tokens(self.file_index).items.len - 1);
    }

    pub inline fn tokenCountFile(self: *Self, file_index: FileIndexType) u32 {
        return if (self.tokenizer.peeked_token == null)
            @truncate(self.tokenizer.unit.tokens(file_index).items.len)
        else
            @truncate(self.tokenizer.unit.tokens(file_index).items.len - 1);
    }

    pub fn currentVirtualEmpty(self: *Self) bool {
        return self.tokenizer.virtual_stack.items.len == 0 or blk: {
            const last = self.tokenizer.virtual_stack.getLast();
            break :blk last.start.index == last.end.index;
        };
    }

    pub fn currentTokenRangeFlags(self: *const Self) TokenRange.Flags.Type {
        if (self.tokenizer.virtual_stack.getLastOrNull()) |vs| {
            return vs.flags;
        }
        return 0;
    }

    pub fn nextVirtual(self: *Self) struct { ?TokenIndex, bool } {
        while (self.tokenizer.virtual_stack.items.len > 0) : (self.tokenizer.virtual_stack.items.len -= 1) {
            const item_index = self.tokenizer.virtual_stack.getLast();
            if (item_index.start.index < item_index.end.index) {
                self.tokenizer.virtual_stack.items[self.tokenizer.virtual_stack.items.len - 1].start.index += 1;
                const tok = self.tokenizer.unit.token(item_index.start);
                if (tok.kind == .identifier) {
                    const ident_str = self.tokenizer.unit.identifierAt(item_index.start);
                    if (self.tokenizer.unit.type_names.contains(ident_str)) {
                        const new_tok_index = TokenIndex{
                            .index = @truncate(self.tokenizer.unit.files.items[item_index.start.file_index].tokens.items.len),
                            .file_index = item_index.start.file_index,
                        };
                        self.tokenizer.unit.files.items[item_index.start.file_index].tokens.append(Token{
                            .kind = .type_name,
                            .start = tok.start,
                        }) catch @panic("OOM");

                        return .{ new_tok_index, (item_index.flags & TokenRange.Flags.ALREADY_EXPANDED) > 0 };
                    }
                }
                return .{ item_index.start, (item_index.flags & TokenRange.Flags.ALREADY_EXPANDED) > 0 };
            }

            if ((item_index.flags & TokenRange.Flags.EXPANSION_ARGUMENTS) > 0) {
                self.tokenizer.expansion_stack.items.len -= 1;
            }

            if ((item_index.flags & TokenRange.Flags.EXPANSION_TOKEN) > 0) {
                const name_tok = self.tokenizer.expansion_ctx_stack.items(.name_token)[self.tokenizer.expansion_ctx_stack.len - 1];
                const tok_name_str = self.tokenizer.unit.identifierAt(name_tok);
                self.tokenizer.expansion_ctx_stack.len -= 1;
                _ = self.tokenizer.disabled_macros.remove(tok_name_str);
            }

            if ((item_index.flags & TokenRange.Flags.EXPANSION_DEPTH) > 0) {
                self.tokenizer.expansion_depth -= 1;
            }
        }

        return .{ null, false };
    }

    pub inline fn currentRangeData(self: *Self) i16 {
        return self.tokenizer.virtual_stack.getLast().data;
    }

    pub fn backVirtual(self: *Self) void {
        self.tokenizer.virtual_stack.items[self.tokenizer.virtual_stack.items.len - 1].start.index -= 1;
    }

    /// Recursively expand an identifier if it's a macro function argument
    /// idx: Index of identifier token
    /// start_token: Outputs first token in found sequence
    /// returns: The argument of the specified macro function
    fn expandIdentifier(self: *Self, idx: TokenIndex, start_token: *Token) ?Argument {
        var ident_str = self.tokenizer.unit.identifierAt(idx);

        var expanded_range: Argument = self.searchNthExpansionArg(0, ident_str) orelse {
            return null;
            // std.log.err("{s}, pos: {}", .{ self.tokenizer.unit.filePos(idx)[0], self.tokenizer.unit.filePos(idx)[1] });
            // std.debug.panic("Expected macro parameter for stringification", .{});
        };
        var single_ident = expanded_range.expandSingleIdentifier() orelse {
            start_token.* = self.tokenizer.unit.token(expanded_range.tokens[0].start);
            return expanded_range;
        };
        start_token.* = self.tokenizer.unit.token(single_ident);
        ident_str = self.tokenizer.unit.identifierAt(single_ident);

        var n: u32 = 1;
        while (true) {
            expanded_range = self.searchNthExpansionArg(n, ident_str) orelse break;
            single_ident = expanded_range.expandSingleIdentifier() orelse break;
            const stok = self.tokenizer.unit.token(single_ident);

            if (start_token.kind == .identifier) {
                start_token.* = stok;
                ident_str = self.tokenizer.unit.identifierAt(single_ident);
                n += 1;
            } else {
                break;
            }
        }
        return expanded_range;
    }

    fn checkIsIdentifier(self: *Self, idx: TokenIndex) ?[]const u8 {
        switch (self.tokenizer.unit.token(idx).kind) {
            .identifier => return self.tokenizer.unit.identifierAt(idx),
            else => return null,
        }
    }

    // fn expandIdentifier1(self: *Self, idx: *TokenIndex, back: bool) ?TokenIndex {
    //     const ident_str = self.tokenizer.unit.identifierAt(idx.*);
    //     if (self.searchExpansionArg(ident_str)) |exp| {
    //         if () {
    //             idx.* = exp[0].tokens[0].start;
    //             return null;
    //             // last_tok = self.tokenizer.unit.token(idx.*);
    //         } else {
    //             if (back) self.backVirtual();
    //             // exp[0].tokens.items[0]
    //             var i = exp[0].tokens.len;
    //             while (i > 0) {
    //                 i -= 1;
    //                 self.tokenizer.pushVirtual(exp[0].tokens[i]);
    //             }

    //             return self.nextVirtual();
    //         }
    //         // exp[0].
    //     }
    //     return null;
    // }

    const ResolveResult = struct {
        token_idx: ?TokenIndex = null,
        do_next_iteration: bool = false,
    };

    /// Consume the next virtual token and expands # and ## recursively.
    /// last_token: previous consumed token, should be null on first call
    /// copy_source_index: used to specify the next index to copy concatenations into.
    ///
    /// Stringification:
    ///     We use the TokenKind.stringified_literal and use the same start_index as the token.
    ///     Later using Unit.stringified_at, we get the actual string value back
    ///
    /// Concatenations:
    ///     We resize the source buffer and copy each token to the end of the buffer.
    pub fn resolveVirtual(self: *Self, last_token: ?TokenIndex, copy_source_index: ?u32) ?TokenIndex {
        const last_arg = if (last_token) |last_tidx| blk: {
            if (self.checkIsIdentifier(last_tidx)) |ident_str| {
                if (self.searchExpansionArg(ident_str)) |arg| {
                    break :blk arg;
                }
            }
            break :blk null;
        } else null;

        var disabled = false;
        if (self.currentVirtualEmpty()) {
            if (last_token) |last_tidx| {
                if (self.checkIsIdentifier(last_tidx)) |ident_str| {
                    if (self.tokenizer.disabled_macros.contains(ident_str)) {
                        disabled = true;
                    }
                }
            }
        }

        const virt = self.nextVirtual();
        if (virt[1]) return virt[0];
        const tidx = virt[0] orelse {
            if (last_arg) |arg| {
                if (arg.hasSingleExpandedToken()) {
                    return arg.expanded_tokens[0].start;
                } else {
                    self.tokenizer.pushVirtualRanges(arg.expanded_tokens);
                    return self.nextVirtual()[0];
                }
            }

            if (last_token) |last_tidx| {
                if (self.checkIsIdentifier(last_tidx)) |ident_str| blk: {
                    if (disabled or self.tokenizer.disabled_macros.contains(ident_str)) {
                        break :blk;
                    }
                    // const last_ex_tok = self.tokenizer.expansion_name_token_stack.getLastOrNull();
                    // if (last_ex_tok) |let| {
                    //     const last_ident_str = self.tokenizer.unit.identifierAt(let);
                    //     if (std.mem.eql(u8, ident_str, last_ident_str)) {
                    //         break :blk;
                    //     }
                    // }
                    if (self.checkExpansion(ident_str, .{ .single = last_tidx })) {
                        return self.resolveVirtual(null, null);
                    }
                }
            }

            return last_token;
        };
        const token = self.tokenizer.unit.token(tidx);
        // const rng_data = self.currentRangeData();

        return switch (token.kind) {
            .identifier => {
                if (last_token != null) {
                    self.backVirtual();
                    if (last_arg) |arg| {
                        if (arg.hasSingleExpandedToken()) {
                            return arg.expanded_tokens[0].start;
                        } else {
                            self.tokenizer.pushVirtualRanges(arg.expanded_tokens);
                            return self.nextVirtual()[0].?;
                        }
                    }
                    return last_token;
                }

                // const ident_str = self.tokenizer.unit.identifierAt(tidx);
                // if (self.checkExpansion(ident_str, if (rng_data >= 0) @intCast(rng_data) else null)) {
                //     return self.resolveVirtual(null, null);
                // }

                return self.resolveVirtual(tidx, null);
            },
            .hash => {
                if (last_arg) |arg| {
                    self.backVirtual();
                    if (arg.hasSingleToken()) {
                        return arg.tokens[0].start;
                    } else {
                        self.tokenizer.pushVirtualRanges(arg.tokens);
                        return self.resolveVirtual(null, null);
                    }
                }

                if (last_token) |last_tidx| {
                    self.backVirtual();
                    if (self.checkIsIdentifier(last_tidx)) |ident_str| blk: {
                        // const last_ex_tok = self.tokenizer.expansion_name_token_stack.getLastOrNull();
                        // if (last_ex_tok) |let| {
                        //     const last_ident_str = self.tokenizer.unit.identifierAt(let);
                        //     if (std.mem.eql(u8, ident_str, last_ident_str)) {
                        //         break :blk;
                        //     }
                        // }
                        if (self.tokenizer.disabled_macros.contains(ident_str)) {
                            break :blk;
                        }
                        if (self.checkExpansion(ident_str, .{ .single = last_tidx })) {
                            return self.resolveVirtual(null, null);
                        }
                    }
                    return last_tidx;
                }

                const next_virt = self.nextVirtual();
                var next_tidx = next_virt[0].?;

                var next_token = self.tokenizer.unit.token(next_tidx);
                if (next_token.kind != .identifier) @panic("Expected macro parameter for stringification");

                if (self.checkIsIdentifier(next_tidx)) |ident_str| {
                    if (self.searchExpansionArg(ident_str)) |arg| {
                        next_tidx = arg.tokens[0].start;
                        if (arg.hasSingleToken()) {
                            // next_tidx = arg.tokens[0].start;
                        } else {
                            // self.tokenizer.pushVirtualRanges(arg.tokens);
                            // next_tidx = self.nextVirtual().?;
                        }
                        next_token = self.tokenizer.unit.token(next_tidx);
                    }
                }
                // }                var start_token: Token = undefined;

                // const expanded_range = self.expandIdentifier(next_tidx.?, &start_token) orelse {
                //     std.log.err("{s}, pos: {}", .{ self.tokenizer.unit.filePos(next_tidx.?)[0], self.tokenizer.unit.filePos(next_tidx.?)[1] });
                //     std.debug.panic("Expected macro parameter for stringification", .{});
                // };

                const new_token = Token{
                    .kind = .stringified_literal,
                    .start = next_token.start,
                };
                const file_index = next_tidx.file_index;
                const new_token_idx = self.tokenizer.unit.files.items[file_index].tokens.items.len;
                self.tokenizer.unit.files.items[file_index].tokens.append(new_token) catch @panic("OOM");

                const new_idx = TokenIndex{
                    .index = @truncate(new_token_idx),
                    .file_index = file_index,
                };

                return self.resolveVirtual(new_idx, null);
            },
            .hashhash => {
                var last_tidx = last_token.?;
                var last_tok = self.tokenizer.unit.token(last_tidx);

                if (self.checkIsIdentifier(last_tidx)) |ident_str| {
                    if (self.searchExpansionArg(ident_str)) |arg| {
                        if (arg.hasSingleToken()) {
                            last_tidx = arg.tokens[0].start;
                            last_tok = self.tokenizer.unit.token(last_tidx);
                        } else {
                            self.backVirtual();
                            self.tokenizer.pushVirtualRanges(arg.tokens);
                            return self.resolveVirtual(null, null);
                        }
                    }
                }

                const next_virt = self.nextVirtual();
                var next_tidx = next_virt[0].?;
                var next_token = self.tokenizer.unit.token(next_tidx);

                if (self.checkIsIdentifier(next_tidx)) |ident_str| {
                    if (self.searchExpansionArg(ident_str)) |arg| {
                        if (arg.hasSingleToken()) {
                            next_tidx = arg.tokens[0].start;
                        } else {
                            self.tokenizer.pushVirtualRanges(arg.tokens);
                            if (self.nextVirtual()[0]) |ntidx| {
                                next_tidx = ntidx;
                            }
                        }
                        next_token = self.tokenizer.unit.token(next_tidx);
                    }
                }

                var new_token = Token{
                    .kind = undefined,
                    .start = last_tok.start,
                };

                const new_token_idx = self.tokenizer.unit.files.items[self.file_index].tokens.items.len;

                const new_idx = TokenIndex{
                    .index = @truncate(new_token_idx),
                    .file_index = self.file_index,
                };

                // var last_tok_slice: []const u8 = undefined;
                // var next_tok_slice: []const u8 = undefined;
                const last_tok_slice = self.tokenizer.unit.tokenSourceSlice(last_tidx);
                const next_tok_slice = self.tokenizer.unit.tokenSourceSlice(next_tidx);

                blk: {
                    if (last_tok.kind.isIntLiteral()) {
                        if (next_token.kind.isIntLiteral()) {
                            new_token.kind = next_token.kind;
                            break :blk;
                        } else if (next_token.kind == .identifier) {
                            new_token.kind = last_tok.kind;
                            break :blk;
                        }
                    } else if (last_tok.kind == .identifier) {
                        if (next_token.kind.isIntLiteral()) {
                            new_token.kind = last_tok.kind;
                            break :blk;
                        } else if (next_token.kind == .identifier) {
                            new_token.kind = last_tok.kind;
                            break :blk;
                        }
                    }

                    self.tokenizer.unit.addDiagnostic(.{
                        .level = .err,
                        .location = self.tokenizer.expansion_ctx_stack.items(.callsite)[self.tokenizer.expansion_ctx_stack.len - 1],
                        .kind = .{ .invalid_concat = .{
                            .left = last_tidx,
                            .right = next_tidx,
                        } },
                    });
                    // TODO: this isn't quite right
                    var i: usize = self.tokenizer.expansion_ctx_stack.len - 1;
                    while (i > 0) {
                        i -= 1;

                        self.tokenizer.unit.addDiagnostic(.{
                            .level = .info,
                            .location = self.tokenizer.expansion_ctx_stack.items(.callsite)[i],
                            .kind = .{
                                .in_macro_expansion = .{
                                    .name = self.tokenizer.expansion_ctx_stack.items(.name_token)[i],
                                },
                            },
                        });
                    }
                    return self.resolveVirtual(null, null);
                }

                if (copy_source_index) |csi| {
                    new_token.start = csi;
                    const src_len = self.source.len;
                    self.source = self.tokenizer.allocator.realloc(self.source, self.source.len + next_tok_slice.len) catch @panic("OOM");
                    self.tokenizer.unit.files.items[self.file_index].source = self.source;

                    @memcpy(self.source[src_len .. src_len + next_tok_slice.len], next_tok_slice);
                } else {
                    new_token.start = @truncate(self.source.len);
                    self.source = self.tokenizer.allocator.realloc(self.source, self.source.len + last_tok_slice.len + next_tok_slice.len) catch @panic("OOM");
                    self.tokenizer.unit.files.items[self.file_index].source = self.source;
                    @memcpy(self.source[new_token.start .. new_token.start + last_tok_slice.len], last_tok_slice);
                    @memcpy(self.source[new_token.start + last_tok_slice.len .. new_token.start + last_tok_slice.len + next_tok_slice.len], next_tok_slice);
                }

                self.tokenizer.unit.files.items[self.file_index].tokens.append(new_token) catch @panic("OOM");

                if (new_token.kind == .identifier) blk: {
                    const ident_str = self.tokenizer.unit.identifierAt(new_idx);
                    // const last_ex_tok = self.tokenizer.expansion_name_token_stack.getLastOrNull();
                    // if (last_ex_tok) |let| {
                    //     const last_ident_str = self.tokenizer.unit.identifierAt(let);
                    //     if (std.mem.eql(u8, ident_str, last_ident_str)) {
                    //         break :blk;
                    //     }
                    // }
                    if (self.tokenizer.disabled_macros.contains(ident_str)) {
                        break :blk;
                    }
                    if (self.checkExpansion(ident_str, .{ .single = new_idx })) {
                        return self.resolveVirtual(null, null);
                    }
                }

                return self.resolveVirtual(new_idx, new_token.start);
            },
            else => {
                if (last_token != null) {
                    self.backVirtual();
                    const last_tidx = last_token.?;
                    if (last_arg) |arg| {
                        if (arg.hasSingleExpandedToken()) {
                            return arg.expanded_tokens[0].start;
                        } else {
                            self.tokenizer.pushVirtualRanges(arg.expanded_tokens);
                            return self.nextVirtual()[0].?;
                        }
                    }

                    if (self.checkIsIdentifier(last_tidx)) |ident_str| blk: {
                        // const last_ex_tok = self.tokenizer.expansion_name_token_stack.getLastOrNull();
                        // if (last_ex_tok) |let| {
                        //     const last_ident_str = self.tokenizer.unit.identifierAt(let);
                        //     if (std.mem.eql(u8, ident_str, last_ident_str)) {
                        //         break :blk;
                        //     }
                        // }
                        if (self.tokenizer.disabled_macros.contains(ident_str)) {
                            break :blk;
                        }
                        if (self.checkExpansion(ident_str, .{ .single = last_tidx })) {
                            return self.resolveVirtual(null, null);
                        }
                    }

                    return last_tidx;
                }

                return self.resolveVirtual(tidx, null);
            },
        };
    }

    /// Consumes whitespace, inactive source code from preprocessor conditions, and returns any virtual tokens.
    /// eol: specifies if we should only go to end of line
    /// return_value: Contains token index of virtual tokens.
    /// returns: true if caller should yield a token (or null if return_value.token_idx is null)
    pub fn consumePhantomTokens(self: *Self, eol: bool, return_value: *ResolveResult) bool {
        const resolve_result = self.resolveVirtual(null, null);
        if (resolve_result) |token| {
            return_value.token_idx = token;
            return true;
        }

        var c: u8 = undefined;
        if (self.tokenizer.conditional_skip) blk: {
            var indent: u32 = 0;
            while (self.pos < self.source_len and self.tokenizer.conditional_skip) : (self.pos += 1) {
                c = self.source[self.pos];

                switch (c) {
                    '/' => {
                        if (self.pos + 1 < self.source_len) {
                            if (self.source[self.pos + 1] == '/') {
                                while (self.isNot(0, '\n')) : (self.pos += 1) {}
                                if (eol) return true;
                            } else if (self.source[self.pos + 1] == '*') {
                                self.pos += 1;
                                while (self.isNot(0, '*') or self.isNot(1, '/')) : (self.pos += 1) {}
                                self.pos += 1; // for */
                            }
                        }
                    },
                    '\n' => {
                        if (eol) return true;
                    },
                    '#' => {
                        const very_start_index = self.pos;
                        self.pos += 1;
                        var start_index = self.pos;
                        var before = true;
                        while (self.pos < self.source_len) : (self.pos += 1) {
                            switch (self.source[self.pos]) {
                                'a'...'z', 'A'...'Z', '_' => {
                                    before = false;
                                },
                                ' ', '\t' => {
                                    if (before) {
                                        start_index += 1;
                                    } else break;
                                },
                                else => break,
                            }
                        }

                        const directive_str = self.source[start_index..self.pos];
                        if (std.mem.eql(u8, directive_str, "ifdef") or std.mem.eql(u8, directive_str, "ifndef") or std.mem.eql(u8, directive_str, "if")) {
                            indent += 1;
                        } else if (std.mem.eql(u8, directive_str, "endif")) {
                            if (indent == 0) {
                                _ = self.tokenizer.if_stack.pop();
                                self.tokenizer.conditional_skip = false;
                                break :blk;
                            }

                            indent -= 1;
                        } else if (std.mem.eql(u8, directive_str, "else")) {
                            if (indent == 0) {
                                const did_condition = self.tokenizer.if_stack.peek();
                                self.tokenizer.conditional_skip = did_condition == 1;
                            }
                        } else if (std.mem.eql(u8, directive_str, "elif")) {
                            if (indent == 0) {
                                const did_condition = self.tokenizer.if_stack.peek();
                                self.tokenizer.conditional_skip = did_condition == 1;
                                if (did_condition == 0) {
                                    self.pos = very_start_index;
                                    break :blk;
                                }
                            }
                        }

                        self.pos -= 1; // since we add one at next iteration

                    },
                    else => {},
                }
            }
        }

        while (self.pos < self.source_len) : (self.pos += 1) {
            c = self.source[self.pos];
            switch (c) {
                ' ', '\t', '\r' => continue,
                '\\' => {
                    while (self.isNot(0, '\n')) : (self.pos += 1) {}
                    continue;
                    // if (eol) {
                    // } else break;
                },
                '\n' => {
                    if (eol) {
                        return true;
                    } else continue;
                },
                '/' => {
                    if (self.pos + 1 < self.source_len) {
                        if (self.source[self.pos + 1] == '/') {
                            while (self.isNot(0, '\n')) : (self.pos += 1) {}
                            return true;
                        } else if (self.source[self.pos + 1] == '*') {
                            self.pos += 1;
                            while (self.isNot(0, '*') or self.isNot(1, '/')) : (self.pos += 1) {}
                            self.pos += 1; // for */
                        } else {
                            break;
                        }
                    }
                },
                else => break,
            }
        } else {
            return true;
        }

        return false;
    }

    /// Consumes the next token.
    /// eol: if true we only tokenize to the end of the current line, then returns null
    pub fn next(self: *Self, eol: bool) ?TokenIndex {
        while (true) {
            var result: ResolveResult = .{};
            if (self.consumePhantomTokens(eol, &result)) {
                if (result.token_idx) |value|
                    return value
                else
                    return null;
            }

            const tok = self.nextNonPhantom() catch {
                std.log.err("Invalid character '{c}'", .{self.source[self.pos]});
                self.pos += 1;
                return null;
            };
            if (tok) |value| {
                return value;
            }
            if (self.included_file) {
                return null;
            }
        }
    }

    /// Reverse search of macro function arguments
    fn searchExpansionArg(self: *Self, arg: []const u8) ?Argument {
        if (self.tokenizer.expansion_stack.items.len == 0) return null;
        var index = self.tokenizer.expansion_stack.items.len;
        if (std.mem.eql(u8, arg, "__VA_ARGS__")) {
            index -= 1;
            return self.tokenizer.expansion_stack.items[index].va_arg;
        }
        while (index > 0) {
            index -= 1;
            if (self.tokenizer.expansion_stack.items[index].map.get(arg)) |value| {
                return value;
            }
        }

        return null;
    }

    /// Reverse search of the nth macro function argument
    fn searchNthExpansionArg(self: *Self, n: u32, arg: []const u8) ?Argument {
        if (self.tokenizer.expansion_stack.items.len == 0) return null;
        var nn = n;
        var index = @as(isize, @intCast(self.tokenizer.expansion_stack.items.len - 1));
        // TODO: do we need last found at all?
        // var last_found: ?Argument = null;
        while (index >= 0) : (index -= 1) {
            if (self.tokenizer.expansion_stack.items[@as(usize, @intCast(index))].map.get(arg)) |value| {
                if (nn == 0) return value;
                nn -= 1;
                // last_found = value;
            }
        }

        return null;
        // return last_found;
    }

    /// Called if we need a certain token kind. This function is internal
    fn expect(self: *Self, kind: TokenKind) TokenIndex {
        const tidx = self.tokenizer.next(false);
        if (tidx == null) @panic("Unexpected EOF");

        const token = self.tokenizer.unit.token(tidx.?);
        if (token.kind == kind) {
            return tidx.?;
        } else {
            std.log.err("File: {s}, pos: {}", .{ self.tokenizer.unit.files.items[tidx.?.file_index].file_path, token.start });

            var i = self.tokenizer.virtual_stack.items.len;
            while (i > 0) {
                i -= 1;
                var start = self.tokenizer.virtual_stack.items[i].start;
                const end = self.tokenizer.virtual_stack.items[i].end;
                while (start.index < end.index) : (start.index += 1) {
                    const tok = self.tokenizer.unit.token(start);
                    std.log.info("{} {s} {}", .{ tok, self.tokenizer.unit.filePos(start)[0], self.tokenizer.unit.filePos(start)[1] });
                }
            }
            std.debug.panic("Expected token {}, found {}, {}", .{ kind, token.kind, self.tokenizer.unit.ivalue(tidx.?) });
        }
    }

    /// Checks if string is a:
    ///     - macro function arg
    ///     - define object
    ///     - define function
    /// and pushes virtual tokens onto the stack, then returns true if was one of the above.
    fn checkExpansion(self: *Self, str: []const u8, callsite: diag.DiagnosticLocation) bool {
        if (self.tokenizer.unit.defines.get(str)) |def| {
            if (def.range.start.index < def.range.end.index) {
                self.tokenizer.pushVirtual(.{
                    .start = def.range.start,
                    .end = def.range.end,
                    .flags = def.range.flags | TokenRange.Flags.EXPANSION_TOKEN,
                });

                self.tokenizer.expansion_ctx_stack.append(self.tokenizer.unit.allocator, .{
                    .name_token = def.name_tok,
                    .callsite = callsite,
                }) catch @panic("OOM");
                self.tokenizer.disabled_macros.put(str, {}) catch @panic("OOM");
            }
            return true;
        }

        if (self.tokenizer.unit.define_fns.get(str)) |def| blk: {
            _ = blk1: {
                const pidx = self.tokenizer.peek(false);
                if (pidx == null or self.tokenizer.unit.token(pidx.?).kind != .open_paren) {
                    break :blk;
                }
                break :blk1 self.tokenizer.next(false).?;
            };

            var argument_map = ArgumentMap.init(self.tokenizer.allocator);
            var va_arg: ?Argument = null;
            var parameter_iter = def.parameters.iterator();

            var indent: u32 = 0;
            var needs_less_params = false;
            var excess_param_start: u32 = undefined;
            var found_count: u32 = 0;

            blk1: {
                var i: u32 = 0;
                var tokens = std.ArrayList(TokenRange).init(self.tokenizer.allocator);
                var expanded_tokens = std.ArrayList(TokenRange).init(self.tokenizer.allocator);
                defer tokens.deinit();
                defer expanded_tokens.deinit();
                var last_token: ?TokenIndex = null;
                var last_token_flags: TokenRange.Flags.Type = 0;
                var range_count: TokenIndexType = 0;
                const old_define = self.tokenizer.in_define;
                defer self.tokenizer.in_define = old_define;
                self.tokenizer.in_define = true;

                while (self.tokenizer.next(false)) |pidx| {
                    const p = self.tokenizer.unit.token(pidx);
                    const flags = self.currentTokenRangeFlags();

                    switch (p.kind) {
                        .open_paren, .open_brace, .open_bracket => indent += 1,
                        .close_paren => {
                            if (indent == 0) {
                                if (last_token != null or found_count > 0) found_count += 1;
                                if (!needs_less_params and i > 0) {
                                    if (last_token) |tidx| {
                                        tokens.append(TokenRange.initEndCount(tidx, range_count)) catch @panic("OOM");
                                        range_count = 0;
                                    }

                                    if ((last_token_flags & TokenRange.Flags.ALREADY_EXPANDED) == 0) {
                                        const vs_len_start = self.tokenizer.virtual_stack.items.len;
                                        var unexp_i = tokens.items.len;
                                        while (unexp_i > 0) {
                                            unexp_i -= 1;
                                            self.tokenizer.pushVirtual(tokens.items[unexp_i]);
                                        }

                                        last_token = null;
                                        range_count = 0;
                                        while (self.tokenizer.virtual_stack.items.len >= vs_len_start + 1) {
                                            const vtok = self.resolveVirtual(null, null);
                                            if (vtok == null) break;
                                            if (last_token) |ltidx| {
                                                if (ltidx.file_index == vtok.?.file_index and vtok.?.index == ltidx.index + 1) {
                                                    range_count += 1;
                                                } else {
                                                    expanded_tokens.append(TokenRange.initEndCount(ltidx, range_count)) catch @panic("OOM");
                                                    range_count = 1;
                                                }
                                            } else {
                                                range_count += 1;
                                            }
                                            last_token = vtok;
                                        }

                                        if (last_token) |ltidx| {
                                            var expanded_token_range = TokenRange.initEndCount(ltidx, range_count);
                                            expanded_token_range.flags |= TokenRange.Flags.ALREADY_EXPANDED;
                                            expanded_tokens.append(expanded_token_range) catch @panic("OOM");
                                        }
                                    }
                                    last_token = null;
                                    range_count = 0;

                                    const tokens_slice = self.tokenizer.allocator.dupe(TokenRange, tokens.items) catch @panic("OOM");
                                    const arg = Argument{
                                        .tokens = tokens_slice,
                                        .expanded_tokens = if ((last_token_flags & TokenRange.Flags.ALREADY_EXPANDED) == 0)
                                            self.tokenizer.allocator.dupe(TokenRange, expanded_tokens.items) catch @panic("OOM")
                                        else
                                            tokens_slice,
                                    };
                                    if (parameter_iter.next()) |param| {
                                        argument_map.put(param.key_ptr.*, arg) catch @panic("OOM");
                                    } else if (def.var_arg) {
                                        va_arg = arg;
                                    } else {
                                        if (!needs_less_params) {
                                            needs_less_params = true;
                                            excess_param_start = p.start;
                                        }
                                    }
                                }
                                break :blk1;
                            }
                            indent -= 1;
                        },
                        .close_brace, .close_bracket => indent -= 1,
                        .comma => {
                            const param = parameter_iter.next();
                            if (indent == 0) {
                                found_count += 1;
                                if (param != null) {
                                    if (last_token) |tidx| {
                                        tokens.append(TokenRange.initEndCount(tidx, range_count)) catch @panic("OOM");
                                        last_token = null;
                                        range_count = 0;
                                    }

                                    if ((last_token_flags & TokenRange.Flags.ALREADY_EXPANDED) == 0) {
                                        const vs_len_start = self.tokenizer.virtual_stack.items.len;
                                        var unexp_i = tokens.items.len;
                                        while (unexp_i > 0) {
                                            unexp_i -= 1;
                                            self.tokenizer.pushVirtual(tokens.items[unexp_i]);
                                        }

                                        last_token = null;
                                        range_count = 0;
                                        while (self.tokenizer.virtual_stack.items.len >= vs_len_start + 1) {
                                            const vtok = self.resolveVirtual(null, null);
                                            if (vtok == null) break;
                                            if (last_token) |ltidx| {
                                                if (ltidx.file_index == vtok.?.file_index and vtok.?.index == ltidx.index + 1) {
                                                    range_count += 1;
                                                } else {
                                                    expanded_tokens.append(TokenRange.initEndCount(ltidx, range_count)) catch @panic("OOM");
                                                    range_count = 1;
                                                }
                                            } else {
                                                range_count += 1;
                                            }
                                            last_token = vtok;
                                        }

                                        if (last_token) |ltidx| {
                                            expanded_tokens.append(TokenRange.initEndCount(ltidx, range_count)) catch @panic("OOM");
                                        }
                                    }
                                    last_token = null;
                                    range_count = 0;

                                    const tokens_slice = self.tokenizer.allocator.dupe(TokenRange, tokens.items) catch @panic("OOM");
                                    const arg = Argument{
                                        .tokens = tokens_slice,
                                        .expanded_tokens = if ((last_token_flags & TokenRange.Flags.ALREADY_EXPANDED) == 0)
                                            self.tokenizer.allocator.dupe(TokenRange, expanded_tokens.items) catch @panic("OOM")
                                        else
                                            tokens_slice,
                                    };
                                    argument_map.put(param.?.key_ptr.*, arg) catch @panic("OOM");

                                    tokens.items.len = 0;
                                    expanded_tokens.items.len = 0;
                                    continue;
                                } else if (!def.var_arg) {
                                    if (!needs_less_params) {
                                        needs_less_params = true;
                                        excess_param_start = self.pos;
                                    }
                                }
                            }
                        },
                        else => {},
                    }
                    if (last_token) |tidx| {
                        if (tidx.file_index == pidx.file_index and pidx.index == tidx.index + 1) {
                            range_count += 1;
                        } else {
                            tokens.append(TokenRange.initEndCount(tidx, range_count)) catch @panic("OOM");
                            range_count = 1;
                        }
                    } else {
                        range_count += 1;
                    }

                    last_token = pidx;
                    last_token_flags = flags;
                    i += 1;
                }
            }

            if (needs_less_params) {
                self.tokenizer.unit.addDiagnostic(.{
                    .level = .err,
                    .location = .{
                        .file_range = .{
                            .position = excess_param_start,
                            .len = @truncate(self.pos - excess_param_start),
                            .file_index = self.file_index,
                        },
                    },
                    .kind = .{ .macro_needs_less_args = .{
                        .macro_name = def.name_tok,
                        .expected = @truncate(def.parameters.count()),
                        .found = found_count,
                    } },
                });
            } else if (argument_map.count() != def.parameters.count()) {
                self.tokenizer.unit.addDiagnostic(.{
                    .level = .err,
                    .location = .{
                        .file_position = .{
                            .position = self.pos,
                            .file_index = self.file_index,
                        },
                    },
                    .kind = .{ .macro_needs_more_args = .{
                        .macro_name = def.name_tok,
                        .expected = @truncate(def.parameters.count()),
                        .found = found_count,
                    } },
                });
            }

            if (def.range.start.index < def.range.end.index) {
                self.tokenizer.expansion_stack.append(ExpansionArgumentMap{
                    .map = argument_map,
                    .va_arg = va_arg,
                    .depth = self.tokenizer.expansion_depth,
                }) catch @panic("OOM");

                self.tokenizer.expansion_depth += 1;

                self.tokenizer.pushVirtual(TokenRange{
                    .start = def.range.start,
                    .end = def.range.end,
                    .flags = TokenRange.Flags.EXPANSION_ARGUMENTS | TokenRange.Flags.EXPANSION_DEPTH | TokenRange.Flags.EXPANSION_TOKEN,
                });
                self.tokenizer.expansion_ctx_stack.append(self.tokenizer.unit.allocator, .{
                    .name_token = def.name_tok,
                    .callsite = callsite,
                }) catch @panic("OOM");
                self.tokenizer.disabled_macros.put(str, {}) catch @panic("OOM");
            }

            return true;
        }

        return false;
    }

    /// Tokenize actual non-phantom tokens.
    /// Returns the token index, or null if we're out of tokens.
    /// Also returns null if included file and we handle above
    /// I think might return null in some other cases
    fn nextNonPhantom(self: *Self) TokenizeError!?TokenIndex {
        const c = self.source[self.pos];
        const start = self.pos;
        const token_kind = blk: {
            switch (c) {
                '0'...'9' => {
                    const IS_FLOAT: u16 = (1 << 0);
                    const DID_DOT: u16 = (1 << 1);
                    const DID_E: u16 = (1 << 2);
                    const DID_P: u16 = (1 << 3);
                    const SIZE: u16 = (1 << 4);
                    const LONG: u16 = (1 << 5);
                    const LONGLONG: u16 = (1 << 6);
                    const UNSIGNED: u16 = (1 << 7);
                    const DEC32: u16 = (1 << 8);
                    const DEC64: u16 = (1 << 9);
                    const DEC128: u16 = (1 << 10);
                    _ = DEC32;
                    _ = DEC64;
                    _ = DEC128;
                    var base: u8 = if (c == '0') 8 else 10;
                    var flags: u16 = 0;
                    var invalid_start: ?u32 = null;
                    var invalid_base: ?u8 = null;
                    var invalid_char: ?u8 = null;
                    while (self.pos < self.source_len) : (self.pos += 1) doneLit: {
                        switch (self.source[self.pos]) {
                            '\'', '_' => continue,
                            '.' => {
                                if ((flags & DID_DOT) > 0) break :doneLit;

                                flags |= DID_DOT;
                                flags |= IS_FLOAT;
                            },
                            'e', 'E' => {
                                if ((flags & IS_FLOAT) == 0 and base == 16) {
                                    continue;
                                }
                                if ((flags & DID_E) > 0) break :doneLit;
                                if ((flags & DID_P) > 0) break :doneLit;

                                flags |= DID_E;
                                flags |= IS_FLOAT;
                            },
                            'p', 'P' => {
                                if ((flags & DID_E) > 0) break :doneLit;
                                if ((flags & DID_P) > 0) break :doneLit;

                                flags |= DID_P;
                                flags |= IS_FLOAT;
                            },
                            'b', 'B' => |x| {
                                if ((flags & IS_FLOAT) > 0) {
                                    continue;
                                } else if (base == 8 and self.pos == start + 1) {
                                    base = 2;
                                } else if (base > 10 and 1 < base - 10) {
                                    continue;
                                } else {
                                    if (invalid_start == null) {
                                        invalid_start = self.pos;
                                        invalid_base = base;
                                        invalid_char = x;
                                    }
                                }
                            },
                            'x', 'X' => |x| {
                                // we allow floats to have hex start
                                if (base == 8 and self.pos == start + 1) {
                                    base = 16;
                                } else {
                                    if (invalid_start == null) {
                                        invalid_start = self.pos;
                                        invalid_base = base;
                                        invalid_char = x;
                                    }
                                }
                            },
                            '0' => continue,
                            '1'...'9' => |x| if ((flags & IS_FLOAT) > 0) {
                                continue;
                            } else if (x - '0' < base) {
                                continue;
                            } else {
                                if (invalid_start == null) {
                                    invalid_start = self.pos;
                                    invalid_base = base;
                                    invalid_char = x;
                                }
                            },
                            'a', 'c', 'd', 'f'...'k' => |x| if (base > 10 and x - 'a' < base - 10) {
                                continue;
                            } else if (x == 'f') {
                                flags |= IS_FLOAT;
                                break;
                            } else {
                                if (invalid_start == null) {
                                    invalid_start = self.pos;
                                    invalid_base = base;
                                    invalid_char = x;
                                }
                            },
                            'A', 'C', 'D', 'F'...'K' => |x| if (base > 10 and x - 'A' < base - 10) {
                                continue;
                            } else if (x == 'F') {
                                flags |= IS_FLOAT;
                                break;
                            } else {
                                if (invalid_start == null) {
                                    invalid_start = self.pos;
                                    invalid_base = base;
                                    invalid_char = x;
                                }
                            },
                            else => break,
                        }
                    }

                    while (self.pos < self.source_len) : (self.pos += 1) {
                        switch (self.source[self.pos]) {
                            'f', 'F' => {
                                self.pos += 1;
                                if ((flags & IS_FLOAT) > 0 and base == 16) {
                                    if ((flags & DID_P) == 0) {
                                        self.tokenizer.unit.addDiagnostic(.{
                                            .level = .err,
                                            .location = .{ .file_position = .{
                                                .file_index = self.file_index,
                                                .position = start,
                                            } },
                                            .kind = .{ .hex_float_requires_exponent = .{} },
                                        });
                                    }
                                }
                                break :blk TokenKind.float_literal;
                            },
                            'u', 'U' => {
                                if ((flags & UNSIGNED) > 0) break;
                                flags |= UNSIGNED;
                            },
                            'z', 'Z' => {
                                if ((flags & (SIZE | LONG | LONGLONG)) > 0) break;
                                flags |= SIZE;
                            },
                            'l', 'L' => {
                                if ((flags & LONG) > 0) {
                                    if ((flags & (LONGLONG | SIZE)) > 0) break;
                                    flags &= ~LONG;
                                    flags |= LONGLONG;
                                } else {
                                    if ((flags & (LONG | SIZE)) > 0) break;
                                    flags |= LONG;
                                }
                            },
                            else => break,
                        }
                    }

                    if (invalid_start) |istart| {
                        self.tokenizer.unit.addDiagnostic(.{
                            .level = .err,
                            .location = .{
                                .file_position = .{
                                    .file_index = self.file_index,
                                    .position = start,
                                },
                            },
                            .kind = .{
                                .invalid_literal_for_int_base = .{
                                    .suffix = .{
                                        .file_range = .{
                                            .position = istart,
                                            .len = @truncate(self.pos - istart),
                                            .file_index = self.file_index,
                                        },
                                    },
                                    .base = invalid_base.?,
                                    .char = invalid_char.?,
                                },
                            },
                        });
                    }

                    if ((flags & IS_FLOAT) > 0 and base == 16) {
                        if ((flags & DID_P) == 0) {
                            self.tokenizer.unit.addDiagnostic(.{
                                .level = .err,
                                .location = .{ .file_position = .{
                                    .file_index = self.file_index,
                                    .position = start,
                                } },
                                .kind = .{ .hex_float_requires_exponent = .{} },
                            });
                        }
                        break :blk TokenKind.double_literal;
                    } else if ((flags & IS_FLOAT) > 0)
                        break :blk TokenKind.double_literal
                    else if ((flags & (LONGLONG | UNSIGNED)) > 0)
                        break :blk TokenKind.unsigned_long_long_literal
                    else if ((flags & (LONG | UNSIGNED)) > 0)
                        break :blk TokenKind.unsigned_long_literal
                    else if ((flags & (SIZE | UNSIGNED)) > 0)
                        break :blk TokenKind.unsigned_size_literal
                    else if ((flags & LONGLONG) > 0)
                        break :blk TokenKind.long_long_literal
                    else if ((flags & LONG) > 0)
                        break :blk TokenKind.long_literal
                    else if ((flags & SIZE) > 0)
                        break :blk TokenKind.size_literal
                    else if ((flags & UNSIGNED) > 0)
                        break :blk TokenKind.unsigned_int_literal
                    else
                        break :blk TokenKind.int_literal;
                },
                '(' => break :blk self.advanceToken(TokenKind.open_paren),
                ')' => break :blk self.advanceToken(TokenKind.close_paren),
                '[' => break :blk self.advanceToken(TokenKind.open_bracket),
                ']' => break :blk self.advanceToken(TokenKind.close_bracket),
                '{' => break :blk self.advanceToken(TokenKind.open_brace),
                '}' => break :blk self.advanceToken(TokenKind.close_brace),
                ',' => break :blk self.advanceToken(TokenKind.comma),
                ';' => break :blk self.advanceToken(TokenKind.semicolon),
                ':' => break :blk self.advanceToken(TokenKind.colon),
                '?' => break :blk self.advanceToken(TokenKind.question),
                '.' => {
                    if (self.pos + 2 < self.source_len) {
                        if (self.source[self.pos + 1] == '.' and self.source[self.pos + 2] == '.') {
                            break :blk self.advanceToken(TokenKind.ellipsis);
                        }
                    }
                    break :blk self.advanceToken(TokenKind.dot);
                },
                '=' => {
                    if (self.pos + 1 < self.source_len) {
                        switch (self.source[self.pos + 1]) {
                            '=' => break :blk self.advanceToken(TokenKind.equality),
                            else => {},
                        }
                    }
                    break :blk self.advanceToken(TokenKind.assignment);
                },
                '!' => {
                    if (self.pos + 1 < self.source_len) {
                        switch (self.source[self.pos + 1]) {
                            '=' => break :blk self.advanceToken(TokenKind.nequality),
                            else => {},
                        }
                    }
                    break :blk self.advanceToken(TokenKind.exclamation);
                },

                '+' => if (self.pos + 1 < self.source_len) {
                    switch (self.source[self.pos + 1]) {
                        '+' => break :blk self.advanceToken(TokenKind.plusplus),
                        '=' => break :blk self.advanceToken(TokenKind.plus_eq),
                        else => break :blk self.advanceToken(TokenKind.plus),
                    }
                } else break :blk self.advanceToken(TokenKind.plus),
                '-' => if (self.pos + 1 < self.source_len) {
                    switch (self.source[self.pos + 1]) {
                        '-' => break :blk self.advanceToken(TokenKind.minusminus),
                        '=' => break :blk self.advanceToken(TokenKind.minus_eq),
                        '>' => break :blk self.advanceToken(TokenKind.arrow),
                        else => break :blk self.advanceToken(TokenKind.minus),
                    }
                } else break :blk self.advanceToken(TokenKind.minus),
                '*' => break :blk self.opEq(.star, .star_eq),
                '/' => break :blk self.opEq(.slash, .slash_eq),
                '%' => break :blk self.opEq(.percent, .percent_eq),

                '&' => {
                    if (self.pos + 1 < self.source_len) {
                        switch (self.source[self.pos + 1]) {
                            '&' => break :blk self.advanceToken(TokenKind.double_ampersand),
                            '=' => break :blk self.advanceToken(TokenKind.ampersand_eq),
                            else => {},
                        }
                    }
                    break :blk self.advanceToken(TokenKind.ampersand);
                },
                '|' => {
                    if (self.pos + 1 < self.source_len) {
                        switch (self.source[self.pos + 1]) {
                            '|' => break :blk self.advanceToken(TokenKind.double_pipe),
                            '=' => break :blk self.advanceToken(TokenKind.pipe_eq),
                            else => {},
                        }
                    }
                    break :blk self.advanceToken(TokenKind.pipe);
                },

                '^' => break :blk self.opEq(.carot, .carot_eq),
                '~' => break :blk self.advanceToken(.tilde),
                '<' => {
                    if (self.pos + 1 < self.source_len) {
                        switch (self.source[self.pos + 1]) {
                            '<' => {
                                if (self.pos + 2 < self.source_len) {
                                    switch (self.source[self.pos + 2]) {
                                        '=' => break :blk self.advanceToken(TokenKind.left_shift_eq),
                                        else => {},
                                    }
                                }
                                break :blk self.advanceToken(TokenKind.bit_left_shift);
                            },

                            '=' => break :blk self.advanceToken(TokenKind.lte),
                            else => {},
                        }
                    }
                    break :blk self.advanceToken(.lt);
                },
                '"' => {
                    self.pos += 1;
                    while (self.pos < self.source_len) : (self.pos += 1) {
                        switch (self.source[self.pos]) {
                            '"' => {
                                if (self.source[self.pos - 1] == '\\') {
                                    continue;
                                } else break;
                            },
                            else => {},
                        }
                    }
                    self.pos += 1;
                    break :blk TokenKind.string_literal;
                },
                '\'' => {
                    self.pos += 1;
                    var last_slash = false;
                    while (self.pos < self.source_len) : (self.pos += 1) {
                        switch (self.source[self.pos]) {
                            '\'' => {
                                if (last_slash and self.source[self.pos - 1] == '\\') continue else break;
                            },
                            '\\' => {
                                last_slash = !last_slash;
                            },
                            else => {},
                        }
                    }
                    self.pos += 1;
                    break :blk TokenKind.char_literal;
                },
                '>' => {
                    if (self.pos + 1 < self.source_len) {
                        switch (self.source[self.pos + 1]) {
                            '>' => {
                                if (self.pos + 2 < self.source_len) {
                                    switch (self.source[self.pos + 2]) {
                                        '=' => break :blk self.advanceToken(TokenKind.right_shift_eq),
                                        else => {},
                                    }
                                }
                                break :blk self.advanceToken(TokenKind.bit_right_shift);
                            },

                            '=' => break :blk self.advanceToken(TokenKind.gte),
                            else => {},
                        }
                    }
                    break :blk self.advanceToken(.gt);
                },
                '@' => break :blk self.advanceToken(.at),

                'a'...'z', 'A'...'Z', '_' => {
                    while (self.pos < self.source_len) : (self.pos += 1) {
                        switch (self.source[self.pos]) {
                            'a'...'z', 'A'...'Z', '_', '0'...'9' => continue,
                            else => break,
                        }
                    }
                    const str = self.source[start..self.pos];

                    if (!self.tokenizer.in_define) {
                        if (self.checkExpansion(str, .{
                            .file_range = .{ .position = start, .len = @truncate(str.len), .file_index = self.file_index },
                        })) {
                            return null;
                        }
                    }

                    if (self.tokenizer.unit.type_names.contains(str)) {
                        break :blk TokenKind.type_name;
                    }

                    if (keyword_map.get(str)) |kind| {
                        break :blk kind;
                    }

                    break :blk TokenKind.identifier;
                },

                '#' => {
                    const old_define = self.tokenizer.in_define;
                    self.tokenizer.in_define = true;
                    defer self.tokenizer.in_define = old_define;

                    self.pos += 1;
                    var start_index = self.pos;
                    var before = true;
                    while (self.pos < self.source_len) : (self.pos += 1) {
                        switch (self.source[self.pos]) {
                            'a'...'z', 'A'...'Z', '_' => {
                                if (old_define) break;
                                before = false;
                            },
                            '#' => if (old_define) continue else break,
                            ' ', '\t' => {
                                if (old_define or !before) break;
                                start_index += 1;
                            },
                            else => break,
                        }
                    }
                    const directive_str = self.source[start_index..self.pos];

                    if (self.tokenizer.in_define and directive_str.len == 0) {
                        break :blk .hash;
                    } else if (std.mem.eql(u8, directive_str, "#")) {
                        break :blk .hashhash;
                    } else if (std.mem.eql(u8, directive_str, "define")) {
                        const first_token = self.tokenizer.next(true); // identifier
                        const second_token = if (self.pos < self.source_len and self.source[self.pos] == '(') // Open paren must be immediately after identifier
                            self.tokenizer.next(true)
                        else
                            null;

                        if (first_token == null) {
                            @panic("Unexpected EOF");
                        }

                        const define_name = self.tokenizer.unit.token(first_token.?);
                        if (define_name.kind == .bool) {
                            while (self.tokenizer.next(true)) |_| {}
                            return null;
                        }
                        std.debug.assert(define_name.kind == .identifier);
                        const define_str = self.tokenizer.unit.identifierAt(first_token.?);
                        var var_arg = false;

                        if (second_token != null and self.tokenizer.unit.token(second_token.?).kind == .open_paren) {
                            // Function type macro
                            var parameter_map = std.StringArrayHashMap(void).init(self.tokenizer.allocator);
                            var next_must_be_arg = false;

                            while (self.tokenizer.next(true)) |pidx| {
                                const p = self.tokenizer.unit.token(pidx);
                                switch (p.kind) {
                                    .identifier => {
                                        parameter_map.put(self.tokenizer.unit.identifierAt(pidx), {}) catch @panic("OOM");
                                        next_must_be_arg = false;
                                    },
                                    .ellipsis => {
                                        var_arg = true;
                                        next_must_be_arg = false;
                                        _ = self.expect(.close_paren);
                                        break;
                                    },
                                    .comma => {
                                        next_must_be_arg = true;
                                    },
                                    .close_paren => {
                                        if (next_must_be_arg) {
                                            while (self.tokenizer.next(true)) |_| {}
                                            self.tokenizer.unit.addDiagnostic(.{
                                                .level = .err,
                                                .location = .{
                                                    .single = pidx,
                                                },
                                                .kind = .{
                                                    .expected_pp_parameter_name = .{ .found = pidx },
                                                },
                                            });

                                            return null;
                                        }
                                        break;
                                    },
                                    else => {
                                        while (self.tokenizer.next(true)) |_| {}
                                        self.tokenizer.unit.addDiagnostic(.{
                                            .level = .err,
                                            .location = .{
                                                .single = pidx,
                                            },
                                            .kind = .{
                                                .expected_pp_parameter_name = .{ .found = pidx },
                                            },
                                        });

                                        return null;
                                    },
                                }
                            }

                            const pidx = TokenIndex{
                                .index = @truncate(self.tokenCount()),
                                .file_index = second_token.?.file_index,
                            };

                            var count: u32 = 0;
                            while (self.tokenizer.next(true)) |_| {
                                count += 1;
                            }

                            const define = self.tokenizer.unit.define_fns.getOrPut(define_str) catch @panic("OOM");

                            define.value_ptr.* = .{
                                .name_tok = first_token.?,
                                .range = .{
                                    .start = pidx,
                                    .end = .{
                                        .index = pidx.index + @as(TokenIndexType, @truncate(count)),
                                        .file_index = pidx.file_index,
                                    },
                                    .flags = TokenRange.Flags.EXPANSION_ARGUMENTS,
                                },
                                .var_arg = var_arg,
                                .parameters = parameter_map,
                            };
                        } else {
                            // Value type macro
                            var count: u32 = 0; // starts at one because of second_token
                            while (self.tokenizer.next(true)) |_| {
                                count += 1;
                            }

                            const define = self.tokenizer.unit.defines.getOrPut(define_str) catch @panic("OOM");

                            define.value_ptr.* = .{
                                .name_tok = first_token.?,
                                .range = .{
                                    .start = .{
                                        .index = first_token.?.index + 1,
                                        .file_index = first_token.?.file_index,
                                    },
                                    .end = .{
                                        .index = first_token.?.index + 1 + @as(TokenIndexType, @truncate(count)),
                                        .file_index = first_token.?.file_index,
                                    },
                                    .flags = 0,
                                },
                            };
                        }
                    } else if (std.mem.eql(u8, directive_str, "include")) {
                        const first_token_index = self.tokenizer.next(true) orelse @panic("Unexpected end of input");
                        const first_token = self.tokenizer.unit.token(first_token_index);

                        const full_file_path, const original_str = if (first_token.kind == .string_literal) blk2: {
                            const file_path = self.tokenizer.unit.stringAt(first_token_index);

                            const full_file_path = self.tokenizer.unit.searchQuoteDirs(self.file_index, file_path) orelse {
                                self.tokenizer.unit.addDiagnostic(.{
                                    .level = .err,
                                    .location = .{
                                        .single = first_token_index,
                                    },
                                    .kind = .{
                                        .file_not_found = .{ .file_name = file_path },
                                    },
                                });
                                return null;
                            };
                            break :blk2 .{ full_file_path, file_path };
                        } else if (first_token.kind == .lt) blk2: {
                            const first_string_token = self.tokenizer.next(false) orelse @panic("Unexpected EOF");
                            const end_index = blk1: {
                                while (self.tokenizer.next(false)) |pidx| {
                                    const p = self.tokenizer.unit.token(pidx);
                                    if (p.kind == .gt) {
                                        break :blk1 pidx;
                                    }
                                } else break :blk1 null;
                            };

                            const start_source_index = self.tokenizer.unit.token(first_string_token).start;
                            const end_source_index = self.tokenizer.unit.token(end_index.?).start;
                            const file_path = self.tokenizer.unit.files.items[self.file_index].source[start_source_index..end_source_index];
                            const full_file_path = self.tokenizer.unit.searchIncludeDirs(file_path) orelse {
                                self.tokenizer.unit.addDiagnostic(.{
                                    .level = .err,
                                    .location = .{
                                        .single = first_token_index,
                                    },
                                    .kind = .{
                                        .file_not_found = .{ .file_name = file_path },
                                    },
                                });
                                return null;
                            };

                            break :blk2 .{ full_file_path, file_path };
                        } else {
                            self.tokenizer.unit.addDiagnostic(.{
                                .level = .err,
                                .location = .{
                                    .single = first_token_index,
                                },
                                .kind = .{ .include_expects_filename = .{} },
                            });
                            return null;
                        };

                        var file = std.fs.openFileAbsolute(full_file_path, .{}) catch {
                            self.tokenizer.unit.addDiagnostic(.{
                                .level = .err,
                                .location = .{
                                    .single = first_token_index,
                                },
                                .kind = .{
                                    .reading_file_failed = .{ .file_name = original_str },
                                },
                            });
                            return null;
                        };
                        const file_contents = file.readToEndAlloc(self.tokenizer.allocator, std.math.maxInt(usize)) catch {
                            self.tokenizer.unit.addDiagnostic(.{
                                .level = .err,
                                .location = .{
                                    .single = first_token_index,
                                },
                                .kind = .{
                                    .reading_file_failed = .{ .file_name = original_str },
                                },
                            });
                            return null;
                        };

                        const unit_file = self.tokenizer.unit.addFile(full_file_path, file_contents);

                        _ = self.tokenizer.initFile(unit_file);

                        self.included_file = true;
                        self.tokenizer.in_define = old_define;
                        return null;
                    } else if (std.mem.eql(u8, directive_str, "ifdef")) {
                        const def_token_index = self.tokenizer.next(true) orelse {
                            self.tokenizer.unit.addDiagnostic(.{
                                .level = .err,
                                .location = .{
                                    .file_range = .{
                                        .position = start,
                                        .len = @truncate(1 + directive_str.len),
                                        .file_index = self.file_index,
                                    },
                                },
                                .kind = .{
                                    .no_macro_name = .{
                                        .directive = .{
                                            .file_range = .{
                                                .position = start,
                                                .len = @truncate(1 + directive_str.len),
                                                .file_index = self.file_index,
                                            },
                                        },
                                    },
                                },
                            });
                            return null;
                        };
                        const def_token = self.tokenizer.unit.token(def_token_index);
                        if (def_token.kind == .identifier) {
                            const def_token_str = self.tokenizer.unit.identifierAt(def_token_index);

                            if (!self.tokenizer.unit.defines.contains(def_token_str) and !self.tokenizer.unit.define_fns.contains(def_token_str)) {
                                self.tokenizer.conditional_skip = true;
                                self.tokenizer.if_stack.push(0) catch @panic("OOM");
                            } else {
                                self.tokenizer.if_stack.push(1) catch @panic("OOM");
                            }
                        } else {
                            self.tokenizer.conditional_skip = true;
                            self.tokenizer.if_stack.push(0) catch @panic("OOM");

                            self.tokenizer.unit.addDiagnostic(.{
                                .level = .err,
                                .location = .{
                                    .single = def_token_index,
                                },
                                .kind = .{ .macro_name_ident = .{} },
                            });
                        }
                    } else if (std.mem.eql(u8, directive_str, "ifndef")) {
                        const def_token_index = self.tokenizer.next(true) orelse {
                            self.tokenizer.unit.addDiagnostic(.{
                                .level = .err,
                                .location = .{
                                    .file_range = .{
                                        .position = start,
                                        .len = @truncate(1 + directive_str.len),
                                        .file_index = self.file_index,
                                    },
                                },
                                .kind = .{
                                    .no_macro_name = .{
                                        .directive = .{
                                            .file_range = .{
                                                .position = start,
                                                .len = @truncate(1 + directive_str.len),
                                                .file_index = self.file_index,
                                            },
                                        },
                                    },
                                },
                            });
                            return null;
                        };
                        const def_token = self.tokenizer.unit.token(def_token_index);
                        if (def_token.kind == .identifier) {
                            const def_token_str = self.tokenizer.unit.identifierAt(def_token_index);

                            if (self.tokenizer.unit.defines.contains(def_token_str) or self.tokenizer.unit.define_fns.contains(def_token_str)) {
                                self.tokenizer.conditional_skip = true;
                                self.tokenizer.if_stack.push(0) catch @panic("OOM");
                            } else {
                                self.tokenizer.if_stack.push(1) catch @panic("OOM");
                            }
                        } else {
                            self.tokenizer.conditional_skip = true;
                            self.tokenizer.if_stack.push(0) catch @panic("OOM");

                            self.tokenizer.unit.addDiagnostic(.{
                                .level = .err,
                                .location = .{
                                    .single = def_token_index,
                                },
                                .kind = .{ .macro_name_ident = .{} },
                            });
                        }
                    } else if (std.mem.eql(u8, directive_str, "if")) {
                        var evaluator = SimpleExpressionEvaluator{
                            .unit = self.tokenizer.unit,
                            .tokenizer = self.tokenizer,
                        };

                        self.tokenizer.in_define = false;
                        const value = evaluator.evaluate() catch |e| {
                            switch (e) {
                                error.UnexpectedToken => {
                                    self.tokenizer.unit.addDiagnostic(.{
                                        .level = .err,
                                        .location = .{
                                            .single = self.tokenizer.peek(true).?,
                                        },
                                        .kind = .{ .invalid_token_in_pp_expression = .{
                                            .found = self.tokenizer.peek(true).?,
                                        } },
                                    });
                                },
                                error.UnexpectedEnd => {
                                    self.tokenizer.unit.addDiagnostic(.{
                                        .level = .err,
                                        .location = .{ .file_position = .{
                                            .position = start + 1 + @as(u32, @truncate(directive_str.len)),
                                            .file_index = self.file_index,
                                        } },
                                        .kind = .{ .invalid_pp_expression = .{} },
                                    });
                                },
                            }
                            return null;
                        };
                        while (self.tokenizer.next(true)) |_| {}
                        self.tokenizer.in_define = true;

                        if (value.int_value == 0) {
                            self.tokenizer.conditional_skip = true;
                            self.tokenizer.if_stack.push(0) catch @panic("OOM");
                        } else {
                            self.tokenizer.if_stack.push(1) catch @panic("OOM");
                        }
                    } else if (std.mem.eql(u8, directive_str, "elif")) {
                        const did_condition = self.tokenizer.if_stack.peek();
                        self.tokenizer.conditional_skip = did_condition == 1;
                        var evaluator = SimpleExpressionEvaluator{
                            .unit = self.tokenizer.unit,
                            .tokenizer = self.tokenizer,
                        };

                        self.tokenizer.in_define = false;
                        const old_skip = self.tokenizer.conditional_skip;
                        self.tokenizer.conditional_skip = false;
                        const value = evaluator.evaluate() catch |e| {
                            switch (e) {
                                error.UnexpectedToken => {
                                    self.tokenizer.unit.addDiagnostic(.{
                                        .level = .err,
                                        .location = .{
                                            .single = self.tokenizer.peek(true).?,
                                        },
                                        .kind = .{ .invalid_token_in_pp_expression = .{
                                            .found = self.tokenizer.peek(true).?,
                                        } },
                                    });
                                },
                                error.UnexpectedEnd => {
                                    self.tokenizer.unit.addDiagnostic(.{
                                        .level = .err,
                                        .location = .{ .file_position = .{
                                            .position = start + 1 + @as(u32, @truncate(directive_str.len)),
                                            .file_index = self.file_index,
                                        } },
                                        .kind = .{ .invalid_pp_expression = .{} },
                                    });
                                },
                            }
                            return null;
                        };
                        while (self.tokenizer.next(true)) |_| {}
                        self.tokenizer.conditional_skip = old_skip;
                        self.tokenizer.in_define = true;

                        if (did_condition == 1 or value.int_value == 0) {
                            self.tokenizer.conditional_skip = true;
                        }
                    } else if (std.mem.eql(u8, directive_str, "undef")) {
                        const def_token_index = self.tokenizer.next(true) orelse {
                            self.tokenizer.unit.addDiagnostic(.{
                                .level = .err,
                                .location = .{
                                    .file_range = .{
                                        .position = start,
                                        .len = @truncate(1 + directive_str.len),
                                        .file_index = self.file_index,
                                    },
                                },
                                .kind = .{
                                    .no_macro_name = .{
                                        .directive = .{
                                            .file_range = .{
                                                .position = start,
                                                .len = @truncate(1 + directive_str.len),
                                                .file_index = self.file_index,
                                            },
                                        },
                                    },
                                },
                            });
                            return null;
                        };
                        const def_token = self.tokenizer.unit.token(def_token_index);
                        if (def_token.kind == .identifier) {
                            const def_token_str = self.tokenizer.unit.identifierAt(def_token_index);

                            if (!self.tokenizer.unit.defines.remove(def_token_str)) {
                                _ = self.tokenizer.unit.define_fns.remove(def_token_str);
                            }
                        } else {
                            while (self.tokenizer.next(true)) |_| {}
                            self.tokenizer.unit.addDiagnostic(.{
                                .level = .err,
                                .location = .{
                                    .single = def_token_index,
                                },
                                .kind = .{ .macro_name_ident = .{} },
                            });
                        }
                    } else if (std.mem.eql(u8, directive_str, "else")) {
                        const did_condition = self.tokenizer.if_stack.peek();
                        self.tokenizer.conditional_skip = did_condition == 1;
                    } else if (std.mem.eql(u8, directive_str, "endif")) {
                        _ = self.tokenizer.if_stack.pop();
                        self.tokenizer.conditional_skip = false;
                    } else if (std.mem.eql(u8, directive_str, "warning")) {
                        const first_token_index = self.tokenizer.next(true) orelse @panic("Unexpected end of input");
                        const first_token = self.tokenizer.unit.token(first_token_index);

                        while (self.tokenizer.next(true)) |_| {}

                        self.tokenizer.unit.addDiagnostic(.{
                            .level = .warning,
                            .location = .{
                                .single = first_token_index,
                            },
                            .kind = .{
                                .user_warning = .{
                                    .message = .{
                                        .file_range = .{
                                            .position = first_token.start,
                                            .len = @truncate(self.pos - first_token.start),
                                            .file_index = self.file_index,
                                        },
                                    },
                                },
                            },
                        });
                    } else if (std.mem.eql(u8, directive_str, "error")) {
                        const first_token_index = self.tokenizer.next(true) orelse @panic("Unexpected end of input");
                        const first_token = self.tokenizer.unit.token(first_token_index);

                        while (self.tokenizer.next(true)) |_| {}

                        self.tokenizer.unit.addDiagnostic(.{
                            .level = .err,
                            .location = .{
                                .single = first_token_index,
                            },
                            .kind = .{
                                .user_error = .{
                                    .message = .{
                                        .file_range = .{
                                            .position = first_token.start,
                                            .len = @truncate(self.pos - first_token.start),
                                            .file_index = self.file_index,
                                        },
                                    },
                                },
                            },
                        });
                    } else if (std.mem.eql(u8, directive_str, "pragma")) {
                        while (self.tokenizer.next(true)) |_| {}
                    } else {
                        while (self.tokenizer.next(true)) |_| {}
                        self.tokenizer.unit.addDiagnostic(.{
                            .level = .err,
                            .location = .{
                                .file_position = .{
                                    .file_index = self.file_index,
                                    .position = start_index,
                                },
                            },
                            .kind = .{ .invalid_pp_directive = .{} },
                        });
                    }

                    return null;
                },
                else => return error.InvalidCharacter,
            }
        };

        const token = Token{
            .kind = token_kind,
            .start = start,
        };

        const index = self.tokenizer.unit.tokens(self.file_index).items.len;
        self.tokenizer.unit.tokens(self.file_index).append(token) catch @panic("OOM");

        const token_index = TokenIndex{
            .index = @truncate(index),
            .file_index = self.file_index,
        };

        return token_index;
    }

    /// A token that can be itself or have an = suffix
    fn opEq(self: *Self, regular: TokenKind, eq: TokenKind) TokenKind {
        if (self.pos + 1 < self.source_len and self.source[self.pos + 1] == '=') {
            return self.advanceToken(eq);
        }
        return self.advanceToken(regular);
    }

    /// Advance source position by the token kind
    fn advanceToken(self: *Self, kind: TokenKind) TokenKind {
        switch (kind) {
            .open_paren,
            .open_brace,
            .open_bracket,
            .close_paren,
            .close_brace,
            .close_bracket,
            .comma,
            .semicolon,
            .assignment,

            .star,
            .slash,
            .percent,
            .plus,
            .minus,
            .lt,
            .gt,
            .ampersand,
            .carot,
            .pipe,
            .exclamation,
            .tilde,
            .dot,
            .colon,
            .question,
            .hash,
            .at,
            => self.pos += 1,

            .plusplus,
            .minusminus,
            .double_ampersand,
            .double_pipe,
            .bit_left_shift,
            .bit_right_shift,
            .equality,
            .nequality,
            .plus_eq,
            .minus_eq,
            .star_eq,
            .slash_eq,
            .percent_eq,
            .ampersand_eq,
            .carot_eq,
            .pipe_eq,
            .arrow,
            .gte,
            .lte,
            .hashhash,
            => self.pos += 2,

            .left_shift_eq,
            .right_shift_eq,
            .ellipsis,
            => self.pos += 3,

            else => @panic("Cannot detmerine for this type of token"),
        }
        return kind;
    }
};

const ExpansionArgumentMap = struct {
    map: ArgumentMap,
    va_arg: ?Argument,
    depth: u32,
};

pub const ExpansionContext = struct {
    name_token: TokenIndex,
    callsite: diag.DiagnosticLocation,
};

/// Tokenizer for unit. The creates FileTokenizers when needed and pops them onto a stack.
/// The next token comes first from the top of the stack
pub const Tokenizer = struct {
    allocator: std.mem.Allocator,
    unit: *Unit,
    file_stack: std.SinglyLinkedList(FileTokenizer),

    virtual_stack: std.ArrayList(TokenRange),
    expansion_stack: std.ArrayList(ExpansionArgumentMap),
    expansion_ctx_stack: std.MultiArrayList(ExpansionContext),
    if_stack: std.BitStack,
    peeked_token: ?TokenIndex = null,
    in_define: bool = false,
    expansion_depth: u32 = 0,
    disabled_macros: std.StringHashMap(void),

    conditional_skip: bool = false,

    const Self = @This();

    pub fn init(allocator: std.mem.Allocator, unit: *Unit) Self {
        return .{
            .allocator = allocator,
            .unit = unit,
            .file_stack = std.SinglyLinkedList(FileTokenizer){},
            .virtual_stack = std.ArrayList(TokenRange).init(allocator),
            .expansion_stack = std.ArrayList(ExpansionArgumentMap).init(allocator),
            .expansion_ctx_stack = .{},
            .disabled_macros = .init(allocator),
            .if_stack = std.BitStack.init(allocator),
        };
    }

    /// Create a new file on the stack.
    pub fn initFile(self: *Self, file_index: FileIndexType) *FileTokenizer {
        // const file_tok = self.file_stack.addOne() catch @panic("OOM");

        var node = self.allocator.create(std.SinglyLinkedList(FileTokenizer).Node) catch @panic("OOM");
        self.file_stack.prepend(node);
        node.data = .{
            .tokenizer = self,
            .source = self.unit.files.items[file_index].source,
            .source_len = @truncate(self.unit.files.items[file_index].source.len),
            .file_index = file_index,
        };

        return &node.data;
    }

    /// Add virtual tokens to the virtual token stack
    fn pushVirtual(self: *Self, range: TokenRange) void {
        self.virtual_stack.append(range) catch @panic("OOM");
    }

    fn pushVirtualRanges(self: *Self, ranges: []const TokenRange) void {
        var i = ranges.len;
        while (i > 0) {
            i -= 1;
            var rng = ranges[i];
            rng.flags |= TokenRange.Flags.ALREADY_EXPANDED;
            self.virtual_stack.append(rng) catch @panic("OOM");
        }
    }

    /// Returns the top of the virtual token stack
    inline fn currentRange(self: *Self) TokenRange {
        return self.virtual_stack.items[self.virtual_stack.items.len - 1];
    }

    /// Returns the top of the file stack
    inline fn currentFile(self: *Self) *FileTokenizer {
        // return &self.file_stack.items[self.file_stack.items.len - 1];
        return &self.file_stack.first.?.data;
    }

    /// Pops the top of the virtual token stack and returns the value.
    fn popVirtual(self: *Self) TokenRange {
        return self.virtual_stack.pop();
    }

    /// Peeks the next token, doesn't actually consume (technically does but only once).
    /// eol: if true, only tokenize to the end of the current line stops right before \n
    pub fn peek(self: *Self, eol: bool) ?TokenIndex {
        if (self.peeked_token) |tok| {
            return tok;
        }

        self.peeked_token = self.rawNext(eol);
        return self.peeked_token;
    }

    /// Consumes and returns the next token. Returns peeked token if it exists.
    /// eol: if true, only tokenize to the end of the current line stops right before \n
    pub fn next(self: *Self, eol: bool) ?TokenIndex {
        if (self.peeked_token) |tok| {
            self.peeked_token = null;
            return tok;
        }

        return self.rawNext(eol);
    }

    /// Consumes and returns next token without the looking at the peek token.
    /// This should probably only be used internally (i don't know of a use case otherwise).
    /// eol: if true, only tokenize to the end of the current line stops right before \n
    pub fn rawNext(self: *Self, eol: bool) ?TokenIndex {
        while (self.file_stack.first != null) {
            var file = self.currentFile();
            if (file.next(eol)) |result| {
                return result;
            } else if (eol) {
                self.in_define = false;
                return null;
            } else if (file.included_file) {
                file.included_file = false;
                continue;
            }
            if (self.file_stack.first != null) {
                file = self.currentFile(); // This might change when calling next
                if (file.pos >= file.source_len) {
                    // self.file_stack.items.len -= 1;
                    _ = self.file_stack.popFirst();

                    self.in_define = false;
                    self.conditional_skip = false;
                }
            }
        }
        return null;
    }

    pub fn printIncludeStack(self: *const Self) void {
        var current = self.file_stack.first;
        while (current != null) : (current = current.?.next) {
            const file = &self.unit.files.items[current.?.data.file_index];
            std.debug.print("Included From \x1b[1m{s}\x1b[0m", .{file.file_path});
        }
    }
};

/// A preprocessor value (only ints are supported for now)
const Value = union(enum) {
    int_value: i64,
    float_value: f64,
};

const EvaluationError = error{
    UnexpectedToken,
    UnexpectedEnd,
};

/// Used for evaluating preprocessor if directives
const SimpleExpressionEvaluator = struct {
    unit: *Unit,
    tokenizer: *Tokenizer,
    pos: u32 = 0,

    const Self = @This();

    pub fn evaluate(self: *Self) !Value {
        return self.evaluateOperatorExpression(0);
    }

    pub fn unaryPrecedenceRight(token: *const Token) u16 {
        return switch (token.kind) {
            .plus,
            .minus,
            .exclamation,
            .tilde,
            => 140,
            else => 0,
        };
    }

    pub fn binaryPrecedence(token: *const Token) u16 {
        return switch (token.kind) {
            .star, .slash, .percent => 130,
            .plus, .minus => 120,
            .bit_left_shift, .bit_right_shift => 110,
            .gt, .gte, .lt, .lte => 100,
            .equality, .nequality => 90,
            .carot => 70,
            .pipe => 60,
            .double_ampersand => 50,
            .double_pipe => 40,
            else => 0,
        };
    }

    pub fn evaluateOperatorExpression(self: *Self, last_prec: u16) EvaluationError!Value {
        var op = self.peek();
        var left: Value = undefined;
        blk: {
            if (op != null) {
                if (op.?.token.kind == .identifier) {
                    // 140 is the precedence of defined
                    if (140 >= last_prec and std.mem.eql(u8, self.unit.identifierAt(op.?.index), "defined")) {
                        self.next();

                        const old_in_define = self.tokenizer.in_define;
                        defer self.tokenizer.in_define = old_in_define;
                        self.tokenizer.in_define = true;

                        op = self.peek();
                        if (op != null and op.?.token.kind == .open_paren) {
                            self.next();
                            op = self.peek();
                            if (op == null) {
                                return error.UnexpectedEnd;
                            }
                            switch (op.?.token.kind) {
                                .identifier => {
                                    self.next();
                                    const ident_str = self.unit.identifierAt(op.?.index);

                                    if (self.unit.defines.contains(ident_str) or self.unit.define_fns.contains(ident_str)) {
                                        left = .{ .int_value = 1 };
                                    } else {
                                        left = .{ .int_value = 0 };
                                    }
                                },

                                .unsigned, .signed, .char, .short, .int, .long, .float, .double, .bool => {
                                    self.next();
                                    left = .{ .int_value = 1 };
                                },
                                else => std.log.err("Founded token {}, file: {s}, pos: {}", .{ op.?.token.kind, self.unit.files.items[op.?.index.file_index].file_path, self.unit.token(op.?.index).start }),
                            }
                            try self.expect(.close_paren);
                        } else {
                            if (op != null and op.?.token.kind != .identifier) {
                                std.log.err("Founded token {}, file: {s}, pos: {}", .{ op.?.token.kind, self.unit.files.items[op.?.index.file_index].file_path, self.unit.token(op.?.index).start });
                                return error.UnexpectedToken;
                            }
                            self.next();
                            const ident_str = self.unit.identifierAt(op.?.index);
                            if (self.unit.defines.contains(ident_str) or self.unit.define_fns.contains(ident_str)) {
                                left = .{ .int_value = 1 };
                            } else {
                                left = .{ .int_value = 0 };
                            }
                        }
                        break :blk;
                    }
                }

                const unary_prec_right = unaryPrecedenceRight(&op.?.token);

                if (unary_prec_right != 0 and unary_prec_right >= last_prec) {
                    self.next();
                    left = try self.evaluateOperatorExpression(unary_prec_right);

                    left = switch (left) {
                        .int_value => Value{ .int_value = switch (op.?.token.kind) {
                            .minus => -left.int_value,
                            .plus => left.int_value,
                            .exclamation => if (left.int_value > 0) 0 else 1,
                            .tilde => ~left.int_value,
                            else => unreachable,
                        } },
                        else => unreachable,
                    };

                    break :blk;
                }
            }

            left = try self.evaluatePrimary();
        }

        while (true) {
            op = self.peek();
            if (op == null) break;
            if (op.?.token.kind == .open_paren) {
                self.next();
                op = self.peek();
                var indent: u32 = 0;
                while (op) |p| : (op = self.peek()) {
                    switch (p.token.kind) {
                        .open_paren => indent += 1,
                        .close_paren => {
                            if (indent == 0) {
                                self.next();
                                break;
                            }
                            indent -= 1;
                        },
                        else => {},
                    }
                    self.next();
                }
                left = .{ .int_value = 0 };
                continue;
            }
            const prec = binaryPrecedence(&op.?.token);

            if (prec <= last_prec or prec == 0) {
                break;
            }
            self.next(); // consume operator

            const right = try self.evaluateOperatorExpression(prec);

            left = switch (left) {
                .int_value => Value{
                    .int_value = switch (op.?.token.kind) {
                        .plus => left.int_value + right.int_value,
                        .minus => left.int_value - right.int_value,
                        .star => left.int_value * right.int_value,
                        .slash => @divTrunc(left.int_value, right.int_value),
                        .bit_left_shift => @shlWithOverflow(left.int_value, @as(u6, @intCast(right.int_value)))[0],
                        .bit_right_shift => @shrExact(left.int_value, @as(u6, @intCast(right.int_value))),
                        .ampersand => left.int_value & right.int_value,
                        .pipe => left.int_value | right.int_value,
                        .carot => left.int_value ^ right.int_value,

                        .gt => if (left.int_value > right.int_value) 1 else 0,
                        .gte => if (left.int_value >= right.int_value) 1 else 0,
                        .lt => if (left.int_value < right.int_value) 1 else 0,
                        .lte => if (left.int_value <= right.int_value) 1 else 0,
                        .double_ampersand => if (left.int_value > 0 and right.int_value > 0) 1 else 0,
                        .double_pipe => if (left.int_value > 0 or right.int_value > 0) 1 else 0,
                        .equality => if (left.int_value == right.int_value) 1 else 0,
                        .nequality => if (left.int_value != right.int_value) 1 else 0,
                        else => unreachable,
                    },
                },
                .float_value => unreachable,
            };
        }

        return left;
    }

    pub fn evaluatePrimary(self: *Self) !Value {
        const ptok = self.peek() orelse {
            return error.UnexpectedEnd;
        };
        switch (ptok.token.kind) {
            .open_paren => {
                self.next();
                const result = try self.evaluate();
                try self.expect(.close_paren);
                return result;
            },
            .int_literal,
            .unsigned_int_literal,
            .long_literal,
            .unsigned_long_literal,
            .long_long_literal,
            .unsigned_long_long_literal,
            => {
                self.next();
                return .{ .int_value = @intCast(self.unit.ivalue(ptok.index)) };
            },
            .identifier => {
                self.next();
                return .{ .int_value = 0 };
                // std.log.err("Founded identifier {s}", .{self.unit.identifierAt(ptok.index)});
                // return error.UnexpectedToken;
            },
            else => {
                std.log.err("Founded token {}, file: {s}, pos: {}", .{ ptok.token.kind, self.unit.filePos(ptok.index)[0], self.unit.filePos(ptok.index)[1] });
                return error.UnexpectedToken;
            },
        }
    }

    const NextResult = struct {
        index: TokenIndex,
        token: Token,
    };

    fn expect(self: *Self, kind: TokenKind) !void {
        const ptok = self.peek();
        if (ptok == null) {
            return error.UnexpectedEnd;
        }

        if (ptok.?.token.kind != kind) {
            std.log.err("Founded token {}, file: {s}, pos: {}", .{ ptok.?.token.kind, self.unit.files.items[ptok.?.index.file_index].file_path, self.unit.token(ptok.?.index) });
            return error.UnexpectedToken;
        }
        self.next();
    }

    fn peek(self: *Self) ?NextResult {
        const index = self.tokenizer.peek(true);
        if (index == null) return null;

        return .{
            .index = index.?,
            .token = self.unit.token(index.?),
        };
    }

    fn next(self: *Self) void {
        _ = self.tokenizer.next(true);
    }
};
