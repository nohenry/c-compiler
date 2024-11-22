const std = @import("std");
const Unit = @import("unit.zig").Unit;
const DefineValue = @import("unit.zig").DefineValue;
const Parser = @import("parser.zig").Parser;

pub const TokenKind = enum(u32) {
    float_literal,
    double_literal,
    int_literal,
    unsigned_int_literal,
    long_literal,
    unsigned_long_literal,
    long_long_literal,
    unsigned_long_long_literal,
    size_literal,
    unsigned_size_literal,
    string_literal,
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

            else => @panic(""),
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
pub const TokenIndex = packed struct(u32) {
    index: TokenIndexType,
    file_index: FileIndexType,
};

pub const TokenRange = struct {
    start: TokenIndex,
    end: TokenIndex,
    flags: Flags.Type,

    pub const Flags = struct {
        pub const Type = u8;
        pub const EXPANSION_ARGUMENTS: Type = (1 << 0);
    };
};

pub const Token = struct {
    kind: TokenKind,
    start: u32,
};

pub const ArgumentMap = std.StringHashMap(DefineValue);

pub const FileTokenizer = struct {
    tokenizer: *Tokenizer,
    pos: u32 = 0,
    source: []const u8,
    file_index: FileIndexType,
    included_file: bool = false,

    const Self = @This();

    inline fn isNot(self: *Self, offsetFromPos: u32, char: u8) bool {
        return (self.pos + offsetFromPos < self.source.len) and self.source[self.pos + offsetFromPos] != char;
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

    pub fn consumeSkippable(self: *Self, eol: bool, return_value: *?TokenIndex) bool {
        while (self.tokenizer.virtual_stack.items.len > 0) : (self.tokenizer.virtual_stack.items.len -= 1) {
            const item_index = self.tokenizer.virtual_stack.getLast();
            if (item_index.start.index < item_index.end.index) {
                const item = self.tokenizer.unit.token(item_index.start);
                self.tokenizer.virtual_stack.items[self.tokenizer.virtual_stack.items.len - 1].start.index += 1;

                if (item.kind == .identifier) {
                    const ident_str = self.tokenizer.unit.identifier(item_index.start);

                    if (self.checkExpansion(ident_str, false)) |value| {
                        return_value.* = value;
                        return true;
                    }
                }

                return_value.* = item_index.start;
                return true;
            }

            if ((item_index.flags & TokenRange.Flags.EXPANSION_ARGUMENTS) > 0) {
                self.tokenizer.expansion_stack.items.len -= 1;
            }
        }

        var c: u8 = undefined;
        if (self.tokenizer.conditional_skip) blk: {
            var indent: u32 = 0;
            while (self.pos < self.source.len and self.tokenizer.conditional_skip) : (self.pos += 1) {
                c = self.source[self.pos];
                if (c == '\n' and eol) {
                    return true;
                }

                if (c == '#') {
                    const very_start_index = self.pos;
                    self.pos += 1;
                    var start_index = self.pos;
                    var before = true;
                    while (self.pos < self.source.len) : (self.pos += 1) {
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
                }
            }
        }

        while (self.pos < self.source.len) : (self.pos += 1) {
            c = self.source[self.pos];
            switch (c) {
                ' ', '\t', '\r' => continue,
                '\\' => {
                    if (eol) {
                        while (self.isNot(0, '\n')) : (self.pos += 1) {}
                        continue;
                    } else break;
                },
                '\n' => {
                    if (eol) {
                        return true;
                    } else continue;
                },
                '/' => {
                    if (self.pos + 1 < self.source.len) {
                        if (self.source[self.pos + 1] == '/') {
                            while (self.isNot(0, '\n')) : (self.pos += 1) {}
                        } else if (self.source[self.pos + 1] == '*') {
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

    pub fn next(self: *Self, eol: bool) ?TokenIndex {
        var return_value: ?TokenIndex = null;
        while (true) {
            if (self.consumeSkippable(eol, &return_value)) {
                if (return_value) |value| return value else return null;
            }

            if (self.nextNonWhitespace(eol)) |value| {
                return value;
            }
            if (self.included_file) {
                return null;
            }
        }
    }

    fn searchExpansionArg(self: *Self, arg: []const u8) ?DefineValue {
        if (self.tokenizer.expansion_stack.items.len == 0) return null;
        var index = @as(isize, @intCast(self.tokenizer.expansion_stack.items.len - 1));
        while (index >= 0) : (index -= 1) {
            if (self.tokenizer.expansion_stack.items[@as(usize, @intCast(index))].get(arg)) |value| {
                return value;
            }
        }

        return null;
    }

    fn expect(self: *Self, kind: TokenKind) TokenIndex {
        const tidx = self.tokenizer.next(false);
        if (tidx == null) @panic("Unexpected EOF");

        const token = self.tokenizer.unit.token(tidx.?);
        if (token.kind == kind) {
            return tidx.?;
        } else {
            std.log.err("File: {s}, pos: {}", .{ self.tokenizer.unit.files.items[tidx.?.file_index].file_path, token.start });
            std.debug.panic("Expected token {}, found {}, {}", .{ kind, token.kind, self.tokenizer.unit.ivalue(tidx.?) });
        }
    }

    fn checkExpansion(self: *Self, str: []const u8, eol: bool) ?TokenIndex {
        if (self.searchExpansionArg(str)) |value| {
            self.tokenizer.pushVirtual(value.range);

            return self.tokenizer.next(eol);
        }

        if (self.tokenizer.unit.defines.get(str)) |def| {
            if (def.range.start.index < def.range.end.index) {
                self.tokenizer.pushVirtual(def.range);
            }
            return self.tokenizer.next(eol);
        }

        if (self.tokenizer.unit.define_fns.get(str)) |def| {
            const open_paren_index = self.expect(.open_paren);

            var argument_map = ArgumentMap.init(self.tokenizer.allocator);
            var parameter_iter = def.parameters.iterator();

            var indent: u32 = 0;

            var argument_start_token_index = TokenIndex{
                .index = open_paren_index.index + 1,
                .file_index = open_paren_index.file_index,
            };
            blk1: {
                var i: u32 = 0;
                while (self.tokenizer.next(false)) |pidx| {
                    const p = self.tokenizer.unit.token(pidx);

                    switch (p.kind) {
                        .open_paren, .open_brace, .open_bracket => indent += 1,
                        .close_paren => {
                            if (indent == 0) {
                                if (i > 0) {
                                    const param = parameter_iter.next() orelse @panic("TODO: parameter arg mismatch");
                                    argument_map.put(param.key_ptr.*, DefineValue{
                                        .range = .{
                                            .start = argument_start_token_index,
                                            .end = pidx,
                                            .flags = 0,
                                        },
                                    }) catch @panic("OOM");
                                }
                                break :blk1;
                            }
                        },
                        .close_brace, .close_bracket => indent -= 1,
                        .comma => {
                            if (indent == 0) {
                                const param = parameter_iter.next() orelse @panic("TODO: parameter arg mismatch");
                                argument_map.put(param.key_ptr.*, DefineValue{
                                    .range = .{
                                        .start = argument_start_token_index,
                                        .end = pidx,
                                        .flags = 0,
                                    },
                                }) catch @panic("OOM");
                                argument_start_token_index.index = @truncate(self.tokenCountFile(argument_start_token_index.file_index));
                            }
                        },
                        else => {},
                    }
                    i += 1;
                }
            }
            // _ = self.expect(.close_paren);

            if (argument_map.count() != def.parameters.count()) {
                std.debug.panic("TODO: error (expected {} arg found {})", .{ def.parameters.count(), argument_map.count() });
            }

            self.tokenizer.expansion_stack.append(argument_map) catch @panic("OOM");

            if (def.range.start.index < def.range.end.index) {
                self.tokenizer.pushVirtual(TokenRange{
                    .start = def.range.start,
                    .end = def.range.end,
                    .flags = TokenRange.Flags.EXPANSION_ARGUMENTS,
                });
            }

            return self.next(eol);
        }

        return null;
    }

    fn nextNonWhitespace(self: *Self, eol: bool) ?TokenIndex {
        const c = self.source[self.pos];
        const start = self.pos;
        const token_kind = blk: {
            switch (c) {
                '0'...'9' => {
                    const IS_FLOAT: u8 = (1 << 0);
                    const DID_DOT: u8 = (1 << 1);
                    const DID_E: u8 = (1 << 2);
                    var base: u8 = if (c == '0') 8 else 10;
                    var flags: u8 = 0;
                    while (self.pos < self.source.len) : (self.pos += 1) doneLit: {
                        switch (self.source[self.pos]) {
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
                                if (base == 8 and self.pos == start + 1) {
                                    base = 2;
                                } else if (base > 10 and 1 < base - 10) {
                                    continue;
                                } else {
                                    std.debug.panic("Found \x1b[1;36m'{c}'\x1b[0m which is invalid for base \x1b[1;33m{}\x1b[0m", .{x, base});
                                }
                            },
                            'x', 'X' => |x| {
                                if (base == 8 and self.pos == start + 1) {
                                    base = 16;
                                } else {
                                    std.debug.panic("Found \x1b[1;36m'{c}'\x1b[0m which is invalid for base \x1b[1;33m{}\x1b[0m", .{x, base});
                                }
                            },
                            '0' => continue,
                            '1'...'9' => |x| if (x - '0' < base) {
                                continue;
                            } else {
                                std.debug.panic("Found \x1b[1;36m'{c}'\x1b[0m which is invalid for base \x1b[1;33m{}\x1b[0m", .{x, base});
                            },
                            'a', 'c', 'd', 'f'...'k' => |x| if (x - 'a' < base - 10) {
                                continue;
                            } else {
                                std.debug.panic("Found \x1b[1;36m'{c}'\x1b[0m which is invalid for base \x1b[1;33m{}\x1b[0m", .{x, base});
                            },
                            'A', 'C', 'D', 'F'...'K' => |x| if (x - 'A' < base - 10) {
                                continue;
                            } else {
                                std.debug.panic("Found \x1b[1;36m'{c}'\x1b[0m which is invalid for base \x1b[1;33m{}\x1b[0m", .{x, base});
                            },
                            else => break,
                        }
                    }

                    var unsigned = false;
                    var long = false;
                    while (self.pos < self.source.len) : (self.pos += 1) {
                        switch (self.source[self.pos]) {
                            'f' => {
                                self.pos += 1;
                                break :blk TokenKind.float_literal;
                            },
                            'u' => {
                                if (long) break;
                                self.pos += 1;
                                unsigned = true;
                            },
                            'z', 'Z' => {
                                if (long) break;
                                self.pos += 1;
                                if (unsigned)
                                    break :blk TokenKind.unsigned_size_literal
                                else
                                    break :blk TokenKind.size_literal;
                            },
                            'l', 'L' => {
                                if (long) {
                                    self.pos += 1;
                                    if (unsigned)
                                        break :blk TokenKind.unsigned_long_long_literal
                                    else
                                        break :blk TokenKind.long_long_literal;
                                } else {
                                    long = true;
                                }
                            },
                            else => break,
                        }
                    }

                    if ((flags & IS_FLOAT) > 0)
                        break :blk TokenKind.double_literal
                    else if (unsigned and long)
                        break :blk TokenKind.unsigned_long_literal
                    else if (unsigned)
                        break :blk TokenKind.unsigned_int_literal
                    else if (long)
                        break :blk TokenKind.long_literal
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
                '.' => {
                    if (self.pos + 2 < self.source.len) {
                        if (self.source[self.pos + 1] == '.' and self.source[self.pos + 2] == '.') {
                            break :blk self.advanceToken(TokenKind.ellipsis);
                        }
                    }
                    break :blk self.advanceToken(TokenKind.dot);
                },
                '=' => {
                    if (self.pos + 1 < self.source.len) {
                        switch (self.source[self.pos + 1]) {
                            '=' => break :blk self.advanceToken(TokenKind.equality),
                            else => {},
                        }
                    }
                    break :blk self.advanceToken(TokenKind.assignment);
                },
                '!' => {
                    if (self.pos + 1 < self.source.len) {
                        switch (self.source[self.pos + 1]) {
                            '=' => break :blk self.advanceToken(TokenKind.nequality),
                            else => {},
                        }
                    }
                    break :blk self.advanceToken(TokenKind.exclamation);
                },

                '+' => if (self.pos + 1 < self.source.len) {
                    switch (self.source[self.pos + 1]) {
                        '+' => break :blk self.advanceToken(TokenKind.plusplus),
                        '=' => break :blk self.advanceToken(TokenKind.plus_eq),
                        else => break :blk self.advanceToken(TokenKind.plus),
                    }
                } else break :blk self.advanceToken(TokenKind.plus),
                '-' => if (self.pos + 1 < self.source.len) {
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
                    if (self.pos + 1 < self.source.len) {
                        switch (self.source[self.pos + 1]) {
                            '&' => break :blk self.advanceToken(TokenKind.double_ampersand),
                            '=' => break :blk self.advanceToken(TokenKind.ampersand_eq),
                            else => {},
                        }
                    }
                    break :blk self.advanceToken(TokenKind.ampersand);
                },
                '|' => {
                    if (self.pos + 1 < self.source.len) {
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
                    if (self.pos + 1 < self.source.len) {
                        switch (self.source[self.pos + 1]) {
                            '<' => {
                                if (self.pos + 2 < self.source.len) {
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
                    while (self.pos < self.source.len) : (self.pos += 1) {
                        switch (self.source[self.pos]) {
                            '"' => break,
                            else => {},
                        }
                    }
                    self.pos += 1;
                    break :blk TokenKind.string_literal;
                },
                '\'' => {
                    self.pos += 1;
                    while (self.pos < self.source.len) : (self.pos += 1) {
                        switch (self.source[self.pos]) {
                            '\'' => break,
                            else => {},
                        }
                    }
                    self.pos += 1;
                    break :blk TokenKind.char_literal;
                },
                '>' => {
                    if (self.pos + 1 < self.source.len) {
                        switch (self.source[self.pos + 1]) {
                            '>' => {
                                if (self.pos + 2 < self.source.len) {
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

                'a'...'z', 'A'...'Z', '_' => {
                    while (self.pos < self.source.len) : (self.pos += 1) {
                        switch (self.source[self.pos]) {
                            'a'...'z', 'A'...'Z', '_', '0'...'9' => continue,
                            else => break,
                        }
                    }
                    const str = self.source[start..self.pos];

                    if (!self.tokenizer.in_define) {
                        if (self.checkExpansion(str, eol)) |token| {
                            return token;
                        }
                    }

                    if (self.tokenizer.unit.type_names.contains(str)) {
                        break :blk TokenKind.type_name;
                    }

                    if (keyword_map.get(str)) |kind| {
                        break :blk kind;
                    }

                    // const index = self.tokenizer.unit.getOrPut(str);

                    break :blk TokenKind.identifier;
                },

                '#' => {
                    const old_define = self.tokenizer.in_define;
                    self.tokenizer.in_define = true;

                    self.pos += 1;
                    var start_index = self.pos;
                    var before = true;
                    while (self.pos < self.source.len) : (self.pos += 1) {
                        switch (self.source[self.pos]) {
                            'a'...'z', 'A'...'Z', '_' => {
                                before = false;
                            },
                            ' ', '\t' => {
                                if (!before) break;
                                start_index += 1;
                            },
                            else => break,
                        }
                    }
                    const directive_str = self.source[start_index..self.pos];

                    if (std.mem.eql(u8, directive_str, "define")) {
                        const first_token = self.tokenizer.next(true); // identifier
                        const second_token = if (self.pos < self.source.len and self.source[self.pos] == '(') // Open paren must be immediately after identifier
                            self.tokenizer.next(true)
                        else
                            null;

                        if (first_token == null) {
                            @panic("Unexpected EOF");
                        }

                        const define_name = self.tokenizer.unit.token(first_token.?);
                        std.debug.assert(define_name.kind == .identifier);
                        const define_str = self.tokenizer.unit.identifier(first_token.?);
                        var var_arg = false;

                        if (second_token != null and self.tokenizer.unit.token(second_token.?).kind == .open_paren) {
                            // Function type macro
                            var parameter_map = std.StringArrayHashMap(void).init(self.tokenizer.allocator);

                            while (self.tokenizer.next(true)) |pidx| {
                                const p = self.tokenizer.unit.token(pidx);
                                switch (p.kind) {
                                    .identifier => parameter_map.put(self.tokenizer.unit.identifier(pidx), {}) catch @panic("OOM"),
                                    .ellipsis => {
                                        var_arg = true;
                                        _ = self.expect(.close_paren);
                                        break;
                                    },
                                    .close_paren => {
                                        break;
                                    },
                                    else => std.debug.panic("TODO: add error handling (expected comma or paren) {}, file: {s}, pos: {}", .{
                                        p.kind,
                                        self.tokenizer.unit.files.items[pidx.file_index].file_path,
                                        self.tokenizer.unit.files.items[pidx.file_index].tokens.items[pidx.index].start,
                                    }),
                                }

                                const ptok = self.tokenizer.peek(true);
                                if (ptok == null) @panic("TODO: expected at least paren");

                                switch (self.tokenizer.unit.token(ptok.?).kind) {
                                    .comma => _ = self.tokenizer.next(true),
                                    .close_paren => {
                                        _ = self.tokenizer.next(true);
                                        break;
                                    },
                                    else => std.debug.panic("TODO: add error handling (expected comma or paren) {}", .{self.tokenizer.unit.token(ptok.?).kind}),
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
                            // if (define.found_existing) {
                            //     std.debug.panic("Found existing define of name \x1b[1;36m'{s}'\x1b[0m", .{define_str});
                            // }

                            define.value_ptr.* = .{
                                .range = .{
                                    .start = pidx,
                                    .end = .{
                                        .index = pidx.index + @as(TokenIndexType, @truncate(count)),
                                        .file_index = pidx.file_index,
                                    },
                                    .flags = 0,
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
                            // if (define.found_existing) {
                            //     std.debug.panic("Found existing define of name \x1b[1;36m'{s}'\x1b[0m", .{define_str});
                            // }

                            define.value_ptr.* = .{
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

                        const full_file_path = if (first_token.kind == .string_literal) blk2: {
                            const file_path = self.tokenizer.unit.stringAt(first_token_index);

                            const full_file_path = self.tokenizer.unit.searchQuoteDirs(self.file_index, file_path) orelse std.debug.panic("Cannot find include file \x1b[1;36m'{s}'\x1b[0m", .{file_path});
                            break :blk2 full_file_path;
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
                            const full_file_path = self.tokenizer.unit.searchIncludeDirs(file_path) orelse std.debug.panic("Cannot find include file \x1b[1;36m'{s}'\x1b[0m", .{file_path});

                            break :blk2 full_file_path;
                        } else {
                            std.debug.panic("Expected string literal or <> for include path. Found {}", .{first_token.kind});
                        };

                        var file = std.fs.openFileAbsolute(full_file_path, .{}) catch @panic("error opening file");
                        const file_contents = file.readToEndAlloc(self.tokenizer.allocator, std.math.maxInt(usize)) catch |e| std.debug.panic("foo {}", .{e});

                        const unit_file = self.tokenizer.unit.addFile(full_file_path, file_contents);

                        _ = self.tokenizer.initFile(unit_file);

                        self.included_file = true;
                        return null;
                    } else if (std.mem.eql(u8, directive_str, "ifdef")) {
                        const def_token_index = self.tokenizer.next(true) orelse @panic("Unexpected EOF. Expected identifier");
                        const def_token = self.tokenizer.unit.token(def_token_index);
                        if (def_token.kind == .identifier) {
                            const def_token_str = self.tokenizer.unit.identifier(def_token_index);

                            if (!self.tokenizer.unit.defines.contains(def_token_str) and !self.tokenizer.unit.define_fns.contains(def_token_str)) {
                                self.tokenizer.conditional_skip = true;
                                self.tokenizer.if_stack.push(0) catch @panic("OOM");
                            } else {
                                self.tokenizer.if_stack.push(1) catch @panic("OOM");
                            }
                        } else {
                            std.debug.panic("Expected identifier but found {}", .{def_token.kind});
                        }
                    } else if (std.mem.eql(u8, directive_str, "ifndef")) {
                        const def_token_index = self.tokenizer.next(true) orelse @panic("Unexpected EOF. Expected identifier");
                        const def_token = self.tokenizer.unit.token(def_token_index);
                        if (def_token.kind == .identifier) {
                            const def_token_str = self.tokenizer.unit.identifier(def_token_index);

                            if (self.tokenizer.unit.defines.contains(def_token_str) or self.tokenizer.unit.define_fns.contains(def_token_str)) {
                                self.tokenizer.conditional_skip = true;
                                self.tokenizer.if_stack.push(0) catch @panic("OOM");
                            } else {
                                self.tokenizer.if_stack.push(1) catch @panic("OOM");
                            }
                        } else {
                            std.debug.panic("Expected identifier but found {}", .{def_token.kind});
                        }
                    } else if (std.mem.eql(u8, directive_str, "if")) {
                        var evaluator = SimpleExpressionEvaluator{
                            .unit = self.tokenizer.unit,
                            .tokenizer = self.tokenizer,
                        };

                        self.tokenizer.in_define = false;
                        const value = evaluator.evaluate() catch @panic("failed to evaluate #if");
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
                        const value = evaluator.evaluate() catch @panic("failed to evaluate #if");
                        while (self.tokenizer.next(true)) |_| {}
                        self.tokenizer.conditional_skip = old_skip;
                        self.tokenizer.in_define = true;

                        if (did_condition == 1 or value.int_value == 0) {
                            self.tokenizer.conditional_skip = true;
                        }
                    } else if (std.mem.eql(u8, directive_str, "undef")) {
                        const def_token_index = self.tokenizer.next(true) orelse @panic("Unexpected EOF. Expected identifier");
                        const def_token = self.tokenizer.unit.token(def_token_index);
                        if (def_token.kind == .identifier) {
                            const def_token_str = self.tokenizer.unit.identifier(def_token_index);

                            if (!self.tokenizer.unit.defines.remove(def_token_str)) {
                                _ = self.tokenizer.unit.define_fns.remove(def_token_str);
                            }
                        } else {
                            std.debug.panic("Expected identifier but found {}", .{def_token.kind});
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
                        if (first_token.kind == .string_literal) {
                            const str_value = self.tokenizer.unit.stringAt(first_token_index);
                            std.log.warn("\x1b[1;33m{s}\x1b[0m", .{str_value});
                        } else if (first_token.kind == .identifier) {
                            var str_value = self.tokenizer.unit.identifier(first_token_index);
                            var writer = std.io.getStdOut().writer();
                            writer.print("#error: ", .{}) catch @panic("IOERR");
                            writer.print("\x1b[1;33m{s}", .{str_value}) catch @panic("IOERR");

                            while (self.tokenizer.next(true)) |p| {
                                const token = self.tokenizer.unit.token(p);
                                switch (token.kind) {
                                    .identifier => {
                                        str_value = self.tokenizer.unit.identifier(first_token_index);
                                        writer.print(" {s}", .{str_value}) catch @panic("IOERR");
                                    },
                                    else => {
                                        writer.print(" <OtherWarning>", .{}) catch @panic("IOERR");
                                    },
                                }
                            }
                            writer.print("\x1b[0m\n", .{}) catch @panic("IOERR");
                        }
                    } else if (std.mem.eql(u8, directive_str, "error")) {
                        const first_token_index = self.tokenizer.next(true) orelse @panic("Unexpected end of input");
                        const first_token = self.tokenizer.unit.token(first_token_index);
                        if (first_token.kind == .string_literal) {
                            const str_value = self.tokenizer.unit.stringAt(first_token_index);
                            std.log.err("\x1b[1;31m{s}\x1b[0m", .{str_value});
                        } else if (first_token.kind == .identifier) {
                            var str_value = self.tokenizer.unit.identifier(first_token_index);
                            var writer = std.io.getStdOut().writer();
                            writer.print("#error: ", .{}) catch @panic("IOERR");
                            writer.print("\x1b[1;31m{s}", .{str_value}) catch @panic("IOERR");

                            while (self.tokenizer.next(true)) |p| {
                                const token = self.tokenizer.unit.token(p);
                                switch (token.kind) {
                                    .identifier => {
                                        str_value = self.tokenizer.unit.identifier(p);
                                        writer.print(" {s}", .{str_value}) catch @panic("IOERR");
                                    },
                                    else => {
                                        writer.print(" <OtherError>", .{}) catch @panic("IOERR");
                                    },
                                }
                            }
                            writer.print("\x1b[0m\n", .{}) catch @panic("IOERR");
                        }
                    }

                    self.tokenizer.in_define = old_define;
                    return null;
                    // return self.tokenizer.next(false);
                },
                else => return null,
            }
        };

        const token = Token{
            .kind = token_kind,
            .start = start,
        };

        const index = self.tokenizer.unit.tokens(self.file_index).items.len;
        self.tokenizer.unit.tokens(self.file_index).append(token) catch @panic("OOM");

        return .{
            .index = @truncate(index),
            .file_index = self.file_index,
        };
    }

    fn opEq(self: *Self, regular: TokenKind, eq: TokenKind) TokenKind {
        if (self.pos + 1 < self.source.len and self.source[self.pos + 1] == '=') {
            return self.advanceToken(eq);
        }
        return self.advanceToken(regular);
    }

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

pub const Tokenizer = struct {
    allocator: std.mem.Allocator,
    unit: *Unit,
    file_stack: std.ArrayList(FileTokenizer),

    virtual_stack: std.ArrayList(TokenRange),
    expansion_stack: std.ArrayList(ArgumentMap),
    if_stack: std.BitStack,
    peeked_token: ?TokenIndex = null,
    in_define: bool = false,

    conditional_skip: bool = false,

    const Self = @This();

    pub fn init(allocator: std.mem.Allocator, unit: *Unit) Self {
        return .{
            .allocator = allocator,
            .unit = unit,
            .file_stack = std.ArrayList(FileTokenizer).init(allocator),
            .virtual_stack = std.ArrayList(TokenRange).init(allocator),
            .expansion_stack = std.ArrayList(ArgumentMap).init(allocator),
            .if_stack = std.BitStack.init(allocator),
        };
    }

    pub fn initFile(self: *Self, file_index: FileIndexType) *FileTokenizer {
        const file_tok = self.file_stack.addOne() catch @panic("OOM");
        file_tok.* = .{
            .tokenizer = self,
            .source = self.unit.files.items[file_index].source,
            .file_index = file_index,
        };

        return file_tok;
    }

    fn pushVirtual(self: *Self, range: TokenRange) void {
        self.virtual_stack.append(range) catch @panic("OOM");
    }

    inline fn currentRange(self: *Self) TokenRange {
        return self.virtual_stack.items[self.virtual_stack.items.len - 1];
    }

    inline fn currentFile(self: *Self) *FileTokenizer {
        return &self.file_stack.items[self.file_stack.items.len - 1];
    }

    fn popVirtual(self: *Self) TokenRange {
        return self.virtual_stack.pop();
    }

    pub fn peek(self: *Self, eol: bool) ?TokenIndex {
        if (self.peeked_token) |tok| {
            return tok;
        }

        self.peeked_token = self.rawNext(eol);
        if (eol and self.peeked_token != null) {
            // if (self.unit.token(self.peeked_token.?).start == 33223) {
            //     std.debug.dumpCurrentStackTrace(null);
            // }
            // std.log.info("SetPeek {}", .{self.unit.token(self.peeked_token.?)});
        }
        return self.peeked_token;
    }

    pub fn next(self: *Self, eol: bool) ?TokenIndex {
        if (self.peeked_token) |tok| {
            self.peeked_token = null;
            if (eol) {
                // if (self.unit.token(tok).start == 33223) {
                //     std.debug.dumpCurrentStackTrace(null);
                // }
                // std.log.info("UnSetPeek {}", .{self.unit.token(tok)});
            }
            return tok;
        }

        return self.rawNext(eol);
    }

    pub fn rawNext(self: *Self, eol: bool) ?TokenIndex {
        while (self.file_stack.items.len > 0) {
            var file = self.currentFile();
            if (file.next(eol)) |result| {
                return result;
            } else if (eol) {
                return null;
            } else if (file.included_file) {
                file.included_file = false;
                continue;
            }
            if (self.file_stack.items.len > 0) {
                file = self.currentFile(); // This might change when calling next
                if (file.pos >= file.source.len) {
                    self.file_stack.items.len -= 1;
                }
            }
        }
        return null;
    }
};

const Value = union(enum) {
    int_value: i64,
    float_value: f64,
};

const EvaluationError = error{
    UnexpectedToken,
    UnexpectedEnd,
};

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
                    if (140 >= last_prec and std.mem.eql(u8, self.unit.identifier(op.?.index), "defined")) {
                        self.next();
                        op = self.peek();
                        if (op != null and op.?.token.kind == .open_paren) {
                            self.next();
                            const old_in_define = self.tokenizer.in_define;
                            defer self.tokenizer.in_define = old_in_define;
                            self.tokenizer.in_define = true;
                            op = self.peek();
                            if (op != null and op.?.token.kind != .identifier) {
                                std.log.err("Founded token {}, file: {s}, pos: {}", .{ op.?.token.kind, self.unit.files.items[op.?.index.file_index].file_path, self.unit.token(op.?.index).start });
                                return error.UnexpectedToken;
                            }
                            self.next();
                            const ident_str = self.unit.identifier(op.?.index);

                            if (self.unit.defines.contains(ident_str) or self.unit.define_fns.contains(ident_str)) {
                                left = .{ .int_value = 1 };
                            } else {
                                left = .{ .int_value = 0 };
                            }
                            try self.expect(.close_paren);
                        } else {
                            if (op != null and op.?.token.kind != .identifier) {
                                return error.UnexpectedToken;
                            }
                            self.next();
                            const ident_str = self.unit.identifier(op.?.index);
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
                // std.log.err("Founded identifier {s}", .{self.unit.identifier(ptok.index)});
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
