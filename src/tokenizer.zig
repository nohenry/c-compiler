const std = @import("std");
const Unit = @import("unit.zig").Unit;
const DefineValue = @import("unit.zig").DefineValue;
const Parser = @import("parser.zig").Parser;

pub const TokenKind = enum(u32) {
    int_literal,
    float_literal,
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

    const Self = @This();
};

pub const Tokenizer = struct {
    allocator: std.mem.Allocator,
    unit: *Unit,
    pos: u32 = 0,
    source: []const u8,
    file_index: FileIndexType,

    virtual_stack: std.ArrayList(TokenRange),
    expansion_stack: std.ArrayList(ArgumentMap),
    if_stack: std.BitStack,
    peeked_token: ?TokenIndex = null,
    in_define: bool = false,

    conditional_skip: bool = false,

    const Self = @This();

    pub fn init(allocator: std.mem.Allocator, unit: *Unit, file_index: FileIndexType) Self {
        return .{
            .allocator = allocator,
            .unit = unit,
            .virtual_stack = std.ArrayList(TokenRange).init(allocator),
            .expansion_stack = std.ArrayList(ArgumentMap).init(allocator),
            .if_stack = std.BitStack.init(allocator),
            .source = unit.files.items[file_index].source,
            .file_index = file_index,
        };
    }

    pub fn initVirtual(allocator: std.mem.Allocator, unit: *Unit, file_index: FileIndexType) Self {
        return .{
            .allocator = allocator,
            .unit = unit,
            .virtual_stack = std.ArrayList(TokenRange).init(allocator),
            .expansion_stack = std.ArrayList(ArgumentMap).init(allocator),
            .if_stack = std.BitStack.init(allocator),
            .source = unit.files.items[file_index].source,
            .file_index = file_index,
        };
    }

    pub fn initFile(self: *Self, file_index: FileIndexType) FileTokenizer {
        return .{
            .tokenizer = self,
            .source = self.unit.files.items[file_index].source,
            .file_index = file_index,
        };
    }

    inline fn isNot(self: *Self, offsetFromPos: u32, char: u8) bool {
        return (self.pos + offsetFromPos < self.source.len) and self.source[self.pos + offsetFromPos] != char;
    }

    pub inline fn tokenCount(self: *Self) u32 {
        return if (self.peeked_token == null)
            @truncate(self.unit.tokens(self.file_index).items.len)
        else
            @truncate(self.unit.tokens(self.file_index).items.len - 1);
    }

    pub inline fn tokenCountFile(self: *Self, file_index: FileIndexType) u32 {
        return if (self.peeked_token == null)
            @truncate(self.unit.tokens(file_index).items.len)
        else
            @truncate(self.unit.tokens(file_index).items.len - 1);
    }

    fn pushVirtual(self: *Self, range: TokenRange) void {
        self.virtual_stack.append(range) catch @panic("OOM");
    }

    inline fn currentRange(self: *Self) TokenRange {
        return self.virtual_stack.items[self.virtual_stack.items.len - 1];
    }

    fn popVirtual(self: *Self) TokenRange {
        return self.virtual_stack.pop();
    }

    pub fn peek(self: *Self) ?TokenIndex {
        if (self.peeked_token) |tok| {
            return tok;
        }

        self.peeked_token = self.next();
        return self.peeked_token;
    }

    pub fn peekEOL(self: *Self) ?TokenIndex {
        if (self.peeked_token) |tok| {
            return tok;
        }

        self.peeked_token = self.nextEOL();
        return self.peeked_token;
    }

    pub fn nextEOL(self: *Self) ?TokenIndex {
        if (self.peeked_token) |tok| {
            self.peeked_token = null;
            return tok;
        }
        while (self.virtual_stack.items.len > 0) : (self.virtual_stack.items.len -= 1) {
            const item_index = self.virtual_stack.getLast();
            if (item_index.start.index < item_index.end.index) {
                const item = self.unit.token(item_index.start);
                self.virtual_stack.items[self.virtual_stack.items.len - 1].start.index += 1;

                if (item.kind == .identifier) {
                    const ident_str = self.unit.identifier(item_index.start);

                    if (self.checkExpansion(ident_str, false)) |value| {
                        return value;
                    }
                }

                return item_index.start;
            }

            if ((item_index.flags & TokenRange.Flags.EXPANSION_ARGUMENTS) > 0) {
                self.expansion_stack.items.len -= 1;
            }
        }

        var c: u8 = undefined;
        while (self.pos < self.source.len) : (self.pos += 1) {
            c = self.source[self.pos];
            switch (c) {
                ' ', '\t', '\r' => continue,
                '\n' => return null,
                '/' => {
                    if (self.pos + 1 < self.source.len) {
                        if (self.source[self.pos + 1] == '/') {
                            while (self.isNot(0, '\n')) : (self.pos += 1) {}
                        } else if (self.source[self.pos + 1] == '*') {
                            while (self.isNot(0, '\n') or self.isNot(0, '*') or self.isNot(1, '/')) : (self.pos += 1) {}
                            self.pos += 2; // for */
                        } else {
                            break;
                        }
                    }
                },
                else => break,
            }
        } else {
            return null;
        }

        return self.nextNonWhitespace(c, true);
    }

    pub fn next(self: *Self) ?TokenIndex {
        if (self.peeked_token) |tok| {
            self.peeked_token = null;
            return tok;
        }
        while (self.virtual_stack.items.len > 0) : (self.virtual_stack.items.len -= 1) {
            const item_index = self.virtual_stack.getLast();
            if (item_index.start.index < item_index.end.index) {
                const item = self.unit.token(item_index.start);
                self.virtual_stack.items[self.virtual_stack.items.len - 1].start.index += 1;

                if (item.kind == .identifier) {
                    const ident_str = self.unit.identifier(item_index.start);

                    if (self.checkExpansion(ident_str, false)) |value| {
                        return value;
                    }
                }

                return item_index.start;
            }

            if ((item_index.flags & TokenRange.Flags.EXPANSION_ARGUMENTS) > 0) {
                self.expansion_stack.items.len -= 1;
            }
        }

        var c: u8 = undefined;
        if (self.conditional_skip) blk: {
            var indent: u32 = 0;
            while (self.pos < self.source.len) : (self.pos += 1) {
                c = self.source[self.pos];

                if (c == '#') {
                    self.pos += 1;
                    const start_index = self.pos;
                    while (self.pos < self.source.len) : (self.pos += 1) {
                        switch (self.source[self.pos]) {
                            'a'...'z', 'A'...'Z', '_' => continue,
                            else => break,
                        }
                    }

                    const directive_str = self.source[start_index..self.pos];
                    if (std.mem.eql(u8, directive_str, "ifdef") or std.mem.eql(u8, directive_str, "ifndef") or std.mem.eql(u8, directive_str, "if")) {
                        indent += 1;
                    } else if (std.mem.eql(u8, directive_str, "endif")) {
                        if (indent == 0) {
                            _ = self.if_stack.pop();
                            self.conditional_skip = false;
                            break :blk;
                        }

                        indent -= 1;
                    } else if (std.mem.eql(u8, directive_str, "else")) {
                        if (indent == 0) {
                            const did_condition = self.if_stack.peek();
                            self.conditional_skip = did_condition == 1;
                        }
                    } else if (std.mem.eql(u8, directive_str, "elif")) {
                        if (indent == 0) {
                            const did_condition = self.if_stack.peek();
                            self.conditional_skip = did_condition == 1;
                            if (did_condition == 0) {
                                self.pos -= @truncate("#elif".len);
                            }
                            break :blk;
                        }
                    }

                    self.pos -= 1; // since we add one at next iteration
                }
            }
        }

        while (self.pos < self.source.len) : (self.pos += 1) {
            c = self.source[self.pos];
            switch (c) {
                ' ', '\t', '\n', '\r' => continue,
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
            return null;
        }

        return self.nextNonWhitespace(c, false);
    }

    fn searchExpansionArg(self: *Self, arg: []const u8) ?DefineValue {
        if (self.expansion_stack.items.len == 0) return null;
        var index = @as(isize, @intCast(self.expansion_stack.items.len - 1));
        while (index >= 0) : (index -= 1) {
            if (self.expansion_stack.items[@as(usize, @intCast(index))].get(arg)) |value| {
                return value;
            }
        }

        return null;
    }

    fn expect(self: *Self, kind: TokenKind) TokenIndex {
        const tidx = self.next();
        if (tidx == null) @panic("Unexpected EOF");

        const token = self.unit.token(tidx.?);
        if (token.kind == kind) {
            return tidx.?;
        } else {
            std.log.err("File: {s}, pos: {}", .{ self.unit.files.items[tidx.?.file_index].file_path, token.start });
            std.debug.panic("Expected token {}, found {}, {}", .{ kind, token.kind, self.unit.ivalue(tidx.?) });
        }
    }

    fn checkExpansion(self: *Self, str: []const u8, eol: bool) ?TokenIndex {
        if (self.searchExpansionArg(str)) |value| {
            self.pushVirtual(value.range);
            return if (eol) self.nextEOL() else self.next();
        }

        if (self.unit.defines.get(str)) |def| {
            if (def.range.start.index < def.range.end.index) {
                self.pushVirtual(def.range);
            }
            return if (eol) self.nextEOL() else self.next();
        }

        if (self.unit.define_fns.get(str)) |def| {
            std.log.info("DefineFunction {s} {}", .{ str, self.unit.files.items[def.range.start.file_index].tokens.items[def.range.start.index].start });
            const open_paren_index = self.expect(.open_paren);

            var argument_map = ArgumentMap.init(self.allocator);
            var parameter_iter = def.parameters.iterator();

            var indent: u32 = 0;

            var argument_start_token_index = TokenIndex{
                .index = open_paren_index.index + 1,
                .file_index = open_paren_index.file_index,
            };
            blk1: {
                var i: u32 = 0;
                while (self.next()) |pidx| {
                    const p = self.unit.token(pidx);

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

            self.expansion_stack.append(argument_map) catch @panic("OOM");

            if (def.range.start.index < def.range.end.index) {
                self.pushVirtual(TokenRange{
                    .start = def.range.start,
                    .end = def.range.end,
                    .flags = TokenRange.Flags.EXPANSION_ARGUMENTS,
                });
            }

            return if (eol) self.nextEOL() else self.next();
        }

        return null;
    }

    fn nextNonWhitespace(self: *Self, c: u8, eol: bool) ?TokenIndex {
        const start = self.pos;
        const token_kind = blk: {
            switch (c) {
                '0'...'9' => {
                    var isFloat = false;
                    var didDot = false;
                    var didE = false;
                    while (self.pos < self.source.len) : (self.pos += 1) doneLit: {
                        switch (self.source[self.pos]) {
                            '.' => {
                                if (didDot) break :doneLit;

                                didDot = true;
                                isFloat = true;
                            },
                            'e' => {
                                if (didE) break :doneLit;

                                didE = true;
                                isFloat = true;
                            },
                            '0'...'9' => continue,
                            else => break,
                        }
                    }

                    if (isFloat)
                        break :blk TokenKind.float_literal
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

                    if (!self.in_define) {
                        if (self.checkExpansion(str, eol)) |token| {
                            return token;
                        }
                    }

                    if (self.unit.type_names.contains(str)) {
                        break :blk TokenKind.type_name;
                    }

                    if (keyword_map.get(str)) |kind| {
                        break :blk kind;
                    }

                    // const index = self.unit.getOrPut(str);

                    break :blk TokenKind.identifier;
                },

                '#' => {
                    const old_define = self.in_define;
                    self.in_define = true;

                    self.pos += 1;
                    const start_index = self.pos;
                    while (self.pos < self.source.len) : (self.pos += 1) {
                        switch (self.source[self.pos]) {
                            'a'...'z', 'A'...'Z', '_' => continue,
                            else => break,
                        }
                    }
                    const directive_str = self.source[start_index..self.pos];

                    if (std.mem.eql(u8, directive_str, "define")) {
                        const first_token = self.nextEOL(); // identifier
                        const second_token = if (self.pos < self.source.len and self.source[self.pos] == '(') // Open paren must be immediately after identifier
                            self.nextEOL()
                        else
                            null;

                        if (first_token == null) {
                            @panic("Unexpected EOF");
                        }

                        const define_name = self.unit.token(first_token.?);
                        std.debug.assert(define_name.kind == .identifier);
                        const define_str = self.unit.identifier(first_token.?);
                        var var_arg = false;

                        if (second_token != null and self.unit.token(second_token.?).kind == .open_paren) {
                            // Function type macro
                            var parameter_map = std.StringArrayHashMap(void).init(self.allocator);

                            while (self.next()) |pidx| {
                                const p = self.unit.token(pidx);
                                switch (p.kind) {
                                    .identifier => parameter_map.put(self.unit.identifier(pidx), {}) catch @panic("OOM"),
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
                                        self.unit.files.items[pidx.file_index].file_path,
                                        self.unit.files.items[pidx.file_index].tokens.items[pidx.index].start,
                                    }),
                                }

                                const ptok = self.peekEOL();
                                if (ptok == null) @panic("TODO: expected at least paren");

                                switch (self.unit.token(ptok.?).kind) {
                                    .comma => _ = self.nextEOL(),
                                    .close_paren => {
                                        _ = self.nextEOL();
                                        break;
                                    },
                                    else => std.debug.panic("TODO: add error handling (expected comma or paren) {}", .{self.unit.token(ptok.?).kind}),
                                }
                            }

                            const pidx = TokenIndex{
                                .index = @truncate(self.tokenCount()),
                                .file_index = second_token.?.file_index,
                            };

                            var count: u32 = 0;
                            while (self.nextEOL()) |_| {
                                count += 1;
                            }

                            const define = self.unit.define_fns.getOrPut(define_str) catch @panic("OOM");
                            if (define.found_existing) {
                                std.debug.panic("Found existing define of name \x1b[1;36m'{s}'\x1b[0m", .{define_str});
                            }

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
                            while (self.nextEOL()) |_| {
                                count += 1;
                            }

                            const define = self.unit.defines.getOrPut(define_str) catch @panic("OOM");
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
                        const first_token_index = self.nextEOL() orelse @panic("Unexpected end of input");
                        const first_token = self.unit.token(first_token_index);

                        const full_file_path = if (first_token.kind == .string_literal) blk2: {
                            const file_path = self.unit.stringAt(first_token_index);

                            const full_file_path = self.unit.searchQuoteDirs(self.file_index, file_path) orelse std.debug.panic("Cannot find include file \x1b[1;36m'{s}'\x1b[0m", .{file_path});
                            break :blk2 full_file_path;
                        } else if (first_token.kind == .lt) blk2: {
                            const first_string_token = self.next() orelse @panic("Unexpected EOF");
                            const end_index = blk1: {
                                while (self.next()) |pidx| {
                                    const p = self.unit.token(pidx);
                                    if (p.kind == .gt) {
                                        break :blk1 pidx;
                                    }
                                } else break :blk1 null;
                            };

                            const start_source_index = self.unit.token(first_string_token).start;
                            const end_source_index = self.unit.token(end_index.?).start;
                            const file_path = self.unit.files.items[self.file_index].source[start_source_index..end_source_index];
                            const full_file_path = self.unit.searchIncludeDirs(file_path) orelse std.debug.panic("Cannot find include file \x1b[1;36m'{s}'\x1b[0m", .{file_path});

                            break :blk2 full_file_path;
                        } else {
                            std.debug.panic("Expected string literal or <> for include path. Found {}", .{first_token.kind});
                        };

                        var file = std.fs.openFileAbsolute(full_file_path, .{}) catch @panic("error opening file");
                        const file_contents = file.readToEndAlloc(self.allocator, std.math.maxInt(usize)) catch |e| std.debug.panic("foo {}", .{e});

                        const unit_file = self.unit.addFile(full_file_path, file_contents);

                        var file_tokenizer = Tokenizer.initVirtual(self.allocator, self.unit, unit_file);
                        const first_file_token = file_tokenizer.next();

                        var actual_tokens = std.ArrayList(Token).init(self.allocator);
                        defer actual_tokens.deinit();
                        var reverse_range = std.ArrayList(TokenRange).init(self.allocator);
                        defer reverse_range.deinit();

                        var last_token_index: TokenIndex = undefined;
                        var last_inserted_index: TokenIndexType = 0;
                        var range_count: u32 = 0;
                        if (first_file_token) |fft| {
                            last_token_index = fft;
                            range_count += 1;
                            last_inserted_index = fft.index;
                            if (fft.file_index == unit_file) {
                                actual_tokens.append(self.unit.token(fft)) catch @panic("OOM");
                            }
                        }

                        std.log.debug("File({}): {s}", .{ unit_file, full_file_path });
                        std.log.debug("{}", .{self.unit.token(.{.file_index = unit_file, .index = 0})});
                        while (file_tokenizer.next()) |pt| {
                            std.log.debug("Token: {}", .{pt});
                            std.log.debug("    {}", .{self.unit.token(pt)});

                            if (pt.file_index != last_token_index.file_index) {
                                reverse_range.append(.{
                                    .start = .{
                                        .index = last_inserted_index,
                                        .file_index = last_token_index.file_index,
                                    },
                                    .end = .{
                                        .index = last_inserted_index + @as(TokenIndexType, @truncate(range_count)),
                                        .file_index = last_token_index.file_index,
                                    },
                                    .flags = 0,
                                }) catch @panic("OOM");
                                range_count = 0;
                                last_inserted_index = pt.index;
                            } else if (pt.index != last_token_index.index + 1) {
                                reverse_range.append(.{
                                    .start = .{
                                        .index = last_inserted_index,
                                        .file_index = last_token_index.file_index,
                                    },
                                    .end = .{
                                        .index = last_inserted_index + @as(TokenIndexType, @truncate(range_count)),
                                        .file_index = last_token_index.file_index,
                                    },
                                    .flags = 0,
                                }) catch @panic("OOM");
                                range_count = 0;
                                last_inserted_index = pt.index;
                            } else {
                                range_count += 1;
                            }

                            if (pt.file_index == unit_file) {
                                actual_tokens.append(self.unit.token(pt)) catch @panic("OOM");
                            }
                            last_token_index = pt;
                        }
                        // self.unit.files.items[unit_file].tokens.items.len = 0;
                        // self.unit.files.items[unit_file].tokens.appendSlice(actual_tokens.items) catch @panic("OOM");

                        std.log.debug("EndFile", .{});

                        if (last_token_index.file_index != unit_file) {
                            reverse_range.append(.{
                                .start = .{
                                    .index = last_inserted_index,
                                    .file_index = last_token_index.file_index,
                                },
                                .end = .{
                                    .index = last_inserted_index + @as(TokenIndexType, @truncate(range_count)),
                                    .file_index = last_token_index.file_index,
                                },
                                .flags = 0,
                            }) catch @panic("OOM");
                        } else if (range_count > 0) {
                            reverse_range.append(.{
                                .start = .{
                                    .index = last_inserted_index,
                                    .file_index = unit_file,
                                },
                                .end = .{
                                    .index = last_inserted_index + @as(TokenIndexType, @truncate(range_count)),
                                    .file_index = unit_file,
                                },
                                .flags = 0,
                            }) catch @panic("OOM");
                        }

                        var i = reverse_range.items.len;
                        while (i > 0) {
                            i -= 1;
                            self.pushVirtual(reverse_range.items[i]);
                        }

                        return self.next();
                    } else if (std.mem.eql(u8, directive_str, "ifdef")) {
                        const def_token_index = self.nextEOL() orelse @panic("Unexpected EOF. Expected identifier");
                        const def_token = self.unit.token(def_token_index);
                        if (def_token.kind == .identifier) {
                            const def_token_str = self.unit.identifier(def_token_index);

                            if (!self.unit.defines.contains(def_token_str) and !self.unit.define_fns.contains(def_token_str)) {
                                self.conditional_skip = true;
                                self.if_stack.push(0) catch @panic("OOM");
                            } else {
                                self.if_stack.push(1) catch @panic("OOM");
                            }
                        } else {
                            std.debug.panic("Expected identifier but found {}", .{def_token.kind});
                        }
                    } else if (std.mem.eql(u8, directive_str, "ifndef")) {
                        const def_token_index = self.nextEOL() orelse @panic("Unexpected EOF. Expected identifier");
                        const def_token = self.unit.token(def_token_index);
                        if (def_token.kind == .identifier) {
                            const def_token_str = self.unit.identifier(def_token_index);

                            if (self.unit.defines.contains(def_token_str) or self.unit.define_fns.contains(def_token_str)) {
                                self.conditional_skip = true;
                                self.if_stack.push(0) catch @panic("OOM");
                            } else {
                                self.if_stack.push(1) catch @panic("OOM");
                            }
                        } else {
                            std.debug.panic("Expected identifier but found {}", .{def_token.kind});
                        }
                    } else if (std.mem.eql(u8, directive_str, "if")) {
                        var evaluator = SimpleExpressionEvaluator{
                            .unit = self.unit,
                            .tokenizer = self,
                        };

                        self.in_define = false;
                        const value = evaluator.evaluate() catch @panic("failed to evaluate #if");
                        while (self.nextEOL()) |_| {}
                        self.in_define = true;

                        if (value.int_value == 0) {
                            self.conditional_skip = true;
                            self.if_stack.push(0) catch @panic("OOM");
                        } else {
                            self.if_stack.push(1) catch @panic("OOM");
                        }
                    } else if (std.mem.eql(u8, directive_str, "elif")) {
                        const did_condition = self.if_stack.peek();
                        self.conditional_skip = did_condition == 1;
                        var evaluator = SimpleExpressionEvaluator{
                            .unit = self.unit,
                            .tokenizer = self,
                        };

                        self.in_define = false;
                        const value = evaluator.evaluate() catch @panic("failed to evaluate #if");
                        while (self.nextEOL()) |_| {}
                        self.in_define = true;

                        if (did_condition == 1 or value.int_value == 0) {
                            self.conditional_skip = true;
                        }
                    } else if (std.mem.eql(u8, directive_str, "undef")) {
                        const def_token_index = self.nextEOL() orelse @panic("Unexpected EOF. Expected identifier");
                        const def_token = self.unit.token(def_token_index);
                        if (def_token.kind == .identifier) {
                            const def_token_str = self.unit.identifier(def_token_index);

                            if (!self.unit.defines.remove(def_token_str)) {
                                _ = self.unit.define_fns.remove(def_token_str);
                            }
                        } else {
                            std.debug.panic("Expected identifier but found {}", .{def_token.kind});
                        }
                    } else if (std.mem.eql(u8, directive_str, "else")) {
                        const did_condition = self.if_stack.peek();
                        self.conditional_skip = did_condition == 1;
                    } else if (std.mem.eql(u8, directive_str, "endif")) {
                        _ = self.if_stack.pop();
                        self.conditional_skip = false;
                    } else if (std.mem.eql(u8, directive_str, "warning")) {
                        const first_token_index = self.nextEOL() orelse @panic("Unexpected end of input");
                        const first_token = self.unit.token(first_token_index);
                        if (first_token.kind == .string_literal) {
                            const str_value = self.unit.stringAt(first_token_index);
                            std.log.warn("\x1b[1;33m{s}\x1b[0m", .{str_value});
                        } else if (first_token.kind == .identifier) {
                            var str_value = self.unit.identifier(first_token_index);
                            std.log.warn("\x1b[1;33m{s}\x1b[0m", .{str_value});

                            while (self.nextEOL()) |p| {
                                const token = self.unit.token(p);
                                switch (token.kind) {
                                    .identifier => {
                                        str_value = self.unit.identifier(first_token_index);
                                        std.log.warn("\x1b[1;33m{s}\x1b[0m", .{str_value});
                                    },
                                    else => {
                                        std.log.warn("\x1b[1;33mOtherWarning\x1b[0m", .{});
                                    }
                                }
                            }

                        }
                    } else if (std.mem.eql(u8, directive_str, "error")) {
                        const first_token_index = self.nextEOL() orelse @panic("Unexpected end of input");
                        const first_token = self.unit.token(first_token_index);
                        if (first_token.kind == .string_literal) {
                            const str_value = self.unit.stringAt(first_token_index);
                            std.log.err("\x1b[1;31m{s}\x1b[0m", .{str_value});
                        } else if (first_token.kind == .identifier) {
                            var str_value = self.unit.identifier(first_token_index);
                            std.log.err("\x1b[1;31m{s}\x1b[0m", .{str_value});

                            while (self.nextEOL()) |p| {
                                const token = self.unit.token(p);
                                switch (token.kind) {
                                    .identifier => {
                                        str_value = self.unit.identifier(p);
                                        std.log.err("\x1b[1;31m{s}\x1b[0m", .{str_value});
                                    },
                                    else => {
                                        std.log.err("\x1b[1;31mOtherWarning\x1b[0m", .{});
                                    }
                                }
                            }

                        }
                    }

                    self.in_define = old_define;
                    return self.next();
                },
                else => return null,
            }
        };

        const token = Token{
            .kind = token_kind,
            .start = start,
        };

        const index = self.unit.tokens(self.file_index).items.len;
        self.unit.tokens(self.file_index).append(token) catch @panic("OOM");

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
        const ptok = self.peek() orelse return error.UnexpectedEnd;
        switch (ptok.token.kind) {
            .open_paren => {
                self.next();
                const result = try self.evaluate();
                try self.expect(.close_paren);
                return result;
            },
            .int_literal => {
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
        if (ptok == null) return error.UnexpectedEnd;

        if (ptok.?.token.kind != kind) {
            std.log.err("Founded token {}, file: {s}, pos: {}", .{ ptok.?.token.kind, self.unit.files.items[ptok.?.index.file_index].file_path, self.unit.token(ptok.?.index) });
            return error.UnexpectedToken;
        }
        self.next();
    }

    fn peek(self: *Self) ?NextResult {
        const index = self.tokenizer.peekEOL();
        if (index == null) return null;

        return .{
            .index = index.?,
            .token = self.unit.token(index.?),
        };
    }

    fn next(self: *Self) void {
        _ = self.tokenizer.nextEOL();
    }
};
