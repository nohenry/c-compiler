const std = @import("std");
const Unit = @import("unit.zig").Unit;
const DefineValue = @import("unit.zig").DefineValue;

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

pub const Tokenizer = struct {
    allocator: std.mem.Allocator,
    unit: *Unit,
    pos: u32 = 0,
    token_index: u8 = 0,
    source: []const u8,
    file_index: FileIndexType,

    virtual_stack: std.ArrayList(TokenRange),
    expansion_stack: std.ArrayList(ArgumentMap),
    virtual_index: u32 = 0,
    peeked_token: ?TokenIndex = null,
    in_define: bool = false,

    const Self = @This();

    pub fn init(allocator: std.mem.Allocator, unit: *Unit, file_index: FileIndexType) Self {
        return .{
            .allocator = allocator,
            .unit = unit,
            .virtual_stack = std.ArrayList(TokenRange).init(allocator),
            .expansion_stack = std.ArrayList(ArgumentMap).init(allocator),
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
            .source = unit.files.items[file_index].source,
            .token_index = 1,
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
        while (self.pos < self.source.len) : (self.pos += 1) {
            c = self.source[self.pos];
            switch (c) {
                ' ', '\t', '\n', '\r' => continue,
                '/' => {
                    if (self.pos + 1 < self.source.len) {
                        if (self.source[self.pos + 1] == '/') {
                            while (self.isNot(0, '\n')) : (self.pos += 1) {}
                            self.pos += 1; // for newline
                        } else if (self.source[self.pos + 1] == '*') {
                            while (self.isNot(0, '*') or self.isNot(1, '/')) : (self.pos += 1) {}
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
            std.debug.panic("Expected token {}, found {}", .{ kind, token.kind });
        }
    }

    fn checkExpansion(self: *Self, str: []const u8, eol: bool) ?TokenIndex {
        std.log.info("CheckExpansion {s}", .{str});
        if (self.searchExpansionArg(str)) |value| {
            self.pushVirtual(value.range);
            return if (eol) self.nextEOL() else self.next();
        }

        if (self.unit.defines.get(str)) |def| {
            self.pushVirtual(def.range);
            return if (eol) self.nextEOL() else self.next();
        }

        if (self.unit.define_fns.get(str)) |def| {
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

            self.pushVirtual(TokenRange{
                .start = def.range.start,
                .end = def.range.end,
                .flags = TokenRange.Flags.EXPANSION_ARGUMENTS,
            });
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
                            'a'...'z', 'A'...'Z', '_' => continue,
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
                        const second_token = self.nextEOL(); // possibly open paren
                        //
                        if (first_token == null) {
                            @panic("Unexpected EOF");
                        }

                        const define_name = self.unit.token(first_token.?);
                        std.debug.assert(define_name.kind == .identifier);
                        const define_str = self.unit.identifier(first_token.?);

                        if (second_token != null and self.unit.token(second_token.?).kind == .open_paren) {
                            // Function type macro
                            var parameter_map = std.StringArrayHashMap(void).init(self.allocator);

                            while (self.next()) |pidx| {
                                const p = self.unit.token(pidx);
                                switch (p.kind) {
                                    .identifier => parameter_map.put(self.unit.identifier(pidx), {}) catch @panic("OOM"),
                                    .close_paren => {
                                        break;
                                    },
                                    else => std.debug.panic("TODO: add error handling (expected comma or paren) {}", .{p.kind}),
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
                                .parameters = parameter_map,
                            };
                        } else {
                            // Value type macro
                            var count: u32 = 1; // starts at one because of second_token
                            while (self.nextEOL()) |_| {
                                count += 1;
                            }

                            const define = self.unit.defines.getOrPut(define_str) catch @panic("OOM");
                            if (define.found_existing) {
                                std.debug.panic("Found existing define of name \x1b[1;36m'{s}'\x1b[0m", .{define_str});
                            }

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
                    } else if (std.mem.eql(u8, directive_str, "define")) {}

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
            .lte,
            .gt,
            .gte,
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
