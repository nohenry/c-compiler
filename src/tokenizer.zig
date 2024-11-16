const std = @import("std");
const Unit = @import("unit.zig").Unit;

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
    @"const",
    @"extern",
    register,
    restrict,
    static,
    thread_local,
    typedef,
    void,
    @"volatile",

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
            .@"const" => "const",
            .@"extern" => "extern",
            .register => "register",
            .restrict => "restrict",
            .static => "static",
            .thread_local => "thread_local",
            .typedef => "typedef",
            .void => "void",
            .@"volatile" => "volatile",

            else => @panic(""),
        };
    }
};

pub const keyword_map = std.StaticStringMap(TokenKind).initComptime(&.{
    .{ "auto", .auto },
    .{ "atomic", .atomic },
    .{ "const", .@"const" },
    .{ "extern", .@"extern" },
    .{ "register", .register },
    .{ "restrict", .restrict },
    .{ "static", .static },
    .{ "thread_local", .thread_local },
    .{ "typedef", .typedef },
    .{ "void", .void },
    .{ "volatile", .@"volatile" },

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

pub const TokenIndex = u32;
pub const Token = struct {
    kind: TokenKind,
    start: u32,

    pub fn ivalue(self: *const Token, unit: *const Unit) u64 {
        var index = self.start;
        var sum: u64 = 0;
        while (index < unit.source.len) : (index += 1) {
            switch (unit.source[index]) {
                '0'...'9' => |c| {
                    sum *= 10;
                    sum += @as(u64, c - '0');
                },
                else => break,
            }
        }

        return sum;
    }

    pub fn fvalue(self: *const Token, unit: *const Unit) f64 {
        var index = self.start;
        var sum: f64 = 0.0;
        var fract: f64 = 0.0;
        var mult: f64 = 0.0;
        var didDot = false;
        var didE = false;
        while (index < unit.source.len) : (index += 1) {
            switch (unit.source[index]) {
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

    pub fn identifier(self: *const Token, unit: *const Unit) []const u8 {
        var index = self.start;
        while (index < unit.source.len) : (index += 1) {
            switch (unit.source[index]) {
                'a'...'z', 'A'...'Z', '_' => continue,
                else => break,
            }
        }

        return unit.source[self.start..index];
    }

    pub fn ppDirective(self: *const Token, unit: *const Unit) []const u8 {
        var index = self.start + 1;
        while (index < unit.source.len) : (index += 1) {
            switch (unit.source[index]) {
                'a'...'z', 'A'...'Z', '_' => continue,
                else => break,
            }
        }

        return unit.source[self.start + 1 .. index];
    }
};

pub const Tokenizer = struct {
    allocator: std.mem.Allocator,
    unit: *Unit,
    pos: u32 = 0,

    peeked_token: ?TokenIndex = null,

    const Self = @This();

    pub fn init(allocator: std.mem.Allocator, unit: *Unit) Self {
        return .{
            .allocator = allocator,
            .unit = unit,
        };
    }

    inline fn isNot(self: *Self, offsetFromPos: u32, char: u8) bool {
        return (self.pos + offsetFromPos < self.unit.source.len) and self.unit.source[self.pos + offsetFromPos] != char;
    }

    pub inline fn tokenCount(self: *Self) u32 {
        return if (self.peeked_token == null)
            @truncate(self.unit.tokens.items.len)
        else
            @truncate(self.unit.tokens.items.len - 1);
    }

    pub fn peek(self: *Self) ?TokenIndex {
        if (self.peeked_token) |tok| {
            return tok;
        } else {
            self.peeked_token = self.next();
            return self.peeked_token;
        }
    }

    pub fn peekEOL(self: *Self) ?TokenIndex {
        if (self.peeked_token) |tok| {
            return tok;
        } else {
            self.peeked_token = self.nextEOL();
            return self.peeked_token;
        }
    }

    pub fn nextEOL(self: *Self) ?TokenIndex {
        if (self.peeked_token) |tok| {
            self.peeked_token = null;
            return tok;
        }

        var c: u8 = undefined;
        while (self.pos < self.unit.source.len) : (self.pos += 1) {
            c = self.unit.source[self.pos];
            switch (c) {
                ' ', '\t', '\r' => continue,
                '\n' => return null,
                '/' => {
                    if (self.pos + 1 < self.unit.source.len) {
                        if (self.unit.source[self.pos + 1] == '/') {
                            while (self.isNot(0, '\n')) : (self.pos += 1) {}
                        } else if (self.unit.source[self.pos + 1] == '*') {
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

        return self.nextNonWhitespace(c);
    }

    pub fn next(self: *Self) ?TokenIndex {
        if (self.peeked_token) |tok| {
            self.peeked_token = null;
            return tok;
        }

        var c: u8 = undefined;
        while (self.pos < self.unit.source.len) : (self.pos += 1) {
            c = self.unit.source[self.pos];
            switch (c) {
                ' ', '\t', '\n', '\r' => continue,
                '/' => {
                    if (self.pos + 1 < self.unit.source.len) {
                        if (self.unit.source[self.pos + 1] == '/') {
                            while (self.isNot(0, '\n')) : (self.pos += 1) {}
                            self.pos += 1; // for newline
                        } else if (self.unit.source[self.pos + 1] == '*') {
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

        return self.nextNonWhitespace(c);
    }

    fn nextNonWhitespace(self: *Self, c: u8) ?TokenIndex {
        const start = self.pos;
        const token_kind = blk: {
            switch (c) {
                '0'...'9' => {
                    var isFloat = false;
                    var didDot = false;
                    var didE = false;
                    while (self.pos < self.unit.source.len) : (self.pos += 1) doneLit: {
                        switch (self.unit.source[self.pos]) {
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
                'a'...'z', 'A'...'Z', '_' => {
                    while (self.pos < self.unit.source.len) : (self.pos += 1) {
                        switch (self.unit.source[self.pos]) {
                            'a'...'z', 'A'...'Z', '_' => continue,
                            else => break,
                        }
                    }
                    const str = self.unit.source[start..self.pos];

                    if (self.unit.type_names.contains(str)) {
                        break :blk TokenKind.type_name;
                    }

                    if (keyword_map.get(str)) |kind| {
                        break :blk kind;
                    }

                    break :blk TokenKind.identifier;
                },
                '(' => break :blk self.advanceToken(TokenKind.open_paren),
                ')' => break :blk self.advanceToken(TokenKind.close_paren),
                '[' => break :blk self.advanceToken(TokenKind.open_bracket),
                ']' => break :blk self.advanceToken(TokenKind.close_bracket),
                '{' => break :blk self.advanceToken(TokenKind.open_brace),
                '}' => break :blk self.advanceToken(TokenKind.close_brace),
                ',' => break :blk self.advanceToken(TokenKind.comma),
                ';' => break :blk self.advanceToken(TokenKind.semicolon),
                '.' => break :blk self.advanceToken(TokenKind.dot),
                '=' => {
                    if (self.pos + 1 < self.unit.source.len) {
                        switch (self.unit.source[self.pos + 1]) {
                            '=' => break :blk self.advanceToken(TokenKind.equality),
                            else => {},
                        }
                    }
                    break :blk self.advanceToken(TokenKind.assignment);
                },
                '!' => {
                    if (self.pos + 1 < self.unit.source.len) {
                        switch (self.unit.source[self.pos + 1]) {
                            '=' => break :blk self.advanceToken(TokenKind.nequality),
                            else => {},
                        }
                    }
                    break :blk self.advanceToken(TokenKind.exclamation);
                },

                '+' => if (self.pos + 1 < self.unit.source.len) {
                    switch (self.unit.source[self.pos + 1]) {
                        '+' => break :blk self.advanceToken(TokenKind.plusplus),
                        '=' => break :blk self.advanceToken(TokenKind.plus_eq),
                        else => break :blk self.advanceToken(TokenKind.plus),
                    }
                } else break :blk self.advanceToken(TokenKind.plus),
                '-' => if (self.pos + 1 < self.unit.source.len) {
                    switch (self.unit.source[self.pos + 1]) {
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
                    if (self.pos + 1 < self.unit.source.len) {
                        switch (self.unit.source[self.pos + 1]) {
                            '&' => break :blk self.advanceToken(TokenKind.double_ampersand),
                            '=' => break :blk self.advanceToken(TokenKind.ampersand_eq),
                            else => {},
                        }
                    }
                    break :blk self.advanceToken(TokenKind.ampersand);
                },
                '|' => {
                    if (self.pos + 1 < self.unit.source.len) {
                        switch (self.unit.source[self.pos + 1]) {
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
                    if (self.pos + 1 < self.unit.source.len) {
                        switch (self.unit.source[self.pos + 1]) {
                            '<' => {
                                if (self.pos + 2 < self.unit.source.len) {
                                    switch (self.unit.source[self.pos + 2]) {
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
                '>' => {
                    if (self.pos + 1 < self.unit.source.len) {
                        switch (self.unit.source[self.pos + 1]) {
                            '>' => {
                                if (self.pos + 2 < self.unit.source.len) {
                                    switch (self.unit.source[self.pos + 2]) {
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

                '#' => {
                    self.pos += 1;
                    while (self.pos < self.unit.source.len) : (self.pos += 1) {
                        switch (self.unit.source[self.pos]) {
                            'a'...'z', 'A'...'Z', '_' => continue,
                            else => break,
                        }
                    }

                    break :blk TokenKind.pp_directive;
                },
                else => return null,
            }
        };

        const token = Token{
            .kind = token_kind,
            .start = start,
        };

        const index = self.unit.tokens.items.len;
        self.unit.tokens.append(token) catch @panic("OOM");

        return @intCast(index);
    }

    fn opEq(self: *Self, regular: TokenKind, eq: TokenKind) TokenKind {
        if (self.pos + 1 < self.unit.source.len and self.unit.source[self.pos + 1] == '=') {
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
            => self.pos += 2,

            else => @panic("Cannot detmerine for this type of token"),
        }
        return kind;
    }
};
