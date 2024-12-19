const std = @import("std");
const tok = @import("tokenizer.zig");
const Unit = @import("unit.zig").Unit;
const TokenIndex = tok.TokenIndex;
const TokenRange = tok.TokenRange;

pub const DiagnosticLevel = enum {
    info,
    warning,
    err,
};

pub const DiagnosticLocation = union(enum) {
    single: TokenIndex,
    range: TokenRange,
    file_position: struct {
        position: u32,
        file_index: tok.FileIndexType,
    },
    file_range: struct {
        position: u32,
        len: tok.TokenIndexType,
        file_index: tok.FileIndexType,
    },
};

pub const DiagnosticKind = union(enum) {
    foo: struct {
        tok: u32,
        bar: u32,
    },
    expected_token: struct {
        token_kind: tok.TokenKind,
        before: TokenIndex,
    },
    expected_token_before_end: struct {
        token_kind: tok.TokenKind,
    },
    expected_expression: struct {
        before: TokenIndex,
    },
    expected_expression_before_end: struct {},
    expected_var_ident: struct {
        before: TokenIndex,
    },
    expected_var_decl: struct {
        before: TokenIndex,
    },
    else_without_if: struct {},
    useless_type_empty_decl: struct {},
    hex_float_requires_exponent: struct {},
    invalid_literal_for_int_base: struct {
        suffix: DiagnosticLocation,
        base: u8,
        char: u8,
    },

    invalid_concat: struct {
        left: TokenIndex,
        right: TokenIndex,
    },
    file_not_found: struct {
        file_name: []const u8,
    },
    reading_file_failed: struct {
        file_name: []const u8,
    },
    include_expects_filename: struct {},
    macro_name_ident: struct {},
    no_macro_name: struct {
        directive: DiagnosticLocation,
    },
    macro_needs_more_args: struct {
        macro_name: TokenIndex,
        expected: u32,
        found: u32,
    },
    macro_needs_less_args: struct {
        macro_name: TokenIndex,
        expected: u32,
        found: u32,
    },
    invalid_pp_directive: struct {},
    expected_pp_parameter_name: struct {
        found: TokenIndex,
    },
    invalid_token_in_pp_expression: struct {
        found: TokenIndex,
    },
    invalid_pp_expression: struct {},

    in_macro_expansion: struct {
        name: TokenIndex,
    },
    user_error: struct {
        message: DiagnosticLocation,
    },
    user_warning: struct {
        message: DiagnosticLocation,
    },

    pub fn errorMessageGnu(self: Self) []const u8 {
        return switch (self) {
            .foo => "this is a foo {[bar]} {[tok]}",
            .expected_token => "expected '{[format_token]}{[token_kind]}{[format_end]}' before '{[format_token]}{[before]}{[format_end]}' token",
            .expected_token_before_end => "expected '{[format_token]}{[token_kind]}{[format_end]}' before end of input",
            .expected_expression => "expected expression before '{[format_token]}{[before]}{[format_end]}' token",
            .expected_expression_before_end => "expected expression at end of input",
            .expected_var_ident => "expected identifier or '{[format_token]}({[format_end]}' before '{[format_token]}{[before]}{[format_end]}' token",
            .expected_var_decl => "expected '{[format_token]}={[format_end]}', '{[format_token]},{[format_end]}', '{[format_token]};{[format_end]}', '{[format_token]}asm{[format_end]}' or '{[format_token]}__attribute__{[format_end]}' before '{[format_token]}{[before]}{[format_end]}'",
            .else_without_if => "'{[format_token]}else{[format_end]}' without a previous '{[format_token]}if{[format_end]}'",
            .useless_type_empty_decl => "useless type name in empty declaration",
            .hex_float_requires_exponent => "hexadecimal floating constants require an exponent",
            // .invalid_literal_for_int_base => "invalid digit \"{[format_token]}{[char]c}{[format_end]}\" on integer constant for base {[format_const]}{[base]}{[format_end]}",
            .invalid_literal_for_int_base => "invalid suffix \"{[format_token]}{[suffix]}{[format_end]}\" on integer constant",
            .invalid_concat => "pasting \"{[format_token]}{[left]}{[format_end]}\" and \"{[format_token]}{[right]}{[format_end]}\" does not give a valid preprocessing token",
            .invalid_pp_directive => "invalid preprocessing directive",
            .macro_name_ident => "macro names must be identifiers",
            .no_macro_name => "no macro name given in {[directive]} directive",
            .macro_needs_more_args => "macro \"{[format_token]}{[macro_name]}{[format_end]}\" requires {[expected]} arguments, but only {[found]} given",
            .macro_needs_less_args => "macro \"{[format_token]}{[macro_name]}{[format_end]}\" passed {[found]} arguments, but takes just {[expected]}",
            .file_not_found => "{[file_name]}: No such file or directory",
            .reading_file_failed => "{[file_name]}: Unable to read file",
            .include_expects_filename => "#include expects \"FILENAME\" or <FILENAME>",
            .expected_pp_parameter_name => "expected parameter name, found {[format_token]}\"{[found]}\"{[format_end]}",
            .in_macro_expansion => "in expansion of macro '{[format_token]}{[name]}{[format_end]}'",
            .invalid_token_in_pp_expression => "token \"{[format_token]}{[found]}{[format_end]}\" is not valid in preprocessor expressions",
            .invalid_pp_expression => "#if with no expression",
            .user_error => "#error {[message]}",
            .user_warning => "#warning {[message]}",
        };
    }

    const Self = @This();
    pub fn write(self: Self, writer: anytype, unit: *Unit, config: FormatConfig) !void {
        switch (self) {
            inline else => |val| {
                try format(config, unit, writer, self.errorMessageGnu(), val);
            },
        }
    }
};

pub const Diagnostic = struct {
    kind: DiagnosticKind,
    location: DiagnosticLocation,
    level: DiagnosticLevel,

    const Self = @This();
    pub fn write(self: Self, writer: anytype, unit: *Unit, config: FormatConfig) !void {
        const cwd_path = try std.fs.cwd().realpathAlloc(unit.allocator, ".");
        switch (self.location) {
            .single => |s| {
                const file_path = try std.fs.path.relative(unit.allocator, cwd_path, unit.files.items[s.file_index].file_path);
                const shortest_path = if (unit.files.items[s.file_index].file_path.len < file_path.len)
                    unit.files.items[s.file_index].file_path
                else
                    file_path;
                const line_info = unit.findLineInfo(s);
                try writer.print("\x1b[37;1m{s}:{}:\x1b[0m ", .{ shortest_path, line_info.line + 1 });
            },
            .range => |rng| {
                const file_path = try std.fs.path.relative(unit.allocator, cwd_path, unit.files.items[rng.start.file_index].file_path);
                const shortest_path = if (unit.files.items[rng.start.file_index].file_path.len < file_path.len)
                    unit.files.items[rng.start.file_index].file_path
                else
                    file_path;
                const line_info_start = unit.findLineInfo(rng.start);
                const line_info_end = unit.findLineInfo(rng.end);
                try writer.print("\x1b[37;1m{s}:{}-{}:\x1b[0m ", .{ shortest_path, line_info_start.line + 1, line_info_end.line + 1 });
            },
            .file_position => |fpos| {
                const file_path = try std.fs.path.relative(unit.allocator, cwd_path, unit.files.items[fpos.file_index].file_path);
                const shortest_path = if (unit.files.items[fpos.file_index].file_path.len < file_path.len)
                    unit.files.items[fpos.file_index].file_path
                else
                    file_path;
                const line_info = unit.findLineInfoFpos(fpos.file_index, fpos.position);
                try writer.print("\x1b[37;1m{s}:{}:\x1b[0m ", .{ shortest_path, line_info.line + 1 });
            },
            .file_range => |rng| {
                const file_path = try std.fs.path.relative(unit.allocator, cwd_path, unit.files.items[rng.file_index].file_path);
                const shortest_path = if (unit.files.items[rng.file_index].file_path.len < file_path.len)
                    unit.files.items[rng.file_index].file_path
                else
                    file_path;
                const line_info = unit.findLineInfoFpos(rng.file_index, rng.position);
                try writer.print("\x1b[37;1m{s}:{}:\x1b[0m ", .{ shortest_path, line_info.line + 1 });
            },
        }
        switch (self.level) {
            .info => {
                if (config.colors) {
                    try writer.writeAll("\x1b[36;1minfo\x1b[0m: ");
                } else {
                    try writer.writeAll("info: ");
                }
            },
            .warning => {
                if (config.colors) {
                    try writer.writeAll("\x1b[33;1mwarning\x1b[0m: ");
                } else {
                    try writer.writeAll("warning: ");
                }
            },
            .err => {
                if (config.colors) {
                    try writer.writeAll("\x1b[31;1merror\x1b[0m: ");
                } else {
                    try writer.writeAll("error: ");
                }
            },
        }
        try self.kind.write(writer, unit, config);
        try writer.writeByte('\n');
    }
};

pub const FormatConfig = struct {
    colors: bool,
};

pub fn format(
    config: FormatConfig,
    unit: *Unit,
    writer: anytype,
    fmt: []const u8,
    args: anytype,
) !void {
    const ArgsType = @TypeOf(args);
    const args_type_info = @typeInfo(ArgsType);
    if (args_type_info != .@"struct") {
        std.debug.panic("expected tuple or struct argument, found " ++ @typeName(ArgsType), .{});
    }

    const fields_info = args_type_info.@"struct".fields;
    if (fields_info.len > 32) {
        std.debug.panic("32 arguments max are supported per format call", .{});
    }
    comptime var kv_list: [fields_info.len]struct { []const u8, u32 } = undefined;
    inline for (fields_info, 0..) |fi, idx| {
        kv_list[idx] = .{ fi.name, idx };
        // @compileLog(fi.name);
    }
    const field_map = std.StaticStringMap(u32).initComptime(kv_list);
    const color_map = std.StaticStringMap([]const u8).initComptime(&.{
        .{ "format_token", "\x1b[37;1m" },
        .{ "format_const", "\x1b[33;1m" },

        .{ "format_bold", "\x1b[1m" },
        .{ "format_italic", "\x1b[3m" },
        .{ "format_underline", "\x1b[4m" },

        .{ "format_red", "\x1b[31m" },
        .{ "format_green", "\x1b[32m" },
        .{ "format_yellow", "\x1b[33m" },
        .{ "format_blue", "\x1b[34m" },
        .{ "format_magenta", "\x1b[35m" },
        .{ "format_cyan", "\x1b[36m" },
        .{ "format_white", "\x1b[37m" },

        .{ "format_end", "\x1b[0m" },
    });
    // std.log.info("{}", .{field_map.has("foo")});

    var arg_state: std.fmt.ArgState = .{ .args_len = fields_info.len };
    var i: usize = 0;
    while (i < fmt.len) {
        const start_index = i;

        while (i < fmt.len) : (i += 1) {
            switch (fmt[i]) {
                '{', '}' => break,
                else => {},
            }
        }

        var end_index = i;
        var unescape_brace = false;

        // Handle {{ and }}, those are un-escaped as single braces
        if (i + 1 < fmt.len and fmt[i + 1] == fmt[i]) {
            unescape_brace = true;
            // Make the first brace part of the literal...
            end_index += 1;
            // ...and skip both
            i += 2;
        }

        // Write out the literal
        if (start_index != end_index) {
            try writer.writeAll(fmt[start_index..end_index]);
        }

        // We've already skipped the other brace, restart the loop
        if (unescape_brace) continue;

        if (i >= fmt.len) break;

        if (fmt[i] == '}') {
            std.debug.panic("missing opening {{", .{});
        }

        // Get past the {
        std.debug.assert(fmt[i] == '{');
        i += 1;

        const fmt_begin = i;
        // Find the closing brace
        while (i < fmt.len and fmt[i] != '}') : (i += 1) {}
        const fmt_end = i;

        if (i >= fmt.len) {
            std.debug.panic("missing closing }}", .{});
        }

        // Get past the }
        std.debug.assert(fmt[i] == '}');
        i += 1;

        const placeholder = try Placeholder.parse(fmt[fmt_begin..fmt_end]);
        if (placeholder.arg == .named) {
            if (color_map.get(placeholder.arg.named)) |col| {
                if (config.colors)
                    try writer.writeAll(col);
                continue;
            }
        }
        const arg_pos = switch (placeholder.arg) {
            .none => null,
            .number => |pos| pos,
            // else => @panic("todo"),
            .named => |arg_name| field_map.get(arg_name) orelse
                std.debug.panic("no argument with name '{s}'", .{arg_name}),
        };

        const width = switch (placeholder.width) {
            .none => null,
            .number => |v| v,
            .named => |arg_name| blk: {
                const arg_i = field_map.get(arg_name) orelse
                    std.debug.panic("no argument with name '{s}'", .{arg_name});
                _ = arg_state.nextArg(arg_i) orelse std.debug.panic("too few arguments", .{});

                switch (arg_i) {
                    inline 0...fields_info.len => |idx| {
                        inline for (fields_info, 0..) |info, fi| {
                            if (comptime @typeInfo(info.type) == .int) {
                                if (fi == idx) {
                                    break :blk @field(args, info.name);
                                }
                            }
                        }
                        std.debug.panic("invalid field", .{});
                    },
                    else => std.debug.panic("invalid field", .{}),
                }
            },
        };

        const precision = switch (placeholder.precision) {
            .none => null,
            .number => |v| v,
            .named => |arg_name| blk: {
                const arg_i = field_map.get(arg_name) orelse
                    std.debug.panic("no argument with name '{s}'", .{arg_name});
                _ = arg_state.nextArg(arg_i) orelse std.debug.panic("too few arguments", .{});
                switch (arg_i) {
                    inline 0...fields_info.len => |idx| {
                        inline for (fields_info, 0..) |info, fi| {
                            if (comptime @typeInfo(info.type) == .int) {
                                if (fi == idx) {
                                    break :blk @field(args, info.name);
                                }
                            }
                        }
                        std.debug.panic("invalid field", .{});
                    },
                    else => std.debug.panic("invalid field", .{}),
                }
            },
            // .named => |arg_name| blk: {
            //     const arg_i = comptime std.meta.fieldIndex(ArgsType, arg_name) orelse
            //         @compileError("no argument with name '" ++ arg_name ++ "'");
            //     _ = arg_state.nextArg(arg_i) orelse @compileError("too few arguments");
            //     break :blk @field(args, arg_name);
            // },
        };

        const arg_to_print = arg_state.nextArg(arg_pos) orelse
            std.debug.panic("too few arguments", .{});

        blk: {
            switch (arg_to_print) {
                inline 0...fields_info.len => |idx| {
                    inline for (fields_info, 0..) |info, fi| {
                        if (fi == idx) {
                            try formatType(
                                config,
                                unit,
                                @field(args, info.name),
                                placeholder.specifier_arg,
                                std.fmt.FormatOptions{
                                    .fill = placeholder.fill,
                                    .alignment = placeholder.alignment,
                                    .width = width,
                                    .precision = precision,
                                },
                                writer,
                                std.options.fmt_max_depth,
                            );
                            break :blk;
                        }
                    }
                },
                else => std.debug.panic("Invalid arg to print", .{}),
            }
        }
        // std.mem.indexOf(comptime T: type, haystack: []const T, needle: []const T)

        // if (fields_info.len > 0) {
        //     try std.fmt.formatType(
        //         @field(args, fields_info[arg_to_print].name),
        //         placeholder.specifier_arg,
        //         std.fmt.FormatOptions{
        //             .fill = placeholder.fill,
        //             .alignment = placeholder.alignment,
        //             .width = width,
        //             .precision = precision,
        //         },
        //         writer,
        //         std.options.fmt_max_depth,
        //     );
        // }
    }
}

pub const Placeholder = struct {
    specifier_arg: []const u8,
    fill: u21,
    alignment: std.fmt.Alignment,
    arg: std.fmt.Specifier,
    width: std.fmt.Specifier,
    precision: std.fmt.Specifier,

    pub fn parse(str: []const u8) !Placeholder {
        const view = try std.unicode.Utf8View.init(str);
        var parser = std.fmt.Parser{
            .iter = view.iterator(),
        };

        // Parse the positional argument number
        const arg = try parser.specifier();

        // Parse the format specifier
        const specifier_arg = parser.until(':');

        // Skip the colon, if present
        if (parser.char()) |ch| {
            if (ch != ':') {
                std.debug.panic("expected : or }}, found '{}'", .{ch});
            }
        }

        // Parse the fill character, if present.
        // When the width field is also specified, the fill character must
        // be followed by an alignment specifier, unless it's '0' (zero)
        // (in which case it's handled as part of the width specifier)
        var fill: ?u21 = if (parser.peek(1)) |ch|
            switch (ch) {
                '<', '^', '>' => parser.char(),
                else => null,
            }
        else
            null;

        // Parse the alignment parameter
        const alignment: ?std.fmt.Alignment = if (parser.peek(0)) |ch| init: {
            switch (ch) {
                '<', '^', '>' => {
                    // consume the character
                    break :init switch (parser.char().?) {
                        '<' => .left,
                        '^' => .center,
                        else => .right,
                    };
                },
                else => break :init null,
            }
        } else null;

        // When none of the fill character and the alignment specifier have
        // been provided, check whether the width starts with a zero.
        if (fill == null and alignment == null) {
            fill = if (parser.peek(0) == '0') '0' else null;
        }

        // Parse the width parameter
        const width = try parser.specifier();

        // Skip the dot, if present
        if (parser.char()) |ch| {
            if (ch != '.') {
                std.debug.panic("expected . or }}, found '{}'", .{ch});
            }
        }

        // Parse the precision parameter
        const precision = try parser.specifier();

        if (parser.char()) |ch| {
            std.debug.panic("extraneous trailing character '{}'", .{ch});
        }

        return Placeholder{
            .specifier_arg = specifier_arg[0..specifier_arg.len],
            .fill = fill orelse ' ',
            .alignment = alignment orelse .right,
            .arg = arg,
            .width = width,
            .precision = precision,
        };
    }
};

pub fn formatType(
    config: FormatConfig,
    unit: *Unit,
    value: anytype,
    fmt: []const u8,
    options: std.fmt.FormatOptions,
    writer: anytype,
    max_depth: usize,
) @TypeOf(writer).Error!void {
    const T = @TypeOf(value);

    switch (@typeInfo(T)) {
        .comptime_int, .int, .comptime_float, .float => {
            return formatIntValue(value, fmt, options, writer);
        },

        .pointer => |ptr_info| switch (ptr_info.size) {
            .One => switch (@typeInfo(ptr_info.child)) {
                .array, .@"enum", .@"union", .@"struct" => {
                    return formatType(config, unit, value.*, fmt, options, writer, max_depth);
                },
                else => return format(config, unit, writer, "{s}", .{@typeName(ptr_info.child)}),
            },
            .Many, .C => {
                if (ptr_info.sentinel) |_| {
                    return formatType(config, unit, std.mem.span(value), fmt, options, writer, max_depth);
                }
                if (ptr_info.child == u8) {
                    return std.fmt.formatBuf(std.mem.span(value), options, writer);
                }
                invalidFmtError(fmt, value);
            },
            .Slice => {
                if (max_depth == 0) {
                    return writer.writeAll("{ ... }");
                }
                if (ptr_info.child == u8) {
                    return std.fmt.formatBuf(value, options, writer);
                }
                try writer.writeAll("{ ");
                for (value, 0..) |elem, i| {
                    try formatType(config, unit, elem, fmt, options, writer, max_depth - 1);
                    if (i != value.len - 1) {
                        try writer.writeAll(", ");
                    }
                }
                try writer.writeAll(" }");
            },
        },
        .array => |info| {
            if (max_depth == 0) {
                return writer.writeAll("{ ... }");
            }
            if (info.child == u8) {
                return std.fmt.formatBuf(&value, options, writer);
            }
            try writer.writeAll("{ ");
            for (value, 0..) |elem, i| {
                try formatType(config, unit, elem, fmt, options, writer, max_depth - 1);
                if (i < value.len - 1) {
                    try writer.writeAll(", ");
                }
            }
            try writer.writeAll(" }");
        },
        .@"struct", .@"union", .@"enum" => {
            switch (T) {
                tok.TokenIndex => {
                    const slice = unit.tokenSourceSlice(value);
                    try writer.writeAll(slice);
                },
                tok.TokenKind => {
                    try writer.writeAll(value.toStr());
                },
                DiagnosticLocation => {
                    switch (value) {
                        .single => |s| {
                            const slice = unit.tokenSourceSlice(s);
                            try writer.writeAll(slice);
                        },
                        .range => |rng| {
                            const slice = unit.tokenSourceSliceRange(rng);
                            try writer.writeAll(slice);
                        },
                        .file_position => {
                            @panic("unimplemented");
                        },
                        .file_range => |rng| {
                            const source = unit.files.items[rng.file_index].source;
                            try writer.writeAll(source[rng.position .. rng.position + rng.len]);
                        },
                    }
                },
                else => {
                    std.debug.panic("Unsupported format type", .{});
                },
            }
        },
        else => std.debug.panic("Unsupported format type", .{}),
    }
}

pub fn invalidFmtError(fmt: []const u8, value: anytype) void {
    std.debug.panic("invalid format string '{s}' for type '" ++ @typeName(@TypeOf(value)) ++ "'", .{fmt});
}

fn formatIntValue(
    value: anytype,
    fmt: []const u8,
    options: std.fmt.FormatOptions,
    writer: anytype,
) !void {
    var base: u8 = 10;
    var case: std.fmt.Case = .lower;

    const int_value = if (@TypeOf(value) == comptime_int) blk: {
        const Int = std.math.IntFittingRange(value, value);
        break :blk @as(Int, value);
    } else value;

    if (fmt.len == 0 or std.mem.eql(u8, fmt, "d")) {
        base = 10;
        case = .lower;
    } else if (std.mem.eql(u8, fmt, "c")) {
        if (@typeInfo(@TypeOf(int_value)).int.bits <= 8) {
            return std.fmt.formatAsciiChar(@as(u8, int_value), options, writer);
        } else {
            std.debug.panic("cannot print integer that is larger than 8 bits as an ASCII character", .{});
        }
    } else if (std.mem.eql(u8, fmt, "u")) {
        if (@typeInfo(@TypeOf(int_value)).int.bits <= 21) {
            return std.fmt.formatUnicodeCodepoint(@as(u21, int_value), options, writer);
        } else {
            std.debug.panic("cannot print integer that is larger than 21 bits as an UTF-8 sequence", .{});
        }
    } else if (std.mem.eql(u8, fmt, "b")) {
        base = 2;
        case = .lower;
    } else if (std.mem.eql(u8, fmt, "x")) {
        base = 16;
        case = .lower;
    } else if (std.mem.eql(u8, fmt, "X")) {
        base = 16;
        case = .upper;
    } else if (std.mem.eql(u8, fmt, "o")) {
        base = 8;
        case = .lower;
    } else {
        std.debug.panic("invalid format string '{s}' for type '" ++ @typeName(@TypeOf(value)) ++ "'", .{fmt});
    }

    return std.fmt.formatInt(int_value, base, case, options, writer);
}
