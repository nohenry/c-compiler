const std = @import("std");
const Unit = @import("unit.zig").Unit;
const Tokenizer = @import("tokenizer.zig").Tokenizer;
const Parser = @import("parser.zig").Parser;
const Node = @import("parser.zig").Node;

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    const file_path = "test/main.cp";
    const source = try std.fs.cwd().readFileAlloc(gpa.allocator(), "test/main.cp", std.math.maxInt(usize));

    const full_path = try std.fs.realpathAlloc(gpa.allocator(), ".");
    const absolute_path = try std.fs.path.resolve(gpa.allocator(), &.{
        full_path,
        file_path,
    });
    defer gpa.allocator().free(absolute_path);
    std.log.info("CWD: {s}", .{absolute_path});

    var unit = Unit.init(gpa.allocator(), absolute_path, source);
    unit.define("__STDC__");
    unit.define("__arm64__");
    var tokenizer = Tokenizer.init(gpa.allocator(), &unit);
    _ = tokenizer.initFile(0);

    if (false){
        var preprocessed_buffer = std.ArrayList(u8).init(gpa.allocator());
        var preprocessed_writer = preprocessed_buffer.writer();

        while (tokenizer.next(false)) |tidx| {
            defer preprocessed_writer.writeByte(' ') catch @panic("oof");
            const token = unit.token(tidx);
            switch (token.kind) {
                .identifier => {
                    const token_source_slice = unit.identifierAt(tidx);
                    try preprocessed_writer.writeAll(token_source_slice);
                },
                .string_literal => {
                    try preprocessed_writer.writeByte('"');
                    const token_source_slice = unit.stringAt(tidx);
                    try preprocessed_writer.writeAll(token_source_slice);
                    try preprocessed_writer.writeByte('"');
                },
                .stringified_literal => {
                    try preprocessed_writer.writeByte('"');
                    const token_source_slice = unit.stringifiedAt(tidx);
                    try preprocessed_writer.writeAll(token_source_slice);
                    try preprocessed_writer.writeByte('"');
                },
                .char_literal => {
                    try preprocessed_writer.writeByte('\'');
                    const token_source_slice = unit.charAt(tidx);
                    try preprocessed_writer.writeByte(token_source_slice);
                    try preprocessed_writer.writeByte('\'');
                },
                else => {
                    if (token.kind.isIntLiteral()) {
                        const token_source_slice = unit.tokenSourceSlice(tidx);
                        try preprocessed_writer.writeAll(token_source_slice);
                        continue;
                    }

                    try preprocessed_writer.writeAll(token.kind.toStr());
                },
            }
        }
        var out_file = try std.fs.cwd().createFile("out.cp", .{});
        try out_file.writeAll(preprocessed_buffer.items);
        return;
    }

    var parser = Parser.init(&unit, &tokenizer);
    const unit_range = try parser.parseUnit();
    std.log.info("Range: {}-{}", .{ unit_range.start, unit_range.count });

    const stdout = std.io.getStdOut();
    var writer = stdout.writer();
    try writer.print("Unit\n", .{});
    for (0..unit_range.count) |i| {
        const node_index = unit.node_ranges.items[i + unit_range.start];
        try Node.writeTree(node_index, &unit, 0, i == unit_range.count - 1, writer);
    }

    // while (tokenizer.next()) |token_index| {
    //     const token = &unit.tokens.items[token_index];
    //     std.log.info("Token({}): {}", .{ token_index, token.* });
    // }
}
