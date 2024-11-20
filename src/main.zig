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
    var tokenizer = Tokenizer.init(gpa.allocator(), &unit, 0);

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
