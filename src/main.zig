const std = @import("std");
const Io = std.Io;
const flate = std.compress.flate;
const zarame = @import("zarame");

pub fn main(init: std.process.Init) !void {
    const io = init.io;
    var buffer: [1024]u8 = undefined;
    var writer = Io.File.stdout().writer(io, &buffer);
    const stdout = &writer.interface;
    const allocator = init.arena.allocator();

    const dictionary = try zarame.dictionary.Dictionary.build(allocator, io);

    try stdout.print("Loaded {d} morphs, matrix rows={d}, cols={d}.\n", .{ dictionary.morphs.len, dictionary.matrix.row_size, dictionary.matrix.col_size });

    const input = "今日はいい天気ですね。";
    var lattice = try zarame.lattice.Lattice.init(allocator, input, dictionary);
    defer lattice.deinit(allocator);
    try lattice.build(allocator);

    std.debug.print("lattice has {d} nodes\n", .{lattice.nodes.len});

    const nodes = try lattice.viterbi(allocator);
    defer allocator.free(nodes);

    for (nodes) |node| {
        try stdout.print("surface={s}, left_id={d}, right_id={d}, cost={d}\n", .{ node.surface, node.left_id, node.right_id, node.cost });
    }

    try stdout.flush();
}
