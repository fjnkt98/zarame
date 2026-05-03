const std = @import("std");
const Io = std.Io;
const flate = std.compress.flate;
const zarame = @import("zarame");

pub fn main(init: std.process.Init) !void {
    const io = init.io;
    const allocator = init.arena.allocator();

    var stdout_buffer: [1024]u8 = undefined;
    var writer = Io.File.stdout().writer(io, &stdout_buffer);
    const stdout = &writer.interface;

    var stdin_buffer: [64 * 1024]u8 = undefined;
    var reader = Io.File.stdin().reader(io, &stdin_buffer);
    const stdin = &reader.interface;

    const dictionary = try zarame.dictionary.Dictionary.load(allocator);
    try stdout.print("Loaded {d} morphs, matrix rows={d}, cols={d}. Double array size is {d}.\n", .{ dictionary.morphs.len, dictionary.matrix.row_size, dictionary.matrix.col_size, dictionary.index.da.base.items.len });
    try stdout.flush();

    while (try stdin.takeDelimiter('\n')) |line| {
        var lattice = try zarame.lattice.Lattice.init(allocator, line, dictionary);
        defer lattice.deinit(allocator);
        try lattice.build(allocator);

        const nodes = try lattice.viterbi(allocator);
        defer allocator.free(nodes);

        for (nodes) |node| {
            try stdout.print("{s}\n", .{node.surface});
        }

        try stdout.flush();
    }

    return std.process.cleanExit(io);
}
