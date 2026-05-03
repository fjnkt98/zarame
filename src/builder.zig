const std = @import("std");
const Io = std.Io;
const flate = std.compress.flate;

const zarame = @import("zarame");

pub fn main(init: std.process.Init) !void {
    const io = init.io;
    const allocator = init.arena.allocator();

    var buffer: [1024]u8 = undefined;
    var writer = Io.File.stdout().writer(io, &buffer);
    const stdout = &writer.interface;

    const dictionary = try zarame.dictionary.Dictionary.build(allocator, io);
    try stdout.print("Loaded {d} morphs, matrix rows={d}, cols={d}.\n", .{ dictionary.morphs.len, dictionary.matrix.row_size, dictionary.matrix.col_size });

    try dictionary.save(io);
    try stdout.print("Dictionary saved.\n", .{});

    try stdout.flush();

    return std.process.cleanExit(io);
}
