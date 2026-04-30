const std = @import("std");
const lib = @import("zarame");
const word = @embedFile("word.txt");

pub fn main(init: std.process.Init) !void {
    std.debug.print("All your {s} are belong to us.\n", .{"codebase"});

    const io = init.io;
    var buffer: [1024]u8 = undefined;
    var writer = std.Io.File.stdout().writer(io, &buffer);
    const stdout = &writer.interface;

    try stdout.print("Run `zig build test` to run the tests.\n", .{});

    try stdout.print("Hello {s}\n", .{word});

    try stdout.flush(); // Don't forget to flush!
}
