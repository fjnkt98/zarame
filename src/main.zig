const std = @import("std");
const Io = std.Io;
const lib = @import("zarame");
const flate = std.compress.flate;

const data = @embedFile("poc_ints.bin.gz");

const Header = packed struct {
    magic: u32,
    version: u32,
    count: u32,
};

const magic = 0x5A524D31; // "ZRM1"

const integers = blk: {
    @setEvalBranchQuota(10_000_000);
    const size = std.mem.readInt(u32, data[data.len - 4 ..][0..4], .little);

    var buf: [size]u8 = undefined;
    var input = Io.Reader.fixed(data);
    var output = Io.Writer.fixed(&buf);
    var dc = flate.Decompress.init(&input, .gzip, &.{});
    _ = dc.reader.streamRemaining(&output) catch @compileError("gzip decompression failed");

    if (buf.len < @sizeOf(Header)) @compileError("binary too short");
    if (std.mem.readInt(u32, buf[0..4], .little) != magic) @compileError("invalid magic");
    if (std.mem.readInt(u32, buf[4..8], .little) != 1) @compileError("unsupported version");

    const count = std.mem.readInt(u32, buf[8..12], .little);
    var result: [count]i32 = undefined;
    for (0..count) |i| {
        const offset = @sizeOf(Header) + i * @sizeOf(i32);
        result[i] = std.mem.readInt(i32, buf[offset..][0..4], .little);
    }
    break :blk result;
};

pub fn main(init: std.process.Init) !void {
    _ = lib;

    const io = init.io;
    var buffer: [1024]u8 = undefined;
    var writer = Io.File.stdout().writer(io, &buffer);
    const stdout = &writer.interface;

    try stdout.print("Loaded {d} integers from comptime-decompressed binary: ", .{integers.len});
    for (integers, 0..) |value, i| {
        if (i > 0) try stdout.print(", ", .{});
        try stdout.print("{d}", .{value});
    }
    try stdout.print("\n", .{});

    try stdout.flush();
}
