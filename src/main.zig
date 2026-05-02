const std = @import("std");
const lib = @import("zarame");
const ints_file_gz = @embedFile("poc_ints.bin.gz");

const Header = packed struct {
    magic: u32,
    version: u32,
    count: u32,
};

const magic = 0x504f4331;

fn decompressGzip(allocator: std.mem.Allocator, compressed: []const u8) ![]u8 {
    var in: std.Io.Reader = .fixed(compressed);
    var out: std.Io.Writer.Allocating = .init(allocator);
    errdefer out.deinit();

    var decompress: std.compress.flate.Decompress = .init(&in, .gzip, &.{});
    _ = try decompress.reader.streamRemaining(&out.writer);
    return out.toOwnedSlice();
}

fn parseInts(allocator: std.mem.Allocator, bytes: []const u8) ![]i32 {
    if (bytes.len < @sizeOf(Header)) {
        return error.InvalidDictionaryBinary;
    }

    const header = Header{
        .magic = std.mem.readInt(u32, bytes[0..4], .little),
        .version = std.mem.readInt(u32, bytes[4..8], .little),
        .count = std.mem.readInt(u32, bytes[8..12], .little),
    };
    if (header.magic != magic) {
        return error.InvalidDictionaryBinary;
    }
    if (header.version != 1) {
        return error.UnsupportedDictionaryVersion;
    }

    const count: usize = @intCast(header.count);
    const expected_len = @sizeOf(Header) + count * @sizeOf(i32);
    if (bytes.len != expected_len) {
        return error.InvalidDictionaryBinary;
    }

    const values = try allocator.alloc(i32, count);
    errdefer allocator.free(values);

    const start = @sizeOf(Header);
    for (0..count) |i| {
        const offset = start + i * @sizeOf(i32);
        const raw: *const [4]u8 = @ptrCast(bytes[offset .. offset + @sizeOf(i32)].ptr);
        values[i] = std.mem.readInt(i32, raw, .little);
    }
    return values;
}

pub fn main(init: std.process.Init) !void {
    _ = lib;
    std.debug.print("All your {s} are belong to us.\n", .{"codebase"});

    const io = init.io;
    var buffer: [1024]u8 = undefined;
    var writer = std.Io.File.stdout().writer(io, &buffer);
    const stdout = &writer.interface;
    const allocator = init.arena.allocator();

    const decompressed = try decompressGzip(allocator, ints_file_gz);
    const ints = try parseInts(allocator, decompressed);

    try stdout.print("Run `zig build test` to run the tests.\n", .{});

    try stdout.print("Loaded {d} ints from gzip: ", .{ints.len});
    for (ints, 0..) |value, i| {
        if (i > 0) {
            try stdout.print(", ", .{});
        }
        try stdout.print("{d}", .{value});
    }
    try stdout.print("\n", .{});

    try stdout.flush(); // Don't forget to flush!
}
