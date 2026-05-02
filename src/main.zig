const std = @import("std");
const Io = std.Io;
const lib = @import("zarame");
const flate = std.compress.flate;
const dictionary = @import("zarame").dictionary;

const gz_data = @embedFile("zarame.dict.gz");

const Header = dictionary.Header;
const magic = dictionary.magic;
const Morph = dictionary.Morph;

const encoded_morph_size = @sizeOf(u32) + @sizeOf(u32) + @sizeOf(i16);

fn loadMorphs(allocator: std.mem.Allocator) ![]Morph {
    // Decompress at runtime.
    var in: Io.Reader = .fixed(gz_data);
    var out: Io.Writer.Allocating = .init(allocator);
    errdefer out.deinit();
    var dc: flate.Decompress = .init(&in, .gzip, &.{});
    _ = try dc.reader.streamRemaining(&out.writer);
    const buf = try out.toOwnedSlice();
    defer allocator.free(buf);

    if (buf.len < @sizeOf(Header)) return error.InvalidDictionary;

    const magic_val = std.mem.readInt(u64, buf[0..8], .little);
    const version = std.mem.readInt(u32, buf[8..12], .little);
    const count = std.mem.readInt(u32, buf[12..16], .little);

    if (magic_val != magic) return error.InvalidDictionary;
    if (version != 1) return error.UnsupportedDictionaryVersion;
    if (buf.len != @sizeOf(Header) + count * encoded_morph_size) return error.InvalidDictionary;

    const morphs = try allocator.alloc(Morph, count);
    errdefer allocator.free(morphs);

    const off_right = @sizeOf(u32);
    const off_cost = off_right + @sizeOf(u32);
    for (0..count) |i| {
        const off = @sizeOf(Header) + i * encoded_morph_size;
        morphs[i] = .{
            .left_id = @as(usize, std.mem.readInt(u32, buf[off..][0..4], .little)),
            .right_id = @as(usize, std.mem.readInt(u32, buf[off + off_right ..][0..4], .little)),
            .cost = std.mem.readInt(i16, buf[off + off_cost ..][0..2], .little),
        };
    }
    return morphs;
}

pub fn main(init: std.process.Init) !void {
    _ = lib;

    const io = init.io;
    var buffer: [1024]u8 = undefined;
    var writer = Io.File.stdout().writer(io, &buffer);
    const stdout = &writer.interface;
    const allocator = init.arena.allocator();

    const morphs = try loadMorphs(allocator);

    try stdout.print("Loaded {d} morphs.\n", .{morphs.len});
    const preview_len: usize = @min(morphs.len, 5);
    for (morphs[0..preview_len], 0..) |morph, i| {
        try stdout.print("  [{d}] left={d} right={d} cost={d}\n", .{ i, morph.left_id, morph.right_id, morph.cost });
    }
    try stdout.flush();
}
