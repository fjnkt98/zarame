const std = @import("std");
const lib = @import("zarame");

const Header = packed struct {
    magic: u32,
    version: u32,
    count: u32,
};

const magic = 0x504f4331;

/// gzip ファイルをビルド時（comptime）に解凍した生バイト列。
/// ランタイムの解凍処理は一切不要。
const poc_bytes: [
    blk: {
        // gzip の末尾 4 バイト (ISIZE) が解凍後サイズ (mod 2^32)
        const gz = @embedFile("poc_ints.bin.gz");
        break :blk std.mem.readInt(u32, gz[gz.len - 4 ..][0..4], .little);
    }
]u8 align(@alignOf(i32)) = blk: {
    @setEvalBranchQuota(10_000_000);
    const gz = @embedFile("poc_ints.bin.gz");
    const sz = std.mem.readInt(u32, gz[gz.len - 4 ..][0..4], .little);
    var buf: [sz]u8 = undefined;
    var in: std.Io.Reader = .fixed(gz);
    var out: std.Io.Writer = .fixed(&buf);
    var dc: std.compress.flate.Decompress = .init(&in, .gzip, &.{});
    _ = dc.reader.streamRemaining(&out) catch @compileError("gzip decompression failed");
    break :blk buf;
};

/// comptime でヘッダーを検証し i32 配列を返す。
/// []const i32（スライス）は comptime では返せないため [N]i32 固定長配列として定義する。
const poc_ints: [
    blk: {
        if (poc_bytes.len < @sizeOf(Header)) @compileError("binary too short");
        if (std.mem.readInt(u32, poc_bytes[0..4], .little) != magic) @compileError("invalid magic");
        if (std.mem.readInt(u32, poc_bytes[4..8], .little) != 1) @compileError("unsupported version");
        const count = std.mem.readInt(u32, poc_bytes[8..12], .little);
        if (poc_bytes.len != @sizeOf(Header) + count * @sizeOf(i32)) @compileError("size mismatch");
        break :blk count;
    }
]i32 = blk: {
    const count = std.mem.readInt(u32, poc_bytes[8..12], .little);
    var arr: [count]i32 = undefined;
    for (0..count) |i| {
        const offset = @sizeOf(Header) + i * @sizeOf(i32);
        arr[i] = std.mem.readInt(i32, poc_bytes[offset..][0..4], .little);
    }
    break :blk arr;
};

pub fn main(init: std.process.Init) !void {
    _ = lib;

    const io = init.io;
    var buffer: [1024]u8 = undefined;
    var writer = std.Io.File.stdout().writer(io, &buffer);
    const stdout = &writer.interface;

    // poc_ints はコンパイル時にパース済みの i32 配列 — ランタイムコストはゼロ
    const ints = &poc_ints;

    try stdout.print("Loaded {d} ints from comptime-decompressed binary: ", .{ints.len});
    for (ints, 0..) |value, i| {
        if (i > 0) try stdout.print(", ", .{});
        try stdout.print("{d}", .{value});
    }
    try stdout.print("\n", .{});

    try stdout.flush();
}
