const std = @import("std");
const lib = @import("zarame");
const ints_file = @embedFile("poc_ints.bin");

const Header = packed struct {
    magic: u32,
    version: u32,
    count: u32,
};

const magic = 0x504f4331;

const parsed_ints = parseInts(ints_file);
const global_ints = parsed_ints[0..];

fn parseInts(comptime bytes: []const u8) [readCount(bytes)]i32 {
    if (bytes.len < @sizeOf(Header)) {
        @compileError("poc_ints.bin is smaller than the header size");
    }

    const header = readHeader(bytes);
    if (header.magic != magic) {
        @compileError("poc_ints.bin has an invalid magic number");
    }
    if (header.version != 1) {
        @compileError("poc_ints.bin has an unsupported version");
    }

    const count = header.count;
    const expected_len = @sizeOf(Header) + count * @sizeOf(i32);
    if (bytes.len != expected_len) {
        @compileError("poc_ints.bin has an unexpected file size");
    }

    var values: [count]i32 = undefined;
    const start = @sizeOf(Header);
    for (0..count) |i| {
        const offset = start + i * @sizeOf(i32);
        values[i] = std.mem.readInt(i32, bytes[offset .. offset + @sizeOf(i32)][0..4], .little);
    }
    return values;
}

fn readCount(comptime bytes: []const u8) comptime_int {
    return readHeader(bytes).count;
}

fn readHeader(comptime bytes: []const u8) Header {
    return .{
        .magic = std.mem.readInt(u32, bytes[0..4], .little),
        .version = std.mem.readInt(u32, bytes[4..8], .little),
        .count = std.mem.readInt(u32, bytes[8..12], .little),
    };
}

pub fn main(init: std.process.Init) !void {
    _ = lib;
    std.debug.print("All your {s} are belong to us.\n", .{"codebase"});

    const io = init.io;
    var buffer: [1024]u8 = undefined;
    var writer = std.Io.File.stdout().writer(io, &buffer);
    const stdout = &writer.interface;

    try stdout.print("Run `zig build test` to run the tests.\n", .{});

    try stdout.print("Loaded {d} ints at comptime: ", .{global_ints.len});
    for (global_ints, 0..) |value, i| {
        if (i > 0) {
            try stdout.print(", ", .{});
        }
        try stdout.print("{d}", .{value});
    }
    try stdout.print("\n", .{});

    try stdout.flush(); // Don't forget to flush!
}
