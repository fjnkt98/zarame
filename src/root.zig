//! By convention, root.zig is the root source file when making a library. If
//! you are making an executable, the convention is to delete this file and
//! start with main.zig instead.
const std = @import("std");
const testing = std.testing;

pub const trie = @import("trie.zig");

test "test submodules" {
    _ = @import("trie.zig");
}

pub export fn add(a: i32, b: i32) i32 {
    return a + b;
}

test "basic add functionality" {
    try testing.expect(add(3, 7) == 10);
}

pub fn MyArray(comptime n: comptime_int) type {
    return struct {
        const Self = @This();

        data: [n]i32,

        pub fn init() Self {
            var data: [n]i32 = undefined;
            for (0..n) |i| {
                data[i] = 0;
            }
            return .{
                .data = data,
            };
        }
    };
}

test "create my array" {
    const a = MyArray(5).init();
    const expected = [_]i32{ 0, 0, 0, 0, 0 };
    try std.testing.expectEqualSlices(i32, &expected, &a.data);

    const b = MyArray(2).init();
    const expected2 = [_]i32{ 0, 0 };
    try std.testing.expectEqualSlices(i32, &expected2, &b.data);
}

const entries_file = @embedFile("entries.csv");

fn lineCount(s: []const u8) !usize {
    var stream = std.io.FixedBufferStream([]const u8){ .buffer = s, .pos = 0 };
    const reader = stream.reader();

    var n: usize = 0;
    var buffer: [65536]u8 = undefined;
    while (try reader.readUntilDelimiterOrEof(&buffer, '\n')) |_| {
        n += 1;
    }
    return n;
}

test "read embeded file" {
    const allocator = std.testing.allocator;
    var stream = std.io.FixedBufferStream([]const u8){ .buffer = std.mem.sliceTo(entries_file, 0), .pos = 0 };
    const reader = stream.reader();

    var count: i32 = 0;
    var result = std.ArrayList(u8).init(allocator);
    defer result.deinit();

    while (try reader.readUntilDelimiterOrEofAlloc(allocator, '\n', 1024)) |line| {
        defer allocator.free(line);

        try result.appendSlice(line);
        try result.append(' ');

        count += 1;
    }

    try std.testing.expectEqual(4, count);
    try std.testing.expectEqualStrings("foo bar baz qux ", result.items);
}

test "count of lines" {
    const n = comptime a: {
        const n = try lineCount(entries_file);
        break :a n;
    };

    try std.testing.expectEqual(4, n);
}

const data_file = @embedFile("data.csv");
const line_count_of_data_csv_derived_at_compile_time = lineCount(data_file) catch @compileError("get count of entries");

test "line count in compile time" {
    try std.testing.expectEqual(6, line_count_of_data_csv_derived_at_compile_time);
}

fn getData() ![]i32 {
    var data: [line_count_of_data_csv_derived_at_compile_time]i32 = undefined;

    var stream = std.io.FixedBufferStream([]const u8){ .buffer = data_file, .pos = 0 };
    const reader = stream.reader();

    var buffer: [65536]u8 = undefined;
    var i: usize = 0;
    while (try reader.readUntilDelimiterOrEof(&buffer, '\n')) |line| {
        const value = try std.fmt.parseInt(i32, line, 10);
        data[i] = value;
        i += 1;
    }

    return data[0..];
}

test "get data" {
    const data = try getData();

    try std.testing.expectEqual(6, data.len);

    const expected = [_]i32{ 1, 2, 3, 5, 8, 13 };
    try std.testing.expectEqualSlices(i32, &expected, data);
}

test "test" {
    const allocator = std.testing.allocator;

    var a = try std.ArrayList(u8).initCapacity(allocator, 4);
    defer a.deinit();
    try std.testing.expectEqual(0, a.items.len);

    try a.append(1);
    try std.testing.expectEqual(1, a.items.len);

    try a.append(2);
    try std.testing.expectEqual(2, a.items.len);

    a.shrinkAndFree(2);
    try std.testing.expectEqual(2, a.items.len);

    try a.append(3);
    try std.testing.expectEqual(3, a.items.len);
}
