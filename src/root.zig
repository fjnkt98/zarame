//! By convention, root.zig is the root source file when making a library. If
//! you are making an executable, the convention is to delete this file and
//! start with main.zig instead.
const std = @import("std");
const testing = std.testing;

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

fn lineCount(comptime s: []const u8) usize {
    comptime var n: usize = 0;
    comptime var iter = std.mem.splitScalar(u8, s, '\n');
    inline while (comptime iter.next()) |_| {
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
    const n = lineCount(entries_file);
    try std.testing.expectEqual(5, n);
}
