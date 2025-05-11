const std = @import("std");

const root: i32 = 0;
const unused: i32 = -1;
const terminator: u8 = '\x00';

fn asc(_: void, a: []const u8, b: []const u8) bool {
    return std.mem.lessThan(u8, a, b);
}

pub fn DoubleArray() type {
    return struct {
        const Self = @This();
        const default_capacity_size: usize = 65536;

        allocator: std.mem.Allocator,
        base: std.ArrayList(i32),
        check: std.ArrayList(i32),
        keywords: std.ArrayList([]const u8),
        ids: std.ArrayList(i32),

        pub fn init(allocator: std.mem.Allocator, keywords: []const []const u8) !Self {
            var base = try std.ArrayList(i32).initCapacity(allocator, default_capacity_size);
            var check = try std.ArrayList(i32).initCapacity(allocator, default_capacity_size);
            for (0..default_capacity_size) |_| {
                try base.append(0);
                try check.append(unused);
            }

            var sorted_keywords = try std.ArrayList([]const u8).initCapacity(allocator, keywords.len);
            for (keywords) |k| {
                const copied: []u8 = try allocator.alloc(u8, k.len);
                @memcpy(copied, k);
                try sorted_keywords.append(copied);
            }
            std.mem.sort([]const u8, sorted_keywords.items, {}, asc);

            var ids = try std.ArrayList(i32).initCapacity(allocator, keywords.len);
            for (0.., sorted_keywords.items) |i, _| {
                try ids.append(@intCast(i + 1));
            }

            return .{
                .allocator = allocator,
                .base = base,
                .check = check,
                .keywords = sorted_keywords,
                .ids = ids,
            };
        }

        pub fn deinit(self: *Self) void {
            self.base.deinit();
            self.check.deinit();
            self.ids.deinit();

            for (self.keywords.items) |k| {
                self.allocator.free(k);
            }
            self.keywords.deinit();
        }

        fn expand(self: *Self) !void {
            const n = self.base.items.len;
            try self.base.resize(2 * n);
            try self.check.resize(2 * n);
            for (n..2 * n) |i| {
                self.base.items[i] = 0;
                self.check.items[i] = unused;
            }
        }
        fn shrink(self: *Self) !void {
            const n = self.base.items.len;
            self.base.shrinkAndFree(n);
            self.check.shrinkAndFree(n);
        }
    };
}

test "create double array" {
    const allocator = std.testing.allocator;
    const keywords = [5][]const u8{
        "a",
        "ac",
        "b",
        "cab",
        "cb",
    };

    var da = try DoubleArray().init(allocator, &keywords);
    defer da.deinit();
}
