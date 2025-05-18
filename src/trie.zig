const std = @import("std");

const root: usize = 0;
const unused: i32 = -1;
const terminator: u8 = '\x00';

fn asc(_: void, a: []const u8, b: []const u8) bool {
    return std.mem.lessThan(u8, a, b);
}

test "sort ascii strings" {
    var keywords = [_][]const u8{
        "ac",
        "b",
        "cb",
        "cab",
        "a",
    };
    std.mem.sort([]const u8, &keywords, {}, asc);
    const expected = [_][]const u8{
        "a",
        "ac",
        "b",
        "cab",
        "cb",
    };
    try std.testing.expectEqualSlices([]const u8, &expected, &keywords);
}

test "sort multi-byte strings" {
    var keywords = [_][]const u8{
        "さしすせそ",
        "あいうえお",
        "かきくけこ",
        "たちつてと",
    };
    std.mem.sort([]const u8, &keywords, {}, asc);
    const expected = [_][]const u8{
        "あいうえお",
        "かきくけこ",
        "さしすせそ",
        "たちつてと",
    };
    try std.testing.expectEqualSlices([]const u8, &expected, &keywords);
}

const DoubleArray = struct {
    const Self = @This();

    allocator: std.mem.Allocator,
    base: std.ArrayList(i32),
    check: std.ArrayList(i32),
    entries: std.ArrayList([]const u8),

    pub fn init(allocator: std.mem.Allocator, entries: []const []const u8) !Self {
        return try Self.initCapacity(allocator, entries, 65536);
    }

    pub fn initCapacity(allocator: std.mem.Allocator, entries: []const []const u8, num: usize) !Self {
        var self = try Self.new(allocator, num);
        try self.build(entries);
        return self;
    }

    fn new(allocator: std.mem.Allocator, num: usize) !Self {
        var base = try std.ArrayList(i32).initCapacity(allocator, num);
        errdefer base.deinit();
        var check = try std.ArrayList(i32).initCapacity(allocator, num);
        errdefer check.deinit();

        // baseの各項目の初期値は「前の未使用項目のインデックス」を負にした値
        // checkの各項目の初期値は「次の未使用項目のインデックス」を負にした値
        //
        // base[0]の初期値は「1」(固定)
        // check[0]の初期値は「最初の未使用項目のインデックス」を負にした値
        //
        // baseの最初の未使用項目の値は「最後の未使用項目のインデックス」を負にした値
        // checkの最後の未使用項目の値は「-1」
        try base.resize(num);
        try check.resize(num);
        base.items[root] = 1;
        check.items[root] = -1;
        for (1..num) |i| {
            base.items[i] = -(@as(i32, @intCast(i)) - 1);
            check.items[i] = -(@as(i32, @intCast(i)) + 1);
        }
        base.items[1] = -(@as(i32, @intCast(num)) - 1);
        check.items[num - 1] = -1;

        return Self{
            .allocator = allocator,
            .base = base,
            .check = check,
            .entries = undefined,
        };
    }

    fn build(self: *Self, entries: []const []const u8) !void {
        var sorted = try std.ArrayList([]const u8).initCapacity(self.allocator, entries.len);
        errdefer sorted.deinit();
        for (entries) |keyword| {
            const entry = try self.allocator.dupe(u8, keyword);
            try sorted.append(entry);
        }
        std.mem.sort([]const u8, sorted.items, {}, asc);

        self.entries = sorted;

        var branches = try std.ArrayList(i32).initCapacity(self.allocator, self.entries.items.len);
        defer branches.deinit();

        for (0.., self.entries.items) |i, _| {
            try branches.append(@intCast(i));
        }

        try self.append(0, 0, branches.items);
    }

    pub fn deinit(self: *Self) void {
        self.base.deinit();
        self.check.deinit();

        for (self.entries.items) |k| {
            self.allocator.free(k);
        }
        self.entries.deinit();
    }

    fn expand(self: *Self) !void {
        const n = self.base.items.len;
        try self.base.resize(2 * n);
        try self.check.resize(2 * n);
        for (n..2 * n) |i| {
            self.base.items[i] = -(@as(i32, @intCast(i)) - 1);
            self.check.items[i] = -(@as(i32, @intCast(i)) + 1);
        }
        const start = -self.check.items[0];
        const end = -self.base.items[@as(usize, @intCast(start))];
        self.base.items[n] = -end;
        self.base.items[@as(usize, @intCast(start))] = -(2 * @as(i32, @intCast(n)) - 1);
        self.check.items[@as(usize, @intCast(end))] = -@as(i32, @intCast(n));
        self.check.items[2 * @as(usize, @intCast(n)) - 1] = -start;
    }

    fn setBase(self: *Self, s: usize, base: i32) !void {
        if (s == root) {
            return;
        }

        if (self.check.items[s] < 0) {
            if (self.base.items[s] == self.check.items[s]) {
                try self.expand();
            }

            const prev = -self.base.items[s];
            const next = -self.check.items[s];
            if (-@as(i32, @intCast(s)) == self.check.items[root]) {
                self.check.items[root] = self.check.items[s];
            }
            self.base.items[@as(usize, @intCast(next))] = self.base.items[s];
            self.check.items[@as(usize, @intCast(prev))] = self.check.items[s];
        }
        self.base.items[s] = base;
    }

    fn setCheck(self: *Self, s: usize, check: i32) !void {
        if (self.base.items[s] == self.check.items[s]) {
            try self.expand();
        }

        const prev = -self.base.items[s];
        const next = -self.check.items[s];
        if (-@as(i32, @intCast(s)) == self.check.items[root]) {
            self.check.items[root] = self.check.items[s];
        }

        self.base.items[@as(usize, @intCast(next))] = self.base.items[s];
        self.check.items[@as(usize, @intCast(prev))] = self.check.items[s];

        self.check.items[s] = check;
    }

    fn seekAndMark(self: *Self, s: usize, chars: []const u8) !void {
        var free: i32 = @intCast(root);
        const representative = chars[0];
        var base: i32 = 0;
        seek: while (true) {
            if (free != root and self.check.items[@as(usize, @intCast(free))] == self.check.items[root]) {
                try self.expand();
            }

            free = -self.check.items[@as(usize, @intCast(free))];
            base = free - @as(i32, representative);
            if (base <= 0) {
                continue :seek;
            }

            for (chars) |c| {
                const t = base + @as(i32, c);
                if (self.check.items[@as(usize, @intCast(t))] >= 0) {
                    continue :seek;
                }
            }
            break :seek;
        }

        try self.setBase(s, base);
        for (chars) |c| {
            const t: usize = @intCast(base + @as(i32, c));
            if (t >= self.base.items.len) {
                try self.expand();
            }
            try self.setCheck(t, @intCast(s));
        }
    }

    fn append(self: *Self, s: usize, index: usize, branches: []const i32) !void {
        var chars = std.ArrayList(u8).init(self.allocator);
        defer chars.deinit();

        var subtree = std.AutoArrayHashMap(u8, std.ArrayList(i32)).init(self.allocator);
        defer {
            for (subtree.values()) |v| {
                v.deinit();
            }
            subtree.deinit();
        }

        for (branches) |id| {
            const word = self.entries.items[@intCast(id)];
            const c = if (index >= word.len) terminator else word[index];

            if (chars.items.len == 0 or chars.getLast() != c) {
                try chars.append(c);
            }
            if (c != terminator) {
                const res = try subtree.getOrPut(c);
                if (res.found_existing) {
                    try res.value_ptr.*.append(id);
                } else {
                    var tree = std.ArrayList(i32).init(self.allocator);
                    try tree.append(id);
                    res.value_ptr.* = tree;
                }
            }
        }

        try self.seekAndMark(s, chars.items);
        for (chars.items) |c| {
            const t: usize = @intCast(self.base.items[s] + @as(i32, c));

            if (subtree.get(c)) |tree| {
                try self.append(t, index + 1, tree.items);
            } else {
                self.base.items[t] = -branches[0];
            }
        }
    }

    pub fn prefixSearch(self: Self, keyword: []const u8) !?i32 {
        var result: ?i32 = null;
        var node: i32 = 0;
        var next: i32 = 0;
        for (keyword) |char| {
            if (char == terminator) {
                break;
            }

            next = self.base.items[@intCast(node)] + char;
            if (@as(usize, @intCast(next)) >= self.base.items.len) {
                break;
            }
            if (self.check.items[@intCast(next)] != node) {
                break;
            }

            const ahead: i32 = self.base.items[@intCast(next)] + @as(i32, @intCast(terminator));
            if (ahead < self.base.items.len and self.check.items[@intCast(ahead)] == next and self.base.items[@intCast(ahead)] <= 0) {
                result = -self.base.items[@intCast(ahead)];
            }
            node = next;
        }

        return result;
    }

    pub fn commonPrefixSearch(self: Self, keyword: []const u8) ![]i32 {
        var results = std.ArrayList(i32).init(self.allocator);
        var node: i32 = 0;
        var next: i32 = 0;
        for (keyword) |char| {
            if (char == terminator) {
                break;
            }

            next = self.base.items[@intCast(node)] + char;
            if (@as(usize, @intCast(next)) >= self.base.items.len) {
                break;
            }
            if (self.check.items[@intCast(next)] != node) {
                break;
            }

            const ahead: i32 = self.base.items[@intCast(next)] + @as(i32, @intCast(terminator));
            if (ahead < self.base.items.len and self.check.items[@intCast(ahead)] == next and self.base.items[@intCast(ahead)] <= 0) {
                const id = -self.base.items[@intCast(ahead)];
                try results.append(id);
            }
            node = next;
        }

        return try results.toOwnedSlice();
    }
};

test "create double array" {
    const allocator = std.testing.allocator;
    const entries = [5][]const u8{
        "a",
        "ac",
        "b",
        "cab",
        "cb",
    };

    var da = try DoubleArray.init(allocator, &entries);
    defer da.deinit();
}

test "set base" {
    const allocator = std.testing.allocator;
    var da = try DoubleArray.new(allocator, 8);
    defer da.deinit();

    const base1 = [_]i32{ 1, -7, -1, -2, -3, -4, -5, -6 };
    const check1 = [_]i32{ -1, -2, -3, -4, -5, -6, -7, -1 };
    try std.testing.expectEqualSlices(i32, &base1, da.base.items);
    try std.testing.expectEqualSlices(i32, &check1, da.check.items);

    try da.setBase(0, 4);
    try std.testing.expectEqualSlices(i32, &base1, da.base.items);
    try std.testing.expectEqualSlices(i32, &check1, da.check.items);

    const base2 = [_]i32{ 1, 4, -7, -2, -3, -4, -5, -6 };
    const check2 = [_]i32{ -2, -2, -3, -4, -5, -6, -7, -2 };
    try da.setBase(1, 4);
    try std.testing.expectEqualSlices(i32, &base2, da.base.items);
    try std.testing.expectEqualSlices(i32, &check2, da.check.items);
}

test "set check" {
    const allocator = std.testing.allocator;
    var da = try DoubleArray.new(allocator, 8);
    defer da.deinit();

    const base1 = [_]i32{ 1, -7, -1, -2, -3, -4, -5, -6 };
    const check1 = [_]i32{ -1, -2, -3, -4, -5, -6, -7, -1 };
    try std.testing.expectEqualSlices(i32, &base1, da.base.items);
    try std.testing.expectEqualSlices(i32, &check1, da.check.items);

    try da.setCheck(1, 4);
    const base2 = [_]i32{ 1, -7, -7, -2, -3, -4, -5, -6 };
    const check2 = [_]i32{ -2, 4, -3, -4, -5, -6, -7, -2 };
    try std.testing.expectEqualSlices(i32, &base2, da.base.items);
    try std.testing.expectEqualSlices(i32, &check2, da.check.items);
}

test "seek and mark" {
    const allocator = std.testing.allocator;
    var da = try DoubleArray.new(allocator, 9);
    defer da.deinit();

    const base1 = [_]i32{ 1, -8, -1, -2, -3, -4, -5, -6, -7 };
    const check1 = [_]i32{ -1, -2, -3, -4, -5, -6, -7, -8, -1 };
    try std.testing.expectEqualSlices(i32, &base1, da.base.items);
    try std.testing.expectEqualSlices(i32, &check1, da.check.items);

    const chars = [_]u8{ 1, 2, 3 };
    try da.seekAndMark(0, &chars);
    const base2 = [_]i32{ 1, -8, -1, -1, -1, -1, -5, -6, -7 };
    const check2 = [_]i32{ -1, -5, 0, 0, 0, -6, -7, -8, -1 };
    try std.testing.expectEqualSlices(i32, &base2, da.base.items);
    try std.testing.expectEqualSlices(i32, &check2, da.check.items);
}

test "common prefix search ascii strings" {
    const allocator = std.testing.allocator;
    const entries = [_][]const u8{
        "a",
        "ac",
        "b",
        "cab",
        "cb",
    };
    var da = try DoubleArray.initCapacity(allocator, &entries, 32);
    defer da.deinit();

    const results = try da.commonPrefixSearch("acb");
    defer allocator.free(results);

    const expected = [_]i32{ 0, 1 };
    try std.testing.expectEqualSlices(i32, &expected, results);
}

test "prefix search ascii strings" {
    const allocator = std.testing.allocator;
    const entries = [_][]const u8{
        "a",
        "ac",
        "b",
        "cab",
        "cb",
    };
    var da = try DoubleArray.initCapacity(allocator, &entries, 32);
    defer da.deinit();
    const result = try da.prefixSearch("cab");
    try std.testing.expect(result != null);
    try std.testing.expectEqual(3, result.?);
}

test "prefix search multi-byte strings" {
    const allocator = std.testing.allocator;
    const entries = [_][]const u8{
        "電気",
        "電気通信",
        "電気通信大学",
        "電気通信大学大学院",
        "電気通信大学大学院電気通信学研究科",
        "電気通信大学院大学",
        "電気通信大学電気通信学部",
    };
    var da = try DoubleArray.init(allocator, &entries);
    defer da.deinit();

    const result = try da.prefixSearch("電気通信大学大学院電気通信学研究科");
    try std.testing.expectEqual(4, result.?);
}
