//! Trie data structure implemented by Double-Array.

const std = @import("std");

const root: usize = 0;
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

fn Queue(comptime T: type) type {
    return struct {
        const Self = @This();

        allocator: std.mem.Allocator,
        buffer: std.ArrayList(T),
        head: usize,
        tail: usize,
        size: usize,

        pub fn init(allocator: std.mem.Allocator) Self {
            return .{
                .allocator = allocator,
                .buffer = std.ArrayList(T).init(allocator),
                .head = 0,
                .tail = 0,
                .size = 0,
            };
        }

        pub fn deinit(self: Self) void {
            self.buffer.deinit();
        }

        pub fn enqueue(self: *Self, item: T) std.mem.Allocator.Error!void {
            if (self.size == self.buffer.items.len) {
                try self.expand();
            }

            self.buffer.items[self.tail] = item;
            self.tail = @mod(self.tail + 1, self.buffer.items.len);
            self.size += 1;
        }

        pub fn dequeue(self: *Self) ?T {
            if (self.size == 0) {
                return null;
            }
            const item = self.buffer.items[self.head];
            self.buffer.items[self.head] = undefined;
            self.head = @mod(self.head + 1, self.buffer.items.len);
            self.size -= 1;
            return item;
        }

        pub fn empty(self: Self) bool {
            return self.size == 0;
        }

        fn expand(self: *Self) std.mem.Allocator.Error!void {
            var new = std.ArrayList(T).init(self.allocator);
            const size = if (self.buffer.items.len == 0) 1 else 2 * self.buffer.items.len;
            try new.resize(size);
            for (0..self.buffer.items.len) |i| {
                new.items[i] = self.buffer.items[@mod(self.head + i, self.buffer.items.len)];
            }
            self.buffer.deinit();
            self.buffer = new;
            self.head = 0;
            self.tail = self.size;
        }
    };
}

test "enqueue and dequeue" {
    const allocator = std.testing.allocator;
    var queue = Queue(i32).init(allocator);
    defer queue.deinit();

    try std.testing.expect(queue.empty());

    try queue.enqueue(1);
    try queue.enqueue(2);
    try queue.enqueue(3);

    try std.testing.expect(!queue.empty());

    try std.testing.expectEqual(1, queue.dequeue().?);
    try std.testing.expectEqual(2, queue.dequeue().?);
    try std.testing.expectEqual(3, queue.dequeue().?);
    try std.testing.expectEqual(null, queue.dequeue());

    try std.testing.expect(queue.empty());
}

test "input large number of items into queue" {
    const allocator = std.testing.allocator;
    var queue = Queue(i32).init(allocator);
    defer queue.deinit();

    for (0..100000) |i| {
        try queue.enqueue(@as(i32, @intCast(i)));
    }
}

/// Item of the queue for BFS.
const Node = struct {
    const Self = @This();

    index: usize,
    depth: usize,
    edges: std.ArrayList(i32),

    pub fn init(index: usize, depth: usize, edges: std.ArrayList(i32)) Self {
        return .{
            .index = index,
            .depth = depth,
            .edges = edges,
        };
    }

    pub fn deinit(self: Self) void {
        self.edges.deinit();
    }
};

pub const DoubleArrayError = error{
    DuplicatedEntryError,
} || std.mem.Allocator.Error;

pub const DoubleArray = struct {
    const Self = @This();

    allocator: std.mem.Allocator,
    base: std.ArrayList(i32),
    check: std.ArrayList(i32),
    entries: std.ArrayList([]const u8),

    /// Initialize DoubleArray with default capacity size. The elements of these arrays are not built at initialization.
    /// You should call the `build()` method to build the elements of these arrays.
    /// The values of `keywords` are copied and stored in sorted order.
    ///
    /// You should deinitialize with `deinit()`.
    pub fn init(allocator: std.mem.Allocator, keywords: []const []const u8) DoubleArrayError!Self {
        return try Self.initCapacity(allocator, keywords, 65536);
    }

    /// Initialize DoubleArray with capacity size. The elements of these arrays are not built at initialization.
    /// You should call the `build()` method to build the elements of these arrays.
    /// The values of `keywords` are copied and stored in sorted order.
    ///
    /// Asserts num is greather than 0.
    ///
    /// You should deinitialize with `deinit()`.
    pub fn initCapacity(allocator: std.mem.Allocator, keywords: []const []const u8, num: usize) DoubleArrayError!Self {
        std.debug.assert(num > 0);

        var base = try std.ArrayList(i32).initCapacity(allocator, num);
        errdefer base.deinit();
        var check = try std.ArrayList(i32).initCapacity(allocator, num);
        errdefer check.deinit();
        var entries = std.ArrayList([]const u8).init(allocator);
        errdefer entries.deinit();

        // Initialize base and check arrays as a doubly-linked list to manage available space.
        try base.resize(num);
        try check.resize(num);
        base.items[root] = 1; // the value of base[0] is 1. (special value)
        check.items[root] = -1; // the value of check[0] represents "leftmost available space index (saved as a negative value)"
        for (1..num) |i| {
            base.items[i] = -(@as(i32, @intCast(i)) - 1); // the value of base[i] represents "previous available space index (saved as a negative value)"
            check.items[i] = -(@as(i32, @intCast(i)) + 1); // the value of check[i] represents "next available space index (saved as a negative value)"
        }
        base.items[1] = -(@as(i32, @intCast(num)) - 1); // the value of base[1] represents "rightmost available space index (saved as a negative value)"
        check.items[num - 1] = -1; // the value of check[1] is -1. (special value)

        // Sort and copy the word entires.
        for (keywords) |w| {
            const word = try allocator.dupe(u8, w);
            try entries.append(word);
        }
        std.mem.sort([]const u8, entries.items, {}, asc);

        if (entries.items.len > 0) {
            var entry = entries.items[0];
            for (entries.items[1..]) |e| {
                if (std.mem.eql(u8, entry, e)) {
                    return DoubleArrayError.DuplicatedEntryError;
                }
                entry = e;
            }
        }

        return Self{
            .allocator = allocator,
            .base = base,
            .check = check,
            .entries = entries,
        };
    }

    /// Build double array.
    pub fn build(self: *Self) std.mem.Allocator.Error!void {
        var branches = try std.ArrayList(i32).initCapacity(self.allocator, self.entries.items.len);
        for (0.., self.entries.items) |i, _| {
            try branches.append(@intCast(i));
        }
        // deinitialized later on by the node that owns it, so we won't do it here.

        var queue = Queue(Node).init(self.allocator);
        defer queue.deinit();
        try queue.enqueue(Node.init(0, 0, branches));

        while (queue.dequeue()) |node| {
            defer node.deinit();

            var chars = std.ArrayList(u8).init(self.allocator);
            defer chars.deinit();
            var subtree = std.AutoArrayHashMap(u8, std.ArrayList(i32)).init(self.allocator);
            defer subtree.deinit();

            for (node.edges.items) |id| {
                const word = self.entries.items[@intCast(id)];
                const char = if (node.depth >= word.len) terminator else word[node.depth];

                if (chars.items.len == 0 or chars.getLast() != char) {
                    try chars.append(char);
                }
                if (char != terminator) {
                    const gop = try subtree.getOrPut(char);
                    if (gop.found_existing) {
                        try gop.value_ptr.*.append(id);
                    } else {
                        var tree = std.ArrayList(i32).init(self.allocator);
                        try tree.append(id);
                        gop.value_ptr.* = tree;
                    }
                }
            }

            const x = try self.seek(chars.items);
            try self.setBase(node.index, x);
            for (chars.items) |char| {
                const t: usize = @intCast(self.base.items[node.index] + @as(i32, char));
                if (t >= self.base.items.len) {
                    try self.expand();
                }
                try self.setCheck(t, @intCast(node.index));

                if (subtree.get(char)) |edges| {
                    const next = Node.init(t, node.depth + 1, edges);
                    try queue.enqueue(next);
                } else {
                    self.base.items[t] = -node.edges.items[0];
                }
            }
        }

        self.shrink();
    }

    /// Release all allocated memory.
    pub fn deinit(self: Self) void {
        self.base.deinit();
        self.check.deinit();

        for (self.entries.items) |e| {
            self.allocator.free(e);
        }
        self.entries.deinit();
    }

    /// Double the array size.
    /// It also performs reconnection processing of the linked list.
    fn expand(self: *Self) std.mem.Allocator.Error!void {
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

    /// Release unused area and reduce array size.
    fn shrink(self: *Self) void {
        var len = self.check.items.len;
        var i: usize = 0;
        while (i < self.check.items.len) {
            if (self.check.items[self.check.items.len -% i -% 1] < 0) {
                len -%= 1;
            } else {
                break;
            }
            i += 1;
        }

        if (len == self.check.items.len) {
            return;
        }

        self.base.shrinkAndFree(len);
        self.check.shrinkAndFree(len);
    }

    /// Set the specified value into `base[s]`, and update linked-list.
    fn setBase(self: *Self, s: usize, value: i32) std.mem.Allocator.Error!void {
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
        self.base.items[s] = value;
    }

    /// Set the value into `check[s]`, and update linked-list.
    fn setCheck(self: *Self, s: usize, value: i32) std.mem.Allocator.Error!void {
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

        self.check.items[s] = value;
    }

    /// Seek the minimum index value `x` for which all transitions are possible.
    fn seek(self: *Self, chars: []const u8) std.mem.Allocator.Error!i32 {
        var free: i32 = @intCast(root);
        const representative = chars[0];
        var x: i32 = 0;
        seek: while (true) {
            if (free != root and self.check.items[@as(usize, @intCast(free))] == self.check.items[root]) {
                try self.expand();
            }

            free = -self.check.items[@as(usize, @intCast(free))];
            x = free - @as(i32, representative);
            if (x <= 0) {
                continue :seek;
            }

            for (chars) |c| {
                const t = x + @as(i32, c);
                if (self.check.items[@as(usize, @intCast(t))] >= 0) {
                    continue :seek;
                }
            }
            break :seek;
        }
        return x;
    }

    /// Returns IDs of the keywords sharing common prefix in the input.
    pub fn commonPrefixSearch(self: Self, input: []const u8) std.mem.Allocator.Error![]i32 {
        var results = std.ArrayList(i32).init(self.allocator);
        var node: i32 = 0;
        var next: i32 = 0;
        for (input) |char| {
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

    /// Returns ID of keyword which has the longest common prefix in the input if found.
    pub fn prefixSearch(self: Self, input: []const u8) ?i32 {
        var result: ?i32 = null;
        var node: i32 = 0;
        var next: i32 = 0;
        for (input) |char| {
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

    /// Returns ID of the keyword if found.
    pub fn search(self: Self, input: []const u8) ?i32 {
        if (input.len == 0) {
            return null;
        }

        var node: i32 = 0;
        var next: i32 = 0;
        for (input) |char| {
            if (char == terminator) {
                return null;
            }

            next = self.base.items[@intCast(node)] + char;
            if (@as(usize, @intCast(next)) >= self.base.items.len) {
                return null;
            }
            if (self.check.items[@intCast(next)] != node) {
                return null;
            }

            node = next;
        }

        const ahead: i32 = self.base.items[@intCast(next)] + @as(i32, @intCast(terminator));
        if (ahead < self.base.items.len and self.check.items[@intCast(ahead)] == next and self.base.items[@intCast(ahead)] <= 0) {
            return -self.base.items[@intCast(ahead)];
        }
        return null;
    }
};

test "expand double array" {
    const allocator = std.testing.allocator;
    var da = try DoubleArray.initCapacity(allocator, &.{}, 4);
    defer da.deinit();

    const base1 = [_]i32{ 1, -3, -1, -2 };
    const check1 = [_]i32{ -1, -2, -3, -1 };
    try std.testing.expectEqualSlices(i32, &base1, da.base.items);
    try std.testing.expectEqualSlices(i32, &check1, da.check.items);

    try da.expand();

    const base2 = [_]i32{ 1, -7, -1, -2, -3, -4, -5, -6 };
    const check2 = [_]i32{ -1, -2, -3, -4, -5, -6, -7, -1 };
    try std.testing.expectEqualSlices(i32, &base2, da.base.items);
    try std.testing.expectEqualSlices(i32, &check2, da.check.items);
}

test "shrink double array" {
    const allocator = std.testing.allocator;
    var da = try DoubleArray.initCapacity(allocator, &.{}, 4);
    defer da.deinit();

    const base1 = [_]i32{ 1, -3, -1, -2 };
    const check1 = [_]i32{ -1, -2, -3, -1 };
    try std.testing.expectEqualSlices(i32, &base1, da.base.items);
    try std.testing.expectEqualSlices(i32, &check1, da.check.items);

    da.shrink();

    try std.testing.expectEqual(0, da.base.items.len);
    try std.testing.expectEqual(0, da.check.items.len);
}

test "build and shrink" {
    const allocator = std.testing.allocator;
    const keywords = [_][]const u8{
        "a",
        "ac",
        "b",
        "cab",
        "cb",
    };
    const cap = 65536;
    var da = try DoubleArray.initCapacity(allocator, &keywords, cap);
    defer da.deinit();

    try std.testing.expectEqual(cap, da.base.items.len);
    try std.testing.expectEqual(cap, da.check.items.len);

    try da.build();

    try std.testing.expect(da.base.items.len < cap);
    try std.testing.expect(da.check.items.len < cap);
}

test "set base" {
    const allocator = std.testing.allocator;
    var da = try DoubleArray.initCapacity(allocator, &.{}, 8);
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
    var da = try DoubleArray.initCapacity(allocator, &.{}, 8);
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

test "seek" {
    const allocator = std.testing.allocator;
    var da = try DoubleArray.initCapacity(allocator, &.{}, 9);
    defer da.deinit();

    const base1 = [_]i32{ 1, -8, -1, -2, -3, -4, -5, -6, -7 };
    const check1 = [_]i32{ -1, -2, -3, -4, -5, -6, -7, -8, -1 };
    try std.testing.expectEqualSlices(i32, &base1, da.base.items);
    try std.testing.expectEqualSlices(i32, &check1, da.check.items);

    const chars = [_]u8{ 1, 2, 3 };
    const x = try da.seek(&chars);
    try std.testing.expectEqual(1, x);
}

test "common prefix search ascii strings" {
    const allocator = std.testing.allocator;
    const keywords = [_][]const u8{
        "a",
        "ac",
        "b",
        "cab",
        "cb",
    };
    var da = try DoubleArray.init(allocator, &keywords);
    defer da.deinit();
    try da.build();

    const results = try da.commonPrefixSearch("acb");
    defer allocator.free(results);

    const expected = [_]i32{ 0, 1 };
    try std.testing.expectEqualSlices(i32, &expected, results);
}

test "prefix search ascii strings" {
    const allocator = std.testing.allocator;
    const keywords = [_][]const u8{
        "a",
        "ac",
        "b",
        "cab",
        "cb",
    };
    var da = try DoubleArray.init(allocator, &keywords);
    defer da.deinit();
    try da.build();

    const result = da.prefixSearch("cab");
    try std.testing.expect(result != null);
    try std.testing.expectEqual(3, result.?);
}

test "prefix search multi-byte strings" {
    const allocator = std.testing.allocator;
    const keywords = [_][]const u8{
        "電気",
        "電気通信",
        "電気通信大学",
        "電気通信大学大学院",
        "電気通信大学大学院電気通信学研究科",
        "電気通信大学院大学",
        "電気通信大学電気通信学部",
    };
    var da = try DoubleArray.init(allocator, &keywords);
    defer da.deinit();
    try da.build();

    const result = da.prefixSearch("電気通信大学大学院電気通信学研究科");
    try std.testing.expectEqual(4, result.?);
}

test "prefix search not found" {
    const allocator = std.testing.allocator;
    const keywords = [_][]const u8{
        "電気",
        "電気通信",
        "電気通信大学",
        "電気通信大学大学院",
        "電気通信大学大学院電気通信学研究科",
        "電気通信大学院大学",
        "電気通信大学電気通信学部",
    };
    var da = try DoubleArray.init(allocator, &keywords);
    defer da.deinit();
    try da.build();

    const result = da.prefixSearch("電通");
    try std.testing.expect(result == null);
}

test "search found input keyword" {
    const allocator = std.testing.allocator;
    const keywords = [_][]const u8{
        "電気",
        "電気通信",
        "電気通信大学",
        "電気通信大学大学院",
        "電気通信大学大学院電気通信学研究科",
        "電気通信大学院大学",
        "電気通信大学電気通信学部",
    };
    var da = try DoubleArray.init(allocator, &keywords);
    defer da.deinit();
    try da.build();

    const result = da.search("電気通信大学院大学");
    try std.testing.expectEqual(5, result.?);
}

test "search not found input keyword" {
    const allocator = std.testing.allocator;
    const keywords = [_][]const u8{
        "電気",
        "電気通信",
        "電気通信大学",
        "電気通信大学大学院",
        "電気通信大学大学院電気通信学研究科",
        "電気通信大学院大学",
        "電気通信大学電気通信学部",
    };
    var da = try DoubleArray.init(allocator, &keywords);
    defer da.deinit();
    try da.build();

    const result = da.search("電通");
    try std.testing.expect(result == null);
}
