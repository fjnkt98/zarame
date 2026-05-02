const std = @import("std");
const trie = @import("trie.zig");

/// Header of the binary dictionary file.
pub const Header = packed struct {
    magic: u64,
    version: u32,
    count: u32,
};

/// Magic number "ZARAME01".
pub const magic: u64 = 0x3130454D4152415A;

pub const Dictionary = struct {
    index: Index,
    morphs: []const Morph,
    pos_table: PosTable,
    matrix: ConnectionMatrix,
};

pub const Index = struct {
    const Self = @This();

    da: trie.DoubleArray,
    dup: std.AutoHashMapUnmanaged(usize, usize),

    pub fn init(allocator: std.mem.Allocator, lex: []const []const u8) !Self {
        if (!std.sort.isSorted([]const u8, lex, {}, trie.stringLessThan)) {
            return error.UnsortedWordsError;
        }

        var dup = std.AutoHashMapUnmanaged(usize, usize).empty;

        var words = try std.ArrayList([]const u8).initCapacity(allocator, lex.len);
        defer words.deinit(allocator);
        var ids = try std.ArrayList(usize).initCapacity(allocator, lex.len);
        defer ids.deinit(allocator);

        var prev_id: usize = 0;
        var prev_word: []const u8 = "";
        for (0.., lex) |i, word| {
            if (std.mem.eql(u8, word, prev_word)) {
                const gop = try dup.getOrPut(allocator, prev_id);
                if (gop.found_existing) {
                    gop.value_ptr.* += 1;
                } else {
                    gop.value_ptr.* = 2;
                }
                continue;
            }

            prev_id = i;
            prev_word = word;
            try words.append(allocator, word);
            try ids.append(allocator, i);
        }

        const da = try trie.DoubleArray.init(allocator, words.items, ids.items, 65535);

        return Self{
            .da = da,
            .dup = dup,
        };
    }

    pub fn deinit(self: *Self, allocator: std.mem.Allocator) void {
        self.da.deinit(allocator);
        self.dup.deinit(allocator);
    }

    pub fn commonPrefixSearch(self: Self, allocator: std.mem.Allocator, input: []const u8) std.mem.Allocator.Error![]trie.SearchResult {
        var res = std.ArrayList(trie.SearchResult).empty;

        const results = try self.da.commonPrefixSearch(allocator, input);
        defer allocator.free(results);

        for (results) |result| {
            const count = self.dup.get(result.id) orelse 1;
            for (0..count) |i| {
                try res.append(allocator, .{
                    .id = result.id + i,
                    .length = result.length,
                });
            }
        }
        return try res.toOwnedSlice(allocator);
    }
};

test "common prefix search multi-byte strings with duplication" {
    const allocator = std.testing.allocator;
    const lex = [_][]const u8{
        "電気",
        "電気",
        "電気通信",
        "電気通信大学",
        "電気通信大学大学院",
        "電気通信大学大学院電気通信学研究科",
        "電気通信大学院大学",
        "電気通信大学電気通信学部",
    };
    var index = try Index.init(allocator, &lex);
    defer index.deinit(allocator);

    const actual = try index.commonPrefixSearch(allocator, "電気通信大学院大学");
    defer allocator.free(actual);

    const expected = [_]trie.SearchResult{
        .{ .id = 0, .length = 6 },
        .{ .id = 1, .length = 6 },
        .{ .id = 2, .length = 12 },
        .{ .id = 3, .length = 18 },
        .{ .id = 6, .length = 27 },
    };
    try std.testing.expectEqualSlices(trie.SearchResult, &expected, actual);
}

pub const Morph = struct {
    const Self = @This();

    left_id: usize,
    right_id: usize,
    cost: i16,

    pub const zero = Self{
        .left_id = 0,
        .right_id = 0,
        .cost = 0,
    };
};

pub const PosTable = struct {
    const Self = @This();

    poss: [][4]usize,
    map: std.AutoArrayHashMapUnmanaged(usize, []const u8),

    pub fn init(allocator: std.mem.Allocator, num: usize) !Self {
        const poss = try allocator.alloc([4]usize, num);
        errdefer allocator.free(poss);
        @memset(poss, .{ 0, 0, 0, 0 });
        return Self{
            .poss = poss,
            .map = std.AutoArrayHashMapUnmanaged(usize, []const u8).empty,
        };
    }

    pub fn deinit(self: *Self, allocator: std.mem.Allocator) void {
        allocator.free(self.poss);
        self.map.deinit(allocator);
    }

    pub fn get(self: Self, id: usize) [4][]const u8 {
        const pos = self.poss[id];
        return .{
            self.map.get(pos[0]) orelse "",
            self.map.get(pos[1]) orelse "",
            self.map.get(pos[2]) orelse "",
            self.map.get(pos[3]) orelse "",
        };
    }
};

pub const ConnectionMatrix = struct {
    const Self = @This();

    row_size: usize,
    col_size: usize,
    data: []const i16,

    pub fn init(allocator: std.mem.Allocator, row_size: usize, col_size: usize) !Self {
        const data = try allocator.alloc(i16, row_size * col_size);
        return Self{
            .row_size = row_size,
            .col_size = col_size,
            .data = data,
        };
    }

    pub fn deinit(self: *Self, allocator: std.mem.Allocator) void {
        allocator.free(self.data);
    }

    pub fn set(self: *Self, left_id: usize, right_id: usize, value: i16) void {
        self.data[left_id * self.col_size + right_id] = value;
    }

    pub fn get(self: Self, left_id: usize, right_id: usize) i16 {
        return self.data[left_id * self.col_size + right_id];
    }
};
