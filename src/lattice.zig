//! Lattice data structure for morphological analysis.

const std = @import("std");
const trie = @import("trie.zig");

/// Implementation of the Lattice data structure.
pub const Lattice = struct {
    const Self = @This();

    input: []const u8,
    begin_nodes: []std.ArrayList([]const u8), // represent node set that begin at the position `i` (a byte position, not a character position)
    end_nodes: []std.ArrayList([]const u8), // represent node set that end at the position `i` (a byte position, not a character position)

    /// Initializes the lattice with the given input string.
    ///
    /// You should de-initialize with `deinit()`.
    pub fn init(allocator: std.mem.Allocator, input: []const u8) std.mem.Allocator.Error!Self {
        const begin_nodes = try allocator.alloc(std.ArrayList([]const u8), input.len + 1);
        errdefer allocator.free(begin_nodes);
        @memset(begin_nodes, std.ArrayList([]const u8).empty);

        const end_nodes = try allocator.alloc(std.ArrayList([]const u8), input.len + 1);
        errdefer allocator.free(end_nodes);
        @memset(end_nodes, std.ArrayList([]const u8).empty);

        return Self{
            .input = input,
            .begin_nodes = begin_nodes,
            .end_nodes = end_nodes,
        };
    }

    /// Releases all allocated memory.
    pub fn deinit(self: *Self, allocator: std.mem.Allocator) void {
        for (self.begin_nodes) |*node| {
            node.*.deinit(allocator);
        }
        for (self.end_nodes) |*node| {
            node.*.deinit(allocator);
        }
        allocator.free(self.begin_nodes);
        allocator.free(self.end_nodes);
    }

    /// Builds the lattice using the given double array.
    pub fn build(self: *Self, allocator: std.mem.Allocator, da: *trie.DoubleArray) !void {
        var i: usize = 0;
        while (i < self.input.len) {
            const size = try std.unicode.utf8ByteSequenceLength(self.input[i]);

            const ids = try da.*.commonPrefixSearch(allocator, self.input[i..]);
            defer allocator.free(ids);

            var has_single_word = false;
            for (ids) |id| {
                const word = da.*.words[id];

                try self.begin_nodes[i].append(allocator, self.input[i .. i + word.len]);
                try self.end_nodes[i + word.len].append(allocator, self.input[i .. i + word.len]);

                if (!has_single_word and size == word.len) {
                    has_single_word = true;
                }
            }

            // If there is no single word that matches the input, we need to add the single character as a dummy node to prevent breaking the path.
            if (!has_single_word) {
                try self.begin_nodes[i].append(allocator, self.input[i .. i + size]);
                try self.end_nodes[i + size].append(allocator, self.input[i .. i + size]);
            }

            i += size;
        }
    }
};

test "build lattice" {
    const allocator = std.testing.allocator;
    const words = [_][]const u8{
        "た",
        "たら",
        "なし",
        "なっ",
        "に",
        "になっ",
        "は",
        "はなし",
        "元気",
    };
    var da = try trie.DoubleArray.init(allocator, &words);
    defer da.deinit(allocator);
    try da.build(allocator);

    var lattice = try Lattice.init(allocator, "はなしたら元気になった");
    defer lattice.deinit(allocator);
    try lattice.build(allocator, &da);

    try std.testing.expectEqual(34, lattice.begin_nodes.len);
    try std.testing.expectEqual(34, lattice.end_nodes.len);

    try std.testing.expectEqualDeep(&[_][]const u8{ "は", "はなし" }, lattice.begin_nodes[0].items);
}
