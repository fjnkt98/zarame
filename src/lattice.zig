//! Lattice data structure for morphological analysis.

const std = @import("std");
const trie = @import("trie.zig");
const dictionary = @import("dictionary.zig");

const MAXIMUM_COST = 1 << 31 - 1;

pub const LatticeBuildError = error{
    Utf8InvalidStartByte,
} || std.mem.Allocator.Error;

pub const NodeClass = enum {
    dummy,
    known,
    unknown,
    // user, TODO: user defined token support
};

pub const LatticeNode = struct {
    id: usize,
    class: NodeClass,
    surface: []const u8,
    min_cost: i32,
    min_prev: ?*LatticeNode,
};

/// Implementation of the Lattice data structure.
pub const Lattice = struct {
    const Self = @This();

    input: []const u8,
    begin_nodes: []std.ArrayList(LatticeNode), // represent node set that begin at the position `i` (a byte position, not a character position)
    end_nodes: []std.ArrayList(LatticeNode), // represent node set that end at the position `i` (a byte position, not a character position)

    /// Initializes the lattice with the given input string.
    ///
    /// You should de-initialize with `deinit()`.
    pub fn init(allocator: std.mem.Allocator, input: []const u8) std.mem.Allocator.Error!Self {
        const begin_nodes = try allocator.alloc(std.ArrayList(LatticeNode), input.len + 1);
        errdefer allocator.free(begin_nodes);
        @memset(begin_nodes, std.ArrayList(LatticeNode).empty);

        const end_nodes = try allocator.alloc(std.ArrayList(LatticeNode), input.len + 1);
        errdefer allocator.free(end_nodes);
        @memset(end_nodes, std.ArrayList(LatticeNode).empty);

        // add BOS node
        try end_nodes[0].append(allocator, LatticeNode{ .id = 0, .class = .dummy, .surface = "BOS", .min_cost = MAXIMUM_COST, .min_prev = null });
        // ad EOS node
        try begin_nodes[input.len].append(allocator, LatticeNode{ .id = 0, .class = .dummy, .surface = "EOS", .min_cost = MAXIMUM_COST, .min_prev = null });

        return Self{
            .input = input,
            .begin_nodes = begin_nodes,
            .end_nodes = end_nodes,
        };
    }

    /// Releases all allocated memory.
    pub fn deinit(self: *Self, allocator: std.mem.Allocator) void {
        for (self.begin_nodes) |*nodes| {
            nodes.*.deinit(allocator);
        }
        for (self.end_nodes) |*nodes| {
            nodes.*.deinit(allocator);
        }
        allocator.free(self.begin_nodes);
        allocator.free(self.end_nodes);
    }

    fn insert(self: *Self, allocator: std.mem.Allocator, id: usize, class: NodeClass, begin: usize, end: usize) std.mem.Allocator.Error!void {
        const node = LatticeNode{
            .id = id,
            .class = class,
            .surface = self.input[begin..end],
            .min_cost = MAXIMUM_COST,
            .min_prev = null,
        };
        try self.begin_nodes[begin].append(allocator, node);
        try self.end_nodes[end].append(allocator, node);
    }

    /// Builds the lattice using the given double array.
    pub fn build(self: *Self, allocator: std.mem.Allocator, dict: dictionary.Dictionary) LatticeBuildError!void {
        var i: usize = 0;
        while (i < self.input.len) {
            const size = try std.unicode.utf8ByteSequenceLength(self.input[i]);

            const ids = try dict.index.commonPrefixSearch(allocator, self.input[i..]);
            defer allocator.free(ids);

            var has_single_word = false;
            for (ids) |id| {
                const word = dict.index.words[id];

                try self.insert(allocator, id, .known, i, i + word.len);

                if (!has_single_word and size == word.len) {
                    has_single_word = true;
                }
            }

            // If there is no single word that matches the input, we need to add the single character as a dummy node to prevent breaking the path.
            if (!has_single_word) {
                try self.insert(allocator, 0, .unknown, i, i + size);
            }

            i += size;
        }
    }

    pub fn viterbi(self: Self, allocator: std.mem.Allocator, dict: dictionary.Dictionary) std.mem.Allocator.Error![]LatticeNode {
        var bos = self.end_nodes[0].items[0];
        bos.min_cost = 0;

        for (0..self.input.len + 1) |i| {
            for (self.begin_nodes[i].items) |*r_node| {
                r_node.*.min_cost = MAXIMUM_COST;
                r_node.*.min_prev = null;

                for (self.end_nodes[i].items) |*l_node| {
                    const r_morph = dict.morphs[r_node.*.id];
                    const l_morph = dict.morphs[l_node.*.id];

                    const emission_cost = if (r_node.*.class == .known) r_morph.cost else 0;
                    const transition_cost = if (r_node.*.class == .known and l_node.*.class == .known) dict.matrix.at(l_morph.right_id, r_morph.left_id) else 0;
                    const cost = l_node.*.min_cost + emission_cost + transition_cost;
                    if (cost < r_node.*.min_cost) {
                        r_node.*.min_cost = cost;
                        r_node.*.min_prev = l_node;
                    }
                }
            }
        }

        var path = std.ArrayList(LatticeNode).empty;
        var node: ?*LatticeNode = &self.begin_nodes[self.input.len].items[0];
        while (node != null) {
            try path.append(allocator, node.?.*);
            node = node.?.min_prev;
        }
        return try path.toOwnedSlice(allocator);
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

    const dict = dictionary.Dictionary{
        .index = da,
        .morphs = &[_]dictionary.Morph{
            .{ .left_id = 0, .right_id = 0, .cost = 1 }, // た
            .{ .left_id = 0, .right_id = 0, .cost = 1 }, // たら
            .{ .left_id = 0, .right_id = 0, .cost = 1 }, // なし
            .{ .left_id = 0, .right_id = 0, .cost = 1 }, // なっ
            .{ .left_id = 0, .right_id = 0, .cost = 1 }, // に
            .{ .left_id = 0, .right_id = 0, .cost = 1 }, // になっ
            .{ .left_id = 0, .right_id = 0, .cost = 1 }, // は
            .{ .left_id = 0, .right_id = 0, .cost = 1 }, // はなし
            .{ .left_id = 0, .right_id = 0, .cost = 1 }, // 元気
        },
        .matrix = dictionary.Matrix{
            .row = 9,
            .col = 9,
            .data = &[_]i16{
                0, 0, 0, 0, 0, 0, 0, 0, 0,
                0, 0, 0, 0, 0, 0, 0, 0, 0,
                0, 0, 0, 0, 0, 0, 0, 0, 0,
                0, 0, 0, 0, 0, 0, 0, 0, 0,
                0, 0, 0, 0, 0, 0, 0, 0, 0,
                0, 0, 0, 0, 0, 0, 0, 0, 0,
                0, 0, 0, 0, 0, 0, 0, 0, 0,
                0, 0, 0, 0, 0, 0, 0, 0, 0,
                0, 0, 0, 0, 0, 0, 0, 0, 0,
            },
        },
    };

    var lattice = try Lattice.init(allocator, "はなしたら元気になった");
    defer lattice.deinit(allocator);
    try lattice.build(allocator, dict);

    try std.testing.expectEqual(34, lattice.begin_nodes.len);
    try std.testing.expectEqual(34, lattice.end_nodes.len);

    var surfaces = try allocator.alloc([]const u8, lattice.begin_nodes[0].items.len);
    defer allocator.free(surfaces);
    for (0.., lattice.begin_nodes[0].items) |i, node| {
        surfaces[i] = node.surface;
    }
    try std.testing.expectEqualDeep(&[_][]const u8{ "は", "はなし" }, surfaces);
}

test "solve viterbi" {
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

    const dict = dictionary.Dictionary{
        .index = da,
        .morphs = &[_]dictionary.Morph{
            .{ .left_id = 0, .right_id = 0, .cost = 1 }, // た
            .{ .left_id = 0, .right_id = 0, .cost = 1 }, // たら
            .{ .left_id = 0, .right_id = 0, .cost = 1 }, // なし
            .{ .left_id = 0, .right_id = 0, .cost = 1 }, // なっ
            .{ .left_id = 0, .right_id = 0, .cost = 1 }, // に
            .{ .left_id = 0, .right_id = 0, .cost = 1 }, // になっ
            .{ .left_id = 0, .right_id = 0, .cost = 1 }, // は
            .{ .left_id = 0, .right_id = 0, .cost = 1 }, // はなし
            .{ .left_id = 0, .right_id = 0, .cost = 1 }, // 元気
        },
        .matrix = dictionary.Matrix{
            .row = 9,
            .col = 9,
            .data = &[_]i16{
                0, 0, 0, 0, 0, 0, 0, 0, 0,
                0, 0, 0, 0, 0, 0, 0, 0, 0,
                0, 0, 0, 0, 0, 0, 0, 0, 0,
                0, 0, 0, 0, 0, 0, 0, 0, 0,
                0, 0, 0, 0, 0, 0, 0, 0, 0,
                0, 0, 0, 0, 0, 0, 0, 0, 0,
                0, 0, 0, 0, 0, 0, 0, 0, 0,
                0, 0, 0, 0, 0, 0, 0, 0, 0,
                0, 0, 0, 0, 0, 0, 0, 0, 0,
            },
        },
    };

    var lattice = try Lattice.init(allocator, "はなしたら元気になった");
    defer lattice.deinit(allocator);
    try lattice.build(allocator, dict);

    const path = try lattice.viterbi(allocator, dict);
    defer allocator.free(path);
}
