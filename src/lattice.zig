//! Lattice data structure for morphological analysis.

const std = @import("std");
const trie = @import("trie.zig");
const dictionary = @import("dictionary.zig");

const MAXIMUM_COST = 1 << 31 - 1;

pub const NodeClass = enum {
    bos,
    eos,
    known,
    unknown,
    // user, TODO: user defined token support
};

pub const Node = struct {
    id: usize, // id in the dictionary
    class: NodeClass,
    surface: []const u8,
    byte_position: usize, // byte position in the input string
    char_position: usize, // character position in the input string
    left_id: usize,
    right_id: usize,
    cost: i32, // emission cost of this node
    min_cost: i32, // minimum cost to reach this node, for use in viterbi algorithm
    min_prev: ?*Node, // previous node in the best path, for use in viterbi algorithm
};

/// Counts the number of UTF-8 characters in the given string.
fn utf8_char_count(s: []const u8) !usize {
    var count: usize = 0;
    var view = try std.unicode.Utf8View.init(s);
    var iter = view.iterator();
    while (iter.nextCodepoint()) |_| {
        count += 1;
    }
    return count;
}

/// Implementation of the Lattice data structure.
pub const Lattice = struct {
    const Self = @This();

    input: []const u8,
    // nodes[i] stores the list of nodes that end at position `i`.
    // Here, "a node ends at position `i`" means that the last character of its surface is the (i - 1)-th character of the input string.
    // `i` is a character position of the input string, not a byte position. The first character is at position 0.
    nodes: []std.ArrayList(Node),
    dict: dictionary.Dictionary,

    /// Initializes the lattice with the given input string.
    ///
    /// You should de-initialize with `deinit()`.
    pub fn init(allocator: std.mem.Allocator, input: []const u8, dict: dictionary.Dictionary) !Self {
        const char_count = try utf8_char_count(input);

        const nodes = try allocator.alloc(std.ArrayList(Node), char_count + 2);
        errdefer allocator.free(nodes);
        @memset(nodes, std.ArrayList(Node).empty);

        return Self{
            .input = input,
            .nodes = nodes,
            .dict = dict,
        };
    }

    /// Releases all allocated memory.
    pub fn deinit(self: *Self, allocator: std.mem.Allocator) void {
        for (self.nodes) |*nodes| {
            nodes.*.deinit(allocator);
        }
        allocator.free(self.nodes);
    }

    /// Inserts a node into the lattice.
    fn insert(
        self: *Self,
        allocator: std.mem.Allocator,
        byte_position: usize,
        char_position: usize,
        id: usize,
        class: NodeClass,
        surface: []const u8,
    ) !void {
        const morph = switch (class) {
            .known => self.dict.morphs[id],
            .unknown => dictionary.Morph{
                .left_id = 0,
                .right_id = 0,
                .cost = 100, // TODO: cost calculation for unknown words
            },
            .bos => dictionary.Morph.zero,
            .eos => dictionary.Morph.zero,
        };
        const node = Node{
            .id = id,
            .class = class,
            .surface = surface,
            .byte_position = byte_position,
            .char_position = char_position,
            .left_id = morph.left_id,
            .right_id = morph.right_id,
            .cost = morph.cost,
            .min_cost = morph.cost,
            .min_prev = null,
        };
        const index = switch (class) {
            .bos => char_position,
            .eos => char_position + 1,
            else => char_position + try utf8_char_count(surface),
        };
        try self.nodes[index].append(allocator, node);
    }

    /// Builds the lattice.
    pub fn build(self: *Self, allocator: std.mem.Allocator) !void {
        var i: usize = 0; // byte position
        var c: usize = 0; // char position

        // add BOS node
        try self.insert(allocator, 0, 0, 0, .bos, "BOS");
        // add EOS node
        try self.insert(allocator, self.input.len, self.nodes.len - 2, 0, .eos, "EOS");

        while (i < self.input.len) {
            const size = try std.unicode.utf8ByteSequenceLength(self.input[i]);

            const results = try self.dict.index.commonPrefixSearch(allocator, self.input[i..]);
            defer allocator.free(results);

            var has_single_word = false;
            for (results) |result| {
                try self.insert(allocator, i, c, result.id, .known, self.input[i .. i + result.length]);

                if (!has_single_word and size == result.length) {
                    has_single_word = true;
                }
            }

            // If there is no single word that matches the input, we need to add the single character as a dummy node to prevent breaking the path.
            if (!has_single_word) {
                try self.insert(allocator, i, c, 0, .unknown, self.input[i .. i + size]);
            }

            i += size;
            c += 1;
        }
    }

    /// Solves the lattice using the Viterbi algorithm and returns the best path.
    ///
    /// The surfaces of the returned nodes are slices of the input string, so they have the same lifetime as the input string.
    ///
    /// You should de-initialize the returned path with `allocator.free()`.
    pub fn viterbi(self: *Self, allocator: std.mem.Allocator) ![]Node {
        // initialize BOS node cost
        self.nodes[0].items[0].min_cost = 0;

        // Forwarding
        for (1..self.nodes.len) |i| {
            const current_nodes = self.nodes[i].items;
            for (current_nodes) |*current_node| {
                const prev_nodes = self.nodes[current_node.*.char_position].items;
                if (prev_nodes.len == 0) {
                    current_node.*.min_cost = MAXIMUM_COST;
                    continue;
                }

                for (0.., prev_nodes) |j, *prev_node| {
                    var transition_cost: i16 = 0;
                    if (current_node.*.class == .known and prev_node.*.class == .known) {
                        transition_cost += self.dict.matrix.get(prev_node.*.right_id, current_node.*.left_id);
                    }
                    const emission_cost = current_node.*.cost;
                    const cost = prev_node.*.min_cost + emission_cost + transition_cost;
                    if (j == 0 or cost < current_node.*.min_cost) {
                        current_node.*.min_cost = cost;
                        current_node.*.min_prev = prev_node;
                    }
                }
            }
        }

        // Get the best path by backtracking from the EOS node.
        var path = std.ArrayList(Node).empty;
        var node: ?*Node = &self.nodes[self.nodes.len - 1].items[0]; // start from EOS node
        while (node != null) {
            try path.append(allocator, node.?.*);
            node = node.?.min_prev;
        }
        std.mem.reverse(Node, path.items);
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
    var index = try dictionary.Index.init(allocator, &words);
    defer index.deinit(allocator);

    var morphs = [_]dictionary.Morph{
        .{ .left_id = 0, .right_id = 1, .cost = 1 }, // た
        .{ .left_id = 0, .right_id = 0, .cost = 1 }, // たら
        .{ .left_id = 0, .right_id = 0, .cost = 1 }, // なし
        .{ .left_id = 0, .right_id = 0, .cost = 1 }, // なっ
        .{ .left_id = 0, .right_id = 0, .cost = 1 }, // に
        .{ .left_id = 0, .right_id = 0, .cost = 1 }, // になっ
        .{ .left_id = 0, .right_id = 0, .cost = 1 }, // は
        .{ .left_id = 0, .right_id = 0, .cost = 1 }, // はなし
        .{ .left_id = 0, .right_id = 0, .cost = 1 }, // 元気
    };
    var pos_table = try dictionary.PosTable.init(allocator, 9);
    defer pos_table.deinit(allocator);

    var matrix = try dictionary.ConnectionMatrix.init(allocator, 9, 9);
    defer matrix.deinit(allocator);

    const dict = dictionary.Dictionary{
        .index = index,
        .morphs = &morphs,
        .pos_table = pos_table,
        .matrix = matrix,
    };

    var lattice = try Lattice.init(allocator, "はなしたら元気になった", dict);
    defer lattice.deinit(allocator);
    try lattice.build(allocator);

    try std.testing.expectEqual(13, lattice.nodes.len);

    try std.testing.expectEqual(lattice.nodes[0].items.len, 1);
    try std.testing.expectEqual(lattice.nodes[0].items[0].surface, "BOS");

    try std.testing.expectEqual(lattice.nodes[12].items.len, 1);
    try std.testing.expectEqual(lattice.nodes[12].items[0].surface, "EOS");
    try std.testing.expectEqual(lattice.nodes[12].items[0].char_position, 11);
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
    var index = try dictionary.Index.init(allocator, &words);
    defer index.deinit(allocator);

    var morphs = [_]dictionary.Morph{
        .{ .left_id = 0, .right_id = 1, .cost = 1 }, // た
        .{ .left_id = 0, .right_id = 0, .cost = 1 }, // たら
        .{ .left_id = 0, .right_id = 0, .cost = 1 }, // なし
        .{ .left_id = 0, .right_id = 0, .cost = 1 }, // なっ
        .{ .left_id = 0, .right_id = 0, .cost = 1 }, // に
        .{ .left_id = 0, .right_id = 0, .cost = 1 }, // になっ
        .{ .left_id = 0, .right_id = 0, .cost = 1 }, // は
        .{ .left_id = 0, .right_id = 0, .cost = 1 }, // はなし
        .{ .left_id = 0, .right_id = 0, .cost = 1 }, // 元気
    };
    var pos_table = try dictionary.PosTable.init(allocator, 9);
    defer pos_table.deinit(allocator);

    var matrix = try dictionary.ConnectionMatrix.init(allocator, 9, 9);
    defer matrix.deinit(allocator);

    const dict = dictionary.Dictionary{
        .index = index,
        .morphs = &morphs,
        .pos_table = pos_table,
        .matrix = matrix,
    };

    const input = "はなしたら元気になった";
    var lattice = try Lattice.init(allocator, input, dict);
    defer lattice.deinit(allocator);
    try lattice.build(allocator);

    const path = try lattice.viterbi(allocator);
    defer allocator.free(path);

    var surfaces = std.ArrayList([]const u8).empty;
    defer surfaces.deinit(allocator);
    for (path) |node| {
        if (node.class != .bos and node.class != .eos) {
            try surfaces.append(allocator, node.surface);
        }
    }
    const actual = try std.mem.concat(allocator, u8, surfaces.items);
    defer allocator.free(actual);
    try std.testing.expectEqualStrings(input, actual);
}
