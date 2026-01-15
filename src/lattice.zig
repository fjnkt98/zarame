//! Lattice structure for Viterbi algorithm in morphological analysis.
//! The lattice represents all possible morpheme sequences for an input text.

const std = @import("std");
const Dictionary = @import("dictionary.zig").Dictionary;
const Entry = @import("dictionary.zig").Entry;

/// A node in the lattice representing a morpheme candidate at a specific position.
pub const LatticeNode = struct {
    const Self = @This();

    /// Start position in the input text (in bytes)
    start: usize,
    /// End position in the input text (in bytes)
    end: usize,
    /// Dictionary entry ID (-1 for BOS/EOS, -2 for unknown word)
    entry_id: i32,
    /// Left context ID for connection cost calculation
    left_id: u16,
    /// Right context ID for connection cost calculation
    right_id: u16,
    /// Word cost from dictionary
    word_cost: i32,
    /// Minimum cumulative cost to reach this node (for Viterbi)
    min_cost: i32,
    /// Index of the previous node in the optimal path
    prev_node: ?usize,

    pub fn init(start: usize, end: usize, entry_id: i32, left_id: u16, right_id: u16, word_cost: i32) Self {
        return .{
            .start = start,
            .end = end,
            .entry_id = entry_id,
            .left_id = left_id,
            .right_id = right_id,
            .word_cost = word_cost,
            .min_cost = std.math.maxInt(i32),
            .prev_node = null,
        };
    }

    /// Create a BOS (Beginning of Sentence) node.
    pub fn createBOS() Self {
        return .{
            .start = 0,
            .end = 0,
            .entry_id = -1,
            .left_id = 0,
            .right_id = 0,
            .word_cost = 0,
            .min_cost = 0,
            .prev_node = null,
        };
    }

    /// Create an EOS (End of Sentence) node.
    pub fn createEOS(pos: usize) Self {
        return .{
            .start = pos,
            .end = pos,
            .entry_id = -1,
            .left_id = 0,
            .right_id = 0,
            .word_cost = 0,
            .min_cost = std.math.maxInt(i32),
            .prev_node = null,
        };
    }
};

/// Lattice structure for morphological analysis using Viterbi algorithm.
pub const Lattice = struct {
    const Self = @This();

    allocator: std.mem.Allocator,
    /// All nodes in the lattice, organized by end position
    nodes: std.ArrayList(std.ArrayList(LatticeNode)),
    /// The input text being analyzed
    text: []const u8,
    /// Index of the BOS node (always at position 0)
    bos_node_idx: usize,
    /// Index of the EOS node
    eos_node_idx: ?usize,

    pub fn init(allocator: std.mem.Allocator, text: []const u8) !Self {
        var nodes = std.ArrayList(std.ArrayList(LatticeNode)).init(allocator);
        // Create nodes array with text.len + 1 positions (for each character boundary + EOS)
        try nodes.resize(text.len + 1);
        for (0..text.len + 1) |i| {
            nodes.items[i] = std.ArrayList(LatticeNode).init(allocator);
        }

        // Add BOS node at position 0
        var bos = LatticeNode.createBOS();
        try nodes.items[0].append(bos);

        return .{
            .allocator = allocator,
            .nodes = nodes,
            .text = text,
            .bos_node_idx = 0,
            .eos_node_idx = null,
        };
    }

    pub fn deinit(self: Self) void {
        for (self.nodes.items) |node_list| {
            node_list.deinit();
        }
        self.nodes.deinit();
    }

    /// Add a node to the lattice at the specified end position.
    pub fn addNode(self: *Self, node: LatticeNode) !void {
        if (node.end > self.text.len) {
            return error.InvalidPosition;
        }
        try self.nodes.items[node.end].append(node);
    }

    /// Add EOS node and return its index.
    pub fn addEOS(self: *Self) !usize {
        const eos = LatticeNode.createEOS(self.text.len);
        try self.nodes.items[self.text.len].append(eos);
        self.eos_node_idx = self.nodes.items[self.text.len].items.len - 1;
        return self.eos_node_idx.?;
    }

    /// Get all nodes at a specific position.
    pub fn getNodesAt(self: Self, pos: usize) []LatticeNode {
        if (pos > self.text.len) {
            return &[_]LatticeNode{};
        }
        return self.nodes.items[pos].items;
    }

    /// Get the substring from the text for a given node.
    pub fn getNodeSurface(self: Self, node: *const LatticeNode) []const u8 {
        if (node.start >= node.end or node.end > self.text.len) {
            return "";
        }
        return self.text[node.start..node.end];
    }
};

test "create lattice" {
    const allocator = std.testing.allocator;
    const text = "東京に行く";

    var lattice = try Lattice.init(allocator, text);
    defer lattice.deinit();

    // BOS should be at position 0
    const bos_nodes = lattice.getNodesAt(0);
    try std.testing.expectEqual(1, bos_nodes.len);
    try std.testing.expectEqual(-1, bos_nodes[0].entry_id);
    try std.testing.expectEqual(0, bos_nodes[0].min_cost);
}

test "add nodes to lattice" {
    const allocator = std.testing.allocator;
    const text = "東京";

    var lattice = try Lattice.init(allocator, text);
    defer lattice.deinit();

    // Add a node for "東" (0-3 bytes in UTF-8)
    const node1 = LatticeNode.init(0, 3, 0, 100, 200, 5000);
    try lattice.addNode(node1);

    // Add a node for "東京" (0-6 bytes in UTF-8)
    const node2 = LatticeNode.init(0, 6, 1, 101, 201, 3000);
    try lattice.addNode(node2);

    // Check nodes at position 3
    const nodes_at_3 = lattice.getNodesAt(3);
    try std.testing.expectEqual(1, nodes_at_3.len);
    try std.testing.expectEqual(0, nodes_at_3[0].entry_id);

    // Check nodes at position 6
    const nodes_at_6 = lattice.getNodesAt(6);
    try std.testing.expectEqual(1, nodes_at_6.len);
    try std.testing.expectEqual(1, nodes_at_6[0].entry_id);
}

test "add EOS node" {
    const allocator = std.testing.allocator;
    const text = "東京";

    var lattice = try Lattice.init(allocator, text);
    defer lattice.deinit();

    const eos_idx = try lattice.addEOS();
    try std.testing.expect(eos_idx >= 0);

    const eos_nodes = lattice.getNodesAt(text.len);
    try std.testing.expectEqual(1, eos_nodes.len);
    try std.testing.expectEqual(-1, eos_nodes[0].entry_id);
    try std.testing.expectEqual(std.math.maxInt(i32), eos_nodes[0].min_cost);
}

test "get node surface" {
    const allocator = std.testing.allocator;
    const text = "東京に行く";

    var lattice = try Lattice.init(allocator, text);
    defer lattice.deinit();

    // "東京" is bytes 0-6
    const node = LatticeNode.init(0, 6, 0, 100, 200, 3000);
    const surface = lattice.getNodeSurface(&node);
    try std.testing.expectEqualStrings("東京", surface);
}
