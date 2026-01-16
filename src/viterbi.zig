//! Viterbi algorithm implementation for Japanese morphological analysis.
//! This module implements the Viterbi algorithm to find the optimal morpheme sequence.

const std = @import("std");
const Dictionary = @import("dictionary.zig").Dictionary;
const Entry = @import("dictionary.zig").Entry;
const Lattice = @import("lattice.zig").Lattice;
const LatticeNode = @import("lattice.zig").LatticeNode;

/// Token represents a morpheme in the analysis result.
pub const Token = struct {
    const Self = @This();

    /// Surface form of the token
    surface: []const u8,
    /// Part-of-speech and other features
    features: []const []const u8,
    /// Position in the original text
    start: usize,
    end: usize,

    pub fn init(surface: []const u8, features: []const []const u8, start: usize, end: usize) Self {
        return .{
            .surface = surface,
            .features = features,
            .start = start,
            .end = end,
        };
    }
};

/// Viterbi algorithm implementation for morphological analysis.
pub fn viterbi(allocator: std.mem.Allocator, lattice: *Lattice, dictionary: *const Dictionary) ![]Token {
    // Forward pass: calculate minimum costs
    try forwardPass(lattice, dictionary);

    // Backward pass: backtrack to find optimal path
    const path = try backwardPass(allocator, lattice);
    defer allocator.free(path);

    // Convert path to tokens
    return try pathToTokens(allocator, lattice, path, dictionary);
}

/// Forward pass: calculate minimum cumulative cost for each node using dynamic programming.
fn forwardPass(lattice: *Lattice, dictionary: *const Dictionary) !void {
    // Iterate through each position in the lattice
    for (0..lattice.nodes.items.len) |pos| {
        // For each node at this position
        for (lattice.nodes.items[pos].items, 0..) |*current_node, current_idx| {
            // Find the best previous node
            if (current_node.start == 0) {
                // This is BOS or starts from beginning, already initialized
                continue;
            }

            // Look at all nodes that could connect to this one
            const prev_nodes = lattice.nodes.items[current_node.start].items;
            for (prev_nodes, 0..) |prev_node, prev_idx| {
                // Calculate cost: previous cumulative cost + connection cost + word cost
                const connection_cost = dictionary.connection_costs.getCost(
                    prev_node.right_id,
                    current_node.left_id,
                );
                const total_cost = prev_node.min_cost + connection_cost + current_node.word_cost;

                // Update if this path is better
                if (total_cost < current_node.min_cost) {
                    current_node.min_cost = total_cost;
                    // Store the index of the previous node within its position
                    // The position itself is current_node.start
                    current_node.prev_node = prev_idx;
                }
            }

            // Update the node in the lattice (nodes are mutable through the pointer)
            _ = current_idx; // Keep for future use if needed
        }
    }
}

/// Backward pass: backtrack from EOS to BOS to find the optimal path.
fn backwardPass(allocator: std.mem.Allocator, lattice: *Lattice) ![]usize {
    var path = std.ArrayList(usize).init(allocator);
    errdefer path.deinit();

    // Start from EOS node
    if (lattice.eos_node_idx == null) {
        return error.NoEOSNode;
    }

    // Get EOS node
    const eos_nodes = lattice.getNodesAt(lattice.text.len);
    if (eos_nodes.len == 0) {
        return error.NoEOSNode;
    }

    // Find the EOS node (should be the one with minimum cost, or the last one)
    var current_pos = lattice.text.len;
    var current_node_idx = lattice.eos_node_idx.?;

    // Backtrack through the lattice
    while (current_pos > 0) {
        const nodes_at_pos = lattice.getNodesAt(current_pos);
        if (current_node_idx >= nodes_at_pos.len) {
            return error.InvalidNodeIndex;
        }

        const current_node = nodes_at_pos[current_node_idx];

        // Add current node to path (we'll reverse later)
        try path.append(current_pos);
        try path.append(current_node_idx);

        // Move to previous node
        if (current_node.prev_node == null) {
            break;
        }

        current_node_idx = current_node.prev_node.?;
        current_pos = current_node.start;
    }

    // Reverse the path to get BOS -> EOS order
    std.mem.reverse(usize, path.items);

    return try path.toOwnedSlice();
}

/// Convert the optimal path to a sequence of tokens.
fn pathToTokens(
    allocator: std.mem.Allocator,
    lattice: *Lattice,
    path: []const usize,
    dictionary: *const Dictionary,
) ![]Token {
    var tokens = std.ArrayList(Token).init(allocator);
    errdefer tokens.deinit();

    // Path contains pairs of (position, node_index)
    var i: usize = 0;
    while (i < path.len) : (i += 2) {
        if (i + 1 >= path.len) break;

        const pos = path[i];
        const node_idx = path[i + 1];

        const nodes_at_pos = lattice.getNodesAt(pos);
        if (node_idx >= nodes_at_pos.len) {
            continue;
        }

        const node = nodes_at_pos[node_idx];

        // Skip BOS and EOS nodes (encoded as entry_id == -1)
        if (node.entry_id == -1) {
            continue;
        }

        // Get the dictionary entry
        const entry = dictionary.getEntry(node.entry_id);
        if (entry == null) {
            continue;
        }

        const surface = lattice.getNodeSurface(&node);
        const token = Token.init(surface, entry.?.features, node.start, node.end);
        try tokens.append(token);
    }

    return try tokens.toOwnedSlice();
}

test "viterbi basic" {
    // This is a placeholder test to ensure the module compiles
    // Real tests would require a full dictionary setup
    const allocator = std.testing.allocator;
    _ = allocator;
}

/// Build lattice from input text using the dictionary.
pub fn buildLattice(
    allocator: std.mem.Allocator,
    text: []const u8,
    dictionary: *const Dictionary,
) !Lattice {
    var lattice = try Lattice.init(allocator, text);
    errdefer lattice.deinit();

    // For each position in the text
    var pos: usize = 0;
    while (pos < text.len) {
        // Try to match dictionary entries starting at this position
        const remaining = text[pos..];
        const entry_ids = try dictionary.lookup(remaining);
        errdefer allocator.free(entry_ids);

        // Add nodes for each matching entry
        for (entry_ids) |entry_id| {
            const entry = dictionary.getEntry(entry_id);
            if (entry == null) continue;

            const e = entry.?;
            const end_pos = pos + e.surface.len;
            if (end_pos > text.len) continue;

            const node = LatticeNode.init(
                pos,
                end_pos,
                entry_id,
                e.left_id,
                e.right_id,
                e.cost,
            );
            try lattice.addNode(node);
        }

        allocator.free(entry_ids);

        // Move to next byte position
        // In a real implementation, you'd want to move by character boundary
        pos += 1;
    }

    // Add EOS node
    _ = try lattice.addEOS();

    return lattice;
}

test "build simple lattice" {
    const allocator = std.testing.allocator;
    _ = allocator;
    // Placeholder test - real implementation would need a dictionary
}
