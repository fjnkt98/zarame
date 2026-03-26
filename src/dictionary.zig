//! Dictionary structures for Japanese morphological analysis.
//! This module defines data structures for dictionary entries, connection costs,
//! and provides the unified Dictionary interface.

const std = @import("std");
const trie = @import("trie.zig");

/// Dictionary entry representing a word in the dictionary.
/// Each entry corresponds to a morpheme with its linguistic features.
pub const Entry = struct {
    const Self = @This();

    /// The surface form (表層形) - the actual text representation
    surface: []const u8,
    /// Left context ID for connection cost lookup
    left_id: u16,
    /// Right context ID for connection cost lookup
    right_id: u16,
    /// Word cost (単語コスト) - lower is better
    cost: i16,
    /// Part-of-speech tags and other features
    features: []const []const u8,

    pub fn init(surface: []const u8, left_id: u16, right_id: u16, cost: i16, features: []const []const u8) Self {
        return .{
            .surface = surface,
            .left_id = left_id,
            .right_id = right_id,
            .cost = cost,
            .features = features,
        };
    }
};

/// Connection cost matrix for calculating costs between morphemes.
/// In Japanese morphological analysis, the cost of connecting two morphemes
/// depends on their context IDs (left_id of the right morpheme and right_id of the left morpheme).
pub const ConnectionCosts = struct {
    const Self = @This();

    allocator: std.mem.Allocator,
    /// The cost matrix stored as a flat array
    /// cost[right_id * forward_size + left_id] gives the connection cost
    costs: []i16,
    /// Size of forward (left) context dimension
    forward_size: usize,
    /// Size of backward (right) context dimension
    backward_size: usize,

    pub fn init(allocator: std.mem.Allocator, forward_size: usize, backward_size: usize) !Self {
        const total_size = forward_size * backward_size;
        const costs = try allocator.alloc(i16, total_size);
        @memset(costs, 0);

        return .{
            .allocator = allocator,
            .costs = costs,
            .forward_size = forward_size,
            .backward_size = backward_size,
        };
    }

    pub fn deinit(self: *Self) void {
        self.allocator.free(self.costs);
    }

    /// Get the connection cost between two morphemes.
    /// right_id: the right context ID of the left morpheme
    /// left_id: the left context ID of the right morpheme
    pub fn getCost(self: Self, right_id: u16, left_id: u16) i16 {
        const index = @as(usize, right_id) * self.forward_size + @as(usize, left_id);
        return self.costs[index];
    }

    /// Set the connection cost between two morphemes.
    pub fn setCost(self: *Self, right_id: u16, left_id: u16, cost: i16) void {
        const index = @as(usize, right_id) * self.forward_size + @as(usize, left_id);
        self.costs[index] = cost;
    }
};

/// Dictionary holds all the data needed for morphological analysis.
pub const Dictionary = struct {
    const Self = @This();

    allocator: std.mem.Allocator,
    /// The double-array trie for efficient prefix matching
    da: trie.DoubleArray,
    /// All dictionary entries indexed by entry ID
    entries: std.ArrayList(Entry),
    /// Connection cost matrix
    connection_costs: ConnectionCosts,

    pub fn init(
        allocator: std.mem.Allocator,
        da: trie.DoubleArray,
        entries: std.ArrayList(Entry),
        connection_costs: ConnectionCosts,
    ) Self {
        return .{
            .allocator = allocator,
            .da = da,
            .entries = entries,
            .connection_costs = connection_costs,
        };
    }

    /// Release resources owned by the dictionary.
    /// Note: This frees the connection_costs and the entries ArrayList structure,
    /// but does NOT free:
    /// - The DoubleArray trie (passed as a const pointer, owned by caller)
    /// - The entry data within entries (surface and features slices are owned by caller)
    /// Caller must manage the lifetime of the trie and entry data separately.
    pub fn deinit(self: *Self) void {
        self.connection_costs.deinit();
        // entries ArrayList is freed, but not the entry data itself
        self.entries.deinit(self.allocator);
    }

    /// Lookup entries that match the given prefix.
    pub fn lookup(self: Self, text: []const u8) ![]i32 {
        return try self.da.commonPrefixSearch(text);
    }

    /// Get an entry by its ID.
    pub fn getEntry(self: Self, id: i32) ?*const Entry {
        if (id < 0 or id >= self.entries.items.len) {
            return null;
        }
        return &self.entries.items[@intCast(id)];
    }
};

test "create entry" {
    const allocator = std.testing.allocator;
    const features = [_][]const u8{ "名詞", "一般", "*", "*" };
    const entry = Entry.init("東京", 1, 2, 3000, &features);

    try std.testing.expectEqualStrings("東京", entry.surface);
    try std.testing.expectEqual(1, entry.left_id);
    try std.testing.expectEqual(2, entry.right_id);
    try std.testing.expectEqual(3000, entry.cost);
    try std.testing.expectEqual(4, entry.features.len);

    _ = allocator;
}

test "connection costs" {
    const allocator = std.testing.allocator;
    var costs = try ConnectionCosts.init(allocator, 10, 10);
    defer costs.deinit();

    try std.testing.expectEqual(0, costs.getCost(0, 0));

    costs.setCost(5, 3, 1234);
    try std.testing.expectEqual(1234, costs.getCost(5, 3));

    costs.setCost(9, 9, -500);
    try std.testing.expectEqual(-500, costs.getCost(9, 9));
}

test "dictionary basic" {
    const allocator = std.testing.allocator;

    // Create a simple trie
    const keywords = [_][]const u8{"東京"};
    var da = try trie.DoubleArray.init(allocator, &keywords);
    defer da.deinit();
    try da.build();

    // Create entries
    var entries = std.ArrayList(Entry).empty;

    const features = [_][]const u8{ "名詞", "固有名詞" };
    try entries.append(allocator, Entry.init("東京", 1, 1, 3000, &features));

    // Create connection costs
    const costs = try ConnectionCosts.init(allocator, 5, 5);

    // Create dictionary
    var dict = Dictionary.init(allocator, da, entries, costs);
    defer dict.deinit();

    // Test lookup
    const ids = try dict.lookup("東京");
    defer allocator.free(ids);
    try std.testing.expectEqual(1, ids.len);

    // Test getEntry
    const entry = dict.getEntry(0);
    try std.testing.expect(entry != null);
    try std.testing.expectEqualStrings("東京", entry.?.surface);
}
