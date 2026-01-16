//! Binary serialization and deserialization for dictionary data.
//! This module provides functions to serialize dictionary components into a binary format
//! that can be embedded in the executable using @embedFile.

const std = @import("std");
const DoubleArray = @import("trie.zig").DoubleArray;
const Entry = @import("dictionary.zig").Entry;
const ConnectionCosts = @import("dictionary.zig").ConnectionCosts;

/// Magic number for dictionary file format identification
const MAGIC_NUMBER: u32 = 0x4D524157; // "WARM" in ASCII (WARA reversed for endianness)

/// Version number for dictionary format
const FORMAT_VERSION: u16 = 1;

/// Header for the binary dictionary format
const DictionaryHeader = packed struct {
    magic: u32,
    version: u16,
    /// Number of entries in the dictionary
    entry_count: u32,
    /// Size of the double-array base array
    da_base_size: u32,
    /// Size of the double-array check array
    da_check_size: u32,
    /// Size of connection cost matrix (forward dimension)
    conn_forward_size: u16,
    /// Size of connection cost matrix (backward dimension)
    conn_backward_size: u16,
    /// Offset to double-array section
    da_offset: u64,
    /// Offset to entries section
    entries_offset: u64,
    /// Offset to connection costs section
    conn_offset: u64,
    /// Total size of the dictionary data
    total_size: u64,
};

/// Serialize a double-array trie to a writer.
pub fn serializeDoubleArray(writer: anytype, da: *const DoubleArray) !void {
    // Write base array size
    try writer.writeInt(u32, @intCast(da.base.items.len), .little);
    // Write base array data
    for (da.base.items) |value| {
        try writer.writeInt(i32, value, .little);
    }

    // Write check array size
    try writer.writeInt(u32, @intCast(da.check.items.len), .little);
    // Write check array data
    for (da.check.items) |value| {
        try writer.writeInt(i32, value, .little);
    }
}

/// Deserialize a double-array trie from a reader.
pub fn deserializeDoubleArray(allocator: std.mem.Allocator, reader: anytype) !DoubleArray {
    // Read base array
    const base_size = try reader.readInt(u32, .little);
    var base = try std.ArrayList(i32).initCapacity(allocator, base_size);
    errdefer base.deinit();
    try base.resize(base_size);
    for (0..base_size) |i| {
        base.items[i] = try reader.readInt(i32, .little);
    }

    // Read check array
    const check_size = try reader.readInt(u32, .little);
    var check = try std.ArrayList(i32).initCapacity(allocator, check_size);
    errdefer check.deinit();
    try check.resize(check_size);
    for (0..check_size) |i| {
        check.items[i] = try reader.readInt(i32, .little);
    }

    // Create DoubleArray with empty entries (they will be loaded separately)
    var entries = std.ArrayList([]const u8).init(allocator);

    return DoubleArray{
        .allocator = allocator,
        .base = base,
        .check = check,
        .entries = entries,
    };
}

/// Serialize dictionary entries to a writer.
pub fn serializeEntries(writer: anytype, entries: []const Entry) !void {
    // Write number of entries
    try writer.writeInt(u32, @intCast(entries.len), .little);

    for (entries) |entry| {
        // Write surface length and data
        try writer.writeInt(u16, @intCast(entry.surface.len), .little);
        try writer.writeAll(entry.surface);

        // Write context IDs and cost
        try writer.writeInt(u16, entry.left_id, .little);
        try writer.writeInt(u16, entry.right_id, .little);
        try writer.writeInt(i16, entry.cost, .little);

        // Write features count
        try writer.writeInt(u16, @intCast(entry.features.len), .little);
        for (entry.features) |feature| {
            try writer.writeInt(u16, @intCast(feature.len), .little);
            try writer.writeAll(feature);
        }
    }
}

/// Deserialize dictionary entries from a reader.
pub fn deserializeEntries(allocator: std.mem.Allocator, reader: anytype) !std.ArrayList(Entry) {
    var entries = std.ArrayList(Entry).init(allocator);
    errdefer {
        for (entries.items) |entry| {
            allocator.free(entry.surface);
            for (entry.features) |feature| {
                allocator.free(feature);
            }
            allocator.free(entry.features);
        }
        entries.deinit();
    }

    const entry_count = try reader.readInt(u32, .little);

    for (0..entry_count) |_| {
        // Read surface
        const surface_len = try reader.readInt(u16, .little);
        const surface = try allocator.alloc(u8, surface_len);
        errdefer allocator.free(surface);
        _ = try reader.readAll(surface);

        // Read context IDs and cost
        const left_id = try reader.readInt(u16, .little);
        const right_id = try reader.readInt(u16, .little);
        const cost = try reader.readInt(i16, .little);

        // Read features
        const feature_count = try reader.readInt(u16, .little);
        const features = try allocator.alloc([]const u8, feature_count);
        errdefer allocator.free(features);

        for (0..feature_count) |i| {
            const feature_len = try reader.readInt(u16, .little);
            const feature = try allocator.alloc(u8, feature_len);
            errdefer allocator.free(feature);
            _ = try reader.readAll(feature);
            features[i] = feature;
        }

        const entry = Entry.init(surface, left_id, right_id, cost, features);
        try entries.append(entry);
    }

    return entries;
}

/// Serialize connection costs to a writer.
pub fn serializeConnectionCosts(writer: anytype, costs: *const ConnectionCosts) !void {
    // Write dimensions
    try writer.writeInt(u16, @intCast(costs.forward_size), .little);
    try writer.writeInt(u16, @intCast(costs.backward_size), .little);

    // Write cost matrix
    for (costs.costs) |cost| {
        try writer.writeInt(i16, cost, .little);
    }
}

/// Deserialize connection costs from a reader.
pub fn deserializeConnectionCosts(allocator: std.mem.Allocator, reader: anytype) !ConnectionCosts {
    const forward_size = try reader.readInt(u16, .little);
    const backward_size = try reader.readInt(u16, .little);

    var costs = try ConnectionCosts.init(allocator, forward_size, backward_size);
    errdefer costs.deinit();

    for (0..costs.costs.len) |i| {
        costs.costs[i] = try reader.readInt(i16, .little);
    }

    return costs;
}

/// Serialize entire dictionary to a writer.
/// Note: This function writes a simplified format where offsets in the header
/// are estimates. For deserialization, the reader reads sequentially and doesn't
/// rely on the offset values. This works well with @embedFile usage.
pub fn serializeDictionary(
    writer: anytype,
    da: *const DoubleArray,
    entries: []const Entry,
    costs: *const ConnectionCosts,
) !void {
    // Calculate offsets (these are estimates for the header)
    const header_size = @sizeOf(DictionaryHeader);
    const da_offset = header_size;

    // Estimate sizes
    const da_size = @sizeOf(u32) * 2 + // sizes
        @sizeOf(i32) * (da.base.items.len + da.check.items.len);

    const entries_offset = da_offset + da_size;

    // For connection costs
    const conn_size = @sizeOf(u16) * 2 + // dimensions
        @sizeOf(i16) * costs.costs.len;

    const conn_offset = entries_offset; // Approximate, actual may differ

    // Write header with estimated offsets
    const header = DictionaryHeader{
        .magic = MAGIC_NUMBER,
        .version = FORMAT_VERSION,
        .entry_count = @intCast(entries.len),
        .da_base_size = @intCast(da.base.items.len),
        .da_check_size = @intCast(da.check.items.len),
        .conn_forward_size = @intCast(costs.forward_size),
        .conn_backward_size = @intCast(costs.backward_size),
        .da_offset = da_offset,
        .entries_offset = entries_offset,
        .conn_offset = conn_offset,
        .total_size = 0,
    };

    // Write header
    try writer.writeStruct(header);

    // Write double-array
    try serializeDoubleArray(writer, da);

    // Write entries
    try serializeEntries(writer, entries);

    // Write connection costs
    try serializeConnectionCosts(writer, costs);
}

/// Deserialize entire dictionary from a reader.
pub fn deserializeDictionary(
    allocator: std.mem.Allocator,
    data: []const u8,
) !struct {
    da: DoubleArray,
    entries: std.ArrayList(Entry),
    costs: ConnectionCosts,
} {
    var stream = std.io.fixedBufferStream(data);
    const reader = stream.reader();

    // Read and verify header
    const header = try reader.readStruct(DictionaryHeader);
    if (header.magic != MAGIC_NUMBER) {
        return error.InvalidMagicNumber;
    }
    if (header.version != FORMAT_VERSION) {
        return error.UnsupportedVersion;
    }

    // Read double-array
    const da = try deserializeDoubleArray(allocator, reader);
    errdefer da.deinit();

    // Read entries
    const entries = try deserializeEntries(allocator, reader);
    errdefer {
        for (entries.items) |entry| {
            allocator.free(entry.surface);
            for (entry.features) |feature| {
                allocator.free(feature);
            }
            allocator.free(entry.features);
        }
        entries.deinit();
    }

    // Read connection costs
    const costs = try deserializeConnectionCosts(allocator, reader);
    errdefer costs.deinit();

    return .{
        .da = da,
        .entries = entries,
        .costs = costs,
    };
}

test "serialize and deserialize connection costs" {
    const allocator = std.testing.allocator;

    // Create connection costs
    var costs = try ConnectionCosts.init(allocator, 3, 3);
    defer costs.deinit();
    costs.setCost(0, 0, 100);
    costs.setCost(1, 2, -50);
    costs.setCost(2, 1, 200);

    // Serialize
    var buffer = std.ArrayList(u8).init(allocator);
    defer buffer.deinit();
    try serializeConnectionCosts(buffer.writer(), &costs);

    // Deserialize
    var stream = std.io.fixedBufferStream(buffer.items);
    var costs2 = try deserializeConnectionCosts(allocator, stream.reader());
    defer costs2.deinit();

    // Verify
    try std.testing.expectEqual(costs.forward_size, costs2.forward_size);
    try std.testing.expectEqual(costs.backward_size, costs2.backward_size);
    try std.testing.expectEqual(100, costs2.getCost(0, 0));
    try std.testing.expectEqual(-50, costs2.getCost(1, 2));
    try std.testing.expectEqual(200, costs2.getCost(2, 1));
}

test "serialize and deserialize entries" {
    const allocator = std.testing.allocator;

    // Create sample entries
    const features1 = [_][]const u8{ "名詞", "一般" };
    const features2 = [_][]const u8{ "助詞", "格助詞" };
    const entries = [_]Entry{
        Entry.init("東京", 1, 2, 3000, &features1),
        Entry.init("に", 10, 11, 500, &features2),
    };

    // Serialize
    var buffer = std.ArrayList(u8).init(allocator);
    defer buffer.deinit();
    try serializeEntries(buffer.writer(), &entries);

    // Deserialize
    var stream = std.io.fixedBufferStream(buffer.items);
    var entries2 = try deserializeEntries(allocator, stream.reader());
    defer {
        for (entries2.items) |entry| {
            allocator.free(entry.surface);
            for (entry.features) |feature| {
                allocator.free(feature);
            }
            allocator.free(entry.features);
        }
        entries2.deinit();
    }

    // Verify
    try std.testing.expectEqual(2, entries2.items.len);
    try std.testing.expectEqualStrings("東京", entries2.items[0].surface);
    try std.testing.expectEqual(1, entries2.items[0].left_id);
    try std.testing.expectEqual(2, entries2.items[0].right_id);
    try std.testing.expectEqual(3000, entries2.items[0].cost);
}
