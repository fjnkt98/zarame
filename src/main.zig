const std = @import("std");
const zarame = @import("zarame");

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    const allocator = gpa.allocator();

    const stdout_file = std.io.getStdOut().writer();
    var bw = std.io.bufferedWriter(stdout_file);
    const stdout = bw.writer();

    try stdout.writeAll("Zarame - Japanese Morphological Analyzer\n");
    try stdout.writeAll("=========================================\n\n");

    // Demonstrate the implemented components
    try demonstrateDoubleArray(allocator, stdout);
    try stdout.writeAll("\n");

    try demonstrateDictionary(allocator, stdout);
    try stdout.writeAll("\n");

    try demonstrateSerialization(allocator, stdout);

    try bw.flush();
}

fn demonstrateDoubleArray(allocator: std.mem.Allocator, writer: anytype) !void {
    try writer.writeAll("1. Double-Array Trie Implementation\n");
    try writer.writeAll("------------------------------------\n");

    const keywords = [_][]const u8{
        "東京",
        "東",
        "京都",
        "大阪",
        "に",
        "行く",
    };

    var da = try zarame.DoubleArray.init(allocator, &keywords);
    defer da.deinit();
    try da.build();

    try writer.print("Built trie with {} keywords\n", .{keywords.len});
    try writer.print("Base array size: {}\n", .{da.base.items.len});
    try writer.print("Check array size: {}\n", .{da.check.items.len});

    // Test search
    const test_word = "東京";
    const result = da.search(test_word);
    if (result) |id| {
        try writer.print("Found '{}' with ID: {}\n", .{ std.zig.fmtEscapes(test_word), id });
    }

    // Test prefix search
    const input = "東京に行く";
    const ids = try da.commonPrefixSearch(input);
    defer allocator.free(ids);
    try writer.print("Common prefix search in '{}': {} matches\n", .{ std.zig.fmtEscapes(input), ids.len });
}

fn demonstrateDictionary(allocator: std.mem.Allocator, writer: anytype) !void {
    try writer.writeAll("2. Dictionary Structures\n");
    try writer.writeAll("-------------------------\n");

    // Create sample dictionary entries
    const features1 = [_][]const u8{ "名詞", "固有名詞", "地名" };
    const features2 = [_][]const u8{ "助詞", "格助詞" };
    const features3 = [_][]const u8{ "動詞", "自立" };

    const entries = [_]zarame.Entry{
        zarame.Entry.init("東京", 1, 1, 3000, &features1),
        zarame.Entry.init("に", 2, 2, 500, &features2),
        zarame.Entry.init("行く", 3, 3, 4000, &features3),
    };

    try writer.print("Created {} dictionary entries\n", .{entries.len});

    for (entries, 0..) |entry, i| {
        try writer.print("  Entry {}: '{}' (left_id={}, right_id={}, cost={})\n", .{
            i,
            std.zig.fmtEscapes(entry.surface),
            entry.left_id,
            entry.right_id,
            entry.cost,
        });
    }

    // Connection costs
    var costs = try zarame.ConnectionCosts.init(allocator, 5, 5);
    defer costs.deinit();

    costs.setCost(1, 2, 100);
    costs.setCost(2, 3, 150);

    try writer.print("\nConnection costs matrix: {}x{}\n", .{ costs.forward_size, costs.backward_size });
    try writer.print("  Cost(1->2): {}\n", .{costs.getCost(1, 2)});
    try writer.print("  Cost(2->3): {}\n", .{costs.getCost(2, 3)});
}

fn demonstrateSerialization(allocator: std.mem.Allocator, writer: anytype) !void {
    try writer.writeAll("3. Binary Serialization\n");
    try writer.writeAll("------------------------\n");

    // Create a small double-array
    const keywords = [_][]const u8{ "東京", "大阪" };
    var da = try zarame.DoubleArray.init(allocator, &keywords);
    defer da.deinit();
    try da.build();

    // Create entries
    const features = [_][]const u8{ "名詞", "固有名詞" };
    const entries = [_]zarame.Entry{
        zarame.Entry.init("東京", 1, 1, 3000, &features),
        zarame.Entry.init("大阪", 1, 1, 3500, &features),
    };

    // Create connection costs
    var costs = try zarame.ConnectionCosts.init(allocator, 3, 3);
    defer costs.deinit();
    costs.setCost(0, 1, 0);
    costs.setCost(1, 0, 0);

    // Serialize to buffer
    var buffer = std.ArrayList(u8).init(allocator);
    defer buffer.deinit();

    try zarame.serialization.serializeDictionary(buffer.writer(), &da, &entries, &costs);

    try writer.print("Serialized dictionary size: {} bytes\n", .{buffer.items.len});
    try writer.writeAll("✓ Ready to embed with @embedFile\n");
    try writer.writeAll("\nImplementation complete! See IMPLEMENTATION_GUIDE.md for details.\n");
}

test "simple test" {
    var list = std.ArrayList(i32).init(std.testing.allocator);
    defer list.deinit();
    try list.append(42);
    try std.testing.expectEqual(@as(i32, 42), list.pop());
}

test "use other module" {
    const zarame_lib = @import("zarame");
    _ = zarame_lib;
}
