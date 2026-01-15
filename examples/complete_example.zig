//! Example: Building and using an embedded dictionary
//! This demonstrates the complete workflow from building a dictionary to using it for tokenization.

const std = @import("std");
const zarame = @import("zarame");

/// Example of building a dictionary from scratch
pub fn buildExampleDictionary(allocator: std.mem.Allocator) !void {
    std.debug.print("=== Building Example Dictionary ===\n", .{});

    // 1. Define surface forms (keywords)
    const keywords = [_][]const u8{
        "東京",
        "に",
        "行く",
        "東",
        "京",
        "大阪",
        "から",
    };

    std.debug.print("1. Building double-array trie with {} keywords...\n", .{keywords.len});

    // 2. Build double-array trie
    var da = try zarame.DoubleArray.init(allocator, &keywords);
    defer da.deinit();
    try da.build();

    std.debug.print("   ✓ Trie built: base={}, check={}\n", .{ da.base.items.len, da.check.items.len });

    // 3. Create dictionary entries with linguistic features
    std.debug.print("2. Creating dictionary entries...\n", .{});

    var entries = std.ArrayList(zarame.Entry).init(allocator);
    defer {
        for (entries.items) |entry| {
            allocator.free(entry.features);
        }
        entries.deinit();
    }

    // Entry 0: 東京 (Tokyo)
    const features_tokyo = try allocator.dupe([]const u8, &[_][]const u8{
        "名詞",
        "固有名詞",
        "地名",
        "一般",
    });
    try entries.append(zarame.Entry.init("東京", 1, 1, 3000, features_tokyo));

    // Entry 1: に (particle "to")
    const features_ni = try allocator.dupe([]const u8, &[_][]const u8{
        "助詞",
        "格助詞",
        "一般",
        "*",
    });
    try entries.append(zarame.Entry.init("に", 2, 2, 500, features_ni));

    // Entry 2: 行く (to go)
    const features_iku = try allocator.dupe([]const u8, &[_][]const u8{
        "動詞",
        "自立",
        "*",
        "*",
    });
    try entries.append(zarame.Entry.init("行く", 3, 3, 4000, features_iku));

    // Entry 3: 東 (east)
    const features_higashi = try allocator.dupe([]const u8, &[_][]const u8{
        "名詞",
        "一般",
        "*",
        "*",
    });
    try entries.append(zarame.Entry.init("東", 1, 1, 5000, features_higashi));

    // Entry 4: 京 (capital)
    const features_kyo = try allocator.dupe([]const u8, &[_][]const u8{
        "名詞",
        "一般",
        "*",
        "*",
    });
    try entries.append(zarame.Entry.init("京", 1, 1, 5000, features_kyo));

    // Entry 5: 大阪 (Osaka)
    const features_osaka = try allocator.dupe([]const u8, &[_][]const u8{
        "名詞",
        "固有名詞",
        "地名",
        "一般",
    });
    try entries.append(zarame.Entry.init("大阪", 1, 1, 3200, features_osaka));

    // Entry 6: から (from)
    const features_kara = try allocator.dupe([]const u8, &[_][]const u8{
        "助詞",
        "格助詞",
        "一般",
        "*",
    });
    try entries.append(zarame.Entry.init("から", 2, 2, 520, features_kara));

    std.debug.print("   ✓ Created {} entries\n", .{entries.items.len});

    // 4. Create connection costs matrix
    std.debug.print("3. Creating connection costs matrix...\n", .{});

    var costs = try zarame.ConnectionCosts.init(allocator, 10, 10);
    defer costs.deinit();

    // Set connection costs (lower = better connection)
    costs.setCost(0, 1, 0); // BOS -> noun: free
    costs.setCost(0, 2, 0); // BOS -> particle: free
    costs.setCost(0, 3, 0); // BOS -> verb: free
    costs.setCost(1, 2, 100); // noun -> particle: good
    costs.setCost(2, 3, 100); // particle -> verb: good
    costs.setCost(1, 1, 500); // noun -> noun: less preferred
    costs.setCost(1, 3, 300); // noun -> verb: ok
    costs.setCost(3, 0, 0); // verb -> EOS: free
    costs.setCost(2, 1, 200); // particle -> noun: ok

    std.debug.print("   ✓ Connection costs configured\n", .{});

    // 5. Serialize to binary file
    std.debug.print("4. Serializing to binary file...\n", .{});

    const file = try std.fs.cwd().createFile("example_dict.bin", .{});
    defer file.close();

    try zarame.serialization.serializeDictionary(
        file.writer(),
        &da,
        entries.items,
        &costs,
    );

    const file_size = try file.getEndPos();
    std.debug.print("   ✓ Dictionary saved: example_dict.bin ({} bytes)\n", .{file_size});
    std.debug.print("\n✓ Dictionary building complete!\n", .{});
    std.debug.print("  You can now use @embedFile(\"example_dict.bin\") to embed it.\n\n", .{});
}

/// Example of loading and using an embedded dictionary
pub fn useEmbeddedDictionary(allocator: std.mem.Allocator, dict_data: []const u8) !void {
    std.debug.print("=== Using Embedded Dictionary ===\n", .{});

    // 1. Deserialize dictionary from binary data
    std.debug.print("1. Loading dictionary from binary...\n", .{});

    const result = try zarame.serialization.deserializeDictionary(allocator, dict_data);

    std.debug.print("   ✓ Loaded {} entries\n", .{result.entries.items.len});
    std.debug.print("   ✓ Trie size: base={}, check={}\n", .{
        result.da.base.items.len,
        result.da.check.items.len,
    });
    std.debug.print("   ✓ Connection costs: {}x{}\n", .{
        result.costs.forward_size,
        result.costs.backward_size,
    });

    // 2. Create dictionary and tokenizer
    std.debug.print("2. Creating tokenizer...\n", .{});

    const dict = zarame.Dictionary.init(
        allocator,
        &result.da,
        result.entries,
        result.costs,
    );

    var tokenizer = zarame.Tokenizer.init(allocator, dict);
    defer {
        // Clean up
        tokenizer.deinit();
        result.da.deinit();
        for (result.entries.items) |entry| {
            allocator.free(entry.surface);
            for (entry.features) |feature| {
                allocator.free(feature);
            }
            allocator.free(entry.features);
        }
    }

    // 3. Tokenize example texts
    std.debug.print("\n3. Tokenizing example texts:\n", .{});

    const test_texts = [_][]const u8{
        "東京に行く",
        "大阪から東京",
    };

    for (test_texts, 0..) |text, i| {
        std.debug.print("\n   Example {}:\n", .{i + 1});
        std.debug.print("   Input:  {s}\n", .{text});
        std.debug.print("   Tokens:\n", .{});

        const tokens = try tokenizer.tokenize(text);
        defer allocator.free(tokens);

        for (tokens, 0..) |token, j| {
            std.debug.print("     {}: {s}\t", .{ j, token.surface });
            for (token.features, 0..) |feature, k| {
                if (k > 0) std.debug.print(",", .{});
                std.debug.print("{s}", .{feature});
            }
            std.debug.print("\n", .{});
        }
    }

    std.debug.print("\n✓ Tokenization complete!\n\n", .{});
}

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    const allocator = gpa.allocator();

    std.debug.print("\n", .{});
    std.debug.print("╔════════════════════════════════════════════════════════╗\n", .{});
    std.debug.print("║  Zarame - Japanese Morphological Analyzer Demo        ║\n", .{});
    std.debug.print("║  Complete Example: Build → Serialize → Embed → Use    ║\n", .{});
    std.debug.print("╚════════════════════════════════════════════════════════╝\n", .{});
    std.debug.print("\n", .{});

    // Part 1: Build a dictionary and save it to a binary file
    try buildExampleDictionary(allocator);

    // Part 2: Load the dictionary and use it
    // In a real application, you would use @embedFile here:
    //   const dict_data = @embedFile("example_dict.bin");
    // For this demo, we'll read it from the file we just created:

    const file = try std.fs.cwd().openFile("example_dict.bin", .{});
    defer file.close();

    const dict_data = try file.readToEndAlloc(allocator, 10 * 1024 * 1024);
    defer allocator.free(dict_data);

    try useEmbeddedDictionary(allocator, dict_data);

    std.debug.print("═══════════════════════════════════════════════════════\n", .{});
    std.debug.print("Demo complete! This demonstrates:\n", .{});
    std.debug.print("  1. Building a dictionary from keywords and entries\n", .{});
    std.debug.print("  2. Serializing to binary format\n", .{});
    std.debug.print("  3. Deserializing from binary (can use @embedFile)\n", .{});
    std.debug.print("  4. Tokenizing Japanese text with Viterbi algorithm\n", .{});
    std.debug.print("═══════════════════════════════════════════════════════\n\n", .{});
}
