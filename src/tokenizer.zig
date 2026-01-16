//! Japanese morphological analyzer (tokenizer) implementation.
//! This module provides the main interface for tokenizing Japanese text.

const std = @import("std");
const Dictionary = @import("dictionary.zig").Dictionary;
const Entry = @import("dictionary.zig").Entry;
const ConnectionCosts = @import("dictionary.zig").ConnectionCosts;
const Lattice = @import("lattice.zig").Lattice;
const LatticeNode = @import("lattice.zig").LatticeNode;
const viterbi = @import("viterbi.zig").viterbi;
const buildLattice = @import("viterbi.zig").buildLattice;
const Token = @import("viterbi.zig").Token;
const DoubleArray = @import("trie.zig").DoubleArray;

/// Character type classification for unknown word handling
pub const CharType = enum {
    Kanji,
    Hiragana,
    Katakana,
    Number,
    Alphabet,
    Other,
};

/// Classify character type for unknown word handling
pub fn classifyChar(c: u21) CharType {
    if (c >= 0x4E00 and c <= 0x9FFF) return .Kanji; // CJK Unified Ideographs
    if (c >= 0x3040 and c <= 0x309F) return .Hiragana; // Hiragana
    if (c >= 0x30A0 and c <= 0x30FF) return .Katakana; // Katakana
    if (c >= '0' and c <= '9') return .Number;
    if ((c >= 'A' and c <= 'Z') or (c >= 'a' and c <= 'z')) return .Alphabet;
    return .Other;
}

/// Japanese morphological analyzer
pub const Tokenizer = struct {
    const Self = @This();

    allocator: std.mem.Allocator,
    dictionary: Dictionary,

    pub fn init(allocator: std.mem.Allocator, dictionary: Dictionary) Self {
        return .{
            .allocator = allocator,
            .dictionary = dictionary,
        };
    }

    /// Deinitialize the tokenizer and its associated dictionary.
    ///
    /// Note: This does *not* free the underlying DoubleArray trie referenced by
    /// the Dictionary. The caller is responsible for managing the lifetime of
    /// that trie (including freeing it, if it was heap-allocated) and must
    /// ensure it outlives any Tokenizer/Dictionary that uses it.
    pub fn deinit(self: Self) void {
        self.dictionary.deinit();
    }

    /// Tokenize Japanese text into morphemes.
    /// Returns an array of tokens. Caller owns the returned slice and must free it.
    pub fn tokenize(self: *Self, text: []const u8) ![]Token {
        if (text.len == 0) {
            return try self.allocator.alloc(Token, 0);
        }

        // Build lattice from input text
        var lattice = try buildLattice(self.allocator, text, &self.dictionary);
        defer lattice.deinit();

        // Apply Viterbi algorithm to find optimal path
        return try viterbi(self.allocator, &lattice, &self.dictionary);
    }

    /// Tokenize and print results (for debugging/testing)
    pub fn tokenizeAndPrint(self: *Self, text: []const u8, writer: anytype) !void {
        const tokens = try self.tokenize(text);
        defer self.allocator.free(tokens);

        for (tokens, 0..) |token, i| {
            try writer.print("{d}:\t{s}\t", .{ i, token.surface });
            for (token.features, 0..) |feature, j| {
                if (j > 0) try writer.writeAll(",");
                try writer.writeAll(feature);
            }
            try writer.writeAll("\n");
        }
    }
};

test "classify characters" {
    try std.testing.expectEqual(CharType.Hiragana, classifyChar('あ'));
    try std.testing.expectEqual(CharType.Hiragana, classifyChar('ん'));
    try std.testing.expectEqual(CharType.Katakana, classifyChar('ア'));
    try std.testing.expectEqual(CharType.Katakana, classifyChar('ン'));
    try std.testing.expectEqual(CharType.Kanji, classifyChar('東'));
    try std.testing.expectEqual(CharType.Kanji, classifyChar('京'));
    try std.testing.expectEqual(CharType.Number, classifyChar('0'));
    try std.testing.expectEqual(CharType.Number, classifyChar('9'));
    try std.testing.expectEqual(CharType.Alphabet, classifyChar('A'));
    try std.testing.expectEqual(CharType.Alphabet, classifyChar('z'));
}

/// Create a simple tokenizer for testing purposes with minimal dictionary
/// WARNING: This function has ownership issues - the returned Tokenizer holds a reference
/// to a DoubleArray that should be freed separately. This is for testing only and should
/// not be used as a pattern for production code. The caller must ensure proper cleanup
/// of all components.
/// TODO: Refactor to return a struct containing all owned components.
pub fn createTestTokenizer(allocator: std.mem.Allocator) !Tokenizer {
    // Create a minimal dictionary for testing
    const keywords = [_][]const u8{
        "東京",
        "に",
        "行く",
        "東",
        "京",
    };

    var da = try DoubleArray.init(allocator, &keywords);
    errdefer da.deinit();
    try da.build();

    // Create entries
    const features_tokyo = try allocator.dupe([]const u8, &[_][]const u8{ "名詞", "固有名詞", "地名" });
    const features_ni = try allocator.dupe([]const u8, &[_][]const u8{ "助詞", "格助詞" });
    const features_iku = try allocator.dupe([]const u8, &[_][]const u8{ "動詞", "自立" });
    const features_to = try allocator.dupe([]const u8, &[_][]const u8{ "名詞", "一般" });
    const features_kyo = try allocator.dupe([]const u8, &[_][]const u8{ "名詞", "一般" });

    var entries = std.ArrayList(Entry).init(allocator);
    try entries.append(Entry.init("東京", 1, 1, 3000, features_tokyo));
    try entries.append(Entry.init("に", 2, 2, 500, features_ni));
    try entries.append(Entry.init("行く", 3, 3, 4000, features_iku));
    try entries.append(Entry.init("東", 1, 1, 5000, features_to));
    try entries.append(Entry.init("京", 1, 1, 5000, features_kyo));

    // Create connection costs
    var costs = try ConnectionCosts.init(allocator, 10, 10);
    // Set some reasonable connection costs
    costs.setCost(0, 1, 0); // BOS -> noun
    costs.setCost(0, 2, 0); // BOS -> particle
    costs.setCost(0, 3, 0); // BOS -> verb
    costs.setCost(1, 2, 100); // noun -> particle (good connection)
    costs.setCost(2, 3, 100); // particle -> verb (good connection)
    costs.setCost(1, 1, 500); // noun -> noun (less preferred)
    costs.setCost(3, 0, 0); // verb -> EOS

    // Create dictionary
    // WARNING: da is a local variable but dict holds a pointer to it
    // This is a known issue in this test helper
    const dict = Dictionary.init(allocator, &da, entries, costs);

    return Tokenizer.init(allocator, dict);
}

test "create test tokenizer" {
    const allocator = std.testing.allocator;
    const tokenizer = try createTestTokenizer(allocator);
    defer tokenizer.deinit();

    // Just verify we can create it
    try std.testing.expect(tokenizer.dictionary.entries.items.len > 0);
}
