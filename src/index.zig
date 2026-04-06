const std = @import("std");
const trie = @import("trie.zig");

const Index = struct {
    const Self = @This();

    da: trie.DoubleArray,
    dup: std.AutoArrayHashMap(usize, usize),

    pub fn init(allocator: std.mem.Allocator, lex: []const []const u8) !Self {
        if (!std.sort.isSorted([]const u8, lex, {}, trie.stringLessThan)) {
            return error.UnsortedWordsError;
        }

        var dup = std.AutoArrayHashMap(usize, usize).init(allocator);

        var words = try std.ArrayList([]const u8).initCapacity(allocator, lex.len);
        defer words.deinit(allocator);
        var ids = try std.ArrayList(usize).initCapacity(allocator, lex.len);
        defer ids.deinit(allocator);

        var prev_id: usize = 0;
        var prev_word: []const u8 = "";
        for (0.., lex) |i, word| {
            if (std.mem.eql(u8, word, prev_word)) {
                const gop = try dup.getOrPut(prev_id);
                if (gop.found_existing) {
                    gop.value_ptr.* += 1;
                } else {
                    gop.value_ptr.* = 1;
                }
                continue;
            }

            prev_id = i;
            prev_word = word;
            try words.append(allocator, word);
            try ids.append(allocator, i);
        }

        const da = try trie.DoubleArray.init(allocator, words.items, ids.items, 65535);

        return Self{
            .da = da,
            .dup = dup,
        };
    }

    pub fn deinit(self: *Self, allocator: std.mem.Allocator) void {
        self.da.deinit(allocator);
        self.dup.deinit();
    }

    // pub fn commonPrefixSearch(self: Self, keyword: []const u8) !
};
