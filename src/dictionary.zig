const std = @import("std");
const trie = @import("trie.zig");
const csv = @import("csv.zig");
const Io = std.Io;

const embedded = @embedFile("zarame.dict.gz");

/// Header of the binary dictionary file.
pub const Header = packed struct {
    magic: u64,
};

/// Magic number "ZARAME01".
pub const magic: u64 = 0x3130454D4152415A;

pub const Dictionary = struct {
    const Self = @This();

    index: Index,
    morphs: []const Morph,
    matrix: ConnectionMatrix,

    pub fn deinit(self: *Self, allocator: std.mem.Allocator) void {
        self.index.deinit(allocator);
        allocator.free(self.morphs);
        self.matrix.deinit(allocator);
    }

    // pub fn load(allocator: std.mem.Allocator, io: Io) !Self {}

    pub fn build(allocator: std.mem.Allocator, io: Io) !Self {
        const index, const morphs = blk: {
            var lex_csv = try Io.Dir.cwd().openFile(io, "src/resource/lex.csv", .{});
            defer lex_csv.close(io);

            var buf: [64 * 1024]u8 = undefined;
            var reader = lex_csv.readerStreaming(io, &buf);

            var rows = std.ArrayList([][]const u8).empty;
            defer {
                for (rows.items) |row| {
                    csv.freeRow(allocator, row);
                }
                rows.deinit(allocator);
            }

            while (try reader.interface.takeDelimiter('\n')) |line| {
                const row = try csv.parseCsv(allocator, line);
                try rows.append(allocator, row);
            }

            std.mem.sort([][]const u8, rows.items, {}, csv.csvLessThan);

            var lex = try std.ArrayList([]const u8).initCapacity(allocator, rows.items.len);
            defer {
                for (lex.items) |entry| {
                    allocator.free(entry);
                }
                lex.deinit(allocator);
            }

            var morphs = try std.ArrayList(Morph).initCapacity(allocator, rows.items.len);

            for (rows.items) |row| {
                if (row.len < 4) {
                    return error.InvalidLexRow;
                }
                const surface = row[0];
                const left_id = try std.fmt.parseInt(usize, row[1], 10);
                const right_id = try std.fmt.parseInt(usize, row[2], 10);
                const cost = try std.fmt.parseInt(i16, row[3], 10);

                try lex.append(allocator, try allocator.dupe(u8, surface));
                try morphs.append(allocator, .{
                    .left_id = left_id,
                    .right_id = right_id,
                    .cost = cost,
                });
            }

            const index = try Index.init(allocator, lex.items);
            break :blk .{ index, try morphs.toOwnedSlice(allocator) };
        };

        const matrix = blk: {
            var matrix_def = try Io.Dir.cwd().openFile(io, "src/resource/matrix.def", .{});
            defer matrix_def.close(io);

            var buf: [64 * 1024]u8 = undefined;
            var reader = matrix_def.readerStreaming(io, &buf);

            const first_line = try reader.interface.takeDelimiter('\n') orelse return error.EmptyMatrixDef;
            var first_splitted = std.mem.splitScalar(u8, first_line, ' ');
            const row_size = try std.fmt.parseInt(usize, first_splitted.first(), 10);
            const col_size = try std.fmt.parseInt(usize, first_splitted.rest(), 10);

            var matrix = try ConnectionMatrix.init(allocator, row_size, col_size);

            while (try reader.interface.takeDelimiter('\n')) |line| {
                var splitted = std.mem.splitScalar(u8, line, ' ');
                const left_id = try std.fmt.parseInt(usize, splitted.first(), 10);
                const right_id = try std.fmt.parseInt(usize, splitted.next().?, 10);
                const cost = try std.fmt.parseInt(i16, splitted.next().?, 10);

                matrix.set(left_id, right_id, cost);
            }

            break :blk matrix;
        };

        return Self{
            .index = index,
            .morphs = morphs,
            .matrix = matrix,
        };
    }

    // pub fn save(self: Self, allocator: std.mem.Allocator, io: Io) !void {}
};

pub const Index = struct {
    const Self = @This();

    da: trie.DoubleArray,
    dup: std.AutoHashMapUnmanaged(usize, usize),

    pub fn init(allocator: std.mem.Allocator, lex: []const []const u8) !Self {
        if (!std.sort.isSorted([]const u8, lex, {}, trie.stringLessThan)) {
            return error.UnsortedLexiconError;
        }

        var dup = std.AutoHashMapUnmanaged(usize, usize).empty;

        var words = try std.ArrayList([]const u8).initCapacity(allocator, lex.len);
        defer words.deinit(allocator);
        var ids = try std.ArrayList(usize).initCapacity(allocator, lex.len);
        defer ids.deinit(allocator);

        var prev_id: usize = 0;
        var prev_word: []const u8 = "";
        for (0.., lex) |i, word| {
            if (std.mem.eql(u8, word, prev_word)) {
                const gop = try dup.getOrPut(allocator, prev_id);
                if (gop.found_existing) {
                    gop.value_ptr.* += 1;
                } else {
                    gop.value_ptr.* = 2;
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
        self.dup.deinit(allocator);
    }

    pub fn commonPrefixSearch(self: Self, allocator: std.mem.Allocator, input: []const u8) std.mem.Allocator.Error![]trie.SearchResult {
        var res = std.ArrayList(trie.SearchResult).empty;

        const results = try self.da.commonPrefixSearch(allocator, input);
        defer allocator.free(results);

        for (results) |result| {
            const count = self.dup.get(result.id) orelse 1;
            for (0..count) |i| {
                try res.append(allocator, .{
                    .id = result.id + i,
                    .length = result.length,
                });
            }
        }
        return try res.toOwnedSlice(allocator);
    }
};

test "common prefix search multi-byte strings with duplication" {
    const allocator = std.testing.allocator;
    const lex = [_][]const u8{
        "電気",
        "電気",
        "電気通信",
        "電気通信大学",
        "電気通信大学大学院",
        "電気通信大学大学院電気通信学研究科",
        "電気通信大学院大学",
        "電気通信大学電気通信学部",
    };
    var index = try Index.init(allocator, &lex);
    defer index.deinit(allocator);

    const actual = try index.commonPrefixSearch(allocator, "電気通信大学院大学");
    defer allocator.free(actual);

    const expected = [_]trie.SearchResult{
        .{ .id = 0, .length = 6 },
        .{ .id = 1, .length = 6 },
        .{ .id = 2, .length = 12 },
        .{ .id = 3, .length = 18 },
        .{ .id = 6, .length = 27 },
    };
    try std.testing.expectEqualSlices(trie.SearchResult, &expected, actual);
}

pub const Morph = struct {
    const Self = @This();

    left_id: usize,
    right_id: usize,
    cost: i16,

    pub const zero = Self{
        .left_id = 0,
        .right_id = 0,
        .cost = 0,
    };
};

// pub const PosTable = struct {
//     const Self = @This();

//     poss: [][4]usize,
//     map: std.AutoArrayHashMapUnmanaged(usize, []const u8),

//     pub fn init(allocator: std.mem.Allocator, num: usize) !Self {
//         const poss = try allocator.alloc([4]usize, num);
//         errdefer allocator.free(poss);
//         @memset(poss, .{ 0, 0, 0, 0 });
//         return Self{
//             .poss = poss,
//             .map = std.AutoArrayHashMapUnmanaged(usize, []const u8).empty,
//         };
//     }

//     pub fn deinit(self: *Self, allocator: std.mem.Allocator) void {
//         allocator.free(self.poss);
//         self.map.deinit(allocator);
//     }

//     pub fn get(self: Self, id: usize) [4][]const u8 {
//         const pos = self.poss[id];
//         return .{
//             self.map.get(pos[0]) orelse "",
//             self.map.get(pos[1]) orelse "",
//             self.map.get(pos[2]) orelse "",
//             self.map.get(pos[3]) orelse "",
//         };
//     }
// };

pub const ConnectionMatrix = struct {
    const Self = @This();

    row_size: usize,
    col_size: usize,
    data: []i16,

    pub fn init(allocator: std.mem.Allocator, row_size: usize, col_size: usize) !Self {
        const data = try allocator.alloc(i16, row_size * col_size);
        @memset(data, 0);
        return Self{
            .row_size = row_size,
            .col_size = col_size,
            .data = data,
        };
    }

    pub fn deinit(self: *Self, allocator: std.mem.Allocator) void {
        allocator.free(self.data);
    }

    pub fn set(self: *Self, left_id: usize, right_id: usize, value: i16) void {
        self.data[left_id * self.col_size + right_id] = value;
    }

    pub fn get(self: Self, left_id: usize, right_id: usize) i16 {
        return self.data[left_id * self.col_size + right_id];
    }
};
