const std = @import("std");
const trie = @import("trie.zig");

pub const Dictionary = struct {
    index: trie.DoubleArray,
    morphs: []const Morph,
    matrix: Matrix,
};

pub const Morph = struct {
    left_id: usize,
    right_id: usize,
    cost: i16,
};

pub const Matrix = struct {
    const Self = @This();

    row: usize,
    col: usize,
    data: []const i16,

    pub fn at(self: Self, row: usize, col: usize) i16 {
        return self.data[row * self.col + col];
    }
};
