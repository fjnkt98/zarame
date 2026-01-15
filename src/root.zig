//! By convention, root.zig is the root source file when making a library. If
//! you are making an executable, the convention is to delete this file and
//! start with main.zig instead.
const std = @import("std");
const testing = std.testing;

// Export all modules for library users
pub const trie = @import("trie.zig");
pub const dictionary = @import("dictionary.zig");
pub const lattice = @import("lattice.zig");
pub const viterbi = @import("viterbi.zig");
pub const serialization = @import("serialization.zig");
pub const tokenizer = @import("tokenizer.zig");

// Re-export commonly used types
pub const DoubleArray = trie.DoubleArray;
pub const Entry = dictionary.Entry;
pub const ConnectionCosts = dictionary.ConnectionCosts;
pub const Dictionary = dictionary.Dictionary;
pub const Lattice = lattice.Lattice;
pub const LatticeNode = lattice.LatticeNode;
pub const Token = viterbi.Token;
pub const Tokenizer = tokenizer.Tokenizer;

test "test submodules" {
    _ = @import("trie.zig");
    _ = @import("dictionary.zig");
    _ = @import("lattice.zig");
    _ = @import("viterbi.zig");
    _ = @import("serialization.zig");
    _ = @import("tokenizer.zig");
}
