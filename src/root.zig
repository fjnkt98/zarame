//! By convention, root.zig is the root source file when making a library. If
//! you are making an executable, the convention is to delete this file and
//! start with main.zig instead.
const std = @import("std");
const testing = std.testing;

pub const csv = @import("csv.zig");
pub const dictionary = @import("dictionary.zig");
pub const lattice = @import("lattice.zig");
pub const trie = @import("trie.zig");

test "test submodules" {
    _ = @import("csv.zig");
    _ = @import("dictionary.zig");
    _ = @import("lattice.zig");
    _ = @import("trie.zig");
}
