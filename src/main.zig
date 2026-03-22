pub fn main() !void {
    std.debug.print("All your {s} are belong to us.\n", .{"codebase"});

    var buffer: [1024]u8 = undefined;
    var writer = std.fs.File.stdout().writer(&buffer);
    const stdout = &writer.interface;

    try stdout.print("Run `zig build test` to run the tests.\n", .{});
    try stdout.flush();
}

test "simple test" {
    const allocator = std.testing.allocator;
    var list = try std.ArrayList(i32).initCapacity(allocator, 1);
    defer list.deinit(allocator); // Try commenting this out and see if zig detects the memory leak!
    try list.append(allocator, 42);
    try std.testing.expectEqual(@as(i32, 42), list.pop());
}

test "use other module" {
    try std.testing.expectEqual(@as(i32, 150), lib.add(100, 50));
}

test "fuzz example" {
    const Context = struct {
        fn testOne(context: @This(), input: []const u8) anyerror!void {
            _ = context;
            // Try passing `--fuzz` to `zig build test` and see if it manages to fail this test case!
            try std.testing.expect(!std.mem.eql(u8, "canyoufindme", input));
        }
    };
    try std.testing.fuzz(Context{}, Context.testOne, .{});
}

const std = @import("std");

/// This imports the separate module containing `root.zig`. Take a look in `build.zig` for details.
const lib = @import("zarame_lib");
