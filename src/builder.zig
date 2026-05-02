const std = @import("std");
const Io = std.Io;
const flate = std.compress.flate;
const dictionary = @import("zarame").dictionary;

const Header = dictionary.Header;
const magic = dictionary.magic;

const usage =
    \\Usage: builder [options]
    \\
    \\Options:
    \\  --output-file OUTPUT_GZIP_FILE
    \\
;

pub fn main(init: std.process.Init) !void {
    const io = init.io;
    const arena = init.arena.allocator();

    const args = try init.minimal.args.toSlice(arena);

    var opt_output_file_path: ?[]const u8 = null;

    {
        var i: usize = 1;
        while (i < args.len) : (i += 1) {
            const arg = args[i];
            if (std.mem.eql(u8, "-h", arg) or std.mem.eql(u8, "--help", arg)) {
                try Io.File.stdout().writeStreamingAll(io, usage);
                return std.process.cleanExit(io);
            } else if (std.mem.eql(u8, "--output-file", arg)) {
                i += 1;
                if (i > args.len) fatal("expected arg after '{s}'", .{arg});
                if (opt_output_file_path != null) fatal("duplicated {s} argument", .{arg});
                opt_output_file_path = args[i];
            } else {
                fatal("unexpected argument: '{s}'", .{arg});
            }
        }
    }

    const output_file_path = opt_output_file_path orelse fatal("missing --output-file", .{});

    var output_file = Io.Dir.cwd().createFile(io, output_file_path, .{}) catch |err| {
        fatal("unable to open '{s}': {s}", .{ output_file_path, @errorName(err) });
    };
    defer output_file.close(io);

    var values = std.ArrayList(i32).empty;
    defer values.deinit(arena);
    for (0..10) |i| {
        try values.append(arena, @as(i32, @intCast((i + 1) * 11)));
    }

    const header = Header{
        .magic = magic,
        .version = 1,
        .count = @intCast(values.items.len),
    };

    var out_buffer: [4096]u8 = undefined;
    var out_writer = output_file.writer(io, &out_buffer);

    var window: [flate.max_window_len]u8 = undefined;
    var compressor = try flate.Compress.init(&out_writer.interface, &window, .gzip, .best);
    try compressor.writer.writeAll(std.mem.asBytes(&header));
    try compressor.writer.writeAll(std.mem.sliceAsBytes(values.items));
    try compressor.finish();
    try out_writer.interface.flush();

    return std.process.cleanExit(io);
}

fn fatal(comptime format: []const u8, args: anytype) noreturn {
    std.debug.print(format, args);
    std.process.exit(1);
}
