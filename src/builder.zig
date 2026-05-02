const std = @import("std");
const Io = std.Io;
const flate = std.compress.flate;
const dictionary = @import("zarame").dictionary;
const csv = @import("zarame").csv;

const Header = dictionary.Header;
const magic = dictionary.magic;
const Morph = dictionary.Morph;

pub fn main(init: std.process.Init) !void {
    const io = init.io;
    const arena = init.arena.allocator();

    const args = try init.minimal.args.toSlice(arena);
    if (args.len != 3) {
        std.debug.print("Usage: builder <lex.csv> <output.gz>\n", .{});
        std.process.exit(1);
    }
    const lex_file_path = args[1];
    const output_file_path = args[2];

    var output_file = Io.Dir.cwd().createFile(io, output_file_path, .{}) catch |err| {
        fatal("unable to open '{s}': {s}", .{ output_file_path, @errorName(err) });
    };
    defer output_file.close(io);

    var lex_file = Io.Dir.cwd().openFile(io, lex_file_path, .{}) catch |err| {
        fatal("unable to open '{s}': {s}", .{ lex_file_path, @errorName(err) });
    };
    defer lex_file.close(io);

    var lex_file_buf: [64 * 1024]u8 = undefined;
    var lex_reader = lex_file.readerStreaming(io, &lex_file_buf);
    var lex_csv = csv.CsvReader.init(arena, &lex_reader.interface);

    var morphs = std.ArrayList(Morph).empty;
    defer morphs.deinit(arena);

    var row_idx: usize = 0;
    while (try lex_csv.next()) |row| {
        defer lex_csv.freeRow(row);
        row_idx += 1;

        if (row.len < 4) {
            fatal("invalid lex row {d}: expected at least 4 columns", .{row_idx});
        }

        const left_id = std.fmt.parseInt(usize, row[1], 10) catch {
            fatal("invalid left_id at lex row {d}", .{row_idx});
        };
        const right_id = std.fmt.parseInt(usize, row[2], 10) catch {
            fatal("invalid right_id at lex row {d}", .{row_idx});
        };
        const cost = std.fmt.parseInt(i16, row[3], 10) catch {
            fatal("invalid cost at lex row {d}", .{row_idx});
        };

        try morphs.append(arena, .{
            .left_id = left_id,
            .right_id = right_id,
            .cost = cost,
        });
    }

    const header = Header{
        .magic = magic,
        .version = 1,
        .count = @intCast(morphs.items.len),
    };

    var out_buffer: [4096]u8 = undefined;
    var out_writer = output_file.writer(io, &out_buffer);

    var window: [flate.max_window_len]u8 = undefined;
    var compressor = try flate.Compress.init(&out_writer.interface, &window, .gzip, .best);
    try compressor.writer.writeAll(std.mem.asBytes(&header));

    var u32_buf: [@sizeOf(u32)]u8 = undefined;
    var i16_buf: [@sizeOf(i16)]u8 = undefined;
    for (morphs.items) |morph| {
        const left_id = std.math.cast(u32, morph.left_id) orelse {
            fatal("left_id out of range for u32", .{});
        };
        const right_id = std.math.cast(u32, morph.right_id) orelse {
            fatal("right_id out of range for u32", .{});
        };

        std.mem.writeInt(u32, &u32_buf, left_id, .little);
        try compressor.writer.writeAll(&u32_buf);
        std.mem.writeInt(u32, &u32_buf, right_id, .little);
        try compressor.writer.writeAll(&u32_buf);
        std.mem.writeInt(i16, &i16_buf, morph.cost, .little);
        try compressor.writer.writeAll(&i16_buf);
    }

    try compressor.finish();
    try out_writer.interface.flush();

    return std.process.cleanExit(io);
}

fn fatal(comptime format: []const u8, args: anytype) noreturn {
    std.debug.print(format, args);
    std.process.exit(1);
}
