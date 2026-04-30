const std = @import("std");

pub const CsvError = error{
    /// Quote is not closed.
    UnterminatedQuote,
    /// LF appeared inside a quoted field.
    NewlineInQuotedField,
    /// Unexpected character after closing quote.
    InvalidCharAfterQuote,
};

/// A minimal CSV reader that reads from an `std.Io.Reader`.
///
/// Usage:
/// ```zig
/// var file_reader = file.reader(io, &file_buf);
/// var csv = CsvReader.init(allocator, &file_reader.interface);
///
/// while (try csv.next()) |row| {
///     defer csv.freeRow(row);
///     // Access fields as row[0], row[1], ...
/// }
/// ```
pub const CsvReader = struct {
    const Self = @This();

    reader: *std.Io.Reader,
    allocator: std.mem.Allocator,

    pub fn init(allocator: std.mem.Allocator, reader: *std.Io.Reader) Self {
        return .{ .reader = reader, .allocator = allocator };
    }

    /// Reads one byte from the underlying reader. Returns null on EOF.
    fn nextByte(self: *Self) !?u8 {
        const slice = self.reader.take(1) catch |err| switch (err) {
            error.EndOfStream => return null,
            else => return err,
        };
        return slice[0];
    }

    /// Reads one row and returns it as `[][]u8`.
    /// Returns null when at EOF with no data remaining.
    /// The caller must free the returned memory with `freeRow`.
    pub fn next(self: *Self) !?[][]u8 {
        const State = enum { field_start, unquoted, quoted, quote_seen };

        var fields: std.ArrayList([]u8) = .empty;
        errdefer {
            for (fields.items) |f| self.allocator.free(f);
            fields.deinit(self.allocator);
        }

        var field: std.ArrayList(u8) = .empty;
        errdefer field.deinit(self.allocator);

        var state: State = .field_start;
        // Tracks whether at least one byte has been consumed to distinguish
        // an empty-but-present row from EOF.
        var has_data = false;

        while (true) {
            const mb = try self.nextByte();

            switch (state) {
                .field_start => {
                    if (mb == null) {
                        if (!has_data) {
                            field.deinit(self.allocator);
                            return null;
                        }
                        try fields.append(self.allocator, try field.toOwnedSlice(self.allocator));
                        return try fields.toOwnedSlice(self.allocator);
                    }
                    has_data = true;
                    switch (mb.?) {
                        '\n' => {
                            try fields.append(self.allocator, try field.toOwnedSlice(self.allocator));
                            return try fields.toOwnedSlice(self.allocator);
                        },
                        ',' => {
                            // Commit empty field; stay in field_start.
                            try fields.append(self.allocator, try field.toOwnedSlice(self.allocator));
                        },
                        '"' => {
                            state = .quoted;
                        },
                        else => |b| {
                            try field.append(self.allocator, b);
                            state = .unquoted;
                        },
                    }
                },
                .unquoted => {
                    if (mb == null) {
                        try fields.append(self.allocator, try field.toOwnedSlice(self.allocator));
                        return try fields.toOwnedSlice(self.allocator);
                    }
                    switch (mb.?) {
                        '\n' => {
                            try fields.append(self.allocator, try field.toOwnedSlice(self.allocator));
                            return try fields.toOwnedSlice(self.allocator);
                        },
                        ',' => {
                            try fields.append(self.allocator, try field.toOwnedSlice(self.allocator));
                            state = .field_start;
                        },
                        else => |b| try field.append(self.allocator, b),
                    }
                },
                .quoted => {
                    if (mb == null) return CsvError.UnterminatedQuote;
                    switch (mb.?) {
                        '"' => state = .quote_seen,
                        '\n' => return CsvError.NewlineInQuotedField,
                        else => |b| try field.append(self.allocator, b),
                    }
                },
                .quote_seen => {
                    if (mb == null) {
                        // Closing quote at EOF.
                        try fields.append(self.allocator, try field.toOwnedSlice(self.allocator));
                        return try fields.toOwnedSlice(self.allocator);
                    }
                    switch (mb.?) {
                        '"' => {
                            // "" is an escaped double-quote.
                            try field.append(self.allocator, '"');
                            state = .quoted;
                        },
                        ',' => {
                            try fields.append(self.allocator, try field.toOwnedSlice(self.allocator));
                            state = .field_start;
                        },
                        '\n' => {
                            try fields.append(self.allocator, try field.toOwnedSlice(self.allocator));
                            return try fields.toOwnedSlice(self.allocator);
                        },
                        else => return CsvError.InvalidCharAfterQuote,
                    }
                },
            }
        }
    }

    /// Frees all memory owned by a row returned from `next`.
    pub fn freeRow(self: *Self, row: [][]u8) void {
        for (row) |f| self.allocator.free(f);
        self.allocator.free(row);
    }
};

// ─── tests ───────────────────────────────────────────────────────────────────

const testing = std.testing;

test "basic row" {
    var r = std.Io.Reader.fixed("a,b,c\n");
    var csv = CsvReader.init(testing.allocator, &r);

    const row = (try csv.next()).?;
    defer csv.freeRow(row);

    try testing.expectEqual(@as(usize, 3), row.len);
    try testing.expectEqualStrings("a", row[0]);
    try testing.expectEqualStrings("b", row[1]);
    try testing.expectEqualStrings("c", row[2]);

    try testing.expect((try csv.next()) == null);
}

test "multiple rows" {
    var r = std.Io.Reader.fixed("a,b\nc,d\n");
    var csv = CsvReader.init(testing.allocator, &r);

    const row1 = (try csv.next()).?;
    defer csv.freeRow(row1);
    try testing.expectEqualStrings("a", row1[0]);
    try testing.expectEqualStrings("b", row1[1]);

    const row2 = (try csv.next()).?;
    defer csv.freeRow(row2);
    try testing.expectEqualStrings("c", row2[0]);
    try testing.expectEqualStrings("d", row2[1]);

    try testing.expect((try csv.next()) == null);
}

test "quoted field" {
    var r = std.Io.Reader.fixed("\"hello world\",b\n");
    var csv = CsvReader.init(testing.allocator, &r);

    const row = (try csv.next()).?;
    defer csv.freeRow(row);

    try testing.expectEqualStrings("hello world", row[0]);
    try testing.expectEqualStrings("b", row[1]);
}

test "escaped quote" {
    // "a""b"",c" -> a"b",c
    var r = std.Io.Reader.fixed("\"a\"\"b\"\",c\"\n");
    var csv = CsvReader.init(testing.allocator, &r);

    const row = (try csv.next()).?;
    defer csv.freeRow(row);

    try testing.expectEqualStrings("a\"b\",c", row[0]);
}

test "empty fields" {
    var r = std.Io.Reader.fixed(",,\n");
    var csv = CsvReader.init(testing.allocator, &r);

    const row = (try csv.next()).?;
    defer csv.freeRow(row);

    try testing.expectEqual(@as(usize, 3), row.len);
    try testing.expectEqualStrings("", row[0]);
    try testing.expectEqualStrings("", row[1]);
    try testing.expectEqualStrings("", row[2]);
}

test "eof without trailing newline" {
    var r = std.Io.Reader.fixed("a,b");
    var csv = CsvReader.init(testing.allocator, &r);

    const row = (try csv.next()).?;
    defer csv.freeRow(row);

    try testing.expectEqual(@as(usize, 2), row.len);
    try testing.expectEqualStrings("a", row[0]);
    try testing.expectEqualStrings("b", row[1]);

    try testing.expect((try csv.next()) == null);
}

test "error on newline in quoted field" {
    var r = std.Io.Reader.fixed("\"a\nb\"\n");
    var csv = CsvReader.init(testing.allocator, &r);

    try testing.expectError(CsvError.NewlineInQuotedField, csv.next());
}

test "error on unterminated quote" {
    var r = std.Io.Reader.fixed("\"abc");
    var csv = CsvReader.init(testing.allocator, &r);

    try testing.expectError(CsvError.UnterminatedQuote, csv.next());
}

test "mixed multi-row csv" {
    // Row 1: plain fields
    // Row 2: quoted field with embedded comma, plain field
    // Row 3: empty field, escaped quote, plain field
    // Row 4: last row without trailing newline
    const input =
        \\name,age,city
        \\"Smith, John",30,Tokyo
        \\,"say ""hi"",",plain
        \\last,row,here
    ;
    var r = std.Io.Reader.fixed(input);
    var csv = CsvReader.init(testing.allocator, &r);

    const row1 = (try csv.next()).?;
    defer csv.freeRow(row1);
    try testing.expectEqual(@as(usize, 3), row1.len);
    try testing.expectEqualStrings("name", row1[0]);
    try testing.expectEqualStrings("age", row1[1]);
    try testing.expectEqualStrings("city", row1[2]);

    const row2 = (try csv.next()).?;
    defer csv.freeRow(row2);
    try testing.expectEqual(@as(usize, 3), row2.len);
    try testing.expectEqualStrings("Smith, John", row2[0]);
    try testing.expectEqualStrings("30", row2[1]);
    try testing.expectEqualStrings("Tokyo", row2[2]);

    const row3 = (try csv.next()).?;
    defer csv.freeRow(row3);
    try testing.expectEqual(@as(usize, 3), row3.len);
    try testing.expectEqualStrings("", row3[0]);
    try testing.expectEqualStrings("say \"hi\",", row3[1]);
    try testing.expectEqualStrings("plain", row3[2]);

    const row4 = (try csv.next()).?;
    defer csv.freeRow(row4);
    try testing.expectEqual(@as(usize, 3), row4.len);
    try testing.expectEqualStrings("last", row4[0]);
    try testing.expectEqualStrings("row", row4[1]);
    try testing.expectEqualStrings("here", row4[2]);

    try testing.expect((try csv.next()) == null);
}

test "multi-byte characters" {
    // Fields containing Japanese (UTF-8 multi-byte) characters,
    // including a quoted field with an embedded comma and an escaped quote.
    var r = std.Io.Reader.fixed("名前,年齢,都市\n\"山田, 太郎\",30,\"東京\"\"都\"\n");
    var csv = CsvReader.init(testing.allocator, &r);

    const row1 = (try csv.next()).?;
    defer csv.freeRow(row1);
    try testing.expectEqual(@as(usize, 3), row1.len);
    try testing.expectEqualStrings("名前", row1[0]);
    try testing.expectEqualStrings("年齢", row1[1]);
    try testing.expectEqualStrings("都市", row1[2]);

    const row2 = (try csv.next()).?;
    defer csv.freeRow(row2);
    try testing.expectEqual(@as(usize, 3), row2.len);
    try testing.expectEqualStrings("山田, 太郎", row2[0]);
    try testing.expectEqualStrings("30", row2[1]);
    try testing.expectEqualStrings("東京\"都", row2[2]);

    try testing.expect((try csv.next()) == null);
}
