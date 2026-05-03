const std = @import("std");

pub const CsvError = error{
    /// Quote is not closed.
    UnterminatedQuote,
    /// LF appeared inside a quoted field.
    NewlineInQuotedField,
    /// Unexpected character after closing quote.
    InvalidCharAfterQuote,
};

pub const State = enum {
    start, // start a line or a field
    unquoted, // in the field with no quotes
    quoted, // in the field with quotes
    quoting, // encountered a quote inside a quoted field, expecting either another quote or a comma
};

pub fn parseCsv(allocator: std.mem.Allocator, line: []const u8) ![][]const u8 {
    var fields = std.ArrayList([]const u8).empty;
    errdefer {
        for (fields.items) |f| {
            allocator.free(f);
        }
        fields.deinit(allocator);
    }

    var field = std.ArrayList(u8).empty;
    errdefer field.deinit(allocator);

    var state: State = .start;

    for (line) |b| {
        switch (state) {
            .start => {
                switch (b) {
                    ',' => {
                        try fields.append(allocator, try field.toOwnedSlice(allocator));
                    },
                    '"' => {
                        state = .quoted;
                    },
                    else => {
                        state = .unquoted;
                        try field.append(allocator, b);
                    },
                }
            },
            .unquoted => {
                switch (b) {
                    ',' => {
                        try fields.append(allocator, try field.toOwnedSlice(allocator));
                        state = .start;
                    },
                    else => {
                        try field.append(allocator, b);
                    },
                }
            },
            .quoted => {
                switch (b) {
                    '"' => {
                        state = .quoting;
                    },
                    else => {
                        try field.append(allocator, b);
                    },
                }
            },
            .quoting => {
                switch (b) {
                    '"' => {
                        try field.append(allocator, '"');
                        state = .quoted;
                    },
                    ',' => {
                        try fields.append(allocator, try field.toOwnedSlice(allocator));
                        state = .start;
                    },
                    else => return CsvError.InvalidCharAfterQuote,
                }
            },
        }
    }

    switch (state) {
        .start, .unquoted, .quoting => {
            try fields.append(allocator, try field.toOwnedSlice(allocator));
        },
        .quoted => {
            return CsvError.UnterminatedQuote;
        },
    }

    return try fields.toOwnedSlice(allocator);
}

pub fn freeRow(allocator: std.mem.Allocator, row: [][]const u8) void {
    for (row) |field| {
        allocator.free(field);
    }
    allocator.free(row);
}

test "basic row" {
    const allocator = std.testing.allocator;
    const row = try parseCsv(allocator, "a,b,c");
    defer freeRow(allocator, row);

    try std.testing.expectEqual(@as(usize, 3), row.len);
    try std.testing.expectEqualStrings("a", row[0]);
    try std.testing.expectEqualStrings("b", row[1]);
    try std.testing.expectEqualStrings("c", row[2]);
}

test "quoted field" {
    const allocator = std.testing.allocator;
    const row = try parseCsv(allocator, "\"hello world\",b");
    defer freeRow(allocator, row);

    try std.testing.expectEqualStrings("hello world", row[0]);
    try std.testing.expectEqualStrings("b", row[1]);
}

test "escaped quote" {
    const allocator = std.testing.allocator;
    const row = try parseCsv(allocator, "\"a\"\"b\"\",c\"");
    defer freeRow(allocator, row);

    try std.testing.expectEqualStrings("a\"b\",c", row[0]);
}

test "empty fields" {
    const allocator = std.testing.allocator;
    const row = try parseCsv(allocator, ",,");
    defer freeRow(allocator, row);

    try std.testing.expectEqual(@as(usize, 3), row.len);
    try std.testing.expectEqualStrings("", row[0]);
    try std.testing.expectEqualStrings("", row[1]);
    try std.testing.expectEqualStrings("", row[2]);
}

test "empty fields with quotes" {
    const allocator = std.testing.allocator;
    const row = try parseCsv(allocator, "\"\",\"\"");
    defer freeRow(allocator, row);

    try std.testing.expectEqual(@as(usize, 2), row.len);
    try std.testing.expectEqualStrings("", row[0]);
    try std.testing.expectEqualStrings("", row[1]);
}

test "error on unterminated quote" {
    const allocator = std.testing.allocator;
    try std.testing.expectError(CsvError.UnterminatedQuote, parseCsv(allocator, "\"abc"));
}

test "multi-byte characters" {
    const allocator = std.testing.allocator;
    const row = try parseCsv(allocator, "\"山田, 太郎\",30,\"東京\"\"都\"");
    defer freeRow(allocator, row);

    try std.testing.expectEqual(@as(usize, 3), row.len);
    try std.testing.expectEqualStrings("山田, 太郎", row[0]);
    try std.testing.expectEqualStrings("30", row[1]);
    try std.testing.expectEqualStrings("東京\"都", row[2]);
}

pub fn csvLessThan(_: void, a: [][]const u8, b: [][]const u8) bool {
    var av: []const u8 = undefined;
    if (a.len == 0) {
        av = "";
    } else {
        av = a[0];
    }
    var bv: []const u8 = undefined;
    if (b.len == 0) {
        bv = "";
    } else {
        bv = b[0];
    }
    return std.mem.lessThan(u8, av, bv);
}

test "sort csv rows" {
    const allocator = std.testing.allocator;

    var rows = std.ArrayList([][]const u8).empty;
    defer {
        for (rows.items) |row| {
            freeRow(allocator, row);
        }
        rows.deinit(allocator);
    }
    try rows.append(allocator, try parseCsv(allocator, "a,foo"));
    try rows.append(allocator, try parseCsv(allocator, "\"c\",qux"));
    try rows.append(allocator, try parseCsv(allocator, "b,baz"));
    try rows.append(allocator, try parseCsv(allocator, "b,bar"));

    std.mem.sort([][]const u8, rows.items, {}, csvLessThan);

    try std.testing.expectEqualStrings("a", rows.items[0][0]);
    try std.testing.expectEqualStrings("foo", rows.items[0][1]);
    try std.testing.expectEqualStrings("b", rows.items[1][0]);
    try std.testing.expectEqualStrings("baz", rows.items[1][1]);
    try std.testing.expectEqualStrings("b", rows.items[2][0]);
    try std.testing.expectEqualStrings("bar", rows.items[2][1]);
    try std.testing.expectEqualStrings("c", rows.items[3][0]);
    try std.testing.expectEqualStrings("qux", rows.items[3][1]);
}
