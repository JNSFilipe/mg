const std = @import("std");

/// Search options expose behavior explicitly instead of relying on global flags.
pub const Options = struct {
    case_sensitive: bool = true,
    wrap: bool = true,
};

pub const Direction = enum {
    forward,
    backward,
};

pub fn find(haystack: []const u8, needle: []const u8, start: usize, direction: Direction, options: Options) ?usize {
    if (needle.len == 0) return @min(start, haystack.len);
    if (needle.len > haystack.len) return null;

    return switch (direction) {
        .forward => findForward(haystack, needle, start, options),
        .backward => findBackward(haystack, needle, start, options),
    };
}

fn findForward(haystack: []const u8, needle: []const u8, start: usize, options: Options) ?usize {
    const first_start = @min(start, haystack.len);
    if (findForwardNoWrap(haystack, needle, first_start, options.case_sensitive)) |idx| return idx;

    if (!options.wrap or first_start == 0) return null;
    return findForwardNoWrap(haystack, needle, 0, options.case_sensitive);
}

fn findForwardNoWrap(haystack: []const u8, needle: []const u8, start: usize, case_sensitive: bool) ?usize {
    if (haystack.len < needle.len) return null;

    const max_start = haystack.len - needle.len;
    if (start > max_start) return null;

    var idx = start;
    while (idx <= max_start) : (idx += 1) {
        if (matchesAt(haystack, needle, idx, case_sensitive)) return idx;
    }
    return null;
}

fn findBackward(haystack: []const u8, needle: []const u8, start: usize, options: Options) ?usize {
    const first_start = @min(start, haystack.len);
    if (findBackwardNoWrap(haystack, needle, first_start, options.case_sensitive)) |idx| return idx;

    if (!options.wrap) return null;
    if (haystack.len < needle.len) return null;

    const last = haystack.len - needle.len;
    if (first_start >= last) return null;
    return findBackwardNoWrap(haystack, needle, last, options.case_sensitive);
}

fn findBackwardNoWrap(haystack: []const u8, needle: []const u8, start: usize, case_sensitive: bool) ?usize {
    if (haystack.len < needle.len) return null;
    const max_start = haystack.len - needle.len;

    var idx = @min(start, max_start);
    while (true) {
        if (matchesAt(haystack, needle, idx, case_sensitive)) return idx;
        if (idx == 0) break;
        idx -= 1;
    }
    return null;
}

fn matchesAt(haystack: []const u8, needle: []const u8, start: usize, case_sensitive: bool) bool {
    for (needle, 0..) |n, i| {
        var lhs = haystack[start + i];
        var rhs = n;
        if (!case_sensitive) {
            lhs = std.ascii.toLower(lhs);
            rhs = std.ascii.toLower(rhs);
        }
        if (lhs != rhs) return false;
    }
    return true;
}

test "find forward and wrap" {
    try std.testing.expectEqual(@as(?usize, 4), find("abc def abc", "def", 0, .forward, .{}));
    try std.testing.expectEqual(@as(?usize, 0), find("abc def abc", "abc", 8, .forward, .{ .wrap = true }));
    try std.testing.expectEqual(@as(?usize, null), find("abc def abc", "abc", 8, .forward, .{ .wrap = false }));
}

test "find backward and wrap" {
    try std.testing.expectEqual(@as(?usize, 8), find("abc def abc", "abc", 10, .backward, .{}));
    try std.testing.expectEqual(@as(?usize, 8), find("abc def abc", "abc", 2, .backward, .{ .wrap = true }));
    try std.testing.expectEqual(@as(?usize, 0), find("abc def abc", "abc", 2, .backward, .{ .wrap = false }));
}

test "case insensitive search" {
    try std.testing.expectEqual(@as(?usize, 0), find("Hello", "he", 0, .forward, .{ .case_sensitive = false }));
    try std.testing.expectEqual(@as(?usize, null), find("Hello", "he", 0, .forward, .{ .case_sensitive = true }));
}
