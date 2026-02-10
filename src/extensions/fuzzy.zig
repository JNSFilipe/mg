const std = @import("std");

/// MatchScore captures how well a candidate matched a query.
/// Higher score means a better match.
pub const MatchScore = struct {
    score: i32,
    first_index: usize,
    last_index: usize,
};

/// score returns a case-insensitive subsequence score.
/// It rewards:
/// - contiguous runs
/// - word starts (`-`, `_`, `/`, `.`, space boundaries)
/// - earlier matches in the candidate
///
/// It penalizes:
/// - gaps between matched characters
/// - long unmatched tails
///
/// Null means the query is not a subsequence of candidate.
pub fn score(candidate: []const u8, query: []const u8) ?MatchScore {
    if (query.len == 0) {
        return .{ .score = 0, .first_index = 0, .last_index = 0 };
    }
    if (candidate.len == 0) return null;

    var q_idx: usize = 0;
    var first_idx: usize = 0;
    var last_idx: usize = 0;
    var prev_match: ?usize = null;
    var total: i32 = 0;

    for (candidate, 0..) |ch, c_idx| {
        if (q_idx >= query.len) break;
        const needle = query[q_idx];
        if (std.ascii.toLower(ch) != std.ascii.toLower(needle)) continue;

        if (q_idx == 0) first_idx = c_idx;
        last_idx = c_idx;

        total += 10;
        if (isWordStart(candidate, c_idx)) total += 8;
        if (ch == needle) total += 1;

        if (prev_match) |prev| {
            const gap = c_idx - prev - 1;
            if (gap == 0) {
                total += 6;
            } else {
                total -= @as(i32, @intCast(@min(gap, 8)));
            }
        } else {
            total -= @as(i32, @intCast(@min(c_idx, 8)));
        }

        prev_match = c_idx;
        q_idx += 1;
    }

    if (q_idx != query.len) return null;

    const tail = candidate.len - 1 - last_idx;
    total -= @as(i32, @intCast(@min(tail, 8)));

    return .{
        .score = total,
        .first_index = first_idx,
        .last_index = last_idx,
    };
}

fn isWordStart(candidate: []const u8, idx: usize) bool {
    if (idx == 0) return true;
    const prev = candidate[idx - 1];
    const current = candidate[idx];

    if (prev == '-' or prev == '_' or prev == '/' or prev == '.' or prev == ' ') {
        return true;
    }
    if (std.ascii.isLower(prev) and std.ascii.isUpper(current)) {
        return true;
    }
    return false;
}

test "fuzzy score matches subsequence and rejects non-match" {
    const ok = score("save-buffer", "svb");
    try std.testing.expect(ok != null);
    try std.testing.expect(score("save-buffer", "zzz") == null);
}

test "fuzzy score prefers word boundaries" {
    const hyphen = score("save-buffer", "sb").?;
    const flat = score("savebuffer", "sb").?;
    try std.testing.expect(hyphen.score > flat.score);
}

test "fuzzy score prefers contiguous runs" {
    const contiguous = score("isearch", "sea").?;
    const gapped = score("s-e-a-r-c-h", "sea").?;
    try std.testing.expect(contiguous.score > gapped.score);
}
