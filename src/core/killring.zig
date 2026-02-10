const std = @import("std");

const Allocator = std.mem.Allocator;

/// Kill ring stores recent cuts with bounded memory and explicit ownership.
pub const KillRing = struct {
    allocator: Allocator,
    entries: std.array_list.Managed([]u8),
    max_entries: usize,

    pub fn init(allocator: Allocator, max_entries: usize) KillRing {
        return .{
            .allocator = allocator,
            .entries = std.array_list.Managed([]u8).init(allocator),
            .max_entries = max_entries,
        };
    }

    pub fn deinit(self: *KillRing) void {
        for (self.entries.items) |entry| {
            self.allocator.free(entry);
        }
        self.entries.deinit();
    }

    pub fn push(self: *KillRing, text: []const u8) !void {
        if (text.len == 0) return;

        const copy = try self.allocator.alloc(u8, text.len);
        std.mem.copyForwards(u8, copy, text);

        try self.entries.insert(0, copy);
        if (self.entries.items.len > self.max_entries) {
            const removed = self.entries.pop().?;
            self.allocator.free(removed);
        }
    }

    pub fn latest(self: *const KillRing) ?[]const u8 {
        if (self.entries.items.len == 0) return null;
        return self.entries.items[0];
    }

    pub fn count(self: *const KillRing) usize {
        return self.entries.items.len;
    }

    pub fn at(self: *const KillRing, index: usize) ?[]const u8 {
        if (index >= self.entries.items.len) return null;
        return self.entries.items[index];
    }

    pub fn appendToLatest(self: *KillRing, text: []const u8) !void {
        if (text.len == 0) return;
        if (self.entries.items.len == 0) {
            try self.push(text);
            return;
        }

        const old = self.entries.items[0];
        const merged = try self.allocator.alloc(u8, old.len + text.len);
        std.mem.copyForwards(u8, merged[0..old.len], old);
        std.mem.copyForwards(u8, merged[old.len..], text);
        self.entries.items[0] = merged;
        self.allocator.free(old);
    }

    pub fn prependToLatest(self: *KillRing, text: []const u8) !void {
        if (text.len == 0) return;
        if (self.entries.items.len == 0) {
            try self.push(text);
            return;
        }

        const old = self.entries.items[0];
        const merged = try self.allocator.alloc(u8, old.len + text.len);
        std.mem.copyForwards(u8, merged[0..text.len], text);
        std.mem.copyForwards(u8, merged[text.len..], old);
        self.entries.items[0] = merged;
        self.allocator.free(old);
    }
};

test "kill ring keeps latest first" {
    var ring = KillRing.init(std.testing.allocator, 3);
    defer ring.deinit();

    try ring.push("one");
    try ring.push("two");

    try std.testing.expectEqualStrings("two", ring.latest().?);
}

test "kill ring trims to max entries" {
    var ring = KillRing.init(std.testing.allocator, 2);
    defer ring.deinit();

    try ring.push("one");
    try ring.push("two");
    try ring.push("three");

    try std.testing.expectEqual(@as(usize, 2), ring.entries.items.len);
    try std.testing.expectEqualStrings("three", ring.entries.items[0]);
    try std.testing.expectEqualStrings("two", ring.entries.items[1]);
}

test "kill ring append and prepend latest entry" {
    var ring = KillRing.init(std.testing.allocator, 4);
    defer ring.deinit();

    try ring.push("mid");
    try ring.appendToLatest(" tail");
    try std.testing.expectEqualStrings("mid tail", ring.latest().?);

    try ring.prependToLatest("head ");
    try std.testing.expectEqualStrings("head mid tail", ring.latest().?);
}
