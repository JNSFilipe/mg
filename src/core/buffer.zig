const std = @import("std");

const Allocator = std.mem.Allocator;

/// Logical cursor position in line/column space.
pub const Position = struct {
    line: usize,
    column: usize,
};

/// Undo records store editor-level operations, not raw memory snapshots.
/// This differs from mg's linked-list undo internals and keeps semantics explicit.
pub const UndoOp = enum {
    insert,
    delete,
};

pub const UndoRecord = struct {
    op: UndoOp,
    at: usize,
    bytes: []u8,

    pub fn clone(allocator: Allocator, op: UndoOp, at: usize, data: []const u8) !UndoRecord {
        const copy = try allocator.alloc(u8, data.len);
        std.mem.copyForwards(u8, copy, data);
        return .{ .op = op, .at = at, .bytes = copy };
    }

    pub fn deinit(self: *UndoRecord, allocator: Allocator) void {
        allocator.free(self.bytes);
    }
};

/// Buffer stores UTF-8 bytes and a line-start index.
/// The line index is rebuilt after edits for clarity in this initial port.
pub const Buffer = struct {
    allocator: Allocator,
    name: std.array_list.Managed(u8),
    text: std.array_list.Managed(u8),
    line_starts: std.array_list.Managed(usize),
    undo_stack: std.array_list.Managed(UndoRecord),
    redo_stack: std.array_list.Managed(UndoRecord),

    pub fn init(allocator: Allocator, name: []const u8) !Buffer {
        var self = Buffer{
            .allocator = allocator,
            .name = std.array_list.Managed(u8).init(allocator),
            .text = std.array_list.Managed(u8).init(allocator),
            .line_starts = std.array_list.Managed(usize).init(allocator),
            .undo_stack = std.array_list.Managed(UndoRecord).init(allocator),
            .redo_stack = std.array_list.Managed(UndoRecord).init(allocator),
        };
        try self.name.appendSlice(name);
        try self.rebuildLineIndex();
        return self;
    }

    pub fn deinit(self: *Buffer) void {
        for (self.undo_stack.items) |*item| item.deinit(self.allocator);
        for (self.redo_stack.items) |*item| item.deinit(self.allocator);
        self.undo_stack.deinit();
        self.redo_stack.deinit();
        self.line_starts.deinit();
        self.text.deinit();
        self.name.deinit();
    }

    pub fn setText(self: *Buffer, data: []const u8) !void {
        self.text.clearRetainingCapacity();
        try self.text.appendSlice(data);
        self.clearUndoRedo();
        try self.rebuildLineIndex();
    }

    pub fn bytes(self: *const Buffer) []const u8 {
        return self.text.items;
    }

    pub fn len(self: *const Buffer) usize {
        return self.text.items.len;
    }

    pub fn lineCount(self: *const Buffer) usize {
        return self.line_starts.items.len;
    }

    pub fn lineSlice(self: *const Buffer, line: usize) []const u8 {
        const clamped_line = @min(line, self.lineCount() - 1);
        const start = self.line_starts.items[clamped_line];

        var end = self.text.items.len;
        if (clamped_line + 1 < self.line_starts.items.len) {
            end = self.line_starts.items[clamped_line + 1];
        }
        if (end > start and self.text.items[end - 1] == '\n') {
            end -= 1;
        }
        return self.text.items[start..end];
    }

    pub fn byteToLineCol(self: *const Buffer, at: usize) Position {
        const clamped = @min(at, self.text.items.len);

        var lo: usize = 0;
        var hi: usize = self.line_starts.items.len;
        while (lo < hi) {
            const mid = lo + (hi - lo) / 2;
            if (self.line_starts.items[mid] <= clamped) {
                lo = mid + 1;
            } else {
                hi = mid;
            }
        }

        const line = if (lo == 0) 0 else lo - 1;
        return .{
            .line = line,
            .column = clamped - self.line_starts.items[line],
        };
    }

    pub fn lineColToByte(self: *const Buffer, line: usize, column: usize) usize {
        const clamped_line = @min(line, self.lineCount() - 1);
        const start = self.line_starts.items[clamped_line];

        var logical_end = self.text.items.len;
        if (clamped_line + 1 < self.line_starts.items.len) {
            logical_end = self.line_starts.items[clamped_line + 1];
        }
        if (logical_end > start and self.text.items[logical_end - 1] == '\n') {
            logical_end -= 1;
        }

        const max_col = logical_end - start;
        return start + @min(column, max_col);
    }

    pub fn insert(self: *Buffer, at: usize, data: []const u8) !void {
        const pos = @min(at, self.text.items.len);
        try self.performInsert(pos, data);
        try self.undo_stack.append(try UndoRecord.clone(self.allocator, .delete, pos, data));
        self.clearRedo();
    }

    pub fn deleteRange(self: *Buffer, at: usize, count: usize) !void {
        const start = @min(at, self.text.items.len);
        const clamped_count = @min(count, self.text.items.len - start);
        if (clamped_count == 0) return;

        const deleted = try self.allocator.alloc(u8, clamped_count);
        std.mem.copyForwards(u8, deleted, self.text.items[start .. start + clamped_count]);

        try self.performDelete(start, clamped_count);
        try self.undo_stack.append(.{ .op = .insert, .at = start, .bytes = deleted });
        self.clearRedo();
    }

    pub fn undo(self: *Buffer) !bool {
        if (self.undo_stack.items.len == 0) return false;

        var record = self.undo_stack.pop().?;
        const inverse = try self.applyRecordAndInverse(record);
        try self.redo_stack.append(inverse);
        record.deinit(self.allocator);
        return true;
    }

    pub fn redo(self: *Buffer) !bool {
        if (self.redo_stack.items.len == 0) return false;

        var record = self.redo_stack.pop().?;
        const inverse = try self.applyRecordAndInverse(record);
        try self.undo_stack.append(inverse);
        record.deinit(self.allocator);
        return true;
    }

    fn applyRecordAndInverse(self: *Buffer, record: UndoRecord) !UndoRecord {
        switch (record.op) {
            .insert => {
                try self.performInsert(record.at, record.bytes);
                return UndoRecord.clone(self.allocator, .delete, record.at, record.bytes);
            },
            .delete => {
                try self.performDelete(record.at, record.bytes.len);
                return UndoRecord.clone(self.allocator, .insert, record.at, record.bytes);
            },
        }
    }

    fn clearUndoRedo(self: *Buffer) void {
        self.clearUndo();
        self.clearRedo();
    }

    fn clearUndo(self: *Buffer) void {
        for (self.undo_stack.items) |*item| item.deinit(self.allocator);
        self.undo_stack.clearRetainingCapacity();
    }

    fn clearRedo(self: *Buffer) void {
        for (self.redo_stack.items) |*item| item.deinit(self.allocator);
        self.redo_stack.clearRetainingCapacity();
    }

    fn performInsert(self: *Buffer, at: usize, data: []const u8) !void {
        const pos = @min(at, self.text.items.len);
        try self.text.insertSlice(pos, data);
        try self.rebuildLineIndex();
    }

    fn performDelete(self: *Buffer, at: usize, count: usize) !void {
        const start = @min(at, self.text.items.len);
        const clamped_count = @min(count, self.text.items.len - start);
        if (clamped_count == 0) return;

        const tail_start = start + clamped_count;
        const new_len = self.text.items.len - clamped_count;
        std.mem.copyForwards(u8, self.text.items[start..new_len], self.text.items[tail_start..]);
        self.text.shrinkRetainingCapacity(new_len);

        try self.rebuildLineIndex();
    }

    fn rebuildLineIndex(self: *Buffer) !void {
        self.line_starts.clearRetainingCapacity();
        try self.line_starts.append(0);

        for (self.text.items, 0..) |byte, idx| {
            if (byte == '\n' and idx + 1 <= self.text.items.len) {
                try self.line_starts.append(idx + 1);
            }
        }
    }
};

test "buffer insert/delete and line index" {
    var buffer = try Buffer.init(std.testing.allocator, "scratch");
    defer buffer.deinit();

    try buffer.insert(0, "hello\nworld");
    try std.testing.expectEqual(@as(usize, 2), buffer.lineCount());
    try std.testing.expectEqualStrings("hello", buffer.lineSlice(0));
    try std.testing.expectEqualStrings("world", buffer.lineSlice(1));

    try buffer.deleteRange(5, 1);
    try std.testing.expectEqual(@as(usize, 1), buffer.lineCount());
    try std.testing.expectEqualStrings("helloworld", buffer.lineSlice(0));
}

test "buffer undo redo" {
    var buffer = try Buffer.init(std.testing.allocator, "undo");
    defer buffer.deinit();

    try buffer.insert(0, "abc");
    try buffer.insert(3, "def");
    try std.testing.expectEqualStrings("abcdef", buffer.bytes());

    try std.testing.expect(try buffer.undo());
    try std.testing.expectEqualStrings("abc", buffer.bytes());

    try std.testing.expect(try buffer.redo());
    try std.testing.expectEqualStrings("abcdef", buffer.bytes());
}

test "buffer position conversion" {
    var buffer = try Buffer.init(std.testing.allocator, "pos");
    defer buffer.deinit();

    try buffer.setText("ab\nxyz\n");

    const p = buffer.byteToLineCol(4);
    try std.testing.expectEqual(@as(usize, 1), p.line);
    try std.testing.expectEqual(@as(usize, 1), p.column);

    const b = buffer.lineColToByte(1, 2);
    try std.testing.expectEqual(@as(usize, 5), b);
}
