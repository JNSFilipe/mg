const std = @import("std");
const buffer_mod = @import("buffer.zig");

const Buffer = buffer_mod.Buffer;

/// Cursor tracks both byte offset and preferred visual column for vertical motion.
/// This models mg behavior while making the state explicit in a typed struct.
pub const Cursor = struct {
    byte_index: usize = 0,
    preferred_column: ?usize = null,
};

/// Window is a view over a single buffer plus viewport state.
pub const Window = struct {
    buffer_id: usize,
    cursor: Cursor = .{},
    mark: ?usize = null,
    top_line: usize = 0,
    rows: usize,
    cols: usize,

    pub fn init(buffer_id: usize, rows: usize, cols: usize) Window {
        return .{
            .buffer_id = buffer_id,
            .rows = rows,
            .cols = cols,
        };
    }

    pub fn resize(self: *Window, rows: usize, cols: usize) void {
        self.rows = rows;
        self.cols = cols;
    }

    pub fn clampCursor(self: *Window, buffer: *const Buffer) void {
        if (self.cursor.byte_index > buffer.len()) {
            self.cursor.byte_index = buffer.len();
        }
        if (self.mark) |m| {
            if (m > buffer.len()) self.mark = buffer.len();
        }
        self.ensureCursorVisible(buffer);
    }

    pub fn moveLeft(self: *Window, buffer: *const Buffer) void {
        _ = buffer;
        if (self.cursor.byte_index > 0) {
            self.cursor.byte_index -= 1;
        }
        self.cursor.preferred_column = null;
    }

    pub fn moveRight(self: *Window, buffer: *const Buffer) void {
        if (self.cursor.byte_index < buffer.len()) {
            self.cursor.byte_index += 1;
        }
        self.cursor.preferred_column = null;
    }

    pub fn moveUp(self: *Window, buffer: *const Buffer) void {
        self.moveVertical(buffer, -1);
    }

    pub fn moveDown(self: *Window, buffer: *const Buffer) void {
        self.moveVertical(buffer, 1);
    }

    pub fn ensureCursorVisible(self: *Window, buffer: *const Buffer) void {
        const cursor_line = buffer.byteToLineCol(self.cursor.byte_index).line;
        if (cursor_line < self.top_line) {
            self.top_line = cursor_line;
            return;
        }

        if (self.rows == 0) return;
        const bottom = self.top_line + self.rows;
        if (cursor_line >= bottom) {
            self.top_line = cursor_line - self.rows + 1;
        }
    }

    fn moveVertical(self: *Window, buffer: *const Buffer, delta: isize) void {
        if (buffer.lineCount() == 0) return;

        const pos = buffer.byteToLineCol(self.cursor.byte_index);
        const target_col = self.cursor.preferred_column orelse pos.column;

        var next_line_signed: isize = @intCast(pos.line);
        next_line_signed += delta;

        if (next_line_signed < 0) {
            next_line_signed = 0;
        }
        const max_line: isize = @intCast(buffer.lineCount() - 1);
        if (next_line_signed > max_line) {
            next_line_signed = max_line;
        }

        const next_line: usize = @intCast(next_line_signed);
        self.cursor.byte_index = buffer.lineColToByte(next_line, target_col);
        self.cursor.preferred_column = target_col;
        self.ensureCursorVisible(buffer);
    }
};

test "window horizontal movement clamps" {
    var buffer = try Buffer.init(std.testing.allocator, "w");
    defer buffer.deinit();
    try buffer.setText("abc");

    var win = Window.init(0, 10, 80);

    win.moveLeft(&buffer);
    try std.testing.expectEqual(@as(usize, 0), win.cursor.byte_index);

    win.moveRight(&buffer);
    win.moveRight(&buffer);
    win.moveRight(&buffer);
    win.moveRight(&buffer);
    try std.testing.expectEqual(@as(usize, 3), win.cursor.byte_index);
}

test "window vertical movement keeps preferred column" {
    var buffer = try Buffer.init(std.testing.allocator, "w");
    defer buffer.deinit();
    try buffer.setText("12345\nxy\n123456\n");

    var win = Window.init(0, 10, 80);
    win.cursor.byte_index = buffer.lineColToByte(0, 4);

    win.moveDown(&buffer);
    const p1 = buffer.byteToLineCol(win.cursor.byte_index);
    try std.testing.expectEqual(@as(usize, 1), p1.line);
    try std.testing.expectEqual(@as(usize, 2), p1.column);

    win.moveDown(&buffer);
    const p2 = buffer.byteToLineCol(win.cursor.byte_index);
    try std.testing.expectEqual(@as(usize, 2), p2.line);
    try std.testing.expectEqual(@as(usize, 4), p2.column);
}
