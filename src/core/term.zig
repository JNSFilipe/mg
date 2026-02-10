const std = @import("std");
const keymap = @import("keymap.zig");

const c = @cImport({
    @cInclude("sys/ioctl.h");
    @cInclude("termios.h");
    @cInclude("unistd.h");
});

const Allocator = std.mem.Allocator;

pub const Size = struct {
    rows: usize,
    cols: usize,
};

/// Terminal wraps raw-mode lifecycle and ANSI output batching.
/// Unlike mg's global tty state, this keeps ownership and cleanup local.
pub const Terminal = struct {
    allocator: Allocator,
    stdin_file: std.fs.File,
    stdout_file: std.fs.File,
    out_buffer: std.array_list.Managed(u8),
    original_termios: ?c.struct_termios = null,
    raw_mode_enabled: bool = false,

    pub fn init(allocator: Allocator) Terminal {
        return .{
            .allocator = allocator,
            .stdin_file = std.fs.File.stdin(),
            .stdout_file = std.fs.File.stdout(),
            .out_buffer = std.array_list.Managed(u8).init(allocator),
        };
    }

    pub fn deinit(self: *Terminal) void {
        if (self.raw_mode_enabled) {
            self.leaveRawMode() catch {};
        }
        self.out_buffer.deinit();
    }

    pub fn enterRawMode(self: *Terminal) !void {
        if (self.raw_mode_enabled) return;

        var current: c.struct_termios = undefined;
        if (c.tcgetattr(c.STDIN_FILENO, &current) != 0) return error.TermiosReadFailed;

        self.original_termios = current;
        var raw = current;
        const IFlag = @TypeOf(raw.c_iflag);
        const OFlag = @TypeOf(raw.c_oflag);
        const CFlag = @TypeOf(raw.c_cflag);
        const LFlag = @TypeOf(raw.c_lflag);

        const iflag_mask: IFlag = @intCast(c.BRKINT | c.ICRNL | c.INPCK | c.ISTRIP | c.IXON);
        const oflag_mask: OFlag = @intCast(c.OPOST);
        const cflag_mask: CFlag = @intCast(c.CS8);
        const lflag_mask: LFlag = @intCast(c.ECHO | c.ICANON | c.IEXTEN | c.ISIG);

        raw.c_iflag &= ~iflag_mask;
        raw.c_oflag &= ~oflag_mask;
        raw.c_cflag |= cflag_mask;
        raw.c_lflag &= ~lflag_mask;
        raw.c_cc[c.VMIN] = 1;
        raw.c_cc[c.VTIME] = 0;

        if (c.tcsetattr(c.STDIN_FILENO, c.TCSAFLUSH, &raw) != 0) return error.TermiosWriteFailed;
        self.raw_mode_enabled = true;
    }

    pub fn leaveRawMode(self: *Terminal) !void {
        if (!self.raw_mode_enabled) return;
        if (self.original_termios) |orig| {
            var restore = orig;
            if (c.tcsetattr(c.STDIN_FILENO, c.TCSAFLUSH, &restore) != 0) {
                return error.TermiosWriteFailed;
            }
        }
        self.raw_mode_enabled = false;
    }

    pub fn size(self: *const Terminal) Size {
        _ = self;
        var ws: c.struct_winsize = undefined;
        if (c.ioctl(c.STDOUT_FILENO, c.TIOCGWINSZ, &ws) == 0 and ws.ws_col > 0 and ws.ws_row > 0) {
            return .{
                .rows = @as(usize, @intCast(ws.ws_row)),
                .cols = @as(usize, @intCast(ws.ws_col)),
            };
        }
        return .{ .rows = 24, .cols = 80 };
    }

    pub fn enterAlternateScreen(self: *Terminal) !void {
        try self.queue("\x1b[?1049h\x1b[H");
    }

    pub fn leaveAlternateScreen(self: *Terminal) !void {
        try self.queue("\x1b[?1049l");
    }

    pub fn hideCursor(self: *Terminal) !void {
        try self.queue("\x1b[?25l");
    }

    pub fn showCursor(self: *Terminal) !void {
        try self.queue("\x1b[?25h");
    }

    pub fn clearScreen(self: *Terminal) !void {
        try self.queue("\x1b[2J");
        try self.moveCursor(1, 1);
    }

    pub fn moveCursor(self: *Terminal, row: usize, col: usize) !void {
        try self.out_buffer.writer().print("\x1b[{d};{d}H", .{ row, col });
    }

    pub fn queue(self: *Terminal, bytes: []const u8) !void {
        try self.out_buffer.appendSlice(bytes);
    }

    pub fn flush(self: *Terminal) !void {
        if (self.out_buffer.items.len == 0) return;
        try self.stdout_file.writeAll(self.out_buffer.items);
        self.out_buffer.clearRetainingCapacity();
    }

    pub fn readKey(self: *Terminal) !keymap.Key {
        var first_buf: [1]u8 = undefined;
        const first_count = try self.stdin_file.read(first_buf[0..]);
        if (first_count == 0) return error.EndOfStream;
        const first = first_buf[0];

        if (first == 0x1b) {
            var second_buf: [1]u8 = undefined;
            const got_second = try self.stdin_file.read(second_buf[0..]);
            if (got_second == 0) return .escape;

            if (second_buf[0] == '[') {
                var third_buf: [1]u8 = undefined;
                const got_third = try self.stdin_file.read(third_buf[0..]);
                if (got_third == 0) return .escape;
                const data = [3]u8{ 0x1b, '[', third_buf[0] };
                return decodeBytes(data[0..]);
            }

            const data = [2]u8{ 0x1b, second_buf[0] };
            return decodeBytes(data[0..]);
        }

        return decodeBytes(&.{first});
    }
};

/// Decodes a terminal byte sequence into a key event.
/// This parser is intentionally minimal in the skeleton and can be extended as needed.
pub fn decodeBytes(bytes: []const u8) keymap.Key {
    if (bytes.len == 0) return .escape;

    const first = bytes[0];
    if (first == 0x1b) {
        if (bytes.len >= 3 and bytes[1] == '[') {
            return switch (bytes[2]) {
                'A' => .arrow_up,
                'B' => .arrow_down,
                'C' => .arrow_right,
                'D' => .arrow_left,
                else => .escape,
            };
        }
        if (bytes.len == 2) {
            const second = bytes[1];
            if (second == 0x7f or second == 0x08) return .meta_backspace;
            if (decodeCtrlLogical(second)) |ctrl| return .{ .meta_ctrl = ctrl };
            if (std.ascii.isPrint(second)) return .{ .meta = std.ascii.toLower(second) };
        }
        return .escape;
    }

    if (first == '\r' or first == '\n') return .enter;
    if (first == '\t') return .tab;
    if (first == 0x7f or first == 0x08) return .backspace;
    if (decodeCtrlLogical(first)) |ctrl| return .{ .ctrl = ctrl };

    return .{ .char = first };
}

fn decodeCtrlLogical(byte: u8) ?u8 {
    return switch (byte) {
        0x00 => '@',
        0x01...0x1a => @as(u8, 'a') + byte - 1,
        // Terminals often encode C-5/C-% as 0x1d.
        0x1d => '5',
        0x1e => '6',
        0x1f => '_',
        else => null,
    };
}

test "decode ascii and control keys" {
    try std.testing.expectEqual(keymap.Key{ .char = 'x' }, decodeBytes(&.{'x'}));
    try std.testing.expectEqual(keymap.Key.tab, decodeBytes(&.{'\t'}));
    try std.testing.expectEqual(keymap.Key{ .ctrl = '@' }, decodeBytes(&.{0x00}));
    try std.testing.expectEqual(keymap.Key{ .ctrl = 'q' }, decodeBytes(&.{0x11}));
    try std.testing.expectEqual(keymap.Key{ .ctrl = '_' }, decodeBytes(&.{0x1f}));
    try std.testing.expectEqual(keymap.Key.backspace, decodeBytes(&.{0x7f}));
    try std.testing.expectEqual(keymap.Key{ .meta = 'f' }, decodeBytes("\x1bf"));
    try std.testing.expectEqual(keymap.Key{ .meta_ctrl = 'q' }, decodeBytes(&.{ 0x1b, 0x11 }));
    try std.testing.expectEqual(keymap.Key{ .meta_ctrl = '5' }, decodeBytes(&.{ 0x1b, 0x1d }));
    try std.testing.expectEqual(keymap.Key.meta_backspace, decodeBytes(&.{ 0x1b, 0x7f }));
}

test "decode arrow keys" {
    try std.testing.expectEqual(keymap.Key.arrow_up, decodeBytes("\x1b[A"));
    try std.testing.expectEqual(keymap.Key.arrow_down, decodeBytes("\x1b[B"));
    try std.testing.expectEqual(keymap.Key.arrow_right, decodeBytes("\x1b[C"));
    try std.testing.expectEqual(keymap.Key.arrow_left, decodeBytes("\x1b[D"));
}

test "terminal queues ansi output" {
    var term = Terminal.init(std.testing.allocator);
    defer term.deinit();

    try term.clearScreen();
    try term.hideCursor();
    try std.testing.expect(term.out_buffer.items.len > 0);
}
