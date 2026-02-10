const std = @import("std");
const editor_mod = @import("core/editor.zig");
const term_mod = @import("core/term.zig");

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer {
        const status = gpa.deinit();
        if (status == .leak) {
            std.log.err("memory leak detected", .{});
        }
    }
    const allocator = gpa.allocator();

    var term = term_mod.Terminal.init(allocator);
    defer term.deinit();

    try term.enterRawMode();
    defer term.leaveRawMode() catch {};

    try term.enterAlternateScreen();
    try term.flush();
    defer {
        term.showCursor() catch {};
        term.leaveAlternateScreen() catch {};
        term.flush() catch {};
    }

    var editor = try editor_mod.Editor.init(allocator, term.size());
    defer editor.deinit();

    const args = try std.process.argsAlloc(allocator);
    defer std.process.argsFree(allocator, args);

    if (args.len > 1) {
        var idx: usize = 1;
        while (idx < args.len) : (idx += 1) {
            try editor.openFile(args[idx]);
        }
    }

    while (!editor.shouldQuit()) {
        try editor.draw(&term);
        const key = try term.readKey();
        try editor.handleKey(key);
    }
}
