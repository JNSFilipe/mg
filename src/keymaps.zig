const std = @import("std");
const keymap_core = @import("core/keymap.zig");

pub const Key = keymap_core.Key;
pub const Command = keymap_core.Command;
pub const DispatchResult = keymap_core.DispatchResult;
pub const KeyMap = keymap_core.KeyMap;

/// Source-as-config keybinding table.
/// Keep all default keychords and discoverable labels in this module.
pub fn bindAll(map: *KeyMap) !void {
    try map.addBinding(&.{.{ .ctrl = 'g' }}, .keyboard_quit);
    try map.addBinding(&.{.{ .ctrl = 'b' }}, .move_left);
    try map.addBinding(&.{.{ .ctrl = 'f' }}, .move_right);
    try map.addBinding(&.{.{ .ctrl = 'p' }}, .move_up);
    try map.addBinding(&.{.{ .ctrl = 'n' }}, .move_down);
    try map.addBinding(&.{.{ .ctrl = 'a' }}, .move_line_start);
    try map.addBinding(&.{.{ .ctrl = 'e' }}, .move_line_end);
    try map.addBinding(&.{.{ .meta = '<' }}, .move_buffer_start);
    try map.addBinding(&.{.{ .meta = '>' }}, .move_buffer_end);
    try map.addBinding(&.{.{ .ctrl = 'v' }}, .page_down);
    try map.addBinding(&.{.{ .meta = 'v' }}, .page_up);
    try map.addBinding(&.{.{ .meta = 'f' }}, .move_word_forward);
    try map.addBinding(&.{.{ .meta = 'b' }}, .move_word_backward);
    try map.addBinding(&.{ .{ .meta = 'g' }, .{ .char = 'g' } }, .goto_line_prompt);
    try map.addBinding(&.{ .{ .meta = 'g' }, .{ .meta = 'g' } }, .goto_line_prompt);
    try map.addBinding(&.{.{ .meta = 'x' }}, .command_prompt);
    try map.addBinding(&.{.{ .ctrl = '@' }}, .set_mark);
    try map.addBinding(&.{ .{ .ctrl = 'u' }, .{ .ctrl = '@' } }, .pop_mark);

    try map.addBinding(&.{.arrow_left}, .move_left);
    try map.addBinding(&.{.arrow_right}, .move_right);
    try map.addBinding(&.{.arrow_up}, .move_up);
    try map.addBinding(&.{.arrow_down}, .move_down);

    try map.addBinding(&.{.backspace}, .delete_backward);
    try map.addBinding(&.{.{ .ctrl = 'd' }}, .delete_forward);
    try map.addBinding(&.{.{ .meta = 'd' }}, .kill_word);
    try map.addBinding(&.{.meta_backspace}, .backward_kill_word);
    try map.addBinding(&.{.enter}, .insert_newline);
    try map.addBinding(&.{.{ .ctrl = 'o' }}, .open_line);
    try map.addBinding(&.{.{ .ctrl = 't' }}, .transpose_chars);
    try map.addBinding(&.{.{ .meta = 't' }}, .transpose_words);
    try map.addBinding(&.{.{ .ctrl = 'w' }}, .kill_region);
    try map.addBinding(&.{.{ .meta = 'w' }}, .copy_region_as_kill);
    try map.addBinding(&.{ .{ .ctrl = 'c' }, .{ .char = 'w' } }, .copy_region_as_kill);
    try map.addBinding(&.{.{ .ctrl = 'k' }}, .kill_line);
    try map.addBinding(&.{.{ .ctrl = 'y' }}, .yank);
    try map.addBinding(&.{.{ .meta = 'y' }}, .yank_pop);
    try map.addBinding(&.{.{ .ctrl = 's' }}, .search_forward);
    try map.addBinding(&.{.{ .ctrl = 'r' }}, .search_backward);
    try map.addBinding(&.{.{ .ctrl = 'q' }}, .quoted_insert);

    try map.addBinding(&.{.{ .ctrl = '_' }}, .undo);
    try map.addBinding(&.{ .{ .ctrl = 'x' }, .{ .char = 'u' } }, .undo);
    try map.addBinding(&.{ .{ .ctrl = 'x' }, .{ .ctrl = 'b' } }, .list_buffers);
    try map.addBinding(&.{ .{ .ctrl = 'x' }, .{ .char = 'b' } }, .buffer_prompt);
    try map.addBinding(&.{ .{ .ctrl = 'x' }, .arrow_right }, .next_buffer);
    try map.addBinding(&.{ .{ .ctrl = 'x' }, .arrow_left }, .previous_buffer);
    try map.addBinding(&.{ .{ .ctrl = 'x' }, .{ .char = '2' } }, .split_window_below);
    try map.addBinding(&.{ .{ .ctrl = 'x' }, .{ .char = '3' } }, .split_window_right);
    try map.addBinding(&.{ .{ .ctrl = 'x' }, .{ .char = '0' } }, .delete_window);
    try map.addBinding(&.{ .{ .ctrl = 'x' }, .{ .char = '1' } }, .delete_other_windows);
    try map.addBinding(&.{ .{ .ctrl = 'x' }, .{ .char = 'o' } }, .other_window);
    try map.addBinding(&.{.{ .meta = '%' }}, .query_replace_prompt);
    try map.addBinding(&.{.{ .meta_ctrl = '5' }}, .query_replace_regexp_prompt);
    try map.addBinding(&.{ .{ .ctrl = 'x' }, .{ .ctrl = 'x' } }, .exchange_point_and_mark);
    try map.addBinding(&.{ .{ .ctrl = 'x' }, .{ .char = 'h' } }, .mark_whole_buffer);
    try map.addBinding(&.{ .{ .ctrl = 'x' }, .{ .char = 'k' } }, .kill_buffer_prompt);
    try map.addBinding(&.{ .{ .ctrl = 'x' }, .{ .ctrl = 'f' } }, .open_file_prompt);
    try map.addBinding(&.{ .{ .ctrl = 'x' }, .{ .ctrl = 'w' } }, .save_as_prompt);
    try map.addBinding(&.{ .{ .ctrl = 'x' }, .{ .ctrl = 's' } }, .save);
    try map.addBinding(&.{ .{ .ctrl = 'x' }, .{ .char = 's' } }, .save_some_buffers);
    try map.addBinding(&.{ .{ .ctrl = 'x' }, .{ .ctrl = 'c' } }, .save_buffers_kill_terminal);
}

/// Which-key and status discoverability labels.
pub fn commandLabel(command: Command) []const u8 {
    return switch (command) {
        .keyboard_quit => "keyboard-quit",
        .move_left => "backward-char",
        .move_right => "forward-char",
        .move_up => "previous-line",
        .move_down => "next-line",
        .move_line_start => "beginning-of-line",
        .move_line_end => "end-of-line",
        .move_buffer_start => "beginning-of-buffer",
        .move_buffer_end => "end-of-buffer",
        .page_up => "scroll-down-command",
        .page_down => "scroll-up-command",
        .move_word_forward => "forward-word",
        .move_word_backward => "backward-word",
        .goto_line_prompt => "goto-line",
        .replace_string_prompt => "replace-string",
        .query_replace_prompt => "query-replace",
        .replace_regexp_prompt => "replace-regexp",
        .query_replace_regexp_prompt => "query-replace-regexp",
        .command_prompt => "M-x",
        .open_file_prompt => "find-file",
        .save_as_prompt => "write-file",
        .buffer_prompt => "switch-buffer",
        .list_buffers => "list-buffers",
        .kill_buffer_prompt => "kill-buffer",
        .split_window_below => "split-window-below",
        .split_window_right => "split-window-right",
        .delete_window => "delete-window",
        .delete_other_windows => "delete-other-windows",
        .other_window => "other-window",
        .search_forward => "isearch-forward",
        .search_backward => "isearch-backward",
        .quoted_insert => "quoted-insert",
        .delete_backward => "delete-backward-char",
        .delete_forward => "delete-char",
        .kill_word => "kill-word",
        .backward_kill_word => "backward-kill-word",
        .insert_newline => "newline",
        .open_line => "open-line",
        .transpose_chars => "transpose-chars",
        .transpose_words => "transpose-words",
        .set_mark => "set-mark-command",
        .pop_mark => "pop-mark",
        .exchange_point_and_mark => "exchange-point-and-mark",
        .kill_region => "kill-region",
        .copy_region_as_kill => "copy-region-as-kill",
        .mark_whole_buffer => "mark-whole-buffer",
        .kill_line => "kill-line",
        .yank => "yank",
        .yank_pop => "yank-pop",
        .undo => "undo",
        .redo => "redo",
        .next_buffer => "next-buffer",
        .previous_buffer => "previous-buffer",
        .save => "save-buffer",
        .save_buffers_kill_terminal => "save-buffers-kill-terminal",
        .save_some_buffers => "save-some-buffers",
        .quit => "quit",
    };
}

test "default bindings keep Emacs core chords" {
    var map = KeyMap.init(std.testing.allocator);
    defer map.deinit();
    try bindAll(&map);

    try std.testing.expectEqual(DispatchResult{ .command = .quoted_insert }, map.dispatch(&.{.{ .ctrl = 'q' }}));
    try std.testing.expectEqual(DispatchResult{ .command = .command_prompt }, map.dispatch(&.{.{ .meta = 'x' }}));
    try std.testing.expectEqual(DispatchResult{ .command = .save_buffers_kill_terminal }, map.dispatch(&.{ .{ .ctrl = 'x' }, .{ .ctrl = 'c' } }));
    try std.testing.expectEqual(DispatchResult{ .command = .save_some_buffers }, map.dispatch(&.{ .{ .ctrl = 'x' }, .{ .char = 's' } }));
    try std.testing.expectEqual(DispatchResult{ .command = .list_buffers }, map.dispatch(&.{ .{ .ctrl = 'x' }, .{ .ctrl = 'b' } }));
    try std.testing.expectEqual(DispatchResult{ .command = .buffer_prompt }, map.dispatch(&.{ .{ .ctrl = 'x' }, .{ .char = 'b' } }));
    try std.testing.expectEqual(DispatchResult{ .command = .next_buffer }, map.dispatch(&.{ .{ .ctrl = 'x' }, .arrow_right }));
    try std.testing.expectEqual(DispatchResult{ .command = .previous_buffer }, map.dispatch(&.{ .{ .ctrl = 'x' }, .arrow_left }));
    try std.testing.expectEqual(DispatchResult{ .command = .pop_mark }, map.dispatch(&.{ .{ .ctrl = 'u' }, .{ .ctrl = '@' } }));
    try std.testing.expectEqual(DispatchResult.prefix, map.dispatch(&.{.{ .meta = 'g' }}));
    try std.testing.expectEqual(DispatchResult{ .command = .goto_line_prompt }, map.dispatch(&.{ .{ .meta = 'g' }, .{ .char = 'g' } }));
    try std.testing.expectEqual(DispatchResult{ .command = .goto_line_prompt }, map.dispatch(&.{ .{ .meta = 'g' }, .{ .meta = 'g' } }));
}
