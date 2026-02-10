const std = @import("std");

const Allocator = std.mem.Allocator;

/// Key is a semantic event, not a raw byte.
/// This decouples terminal decoding from command dispatch.
pub const Key = union(enum) {
    char: u8,
    ctrl: u8,
    meta: u8,
    meta_ctrl: u8,
    meta_backspace,
    tab,
    enter,
    backspace,
    escape,
    arrow_up,
    arrow_down,
    arrow_left,
    arrow_right,
};

pub const Command = enum {
    keyboard_quit,
    move_left,
    move_right,
    move_up,
    move_down,
    move_line_start,
    move_line_end,
    move_buffer_start,
    move_buffer_end,
    page_up,
    page_down,
    move_word_forward,
    move_word_backward,
    goto_line_prompt,
    command_prompt,
    open_file_prompt,
    save_as_prompt,
    buffer_prompt,
    list_buffers,
    kill_buffer_prompt,
    replace_string_prompt,
    query_replace_prompt,
    replace_regexp_prompt,
    query_replace_regexp_prompt,
    split_window_below,
    split_window_right,
    delete_window,
    delete_other_windows,
    other_window,
    search_forward,
    search_backward,
    quoted_insert,
    delete_backward,
    delete_forward,
    kill_word,
    backward_kill_word,
    insert_newline,
    open_line,
    transpose_chars,
    transpose_words,
    set_mark,
    pop_mark,
    exchange_point_and_mark,
    kill_region,
    copy_region_as_kill,
    mark_whole_buffer,
    kill_line,
    yank,
    yank_pop,
    undo,
    redo,
    next_buffer,
    previous_buffer,
    save,
    save_buffers_kill_terminal,
    save_some_buffers,
    quit,
};

pub const DispatchResult = union(enum) {
    none,
    prefix,
    command: Command,
};

pub const Binding = struct {
    keys: []Key,
    command: Command,
};

/// Keymap is intentionally data-driven.
/// mg uses static C tables; Zig can keep runtime-configurable bindings with owned slices.
pub const KeyMap = struct {
    allocator: Allocator,
    bindings: std.array_list.Managed(Binding),

    pub fn init(allocator: Allocator) KeyMap {
        return .{
            .allocator = allocator,
            .bindings = std.array_list.Managed(Binding).init(allocator),
        };
    }

    pub fn deinit(self: *KeyMap) void {
        for (self.bindings.items) |binding| {
            self.allocator.free(binding.keys);
        }
        self.bindings.deinit();
    }

    pub fn addBinding(self: *KeyMap, sequence: []const Key, command: Command) !void {
        const owned = try self.allocator.alloc(Key, sequence.len);
        std.mem.copyForwards(Key, owned, sequence);
        try self.bindings.append(.{
            .keys = owned,
            .command = command,
        });
    }

    pub fn dispatch(self: *const KeyMap, pending: []const Key) DispatchResult {
        var saw_prefix = false;
        for (self.bindings.items) |binding| {
            if (sequenceEql(binding.keys, pending)) {
                return .{ .command = binding.command };
            }
            if (isPrefix(pending, binding.keys)) {
                saw_prefix = true;
            }
        }

        if (saw_prefix) {
            return .prefix;
        }
        return .none;
    }

    fn sequenceEql(a: []const Key, b: []const Key) bool {
        if (a.len != b.len) return false;
        for (a, b) |lhs, rhs| {
            if (!std.meta.eql(lhs, rhs)) return false;
        }
        return true;
    }

    fn isPrefix(prefix: []const Key, whole: []const Key) bool {
        if (prefix.len > whole.len) return false;
        for (prefix, 0..) |k, idx| {
            if (!std.meta.eql(k, whole[idx])) return false;
        }
        return true;
    }
};

test "keymap dispatches exact command" {
    var map = KeyMap.init(std.testing.allocator);
    defer map.deinit();

    try map.addBinding(&.{.{ .ctrl = 'q' }}, .quit);
    const result = map.dispatch(&.{.{ .ctrl = 'q' }});
    try std.testing.expectEqual(DispatchResult{ .command = .quit }, result);
}

test "keymap recognizes prefixes" {
    var map = KeyMap.init(std.testing.allocator);
    defer map.deinit();

    try map.addBinding(&.{ .{ .ctrl = 'x' }, .{ .ctrl = 's' } }, .save);
    try std.testing.expectEqual(DispatchResult.prefix, map.dispatch(&.{.{ .ctrl = 'x' }}));
    try std.testing.expectEqual(DispatchResult{ .command = .save }, map.dispatch(&.{ .{ .ctrl = 'x' }, .{ .ctrl = 's' } }));
    try std.testing.expectEqual(DispatchResult.none, map.dispatch(&.{.{ .ctrl = 'q' }}));
}

test "keymap dispatches C-M chord" {
    var map = KeyMap.init(std.testing.allocator);
    defer map.deinit();

    try map.addBinding(&.{.{ .meta_ctrl = '5' }}, .query_replace_regexp_prompt);
    const result = map.dispatch(&.{.{ .meta_ctrl = '5' }});
    try std.testing.expectEqual(DispatchResult{ .command = .query_replace_regexp_prompt }, result);
}
