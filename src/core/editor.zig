const builtin = @import("builtin");
const std = @import("std");
const keymaps_mod = @import("../keymaps.zig");
const buffer_mod = @import("buffer.zig");
const fileio_mod = @import("fileio.zig");
const keymap_mod = @import("keymap.zig");
const killring_mod = @import("killring.zig");
const fuzzy_mod = @import("../extensions/fuzzy.zig");
const search_mod = @import("search.zig");
const syntax_mod = @import("../extensions/syntax.zig");
const term_mod = @import("term.zig");
const window_mod = @import("window.zig");
const c = @cImport({
    @cInclude("regex.h");
});

const Allocator = std.mem.Allocator;
const Buffer = buffer_mod.Buffer;
const FileIO = fileio_mod.FileIO;
const Key = keymap_mod.Key;
const KeyMap = keymap_mod.KeyMap;
const KillRing = killring_mod.KillRing;
const Command = keymap_mod.Command;
const SyntaxEngine = syntax_mod.SyntaxEngine;
const Window = window_mod.Window;

const MinibufferKind = enum {
    open_file,
    save_as,
    switch_buffer,
    kill_buffer,
    kill_buffer_confirm,
    quit_confirm,
    goto_line,
    command,
    replace_from,
    replace_to,
    query_replace_from,
    query_replace_to,
    replace_regexp_from,
    replace_regexp_to,
    query_replace_regexp_from,
    query_replace_regexp_to,
};

const MinibufferState = struct {
    kind: MinibufferKind,
    history_index: ?usize = null,
    tab_count: usize = 0,
    ido_index: usize = 0,
};

const ISearchState = struct {
    direction: search_mod.Direction,
    anchor: usize,
    matched: bool = true,
};

const QueryReplaceState = struct {
    kind: enum {
        literal,
        regexp,
    },
    from: []u8,
    to: []u8,
    current_start: usize,
    current_end: usize,
    captures: [10]?ByteRange = [_]?ByteRange{null} ** 10,
    replaced: usize = 0,
    skipped: usize = 0,
};

const SaveSomeBuffersState = struct {
    const Phase = enum {
        decision,
        write_path,
    };

    dirty_indices: std.array_list.Managed(usize),
    path_input: std.array_list.Managed(u8),
    phase: Phase = .decision,
    index: usize = 0,
    saved: usize = 0,
    skipped: usize = 0,
    failed: usize = 0,
    quit_after: bool = false,
    canceled: bool = false,
    auto_yes_remaining: bool = false,
};

const SaveBufferOutcome = enum {
    saved,
    needs_path,
};

const InputMode = union(enum) {
    normal,
    minibuffer: MinibufferState,
    isearch: ISearchState,
    query_replace: QueryReplaceState,
    save_some_buffers: SaveSomeBuffersState,
    quoted_insert,
};

const NamedCommandId = enum {
    keyboard_quit,
    find_file,
    write_file,
    save_buffer,
    save_some_buffers,
    switch_buffer,
    list_buffers,
    kill_buffer,
    next_buffer,
    previous_buffer,
    split_window_below,
    split_window_right,
    delete_window,
    delete_other_windows,
    other_window,
    beginning_of_line,
    end_of_line,
    beginning_of_buffer,
    end_of_buffer,
    scroll_up_command,
    scroll_down_command,
    goto_line,
    replace_string,
    query_replace,
    replace_regexp,
    query_replace_regexp,
    forward_word,
    backward_word,
    delete_char,
    kill_word,
    backward_kill_word,
    open_line,
    transpose_chars,
    transpose_words,
    set_mark_command,
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
    isearch_forward,
    isearch_backward,
    quoted_insert,
    save_buffers_kill_terminal,
    quit,
};

const CommandSpec = struct {
    id: NamedCommandId,
    name: []const u8,
    aliases: []const []const u8,
    description: []const u8,
};

const CommandMatch = struct {
    spec: *const CommandSpec,
    score: fuzzy_mod.MatchScore,
};

const BufferMatch = struct {
    buffer_index: usize,
    name: []const u8,
    score: fuzzy_mod.MatchScore,
};

const PathMatch = struct {
    name_len: usize,
    name: [std.fs.max_name_bytes]u8,
    is_dir: bool,
    score: fuzzy_mod.MatchScore,

    fn slice(self: *const PathMatch) []const u8 {
        return self.name[0..self.name_len];
    }
};

const PathInput = struct {
    dir_prefix: []const u8,
    dir_path: []const u8,
    base: []const u8,
};

const PrefixOption = struct {
    key: Key,
    command: ?Command = null,
    has_more: bool = false,
};

const ByteRange = struct {
    start: usize,
    end: usize,
};

const SplitAxis = enum {
    horizontal,
    vertical,
};

const SplitNodeKind = union(enum) {
    leaf: usize, // window index
    split: struct {
        axis: SplitAxis,
        first: usize, // child node index
        second: usize, // child node index
    },
};

const SplitNode = struct {
    parent: ?usize = null,
    kind: SplitNodeKind,
};

const WindowLayout = struct {
    top_row: usize, // 1-based terminal row
    left_col: usize, // 1-based terminal col
    body_rows: usize,
    body_cols: usize,
    modeline_row: ?usize, // 1-based terminal row; null means no per-window modeline
    separator_col: ?usize, // vertical separator drawn after this pane
};

const Rect = struct {
    top_row: usize,
    left_col: usize,
    rows: usize,
    cols: usize,
};

const VerticalSeparator = struct {
    col: usize,
    top_row: usize,
    bottom_row: usize,
};

const YankState = struct {
    at: usize,
    len: usize,
    ring_index: usize,
};

const command_specs = [_]CommandSpec{
    .{ .id = .keyboard_quit, .name = "keyboard-quit", .aliases = &.{ "cancel", "abort" }, .description = "Cancel the current operation" },
    .{ .id = .find_file, .name = "find-file", .aliases = &.{ "ff", "open" }, .description = "Open a file into a new buffer" },
    .{ .id = .write_file, .name = "write-file", .aliases = &.{ "wf", "save-as" }, .description = "Save active buffer to a new path" },
    .{ .id = .save_buffer, .name = "save-buffer", .aliases = &.{"save"}, .description = "Save active buffer to current file" },
    .{ .id = .save_some_buffers, .name = "save-some-buffers", .aliases = &.{ "ssb", "save-modified-buffers" }, .description = "Iterate through modified buffers and save selectively" },
    .{ .id = .save_buffers_kill_terminal, .name = "save-buffers-kill-terminal", .aliases = &.{ "sbkt", "exit-editor" }, .description = "Save modified buffers, then exit editor" },
    .{ .id = .switch_buffer, .name = "switch-buffer", .aliases = &.{ "sb", "buffer" }, .description = "Switch to another buffer via IDO list" },
    .{ .id = .list_buffers, .name = "list-buffers", .aliases = &.{ "lb", "buffers" }, .description = "Show *Buffer List* in current window" },
    .{ .id = .kill_buffer, .name = "kill-buffer", .aliases = &.{ "kb", "close-buffer" }, .description = "Kill a buffer" },
    .{ .id = .next_buffer, .name = "next-buffer", .aliases = &.{"nb"}, .description = "Switch to the next buffer" },
    .{ .id = .previous_buffer, .name = "previous-buffer", .aliases = &.{ "pb", "prev-buffer" }, .description = "Switch to the previous buffer" },
    .{ .id = .split_window_below, .name = "split-window-below", .aliases = &.{ "swb", "split-window" }, .description = "Split current window into two stacked windows" },
    .{ .id = .split_window_right, .name = "split-window-right", .aliases = &.{ "swr", "vertical-split" }, .description = "Split current window into two side-by-side windows" },
    .{ .id = .delete_window, .name = "delete-window", .aliases = &.{ "dw", "close-window" }, .description = "Delete current window" },
    .{ .id = .delete_other_windows, .name = "delete-other-windows", .aliases = &.{ "do", "one-window" }, .description = "Keep only current window" },
    .{ .id = .other_window, .name = "other-window", .aliases = &.{ "ow", "cycle-window" }, .description = "Cycle to the next window" },
    .{ .id = .beginning_of_line, .name = "beginning-of-line", .aliases = &.{"bol"}, .description = "Move cursor to beginning of line" },
    .{ .id = .end_of_line, .name = "end-of-line", .aliases = &.{"eol"}, .description = "Move cursor to end of line" },
    .{ .id = .beginning_of_buffer, .name = "beginning-of-buffer", .aliases = &.{ "bob", "top" }, .description = "Move cursor to start of buffer" },
    .{ .id = .end_of_buffer, .name = "end-of-buffer", .aliases = &.{ "eob", "bottom" }, .description = "Move cursor to end of buffer" },
    .{ .id = .scroll_up_command, .name = "scroll-up-command", .aliases = &.{ "su", "page-down" }, .description = "Scroll forward by one page" },
    .{ .id = .scroll_down_command, .name = "scroll-down-command", .aliases = &.{ "sd", "page-up" }, .description = "Scroll backward by one page" },
    .{ .id = .goto_line, .name = "goto-line", .aliases = &.{ "gl", "line" }, .description = "Move point to a line number" },
    .{ .id = .replace_string, .name = "replace-string", .aliases = &.{ "rs", "replace" }, .description = "Replace all literal matches from point to end" },
    .{ .id = .query_replace, .name = "query-replace", .aliases = &.{ "qr", "replace-query" }, .description = "Interactively replace matches one by one" },
    .{ .id = .replace_regexp, .name = "replace-regexp", .aliases = &.{ "rr", "re-replace" }, .description = "Replace all regexp matches from point to end" },
    .{ .id = .query_replace_regexp, .name = "query-replace-regexp", .aliases = &.{ "qrr", "re-query-replace" }, .description = "Interactively replace regexp matches" },
    .{ .id = .forward_word, .name = "forward-word", .aliases = &.{"fw"}, .description = "Move forward over one word" },
    .{ .id = .backward_word, .name = "backward-word", .aliases = &.{"bw"}, .description = "Move backward over one word" },
    .{ .id = .delete_char, .name = "delete-char", .aliases = &.{ "dc", "del" }, .description = "Delete character at point" },
    .{ .id = .kill_word, .name = "kill-word", .aliases = &.{ "kw", "dw" }, .description = "Kill from point to end of next word" },
    .{ .id = .backward_kill_word, .name = "backward-kill-word", .aliases = &.{ "bkw", "bdw" }, .description = "Kill from point backward to start of word" },
    .{ .id = .open_line, .name = "open-line", .aliases = &.{"ol"}, .description = "Insert a newline after point, keeping point" },
    .{ .id = .transpose_chars, .name = "transpose-chars", .aliases = &.{"tc"}, .description = "Transpose characters around point" },
    .{ .id = .transpose_words, .name = "transpose-words", .aliases = &.{"tw"}, .description = "Transpose neighboring words" },
    .{ .id = .set_mark_command, .name = "set-mark-command", .aliases = &.{ "set-mark", "sm" }, .description = "Set mark at point" },
    .{ .id = .pop_mark, .name = "pop-mark", .aliases = &.{ "pop-mark-command", "pm" }, .description = "Pop to previous mark in mark ring" },
    .{ .id = .exchange_point_and_mark, .name = "exchange-point-and-mark", .aliases = &.{ "xpm", "exchange-mark" }, .description = "Swap point and mark" },
    .{ .id = .kill_region, .name = "kill-region", .aliases = &.{ "kr", "cut-region" }, .description = "Kill text between mark and point" },
    .{ .id = .copy_region_as_kill, .name = "copy-region-as-kill", .aliases = &.{ "crk", "copy-region" }, .description = "Copy region to kill ring" },
    .{ .id = .mark_whole_buffer, .name = "mark-whole-buffer", .aliases = &.{ "mwb", "select-all" }, .description = "Mark the entire buffer" },
    .{ .id = .kill_line, .name = "kill-line", .aliases = &.{"kl"}, .description = "Kill text from cursor to line end" },
    .{ .id = .yank, .name = "yank", .aliases = &.{"paste"}, .description = "Insert latest killed text" },
    .{ .id = .yank_pop, .name = "yank-pop", .aliases = &.{ "yp", "paste-cycle" }, .description = "Replace last yank with previous kill ring entry" },
    .{ .id = .undo, .name = "undo", .aliases = &.{"u"}, .description = "Undo latest edit" },
    .{ .id = .redo, .name = "redo", .aliases = &.{"r"}, .description = "Redo latest undone edit" },
    .{ .id = .isearch_forward, .name = "isearch-forward", .aliases = &.{ "isf", "search" }, .description = "Start forward incremental search" },
    .{ .id = .isearch_backward, .name = "isearch-backward", .aliases = &.{"isb"}, .description = "Start backward incremental search" },
    .{ .id = .quoted_insert, .name = "quoted-insert", .aliases = &.{ "qi", "quote" }, .description = "Insert next key literally" },
    .{ .id = .quit, .name = "quit", .aliases = &.{ "q", "exit-now" }, .description = "Exit editor immediately" },
};

/// Buffer slot owns per-buffer metadata that was globally scattered in mg.
const BufferSlot = struct {
    buffer: Buffer,
    file_path: ?[]u8 = null,
    dirty: bool = false,
    mark_ring: std.array_list.Managed(usize),

    fn initScratch(allocator: Allocator) !BufferSlot {
        var scratch = try Buffer.init(allocator, "*scratch*");
        try scratch.setText("");
        return .{
            .buffer = scratch,
            .mark_ring = std.array_list.Managed(usize).init(allocator),
        };
    }

    fn initNamed(allocator: Allocator, name: []const u8) !BufferSlot {
        var b = try Buffer.init(allocator, name);
        try b.setText("");
        return .{
            .buffer = b,
            .mark_ring = std.array_list.Managed(usize).init(allocator),
        };
    }

    fn deinit(self: *BufferSlot, allocator: Allocator) void {
        if (self.file_path) |path| allocator.free(path);
        self.mark_ring.deinit();
        self.buffer.deinit();
    }

    fn setFilePath(self: *BufferSlot, allocator: Allocator, path: []const u8) !void {
        if (self.file_path) |old| allocator.free(old);
        const owned = try allocator.alloc(u8, path.len);
        std.mem.copyForwards(u8, owned, path);
        self.file_path = owned;
    }

    fn displayName(self: *const BufferSlot) []const u8 {
        if (self.file_path) |path| return std.fs.path.basename(path);
        return self.buffer.name.items;
    }
};

/// Editor is the orchestrator for buffers, windows, commands and rendering.
/// mg has many global singletons; Zig keeps the mutable state in one owned struct.
pub const Editor = struct {
    allocator: Allocator,
    buffers: std.array_list.Managed(BufferSlot),
    windows: std.array_list.Managed(Window),
    split_nodes: std.array_list.Managed(SplitNode),
    split_root: usize,
    active_window: usize,
    keymap: KeyMap,
    pending_keys: std.array_list.Managed(Key),
    status_line: std.array_list.Managed(u8),
    last_search: std.array_list.Managed(u8),
    kill_ring: KillRing,
    input_mode: InputMode,
    input_buffer: std.array_list.Managed(u8),
    syntax: SyntaxEngine,
    minibuffer_hint: std.array_list.Managed(u8),
    minibuffer_backup: std.array_list.Managed(u8),
    prefix_hint: std.array_list.Managed(u8),
    open_history: std.array_list.Managed([]u8),
    save_history: std.array_list.Managed([]u8),
    buffer_history: std.array_list.Managed([]u8),
    command_history: std.array_list.Managed([]u8),
    buffer_list_entries: std.array_list.Managed(usize),
    buffer_list_delete_marks: std.AutoHashMap(usize, void),
    buffer_list_return_buffer: ?usize,
    pending_kill_buffer_idx: ?usize,
    pending_replace_from: ?[]u8,
    pending_query_replace_from: ?[]u8,
    pending_replace_regexp_from: ?[]u8,
    pending_query_replace_regexp_from: ?[]u8,
    last_command_was_kill: bool,
    last_kill_cursor: ?usize,
    last_command: ?Command,
    last_yank: ?YankState,
    quit_requested: bool,

    pub fn init(allocator: Allocator, size: term_mod.Size) !Editor {
        var self = Editor{
            .allocator = allocator,
            .buffers = std.array_list.Managed(BufferSlot).init(allocator),
            .windows = std.array_list.Managed(Window).init(allocator),
            .split_nodes = std.array_list.Managed(SplitNode).init(allocator),
            .split_root = 0,
            .active_window = 0,
            .keymap = KeyMap.init(allocator),
            .pending_keys = std.array_list.Managed(Key).init(allocator),
            .status_line = std.array_list.Managed(u8).init(allocator),
            .last_search = std.array_list.Managed(u8).init(allocator),
            .kill_ring = KillRing.init(allocator, 32),
            .input_mode = .normal,
            .input_buffer = std.array_list.Managed(u8).init(allocator),
            .syntax = SyntaxEngine.init(allocator),
            .minibuffer_hint = std.array_list.Managed(u8).init(allocator),
            .minibuffer_backup = std.array_list.Managed(u8).init(allocator),
            .prefix_hint = std.array_list.Managed(u8).init(allocator),
            .open_history = std.array_list.Managed([]u8).init(allocator),
            .save_history = std.array_list.Managed([]u8).init(allocator),
            .buffer_history = std.array_list.Managed([]u8).init(allocator),
            .command_history = std.array_list.Managed([]u8).init(allocator),
            .buffer_list_entries = std.array_list.Managed(usize).init(allocator),
            .buffer_list_delete_marks = std.AutoHashMap(usize, void).init(allocator),
            .buffer_list_return_buffer = null,
            .pending_kill_buffer_idx = null,
            .pending_replace_from = null,
            .pending_query_replace_from = null,
            .pending_replace_regexp_from = null,
            .pending_query_replace_regexp_from = null,
            .last_command_was_kill = false,
            .last_kill_cursor = null,
            .last_command = null,
            .last_yank = null,
            .quit_requested = false,
        };
        errdefer self.deinit();

        try self.buffers.append(try BufferSlot.initScratch(allocator));

        const text_rows = if (size.rows > 0) size.rows - 1 else 0;
        try self.windows.append(Window.init(0, text_rows, size.cols));
        try self.split_nodes.append(.{ .kind = .{ .leaf = 0 } });
        self.split_root = 0;

        try keymaps_mod.bindAll(&self.keymap);
        try self.setStatus("C-g cancel | M-x commands | C-v/M-v page | M-</M-> buffer | C-x C-c quit");

        if (!builtin.is_test) {
            self.loadHistory() catch {};
        }

        return self;
    }

    pub fn deinit(self: *Editor) void {
        if (!builtin.is_test) {
            self.saveHistory() catch {};
        }
        if (self.pending_replace_from) |from| self.allocator.free(from);
        if (self.pending_query_replace_from) |from| self.allocator.free(from);
        if (self.pending_replace_regexp_from) |from| self.allocator.free(from);
        if (self.pending_query_replace_regexp_from) |from| self.allocator.free(from);
        switch (self.input_mode) {
            .query_replace => |state| {
                self.allocator.free(state.from);
                self.allocator.free(state.to);
            },
            .save_some_buffers => |state| {
                var cleanup = state;
                cleanup.dirty_indices.deinit();
                cleanup.path_input.deinit();
            },
            else => {},
        }
        for (self.buffers.items) |*slot| slot.deinit(self.allocator);
        self.buffers.deinit();
        self.windows.deinit();
        self.split_nodes.deinit();
        self.keymap.deinit();
        self.pending_keys.deinit();
        self.status_line.deinit();
        self.last_search.deinit();
        self.kill_ring.deinit();
        self.input_buffer.deinit();
        self.syntax.deinit();
        self.minibuffer_hint.deinit();
        self.minibuffer_backup.deinit();
        self.prefix_hint.deinit();
        self.freeHistory(&self.open_history);
        self.freeHistory(&self.save_history);
        self.freeHistory(&self.buffer_history);
        self.freeHistory(&self.command_history);
        self.buffer_list_entries.deinit();
        self.buffer_list_delete_marks.deinit();
    }

    pub fn shouldQuit(self: *const Editor) bool {
        return self.quit_requested;
    }

    pub fn openFile(self: *Editor, path: []const u8) !void {
        var slot = try BufferSlot.initNamed(self.allocator, std.fs.path.basename(path));
        errdefer slot.deinit(self.allocator);

        var opened_existing = true;
        FileIO.loadIntoBuffer(self.allocator, path, &slot.buffer) catch |err| switch (err) {
            error.FileNotFound => {
                opened_existing = false;
            },
            else => return err,
        };
        try slot.setFilePath(self.allocator, path);
        slot.dirty = false;

        try self.buffers.append(slot);
        const new_index = self.buffers.items.len - 1;

        const win = self.activeWindow();
        win.buffer_id = new_index;
        win.cursor = .{};
        win.mark = null;
        win.top_line = 0;

        if (opened_existing) {
            try self.setStatusFmt("Opened {s}", .{std.fs.path.basename(path)});
        } else {
            try self.setStatusFmt("New file {s}", .{std.fs.path.basename(path)});
        }
    }

    pub fn resize(self: *Editor, size: term_mod.Size) void {
        const text_rows = if (size.rows > 0) size.rows - 1 else 0;
        const win_count = self.windows.items.len;
        const layouts = self.allocator.alloc(WindowLayout, win_count) catch return;
        defer self.allocator.free(layouts);
        var separators = std.array_list.Managed(VerticalSeparator).init(self.allocator);
        defer separators.deinit();
        self.computeWindowLayouts(text_rows, size.cols, layouts, &separators) catch return;

        for (self.windows.items, 0..) |*win, idx| {
            const layout = layouts[idx];
            win.resize(layout.body_rows, layout.body_cols);
        }
    }

    pub fn handleKey(self: *Editor, key: Key) !void {
        switch (self.input_mode) {
            .normal => {},
            else => {
                self.prefix_hint.clearRetainingCapacity();
                self.pending_keys.clearRetainingCapacity();
                try self.handleInputModeKey(key);
                return;
            },
        }

        if (std.meta.eql(key, Key{ .ctrl = 'g' })) {
            self.pending_keys.clearRetainingCapacity();
            self.prefix_hint.clearRetainingCapacity();
            const win = self.activeWindow();
            win.mark = null;
            self.last_command_was_kill = false;
            self.last_kill_cursor = null;
            self.last_yank = null;
            self.last_command = .keyboard_quit;
            try self.setStatus("Quit");
            return;
        }

        try self.pending_keys.append(key);

        switch (self.keymap.dispatch(self.pending_keys.items)) {
            .command => |command| {
                self.prefix_hint.clearRetainingCapacity();
                self.pending_keys.clearRetainingCapacity();
                try self.executeCommand(command);
            },
            .prefix => {
                try self.refreshPrefixHint();
                return;
            },
            .none => {
                self.prefix_hint.clearRetainingCapacity();
                if (self.pending_keys.items.len == 1) {
                    const single = self.pending_keys.items[0];
                    self.pending_keys.clearRetainingCapacity();
                    try self.handleFallback(single);
                } else {
                    self.pending_keys.clearRetainingCapacity();
                }
            },
        }
    }

    fn refreshPrefixHint(self: *Editor) !void {
        self.prefix_hint.clearRetainingCapacity();
        if (self.pending_keys.items.len == 0) return;

        var options = std.array_list.Managed(PrefixOption).init(self.allocator);
        defer options.deinit();

        for (self.keymap.bindings.items) |binding| {
            if (!isKeyPrefix(self.pending_keys.items, binding.keys)) continue;
            if (binding.keys.len <= self.pending_keys.items.len) continue;

            const next_key = binding.keys[self.pending_keys.items.len];
            var found_index: ?usize = null;
            for (options.items, 0..) |opt, idx| {
                if (std.meta.eql(opt.key, next_key)) {
                    found_index = idx;
                    break;
                }
            }

            if (found_index == null) {
                try options.append(.{ .key = next_key });
                found_index = options.items.len - 1;
            }

            const opt = &options.items[found_index.?];
            if (binding.keys.len == self.pending_keys.items.len + 1) {
                opt.command = binding.command;
            } else {
                opt.has_more = true;
            }
        }

        if (options.items.len == 0) return;

        for (self.pending_keys.items, 0..) |k, idx| {
            if (idx > 0) try self.prefix_hint.appendSlice(" ");
            try appendKeyName(&self.prefix_hint, k);
        }
        try self.prefix_hint.appendSlice(": ");

        const limit: usize = 8;
        var shown: usize = 0;
        for (options.items) |opt| {
            if (shown >= limit) break;
            if (shown > 0) try self.prefix_hint.appendSlice("  ");
            try appendKeyName(&self.prefix_hint, opt.key);

            if (opt.command) |cmd| {
                try self.prefix_hint.writer().print(" {s}", .{keymaps_mod.commandLabel(cmd)});
            } else if (opt.has_more) {
                try self.prefix_hint.appendSlice(" prefix");
            }
            shown += 1;
        }
        if (options.items.len > shown) {
            try self.prefix_hint.writer().print("  (+{d})", .{options.items.len - shown});
        }
    }

    fn isKeyPrefix(prefix: []const Key, whole: []const Key) bool {
        if (prefix.len > whole.len) return false;
        for (prefix, 0..) |p, idx| {
            if (!std.meta.eql(p, whole[idx])) return false;
        }
        return true;
    }

    fn appendKeyName(out: *std.array_list.Managed(u8), key: Key) !void {
        switch (key) {
            .char => |ch| try out.writer().print("{c}", .{ch}),
            .ctrl => |ch| try out.writer().print("C-{c}", .{ch}),
            .meta => |ch| try out.writer().print("M-{c}", .{ch}),
            .meta_ctrl => |ch| {
                if (ch == '5') {
                    try out.appendSlice("C-M-%");
                } else {
                    try out.writer().print("C-M-{c}", .{ch});
                }
            },
            .meta_backspace => try out.appendSlice("M-BS"),
            .tab => try out.appendSlice("TAB"),
            .enter => try out.appendSlice("RET"),
            .backspace => try out.appendSlice("BS"),
            .escape => try out.appendSlice("ESC"),
            .arrow_up => try out.appendSlice("<up>"),
            .arrow_down => try out.appendSlice("<down>"),
            .arrow_left => try out.appendSlice("<left>"),
            .arrow_right => try out.appendSlice("<right>"),
        }
    }

    pub fn draw(self: *Editor, term: *term_mod.Terminal) !void {
        const size = term.size();
        self.resize(size);
        const text_rows = if (size.rows > 0) size.rows - 1 else 0;
        const win_count = self.windows.items.len;
        std.debug.assert(win_count > 0);
        const active_idx = @min(self.active_window, win_count - 1);

        const layouts = try self.allocator.alloc(WindowLayout, win_count);
        defer self.allocator.free(layouts);
        var separators = std.array_list.Managed(VerticalSeparator).init(self.allocator);
        defer separators.deinit();
        try self.computeWindowLayouts(text_rows, size.cols, layouts, &separators);

        for (0..win_count) |idx| {
            const win = &self.windows.items[idx];
            const buf = &self.buffers.items[win.buffer_id].buffer;
            win.clampCursor(buf);
        }

        // Keep cursor hidden while redrawing to avoid flicker,
        // then re-enable it at the final logical position.
        try term.hideCursor();
        try term.clearScreen();

        for (0..win_count) |idx| {
            const win = &self.windows.items[idx];
            const slot = &self.buffers.items[win.buffer_id];
            const buf = &slot.buffer;
            const layout = layouts[idx];
            const style_map: ?[]syntax_mod.Style = self.syntax.buildStyleMap(slot.file_path, buf.bytes()) catch null;
            defer if (style_map) |styles| self.allocator.free(styles);
            const visual_region = self.regionBounds(win);

            for (0..layout.body_rows) |row| {
                const line_idx = win.top_line + row;
                const draw_row = layout.top_row + row;
                if (draw_row > text_rows) break;
                try term.moveCursor(draw_row, layout.left_col);
                var emitted: usize = 0;

                if (line_idx < buf.lineCount()) {
                    const line = buf.lineSlice(line_idx);
                    const visible_len = @min(line.len, layout.body_cols);
                    emitted = visible_len;
                    if (visible_len > 0) {
                        const line_start = buf.lineColToByte(line_idx, 0);
                        const line_end = line_start + visible_len;
                        const line_styles: ?[]const syntax_mod.Style = if (style_map) |styles|
                            styles[@min(line_start, styles.len)..@min(line_end, styles.len)]
                        else
                            null;

                        try queueHighlightedLine(
                            term,
                            line[0..visible_len],
                            line_styles,
                            line_start,
                            visual_region,
                        );
                    }
                } else {
                    if (layout.body_cols > 0) try term.queue("~");
                    emitted = @min(@as(usize, 1), layout.body_cols);
                }
                if (layout.body_cols > emitted) {
                    const pad = layout.body_cols - emitted;
                    for (0..pad) |_| try term.queue(" ");
                }
            }

            if (layout.modeline_row) |row| {
                if (row <= text_rows) {
                    try self.queueWindowModeline(
                        term,
                        slot,
                        win,
                        idx == active_idx,
                        idx,
                        win_count,
                        layout.left_col,
                        layout.body_cols,
                        row,
                    );
                }
            }
        }

        for (separators.items) |sep| {
            if (sep.col > size.cols) continue;
            const top = @max(@as(usize, 1), sep.top_row);
            const bottom = @min(sep.bottom_row, text_rows);
            if (bottom < top) continue;
            var row = top;
            while (row <= bottom) : (row += 1) {
                try term.moveCursor(row, sep.col);
                try term.queue("\x1b[2m|\x1b[m");
            }
        }

        if (size.rows > 0) {
            try term.moveCursor(size.rows, 1);
            try term.queue("\x1b[m");
            try term.queue("\x1b[K");

            var dynamic_status = std.array_list.Managed(u8).init(self.allocator);
            defer dynamic_status.deinit();
            const status_text = try self.renderStatusText(&dynamic_status);

            const status_len = @min(status_text.len, size.cols);
            try term.queue(status_text[0..status_len]);
            if (size.cols > status_len) {
                for (0..size.cols - status_len) |_| try term.queue(" ");
            }
        }

        const active_win = &self.windows.items[active_idx];
        const active_buf = &self.buffers.items[active_win.buffer_id].buffer;
        const active_layout = layouts[active_idx];
        const cursor_pos = active_buf.byteToLineCol(active_win.cursor.byte_index);
        const cursor_row = if (active_layout.body_rows == 0)
            if (active_layout.modeline_row) |mr| if (mr <= text_rows) mr else if (active_layout.top_row <= text_rows) active_layout.top_row else if (size.rows > 0) size.rows else 1 else if (active_layout.top_row <= text_rows) active_layout.top_row else if (size.rows > 0) size.rows else 1
        else
            active_layout.top_row + (cursor_pos.line - active_win.top_line);
        const cursor_col = if (active_layout.body_cols == 0)
            active_layout.left_col
        else
            @min(active_layout.left_col + cursor_pos.column, active_layout.left_col + active_layout.body_cols - 1);
        try term.moveCursor(cursor_row, cursor_col);
        try term.showCursor();

        try term.flush();
    }

    fn computeWindowLayouts(
        self: *Editor,
        text_rows: usize,
        total_cols: usize,
        layouts: []WindowLayout,
        separators: *std.array_list.Managed(VerticalSeparator),
    ) !void {
        for (layouts) |*layout| {
            layout.* = .{
                .top_row = 1,
                .left_col = 1,
                .body_rows = 0,
                .body_cols = total_cols,
                .modeline_row = null,
                .separator_col = null,
            };
        }
        separators.clearRetainingCapacity();

        if (self.windows.items.len == 0) return;
        if (self.split_nodes.items.len == 0 or self.split_root >= self.split_nodes.items.len) {
            try self.rebuildSplitTreeFromWindows();
        }

        const root_rect = Rect{
            .top_row = 1,
            .left_col = 1,
            .rows = text_rows,
            .cols = total_cols,
        };
        try self.layoutSplitNode(self.split_root, root_rect, layouts, separators);
    }

    fn layoutSplitNode(
        self: *Editor,
        node_idx: usize,
        rect: Rect,
        layouts: []WindowLayout,
        separators: *std.array_list.Managed(VerticalSeparator),
    ) !void {
        if (node_idx >= self.split_nodes.items.len) return;
        const node = self.split_nodes.items[node_idx];
        switch (node.kind) {
            .leaf => |win_idx| {
                if (win_idx >= layouts.len) return;
                const body_rows = if (rect.rows > 0) rect.rows - 1 else 0;
                layouts[win_idx] = .{
                    .top_row = rect.top_row,
                    .left_col = rect.left_col,
                    .body_rows = body_rows,
                    .body_cols = rect.cols,
                    .modeline_row = if (rect.rows > 0) rect.top_row + rect.rows - 1 else null,
                    .separator_col = null,
                };
            },
            .split => |split| {
                switch (split.axis) {
                    .horizontal => {
                        const first_rows = if (rect.rows > 0) rect.rows - (rect.rows / 2) else 0;
                        const second_rows = rect.rows - first_rows;
                        const first_rect = Rect{
                            .top_row = rect.top_row,
                            .left_col = rect.left_col,
                            .rows = first_rows,
                            .cols = rect.cols,
                        };
                        const second_rect = Rect{
                            .top_row = rect.top_row + first_rows,
                            .left_col = rect.left_col,
                            .rows = second_rows,
                            .cols = rect.cols,
                        };
                        try self.layoutSplitNode(split.first, first_rect, layouts, separators);
                        try self.layoutSplitNode(split.second, second_rect, layouts, separators);
                    },
                    .vertical => {
                        const separator_count: usize = if (rect.cols > 0) 1 else 0;
                        const usable = rect.cols - @min(separator_count, rect.cols);
                        const first_cols = if (usable > 0) usable - (usable / 2) else 0;
                        const second_cols = usable - first_cols;
                        const sep_col = if (rect.cols > first_cols) rect.left_col + first_cols else null;

                        const first_rect = Rect{
                            .top_row = rect.top_row,
                            .left_col = rect.left_col,
                            .rows = rect.rows,
                            .cols = first_cols,
                        };
                        const second_rect = Rect{
                            .top_row = rect.top_row,
                            .left_col = if (sep_col) |col| col + 1 else rect.left_col + first_cols,
                            .rows = rect.rows,
                            .cols = second_cols,
                        };
                        if (sep_col) |col| {
                            if (rect.rows > 0) {
                                try separators.append(.{
                                    .col = col,
                                    .top_row = rect.top_row,
                                    .bottom_row = rect.top_row + rect.rows - 1,
                                });
                            }
                        }

                        try self.layoutSplitNode(split.first, first_rect, layouts, separators);
                        try self.layoutSplitNode(split.second, second_rect, layouts, separators);
                    },
                }
            },
        }
    }

    fn windowLayoutFor(
        axis: SplitAxis,
        text_rows: usize,
        total_cols: usize,
        window_count: usize,
        window_index: usize,
    ) WindowLayout {
        if (window_count == 0) {
            return .{
                .top_row = 1,
                .left_col = 1,
                .body_rows = 0,
                .body_cols = 0,
                .modeline_row = null,
                .separator_col = null,
            };
        }

        return switch (axis) {
            .horizontal => horizontalWindowLayout(text_rows, total_cols, window_count, window_index),
            .vertical => verticalWindowLayout(text_rows, total_cols, window_count, window_index),
        };
    }

    fn horizontalWindowLayout(text_rows: usize, total_cols: usize, window_count: usize, window_index: usize) WindowLayout {
        const usable_body = if (text_rows > window_count) text_rows - window_count else 0;
        const base_body = usable_body / window_count;
        const remainder = usable_body % window_count;
        const before_extra = @min(window_index, remainder);
        const this_extra: usize = if (window_index < remainder) 1 else 0;
        const body_rows = base_body + this_extra;
        const top_row = 1 + window_index * base_body + before_extra + window_index;

        return .{
            .top_row = top_row,
            .left_col = 1,
            .body_rows = body_rows,
            .body_cols = total_cols,
            .modeline_row = top_row + body_rows,
            .separator_col = null,
        };
    }

    fn verticalWindowLayout(text_rows: usize, total_cols: usize, window_count: usize, window_index: usize) WindowLayout {
        const separator_count = window_count - 1;
        const usable_cols = if (total_cols > separator_count) total_cols - separator_count else 0;
        const base_cols = usable_cols / window_count;
        const remainder = usable_cols % window_count;
        const before_extra = @min(window_index, remainder);
        const this_extra: usize = if (window_index < remainder) 1 else 0;
        const body_cols = base_cols + this_extra;
        const left_col = 1 + window_index * base_cols + before_extra + window_index;
        const body_rows = if (text_rows > 0) text_rows - 1 else 0;
        const modeline_row: ?usize = if (text_rows > 0) text_rows else null;
        const separator_col: ?usize = if (window_index + 1 < window_count) left_col + body_cols else null;

        return .{
            .top_row = 1,
            .left_col = left_col,
            .body_rows = body_rows,
            .body_cols = body_cols,
            .modeline_row = modeline_row,
            .separator_col = separator_col,
        };
    }

    fn queueWindowModeline(
        self: *Editor,
        term: *term_mod.Terminal,
        slot: *const BufferSlot,
        win: *const Window,
        is_active: bool,
        index: usize,
        total: usize,
        left_col: usize,
        cols: usize,
        row: usize,
    ) !void {
        if (cols == 0) return;

        try term.moveCursor(row, left_col);
        try term.queue(if (is_active) "\x1b[7;1m" else "\x1b[7m");

        const pos = slot.buffer.byteToLineCol(win.cursor.byte_index);
        const mark = if (win.mark != null) " MARK" else "";

        var text = std.array_list.Managed(u8).init(self.allocator);
        defer text.deinit();
        try text.writer().print("{s} [{s}{s}]  L{d}:C{d}{s}  ({d}/{d})", .{
            if (is_active) "*" else "-",
            slot.displayName(),
            if (slot.dirty) "*" else "",
            pos.line + 1,
            pos.column + 1,
            mark,
            index + 1,
            total,
        });

        const visible_len = @min(text.items.len, cols);
        if (visible_len > 0) {
            try term.queue(text.items[0..visible_len]);
        }
        if (cols > visible_len) {
            for (0..cols - visible_len) |_| try term.queue(" ");
        }
        try term.queue("\x1b[m");
    }

    fn queueHighlightedLine(
        term: *term_mod.Terminal,
        text: []const u8,
        styles: ?[]const syntax_mod.Style,
        line_start: usize,
        region: ?ByteRange,
    ) !void {
        if (text.len == 0) return;

        var current_style: syntax_mod.Style = .normal;
        var current_selected = false;

        var start: usize = 0;
        var idx: usize = 0;
        while (idx < text.len) : (idx += 1) {
            const style = if (styles) |line_styles|
                if (idx < line_styles.len) line_styles[idx] else .normal
            else
                .normal;
            const selected = if (region) |r|
                isByteInRegion(r, line_start + idx)
            else
                false;

            if (idx == 0) {
                current_style = style;
                current_selected = selected;
                if (current_selected) try term.queue("\x1b[7m");
                if (current_style != .normal) try term.queue(syntax_mod.ansiForStyle(current_style));
                continue;
            }

            if (style == current_style and selected == current_selected) continue;

            try term.queue(text[start..idx]);

            if (selected != current_selected) {
                try term.queue(if (selected) "\x1b[7m" else "\x1b[27m");
                current_selected = selected;
            }
            if (style != current_style) {
                try term.queue(syntax_mod.ansiForStyle(style));
                current_style = style;
            }

            start = idx;
        }

        try term.queue(text[start..text.len]);
        if (current_selected) try term.queue("\x1b[27m");
        if (current_style != .normal) try term.queue(syntax_mod.ansiForStyle(.normal));
    }

    fn isByteInRegion(region: ByteRange, byte_index: usize) bool {
        return byte_index >= region.start and byte_index < region.end;
    }

    fn handleInputModeKey(self: *Editor, key: Key) !void {
        switch (self.input_mode) {
            .normal => unreachable,
            .minibuffer => |state| {
                var next = state;
                const keep = try self.handleMinibufferKey(&next, key);
                if (keep) {
                    self.input_mode = .{ .minibuffer = next };
                } else {
                    switch (self.input_mode) {
                        .normal => {
                            self.input_mode = .normal;
                            self.input_buffer.clearRetainingCapacity();
                            self.minibuffer_backup.clearRetainingCapacity();
                            self.minibuffer_hint.clearRetainingCapacity();
                        },
                        .minibuffer => |active| {
                            if (active.kind == state.kind) {
                                self.input_mode = .normal;
                                self.input_buffer.clearRetainingCapacity();
                                self.minibuffer_backup.clearRetainingCapacity();
                                self.minibuffer_hint.clearRetainingCapacity();
                            }
                        },
                        else => {},
                    }
                }
            },
            .isearch => |state| {
                var next = state;
                const keep = try self.handleISearchKey(&next, key);
                if (keep) {
                    self.input_mode = .{ .isearch = next };
                } else {
                    self.input_mode = .normal;
                    self.input_buffer.clearRetainingCapacity();
                }
            },
            .query_replace => |state| {
                var next = state;
                const keep = try self.handleQueryReplaceKey(&next, key);
                if (keep) {
                    self.input_mode = .{ .query_replace = next };
                } else {
                    self.allocator.free(next.from);
                    self.allocator.free(next.to);
                    self.input_buffer.clearRetainingCapacity();
                    self.minibuffer_backup.clearRetainingCapacity();
                    self.minibuffer_hint.clearRetainingCapacity();
                    self.input_mode = .normal;
                }
            },
            .save_some_buffers => |state| {
                var next = state;
                const keep = try self.handleSaveSomeBuffersKey(&next, key);
                if (keep) {
                    self.input_mode = .{ .save_some_buffers = next };
                } else {
                    const should_quit = next.quit_after and !next.canceled and next.failed == 0;
                    next.dirty_indices.deinit();
                    next.path_input.deinit();
                    self.input_mode = .normal;
                    if (should_quit) {
                        self.quit_requested = true;
                    } else if (next.quit_after and !next.canceled and next.failed > 0) {
                        try self.setStatus("Quit canceled due save failures");
                    }
                }
            },
            .quoted_insert => {
                try self.handleQuotedInsertKey(key);
                self.input_mode = .normal;
            },
        }
    }

    fn handleMinibufferKey(self: *Editor, state: *MinibufferState, key: Key) !bool {
        switch (key) {
            .escape => {
                self.clearPendingForKind(state.kind);
                try self.setStatus("Canceled");
                return false;
            },
            .ctrl => |ch| {
                if (ch == 'g') {
                    self.clearPendingForKind(state.kind);
                    try self.setStatus("Canceled");
                    return false;
                }
                if (ch == 'u') {
                    self.input_buffer.clearRetainingCapacity();
                    state.history_index = null;
                    state.tab_count = 0;
                    state.ido_index = 0;
                    try self.refreshMinibufferHint(state.*, false);
                    return true;
                }
                if (isIdoKind(state.kind) and ch == 's') {
                    try self.cycleCommandCandidate(state, true);
                    state.tab_count = 1;
                    try self.refreshMinibufferHint(state.*, true);
                    return true;
                }
                if (isIdoKind(state.kind) and ch == 'r') {
                    try self.cycleCommandCandidate(state, false);
                    state.tab_count = 1;
                    try self.refreshMinibufferHint(state.*, true);
                }
                if (ch == 'p' and state.kind != .goto_line and state.kind != .kill_buffer_confirm and state.kind != .quit_confirm) {
                    try self.minibufferHistoryUp(state);
                    state.tab_count = 0;
                    state.ido_index = 0;
                    try self.refreshMinibufferHint(state.*, false);
                    return true;
                }
                if (ch == 'n' and state.kind != .goto_line and state.kind != .kill_buffer_confirm and state.kind != .quit_confirm) {
                    try self.minibufferHistoryDown(state);
                    state.tab_count = 0;
                    state.ido_index = 0;
                    try self.refreshMinibufferHint(state.*, false);
                    return true;
                }
                return true;
            },
            .backspace => {
                if (self.input_buffer.items.len > 0) {
                    self.input_buffer.shrinkRetainingCapacity(self.input_buffer.items.len - 1);
                    state.history_index = null;
                    state.tab_count = 0;
                    state.ido_index = 0;
                    try self.refreshMinibufferHint(state.*, false);
                }
                return true;
            },
            .arrow_up => {
                if (state.kind != .goto_line and state.kind != .kill_buffer_confirm and state.kind != .quit_confirm) {
                    try self.minibufferHistoryUp(state);
                }
                state.tab_count = 0;
                state.ido_index = 0;
                try self.refreshMinibufferHint(state.*, false);
                return true;
            },
            .arrow_down => {
                if (state.kind != .goto_line and state.kind != .kill_buffer_confirm and state.kind != .quit_confirm) {
                    try self.minibufferHistoryDown(state);
                }
                state.tab_count = 0;
                state.ido_index = 0;
                try self.refreshMinibufferHint(state.*, false);
                return true;
            },
            .arrow_right => {
                if (isIdoKind(state.kind)) {
                    try self.cycleCommandCandidate(state, true);
                    state.tab_count = 1;
                    try self.refreshMinibufferHint(state.*, true);
                }
                return true;
            },
            .arrow_left => {
                if (isIdoKind(state.kind)) {
                    try self.cycleCommandCandidate(state, false);
                    state.tab_count = 1;
                    try self.refreshMinibufferHint(state.*, true);
                }
                return true;
            },
            .tab => {
                state.tab_count += 1;
                switch (state.kind) {
                    .command => {
                        if (state.tab_count == 1) {
                            try self.completeCommandInput(false);
                            state.ido_index = 0;
                        } else {
                            try self.cycleCommandCandidate(state, true);
                        }
                    },
                    .open_file, .save_as => {
                        if (state.tab_count > 1) {
                            try self.cycleCommandCandidate(state, true);
                        }
                        try self.completePathInput(state.*);
                    },
                    .switch_buffer, .kill_buffer => {
                        if (state.tab_count > 1) {
                            try self.cycleCommandCandidate(state, true);
                        }
                        try self.completeBufferInput(state.*);
                    },
                    .kill_buffer_confirm,
                    .quit_confirm,
                    .goto_line,
                    .replace_from,
                    .replace_to,
                    .query_replace_from,
                    .query_replace_to,
                    .replace_regexp_from,
                    .replace_regexp_to,
                    .query_replace_regexp_from,
                    .query_replace_regexp_to,
                    => {},
                }
                try self.refreshMinibufferHint(state.*, true);
                return true;
            },
            .enter => {
                const allow_empty = state.kind == .replace_to or
                    state.kind == .query_replace_to or
                    state.kind == .replace_regexp_to or
                    state.kind == .query_replace_regexp_to or
                    state.kind == .kill_buffer_confirm;
                if (!allow_empty and self.input_buffer.items.len == 0) {
                    self.clearPendingForKind(state.kind);
                    try self.setStatus("Canceled");
                    return false;
                }

                const input = self.input_buffer.items;

                switch (state.kind) {
                    .open_file => {
                        const resolved = try self.resolvePathForInput(state.*, input);
                        defer if (resolved) |path| self.allocator.free(path);
                        const path = resolved orelse input;
                        try self.addHistoryEntry(.open_file, path);
                        self.openFile(path) catch |err| {
                            try self.setStatusFmt("Open failed: {s}", .{@errorName(err)});
                        };
                    },
                    .save_as => {
                        try self.addHistoryEntry(.save_as, input);
                        self.saveAs(input) catch |err| {
                            try self.setStatusFmt("Save-as failed: {s}", .{@errorName(err)});
                        };
                    },
                    .switch_buffer => {
                        const idx = try self.resolveBufferForInput(state.*, input) orelse {
                            try self.setStatusFmt("No matching buffer: {s}", .{input});
                            return true;
                        };
                        try self.switchToBuffer(idx);
                        try self.addHistoryEntry(.switch_buffer, self.buffers.items[idx].displayName());
                    },
                    .kill_buffer => {
                        const idx = try self.resolveBufferForInput(state.*, input) orelse {
                            try self.setStatusFmt("No matching buffer: {s}", .{input});
                            return true;
                        };
                        if (self.buffers.items[idx].dirty) {
                            self.pending_kill_buffer_idx = idx;
                            try self.startMinibuffer(.kill_buffer_confirm);
                            return false;
                        }
                        const killed = try self.killBufferAt(idx);
                        if (!killed) return true;
                        try self.addHistoryEntry(.kill_buffer, input);
                    },
                    .kill_buffer_confirm => {
                        const confirmation = std.mem.trim(u8, input, " \t");
                        if (!std.ascii.eqlIgnoreCase(confirmation, "yes")) {
                            try self.setStatus("Type yes to kill modified buffer, or C-g to cancel");
                            return true;
                        }
                        const idx = self.pending_kill_buffer_idx orelse {
                            try self.setStatus("No pending buffer kill");
                            return false;
                        };
                        self.pending_kill_buffer_idx = null;
                        if (idx >= self.buffers.items.len) {
                            try self.setStatus("Pending buffer no longer exists");
                            return false;
                        }
                        const target_name = try std.fmt.allocPrint(self.allocator, "{s}", .{self.buffers.items[idx].displayName()});
                        defer self.allocator.free(target_name);
                        const killed = try self.killBufferAt(idx);
                        if (!killed) return true;
                        try self.addHistoryEntry(.kill_buffer, target_name);
                    },
                    .quit_confirm => {
                        const confirmation = std.mem.trim(u8, input, " \t");
                        if (std.ascii.eqlIgnoreCase(confirmation, "yes") or std.ascii.eqlIgnoreCase(confirmation, "y")) {
                            self.quit_requested = true;
                            return false;
                        }
                        if (std.ascii.eqlIgnoreCase(confirmation, "no") or std.ascii.eqlIgnoreCase(confirmation, "n")) {
                            try self.setStatus("Canceled");
                            return false;
                        }
                        try self.setStatus("Type yes to quit with unsaved changes, or no to cancel");
                        return true;
                    },
                    .command => {
                        const spec = try self.resolveCommandForInput(state.*, input) orelse {
                            try self.setStatusFmt("Unknown command: {s}", .{input});
                            return true;
                        };

                        self.executeNamedCommand(spec.name) catch |err| {
                            try self.setStatusFmt("Command failed: {s}", .{@errorName(err)});
                            return true;
                        };
                        try self.addHistoryEntry(.command, spec.name);
                    },
                    .goto_line => {
                        const line = std.fmt.parseUnsigned(usize, std.mem.trim(u8, input, " \t"), 10) catch {
                            try self.setStatusFmt("Invalid line number: {s}", .{input});
                            return true;
                        };
                        try self.gotoLine(line);
                    },
                    .replace_from => {
                        const query = std.mem.trim(u8, input, " \t");
                        if (query.len == 0) {
                            try self.setStatus("Replace source cannot be empty");
                            return true;
                        }
                        if (self.pending_replace_from) |old| self.allocator.free(old);
                        const owned = try self.allocator.alloc(u8, query.len);
                        std.mem.copyForwards(u8, owned, query);
                        self.pending_replace_from = owned;
                        try self.addHistoryEntry(.replace_from, query);
                        try self.startMinibuffer(.replace_to);
                    },
                    .replace_to => {
                        const from = self.pending_replace_from orelse {
                            try self.setStatus("Replace source missing");
                            return false;
                        };
                        defer {
                            self.allocator.free(from);
                            self.pending_replace_from = null;
                        }
                        try self.addHistoryEntry(.replace_to, input);
                        try self.replaceStringAll(from, input);
                    },
                    .query_replace_from => {
                        const query = std.mem.trim(u8, input, " \t");
                        if (query.len == 0) {
                            try self.setStatus("Replace source cannot be empty");
                            return true;
                        }
                        if (self.pending_query_replace_from) |old| self.allocator.free(old);
                        const owned = try self.allocator.alloc(u8, query.len);
                        std.mem.copyForwards(u8, owned, query);
                        self.pending_query_replace_from = owned;
                        try self.addHistoryEntry(.query_replace_from, query);
                        try self.startMinibuffer(.query_replace_to);
                    },
                    .query_replace_to => {
                        const from = self.pending_query_replace_from orelse {
                            try self.setStatus("Replace source missing");
                            return false;
                        };
                        self.pending_query_replace_from = null;
                        errdefer self.allocator.free(from);
                        try self.addHistoryEntry(.query_replace_to, input);
                        try self.startQueryReplace(from, input);
                    },
                    .replace_regexp_from => {
                        const query = std.mem.trim(u8, input, " \t");
                        if (query.len == 0) {
                            try self.setStatus("Regexp source cannot be empty");
                            return true;
                        }
                        if (self.pending_replace_regexp_from) |old| self.allocator.free(old);
                        const owned = try self.allocator.alloc(u8, query.len);
                        std.mem.copyForwards(u8, owned, query);
                        self.pending_replace_regexp_from = owned;
                        try self.addHistoryEntry(.replace_regexp_from, query);
                        try self.startMinibuffer(.replace_regexp_to);
                    },
                    .replace_regexp_to => {
                        const from = self.pending_replace_regexp_from orelse {
                            try self.setStatus("Regexp source missing");
                            return false;
                        };
                        defer {
                            self.allocator.free(from);
                            self.pending_replace_regexp_from = null;
                        }
                        try self.addHistoryEntry(.replace_regexp_to, input);
                        try self.replaceRegexpAll(from, input);
                    },
                    .query_replace_regexp_from => {
                        const query = std.mem.trim(u8, input, " \t");
                        if (query.len == 0) {
                            try self.setStatus("Regexp source cannot be empty");
                            return true;
                        }
                        if (self.pending_query_replace_regexp_from) |old| self.allocator.free(old);
                        const owned = try self.allocator.alloc(u8, query.len);
                        std.mem.copyForwards(u8, owned, query);
                        self.pending_query_replace_regexp_from = owned;
                        try self.addHistoryEntry(.query_replace_regexp_from, query);
                        try self.startMinibuffer(.query_replace_regexp_to);
                    },
                    .query_replace_regexp_to => {
                        const from = self.pending_query_replace_regexp_from orelse {
                            try self.setStatus("Regexp source missing");
                            return false;
                        };
                        self.pending_query_replace_regexp_from = null;
                        errdefer self.allocator.free(from);
                        try self.addHistoryEntry(.query_replace_regexp_to, input);
                        try self.startQueryReplaceRegexp(from, input);
                    },
                }
                return false;
            },
            .char => |ch| {
                if (std.ascii.isPrint(ch)) {
                    try self.input_buffer.append(ch);
                    state.history_index = null;
                    state.tab_count = 0;
                    state.ido_index = 0;
                    try self.refreshMinibufferHint(state.*, false);
                }
                return true;
            },
            .meta => |ch| {
                if (ch == 'p' and state.kind != .goto_line and state.kind != .kill_buffer_confirm and state.kind != .quit_confirm) {
                    try self.minibufferHistoryUp(state);
                    state.tab_count = 0;
                    state.ido_index = 0;
                    try self.refreshMinibufferHint(state.*, false);
                    return true;
                }
                if (ch == 'n' and state.kind != .goto_line and state.kind != .kill_buffer_confirm and state.kind != .quit_confirm) {
                    try self.minibufferHistoryDown(state);
                    state.tab_count = 0;
                    state.ido_index = 0;
                    try self.refreshMinibufferHint(state.*, false);
                    return true;
                }
                return true;
            },
            .meta_ctrl, .meta_backspace => return true,
        }
    }

    fn handleISearchKey(self: *Editor, state: *ISearchState, key: Key) !bool {
        switch (key) {
            .escape => {
                const win = self.activeWindow();
                const buf = &self.activeSlot().buffer;
                win.cursor.byte_index = state.anchor;
                win.cursor.preferred_column = null;
                win.ensureCursorVisible(buf);
                try self.setStatus("I-search canceled");
                return false;
            },
            .ctrl => |ch| {
                if (ch == 'g') {
                    const win = self.activeWindow();
                    const buf = &self.activeSlot().buffer;
                    win.cursor.byte_index = state.anchor;
                    win.cursor.preferred_column = null;
                    win.ensureCursorVisible(buf);
                    try self.setStatus("I-search canceled");
                    return false;
                }
                if (ch == 's') {
                    state.direction = .forward;
                    state.matched = self.isearchFind(state.*, true);
                    return true;
                }
                if (ch == 'r') {
                    state.direction = .backward;
                    state.matched = self.isearchFind(state.*, true);
                    return true;
                }
                return true;
            },
            .backspace => {
                if (self.input_buffer.items.len > 0) {
                    self.input_buffer.shrinkRetainingCapacity(self.input_buffer.items.len - 1);
                }
                state.matched = self.isearchFind(state.*, false);
                return true;
            },
            .enter => {
                self.last_search.clearRetainingCapacity();
                try self.last_search.appendSlice(self.input_buffer.items);
                if (self.input_buffer.items.len > 0) {
                    if (state.matched) {
                        try self.setStatusFmt("Search: {s}", .{self.input_buffer.items});
                    } else {
                        try self.setStatusFmt("Not found: {s}", .{self.input_buffer.items});
                    }
                } else {
                    try self.setStatus("Search done");
                }
                return false;
            },
            .char => |ch| {
                if (std.ascii.isPrint(ch)) {
                    try self.input_buffer.append(ch);
                    state.matched = self.isearchFind(state.*, false);
                }
                return true;
            },
            else => return true,
        }
    }

    fn clearPendingForKind(self: *Editor, kind: MinibufferKind) void {
        switch (kind) {
            .kill_buffer, .kill_buffer_confirm => {
                self.pending_kill_buffer_idx = null;
            },
            .quit_confirm => {},
            .replace_from, .replace_to => {
                if (self.pending_replace_from) |from| {
                    self.allocator.free(from);
                    self.pending_replace_from = null;
                }
            },
            .query_replace_from, .query_replace_to => {
                if (self.pending_query_replace_from) |from| {
                    self.allocator.free(from);
                    self.pending_query_replace_from = null;
                }
            },
            .replace_regexp_from, .replace_regexp_to => {
                if (self.pending_replace_regexp_from) |from| {
                    self.allocator.free(from);
                    self.pending_replace_regexp_from = null;
                }
            },
            .query_replace_regexp_from, .query_replace_regexp_to => {
                if (self.pending_query_replace_regexp_from) |from| {
                    self.allocator.free(from);
                    self.pending_query_replace_regexp_from = null;
                }
            },
            else => {},
        }
    }

    const RegexMatch = struct {
        start: usize,
        end: usize,
        captures: [10]?ByteRange = [_]?ByteRange{null} ** 10,
    };

    fn nextLiteralMatch(text: []const u8, needle: []const u8, start: usize) ?RegexMatch {
        const at = search_mod.find(text, needle, start, .forward, .{ .case_sensitive = true, .wrap = false }) orelse return null;
        var captures = [_]?ByteRange{null} ** 10;
        captures[0] = .{ .start = at, .end = at + needle.len };
        return .{
            .start = at,
            .end = at + needle.len,
            .captures = captures,
        };
    }

    fn compileRegex(self: *Editor, pattern: []const u8) !c.regex_t {
        const pattern_z = try self.allocator.allocSentinel(u8, pattern.len, 0);
        defer self.allocator.free(pattern_z);
        std.mem.copyForwards(u8, pattern_z[0..pattern.len], pattern);

        var regex: c.regex_t = undefined;
        const rc = c.regcomp(&regex, pattern_z.ptr, c.REG_EXTENDED);
        if (rc != 0) return error.InvalidRegex;
        return regex;
    }

    fn findRegexMatch(self: *Editor, regex: *const c.regex_t, text: []const u8, start: usize) !?RegexMatch {
        if (start > text.len) return null;

        const segment = text[start..];
        const segment_z = try self.allocator.allocSentinel(u8, segment.len, 0);
        defer self.allocator.free(segment_z);
        std.mem.copyForwards(u8, segment_z[0..segment.len], segment);

        var raw_matches: [10]c.regmatch_t = undefined;
        const rc = c.regexec(regex, segment_z.ptr, raw_matches.len, &raw_matches, 0);
        if (rc == c.REG_NOMATCH) return null;
        if (rc != 0) return error.RegexExecFailed;
        if (raw_matches[0].rm_so < 0 or raw_matches[0].rm_eo < raw_matches[0].rm_so) return null;

        var captures = [_]?ByteRange{null} ** 10;
        for (raw_matches, 0..) |raw, idx| {
            if (raw.rm_so < 0 or raw.rm_eo < raw.rm_so) continue;
            const rel_start: usize = @intCast(raw.rm_so);
            const rel_end: usize = @intCast(raw.rm_eo);
            captures[idx] = .{
                .start = start + rel_start,
                .end = start + rel_end,
            };
        }

        return .{
            .start = captures[0].?.start,
            .end = captures[0].?.end,
            .captures = captures,
        };
    }

    fn appendRegexReplacement(
        self: *Editor,
        out: *std.array_list.Managed(u8),
        replacement: []const u8,
        source_text: []const u8,
        match: RegexMatch,
    ) !void {
        _ = self;
        var idx: usize = 0;
        while (idx < replacement.len) : (idx += 1) {
            if (replacement[idx] != '\\') {
                try out.append(replacement[idx]);
                continue;
            }
            if (idx + 1 >= replacement.len) {
                try out.append('\\');
                continue;
            }

            const esc = replacement[idx + 1];
            if (esc == '&') {
                try out.appendSlice(source_text[match.start..match.end]);
            } else if (esc >= '1' and esc <= '9') {
                const cap_idx = esc - '0';
                if (match.captures[cap_idx]) |cap| {
                    try out.appendSlice(source_text[cap.start..cap.end]);
                }
            } else {
                try out.append(esc);
            }
            idx += 1;
        }
    }

    fn replaceStringAll(self: *Editor, from: []const u8, to: []const u8) !void {
        if (from.len == 0) {
            try self.setStatus("Replace source cannot be empty");
            return;
        }

        const win = self.activeWindow();
        const slot = self.activeSlot();
        const buf = &slot.buffer;

        var count: usize = 0;
        var search_at = win.cursor.byte_index;
        while (nextLiteralMatch(buf.bytes(), from, search_at)) |match| {
            try buf.deleteRange(match.start, match.end - match.start);
            self.adjustMarkAfterDelete(win, match.start, match.end - match.start);
            try buf.insert(match.start, to);
            self.adjustMarkAfterInsert(win, match.start, to.len);
            count += 1;
            search_at = match.start + to.len;
        }

        if (count == 0) {
            try self.setStatusFmt("No matches for {s}", .{from});
            return;
        }

        slot.dirty = true;
        win.cursor.byte_index = @min(search_at, buf.len());
        win.cursor.preferred_column = null;
        win.ensureCursorVisible(buf);
        try self.setStatusFmt("Replaced {d} occurrence{s}", .{ count, if (count == 1) "" else "s" });
    }

    fn replaceRegexpAll(self: *Editor, pattern: []const u8, replacement: []const u8) !void {
        if (pattern.len == 0) {
            try self.setStatus("Regexp source cannot be empty");
            return;
        }

        var regex = self.compileRegex(pattern) catch {
            try self.setStatusFmt("Invalid regexp: {s}", .{pattern});
            return;
        };
        defer c.regfree(&regex);

        const win = self.activeWindow();
        const slot = self.activeSlot();
        const buf = &slot.buffer;

        var count: usize = 0;
        var search_at = win.cursor.byte_index;
        while (try self.findRegexMatch(&regex, buf.bytes(), search_at)) |match| {
            var expanded = std.array_list.Managed(u8).init(self.allocator);
            defer expanded.deinit();
            const snapshot = buf.bytes();
            try self.appendRegexReplacement(&expanded, replacement, snapshot, match);

            try buf.deleteRange(match.start, match.end - match.start);
            self.adjustMarkAfterDelete(win, match.start, match.end - match.start);
            try buf.insert(match.start, expanded.items);
            self.adjustMarkAfterInsert(win, match.start, expanded.items.len);

            count += 1;
            const next_base = match.start + expanded.items.len;
            search_at = if (match.end == match.start)
                @min(buf.len(), next_base + 1)
            else
                next_base;
        }

        if (count == 0) {
            try self.setStatusFmt("No matches for {s}", .{pattern});
            return;
        }

        slot.dirty = true;
        win.cursor.byte_index = @min(search_at, buf.len());
        win.cursor.preferred_column = null;
        win.ensureCursorVisible(buf);
        try self.setStatusFmt("Replaced {d} regexp match{s}", .{ count, if (count == 1) "" else "es" });
    }

    fn startQueryReplace(self: *Editor, from_owned: []u8, to: []const u8) !void {
        if (from_owned.len == 0) {
            self.allocator.free(from_owned);
            try self.setStatus("Replace source cannot be empty");
            return;
        }

        const to_owned = try self.allocator.alloc(u8, to.len);
        errdefer self.allocator.free(to_owned);
        std.mem.copyForwards(u8, to_owned, to);

        const win = self.activeWindow();
        const buf = &self.activeSlot().buffer;
        const next = nextLiteralMatch(buf.bytes(), from_owned, win.cursor.byte_index) orelse {
            try self.setStatusFmt("No matches for {s}", .{from_owned});
            self.allocator.free(from_owned);
            self.allocator.free(to_owned);
            return;
        };

        win.cursor.byte_index = next.start;
        win.cursor.preferred_column = null;
        win.ensureCursorVisible(buf);
        self.input_mode = .{
            .query_replace = .{
                .kind = .literal,
                .from = from_owned,
                .to = to_owned,
                .current_start = next.start,
                .current_end = next.end,
                .captures = next.captures,
            },
        };
    }

    fn startQueryReplaceRegexp(self: *Editor, from_owned: []u8, to: []const u8) !void {
        if (from_owned.len == 0) {
            self.allocator.free(from_owned);
            try self.setStatus("Regexp source cannot be empty");
            return;
        }

        var regex = self.compileRegex(from_owned) catch {
            try self.setStatusFmt("Invalid regexp: {s}", .{from_owned});
            self.allocator.free(from_owned);
            return;
        };
        defer c.regfree(&regex);

        const to_owned = try self.allocator.alloc(u8, to.len);
        errdefer self.allocator.free(to_owned);
        std.mem.copyForwards(u8, to_owned, to);

        const win = self.activeWindow();
        const buf = &self.activeSlot().buffer;
        const next = try self.findRegexMatch(&regex, buf.bytes(), win.cursor.byte_index) orelse {
            try self.setStatusFmt("No matches for {s}", .{from_owned});
            self.allocator.free(from_owned);
            self.allocator.free(to_owned);
            return;
        };

        win.cursor.byte_index = next.start;
        win.cursor.preferred_column = null;
        win.ensureCursorVisible(buf);
        self.input_mode = .{
            .query_replace = .{
                .kind = .regexp,
                .from = from_owned,
                .to = to_owned,
                .current_start = next.start,
                .current_end = next.end,
                .captures = next.captures,
            },
        };
    }

    fn finishQueryReplace(self: *Editor, state: *const QueryReplaceState, canceled: bool) !void {
        if (canceled) {
            try self.setStatusFmt("Query replace canceled: {d} replaced, {d} skipped", .{ state.replaced, state.skipped });
        } else {
            try self.setStatusFmt("Query replace done: {d} replaced, {d} skipped", .{ state.replaced, state.skipped });
        }
    }

    fn advanceQueryReplace(self: *Editor, state: *QueryReplaceState, from_at: usize) !bool {
        const win = self.activeWindow();
        const buf = &self.activeSlot().buffer;
        const next = switch (state.kind) {
            .literal => nextLiteralMatch(buf.bytes(), state.from, from_at),
            .regexp => blk: {
                var regex = self.compileRegex(state.from) catch {
                    try self.setStatusFmt("Invalid regexp: {s}", .{state.from});
                    return false;
                };
                defer c.regfree(&regex);
                break :blk try self.findRegexMatch(&regex, buf.bytes(), from_at);
            },
        } orelse {
            try self.finishQueryReplace(state, false);
            return false;
        };

        state.current_start = next.start;
        state.current_end = next.end;
        state.captures = next.captures;
        win.cursor.byte_index = next.start;
        win.cursor.preferred_column = null;
        win.ensureCursorVisible(buf);
        return true;
    }

    fn queryReplaceCurrent(self: *Editor, state: *QueryReplaceState) !usize {
        const win = self.activeWindow();
        const slot = self.activeSlot();
        const buf = &slot.buffer;
        const at = state.current_start;
        const end = state.current_end;
        if (at > end or end > buf.len()) return @min(buf.len(), end);

        var replacement = std.array_list.Managed(u8).init(self.allocator);
        defer replacement.deinit();
        const snapshot = buf.bytes();
        if (state.kind == .literal) {
            try replacement.appendSlice(state.to);
        } else {
            const match = RegexMatch{
                .start = state.current_start,
                .end = state.current_end,
                .captures = state.captures,
            };
            try self.appendRegexReplacement(&replacement, state.to, snapshot, match);
        }

        try buf.deleteRange(at, end - at);
        self.adjustMarkAfterDelete(win, at, end - at);
        try buf.insert(at, replacement.items);
        self.adjustMarkAfterInsert(win, at, replacement.items.len);
        slot.dirty = true;
        state.replaced += 1;
        if (end == at) {
            return @min(buf.len(), at + replacement.items.len + 1);
        }
        return at + replacement.items.len;
    }

    fn handleQueryReplaceKey(self: *Editor, state: *QueryReplaceState, key: Key) !bool {
        switch (key) {
            .escape => {
                try self.finishQueryReplace(state, true);
                return false;
            },
            .ctrl => |ch| {
                if (ch == 'g') {
                    try self.finishQueryReplace(state, true);
                    return false;
                }
                return true;
            },
            .enter => {
                const next_at = try self.queryReplaceCurrent(state);
                return self.advanceQueryReplace(state, next_at);
            },
            .char => |ch| {
                if (ch == 'y' or ch == ' ') {
                    const next_at = try self.queryReplaceCurrent(state);
                    return self.advanceQueryReplace(state, next_at);
                }
                if (ch == 'n') {
                    state.skipped += 1;
                    const next_at = if (state.current_end == state.current_start)
                        state.current_start + 1
                    else
                        state.current_end;
                    return self.advanceQueryReplace(state, next_at);
                }
                if (ch == '!') {
                    while (true) {
                        const next_at = try self.queryReplaceCurrent(state);
                        const keep = try self.advanceQueryReplace(state, next_at);
                        if (!keep) return false;
                    }
                }
                if (ch == 'q') {
                    try self.finishQueryReplace(state, true);
                    return false;
                }
                return true;
            },
            else => return true,
        }
    }

    fn ctrlLogicalToByte(ch: u8) ?u8 {
        const lower = std.ascii.toLower(ch);
        if (lower == '@') return 0x00;
        if (lower >= 'a' and lower <= 'z') {
            return lower - 'a' + 1;
        }
        return switch (ch) {
            '5' => 0x1d,
            '6' => 0x1e,
            '_' => 0x1f,
            else => null,
        };
    }

    fn quotedByteForKey(key: Key) ?u8 {
        return switch (key) {
            .char => |ch| ch,
            .ctrl => |ch| ctrlLogicalToByte(ch),
            .meta => |ch| ch,
            .meta_ctrl => |ch| ctrlLogicalToByte(ch),
            .meta_backspace => 0x08,
            .tab => '\t',
            .enter => '\n',
            .backspace => 0x7f,
            .escape => 0x1b,
            .arrow_up, .arrow_down, .arrow_left, .arrow_right => null,
        };
    }

    fn handleQuotedInsertKey(self: *Editor, key: Key) !void {
        const byte = quotedByteForKey(key) orelse {
            try self.setStatus("Quoted insert canceled");
            return;
        };

        try self.insertByte(byte);
        try self.setStatusFmt("Quoted 0x{X:0>2}", .{byte});
    }

    fn ensureBufferListBuffer(self: *Editor) !usize {
        for (self.buffers.items, 0..) |slot, idx| {
            if (slot.file_path == null and std.mem.eql(u8, slot.buffer.name.items, "*Buffer List*")) {
                return idx;
            }
        }

        try self.buffers.append(try BufferSlot.initNamed(self.allocator, "*Buffer List*"));
        return self.buffers.items.len - 1;
    }

    fn isBufferListBufferIndex(self: *Editor, buffer_idx: usize) bool {
        if (buffer_idx >= self.buffers.items.len) return false;
        const slot = &self.buffers.items[buffer_idx];
        return slot.file_path == null and std.mem.eql(u8, slot.buffer.name.items, "*Buffer List*");
    }

    fn activeIsBufferList(self: *Editor) bool {
        return self.isBufferListBufferIndex(self.activeWindow().buffer_id);
    }

    fn currentBufferListLine(self: *Editor) usize {
        const win = self.activeWindow();
        const buf = &self.activeSlot().buffer;
        return buf.byteToLineCol(win.cursor.byte_index).line;
    }

    fn bufferListSelectedEntry(self: *Editor) ?usize {
        if (!self.activeIsBufferList()) return null;
        const line = self.currentBufferListLine();
        if (line < 2) return null;
        const entry_idx = line - 2;
        if (entry_idx >= self.buffer_list_entries.items.len) return null;
        return entry_idx;
    }

    fn bufferListSelectedBuffer(self: *Editor) ?usize {
        const entry_idx = self.bufferListSelectedEntry() orelse return null;
        return self.buffer_list_entries.items[entry_idx];
    }

    fn moveCursorToBufferListEntry(self: *Editor, entry_idx: usize) void {
        if (!self.activeIsBufferList()) return;
        const win = self.activeWindow();
        const buf = &self.activeSlot().buffer;
        const target_line = @min(entry_idx + 2, if (buf.lineCount() == 0) 0 else buf.lineCount() - 1);
        win.cursor.byte_index = buf.lineColToByte(target_line, 0);
        win.cursor.preferred_column = null;
        win.ensureCursorVisible(buf);
    }

    fn listBuffers(self: *Editor) !void {
        const list_idx = try self.ensureBufferListBuffer();
        const was_list = self.activeIsBufferList();
        const current_buffer_id = self.activeWindow().buffer_id;
        if (!was_list) {
            self.buffer_list_return_buffer = current_buffer_id;
        }

        const preserved_entry = if (was_list) self.bufferListSelectedEntry() else null;

        var table = std.array_list.Managed(u8).init(self.allocator);
        defer table.deinit();
        self.buffer_list_entries.clearRetainingCapacity();
        try table.appendSlice(" C M D Buffer                 File\n");
        try table.appendSlice(" - - - ------                 ----\n");
        for (self.buffers.items, 0..) |slot, idx| {
            const current: u8 = if (idx == current_buffer_id) '.' else ' ';
            const modified: u8 = if (slot.dirty) '*' else ' ';
            const delete_mark: u8 = if (self.buffer_list_delete_marks.contains(idx)) 'D' else ' ';
            try table.writer().print(" {c} {c} {c} {s}", .{ current, modified, delete_mark, slot.buffer.name.items });
            if (slot.file_path) |path| {
                try table.writer().print("    {s}", .{path});
            }
            try table.append('\n');
            try self.buffer_list_entries.append(idx);
        }

        const list_slot = &self.buffers.items[list_idx];
        try list_slot.buffer.setText(table.items);
        list_slot.dirty = false;

        const win = self.activeWindow();
        win.buffer_id = list_idx;
        win.cursor = .{};
        win.mark = null;
        win.top_line = 0;
        if (preserved_entry) |entry| {
            self.moveCursorToBufferListEntry(@min(entry, if (self.buffer_list_entries.items.len == 0) 0 else self.buffer_list_entries.items.len - 1));
        } else {
            self.moveCursorToBufferListEntry(0);
        }
        try self.setStatus("Buffer list");
    }

    fn bufferListVisitSelected(self: *Editor) !void {
        const buffer_idx = self.bufferListSelectedBuffer() orelse {
            try self.setStatus("No buffer on this line");
            return;
        };
        if (self.isBufferListBufferIndex(buffer_idx)) {
            try self.setStatus("On *Buffer List*");
            return;
        }

        self.buffer_list_return_buffer = null;
        self.buffer_list_delete_marks.clearRetainingCapacity();
        try self.switchToBuffer(buffer_idx);
    }

    fn bufferListUnmarkSelected(self: *Editor) !void {
        const buffer_idx = self.bufferListSelectedBuffer() orelse return;
        _ = self.buffer_list_delete_marks.remove(buffer_idx);
        try self.listBuffers();
    }

    fn bufferListMarkDeleteSelected(self: *Editor) !void {
        const entry_idx = self.bufferListSelectedEntry() orelse {
            try self.setStatus("No buffer on this line");
            return;
        };
        const buffer_idx = self.buffer_list_entries.items[entry_idx];
        if (self.isBufferListBufferIndex(buffer_idx)) {
            try self.setStatus("Cannot mark *Buffer List*");
            return;
        }

        try self.buffer_list_delete_marks.put(buffer_idx, {});
        try self.listBuffers();
        const next_entry = @min(entry_idx + 1, if (self.buffer_list_entries.items.len == 0) 0 else self.buffer_list_entries.items.len - 1);
        self.moveCursorToBufferListEntry(next_entry);
    }

    fn bufferListExecuteDeletes(self: *Editor) !void {
        var targets = std.array_list.Managed(usize).init(self.allocator);
        defer targets.deinit();

        var it = self.buffer_list_delete_marks.iterator();
        while (it.next()) |entry| {
            try targets.append(entry.key_ptr.*);
        }
        if (targets.items.len == 0) {
            try self.setStatus("No buffers marked");
            return;
        }

        std.sort.block(usize, targets.items, {}, struct {
            fn lessThan(_: void, lhs: usize, rhs: usize) bool {
                return lhs > rhs;
            }
        }.lessThan);

        var killed_count: usize = 0;
        for (targets.items) |idx| {
            if (idx >= self.buffers.items.len) continue;
            if (self.isBufferListBufferIndex(idx)) continue;
            if (try self.killBufferAt(idx)) {
                killed_count += 1;
            }
        }

        self.buffer_list_delete_marks.clearRetainingCapacity();
        try self.listBuffers();
        try self.setStatusFmt("Killed {d} buffer{s}", .{ killed_count, if (killed_count == 1) "" else "s" });
    }

    fn quitBufferList(self: *Editor) !void {
        const return_idx = self.buffer_list_return_buffer orelse {
            try self.setStatus("No previous buffer");
            return;
        };
        self.buffer_list_return_buffer = null;
        self.buffer_list_delete_marks.clearRetainingCapacity();
        if (return_idx >= self.buffers.items.len or self.isBufferListBufferIndex(return_idx)) {
            try self.setStatus("Previous buffer unavailable");
            return;
        }
        try self.switchToBuffer(return_idx);
    }

    fn handleBufferListKey(self: *Editor, key: Key) !bool {
        if (!self.activeIsBufferList()) return false;

        switch (key) {
            .enter => {
                try self.bufferListVisitSelected();
                return true;
            },
            .char => |ch| switch (ch) {
                'f' => {
                    try self.bufferListVisitSelected();
                    return true;
                },
                'd', 'k' => {
                    try self.bufferListMarkDeleteSelected();
                    return true;
                },
                'u' => {
                    try self.bufferListUnmarkSelected();
                    return true;
                },
                'x' => {
                    try self.bufferListExecuteDeletes();
                    return true;
                },
                'g' => {
                    try self.listBuffers();
                    return true;
                },
                'q' => {
                    try self.quitBufferList();
                    return true;
                },
                else => return false,
            },
            else => return false,
        }
    }

    fn executeCommand(self: *Editor, command: Command) !void {
        const win = self.activeWindow();
        const slot = self.activeSlot();
        const buf = &slot.buffer;
        var did_kill = false;

        switch (command) {
            .keyboard_quit => {
                self.pending_keys.clearRetainingCapacity();
                self.prefix_hint.clearRetainingCapacity();
                win.mark = null;
                try self.setStatus("Quit");
            },
            .move_left => win.moveLeft(buf),
            .move_right => win.moveRight(buf),
            .move_up => win.moveUp(buf),
            .move_down => win.moveDown(buf),
            .move_line_start => {
                const pos = buf.byteToLineCol(win.cursor.byte_index);
                win.cursor.byte_index = buf.lineColToByte(pos.line, 0);
                win.cursor.preferred_column = null;
                win.ensureCursorVisible(buf);
            },
            .move_line_end => {
                const pos = buf.byteToLineCol(win.cursor.byte_index);
                win.cursor.byte_index = buf.lineColToByte(pos.line, std.math.maxInt(usize));
                win.cursor.preferred_column = null;
                win.ensureCursorVisible(buf);
            },
            .move_buffer_start => {
                win.cursor.byte_index = 0;
                win.cursor.preferred_column = null;
                win.ensureCursorVisible(buf);
            },
            .move_buffer_end => {
                win.cursor.byte_index = buf.len();
                win.cursor.preferred_column = null;
                win.ensureCursorVisible(buf);
            },
            .page_up => self.pageMove(-1),
            .page_down => self.pageMove(1),
            .move_word_forward => try self.moveWordForward(),
            .move_word_backward => try self.moveWordBackward(),
            .goto_line_prompt => try self.startMinibuffer(.goto_line),
            .replace_string_prompt => try self.startMinibuffer(.replace_from),
            .query_replace_prompt => try self.startMinibuffer(.query_replace_from),
            .replace_regexp_prompt => try self.startMinibuffer(.replace_regexp_from),
            .query_replace_regexp_prompt => try self.startMinibuffer(.query_replace_regexp_from),
            .command_prompt => try self.startMinibuffer(.command),
            .open_file_prompt => try self.startMinibuffer(.open_file),
            .save_as_prompt => try self.startMinibuffer(.save_as),
            .buffer_prompt => try self.startMinibuffer(.switch_buffer),
            .list_buffers => try self.listBuffers(),
            .kill_buffer_prompt => try self.startMinibuffer(.kill_buffer),
            .split_window_below => try self.splitWindowBelow(),
            .split_window_right => try self.splitWindowRight(),
            .delete_window => try self.deleteWindow(),
            .delete_other_windows => try self.deleteOtherWindows(),
            .other_window => try self.otherWindow(),
            .search_forward => try self.startISearch(.forward),
            .search_backward => try self.startISearch(.backward),
            .quoted_insert => {
                self.input_mode = .quoted_insert;
                try self.setStatus("Quoted insert:");
            },
            .delete_backward => {
                if (win.cursor.byte_index > 0) {
                    const at = win.cursor.byte_index - 1;
                    try buf.deleteRange(at, 1);
                    self.adjustMarkAfterDelete(win, at, 1);
                    win.cursor.byte_index = at;
                    win.cursor.preferred_column = null;
                    slot.dirty = true;
                    win.ensureCursorVisible(buf);
                }
            },
            .delete_forward => {
                if (win.cursor.byte_index < buf.len()) {
                    try buf.deleteRange(win.cursor.byte_index, 1);
                    self.adjustMarkAfterDelete(win, win.cursor.byte_index, 1);
                    win.cursor.preferred_column = null;
                    slot.dirty = true;
                    win.ensureCursorVisible(buf);
                }
            },
            .kill_word => did_kill = try self.killWordForward(),
            .backward_kill_word => did_kill = try self.killWordBackward(),
            .insert_newline => {
                try buf.insert(win.cursor.byte_index, "\n");
                self.adjustMarkAfterInsert(win, win.cursor.byte_index, 1);
                win.cursor.byte_index += 1;
                win.cursor.preferred_column = null;
                slot.dirty = true;
                win.ensureCursorVisible(buf);
            },
            .open_line => {
                const at = win.cursor.byte_index;
                try buf.insert(at, "\n");
                self.adjustMarkAfterInsert(win, at, 1);
                win.cursor.byte_index = at;
                win.cursor.preferred_column = null;
                slot.dirty = true;
                win.ensureCursorVisible(buf);
            },
            .transpose_chars => try self.transposeChars(),
            .transpose_words => try self.transposeWords(),
            .set_mark => try self.setMarkCommand(),
            .pop_mark => try self.popMarkCommand(),
            .exchange_point_and_mark => {
                if (win.mark) |mark| {
                    win.mark = win.cursor.byte_index;
                    win.cursor.byte_index = mark;
                    win.cursor.preferred_column = null;
                    win.ensureCursorVisible(buf);
                    try self.setStatus("Exchanged point and mark");
                } else {
                    try self.setStatus("No mark set");
                }
            },
            .kill_region => did_kill = try self.killRegion(true),
            .copy_region_as_kill => _ = try self.killRegion(false),
            .mark_whole_buffer => {
                win.mark = buf.len();
                win.cursor.byte_index = 0;
                win.cursor.preferred_column = null;
                win.ensureCursorVisible(buf);
                try self.setStatus("Mark set (whole buffer)");
            },
            .kill_line => did_kill = try self.killLine(),
            .yank => try self.yank(),
            .yank_pop => try self.yankPop(),
            .undo => {
                if (try buf.undo()) {
                    win.clampCursor(buf);
                    slot.dirty = true;
                    try self.setStatus("Undo");
                }
            },
            .redo => {
                if (try buf.redo()) {
                    win.clampCursor(buf);
                    slot.dirty = true;
                    try self.setStatus("Redo");
                }
            },
            .next_buffer => try self.nextBuffer(),
            .previous_buffer => try self.previousBuffer(),
            .save => try self.saveActiveBuffer(),
            .save_some_buffers => try self.startSaveSomeBuffers(false),
            .save_buffers_kill_terminal => {
                if (self.countDirtyBuffers() > 0) {
                    try self.startSaveSomeBuffers(true);
                } else {
                    self.quit_requested = true;
                }
            },
            .quit => {
                self.quit_requested = true;
            },
        }

        if (command != .yank and command != .yank_pop) {
            self.last_yank = null;
        }
        self.last_command_was_kill = did_kill;
        self.last_kill_cursor = if (did_kill) self.activeWindow().cursor.byte_index else null;
        self.last_command = command;
    }

    fn handleFallback(self: *Editor, key: Key) !void {
        if (try self.handleBufferListKey(key)) return;

        switch (key) {
            .char => |ch| {
                if (std.ascii.isPrint(ch) or ch == '\t') {
                    try self.insertByte(ch);
                }
            },
            .tab => try self.insertByte('\t'),
            .enter => try self.executeCommand(.insert_newline),
            .backspace => try self.executeCommand(.delete_backward),
            else => {},
        }
    }

    fn insertByte(self: *Editor, byte: u8) !void {
        const win = self.activeWindow();
        const slot = self.activeSlot();
        const buf = &slot.buffer;
        const single = [1]u8{byte};

        try buf.insert(win.cursor.byte_index, single[0..]);
        self.adjustMarkAfterInsert(win, win.cursor.byte_index, 1);
        win.cursor.byte_index += 1;
        win.cursor.preferred_column = null;
        slot.dirty = true;
        win.ensureCursorVisible(buf);
        self.last_command_was_kill = false;
        self.last_kill_cursor = null;
        self.last_yank = null;
        self.last_command = null;
    }

    fn saveAs(self: *Editor, path: []const u8) !void {
        const slot = self.activeSlot();
        try slot.setFilePath(self.allocator, path);
        try FileIO.saveFromBuffer(path, &slot.buffer);
        slot.dirty = false;
        try self.setStatusFmt("Saved {s}", .{std.fs.path.basename(path)});
    }

    fn saveActiveBuffer(self: *Editor) !void {
        const slot = self.activeSlot();

        const path = slot.file_path orelse {
            try self.setStatus("No file path. Use Ctrl-X Ctrl-W for save-as.");
            return;
        };

        try FileIO.saveFromBuffer(path, &slot.buffer);
        slot.dirty = false;
        try self.setStatusFmt("Saved {s}", .{std.fs.path.basename(path)});
    }

    fn countDirtyBuffers(self: *const Editor) usize {
        var count: usize = 0;
        for (self.buffers.items) |slot| {
            if (slot.dirty) count += 1;
        }
        return count;
    }

    fn startSaveSomeBuffers(self: *Editor, quit_after: bool) !void {
        var dirty_indices = std.array_list.Managed(usize).init(self.allocator);
        errdefer dirty_indices.deinit();

        for (self.buffers.items, 0..) |slot, idx| {
            if (slot.dirty) {
                try dirty_indices.append(idx);
            }
        }

        if (dirty_indices.items.len == 0) {
            if (quit_after) {
                self.quit_requested = true;
            } else {
                try self.setStatus("No modified buffers");
            }
            return;
        }

        var path_input = std.array_list.Managed(u8).init(self.allocator);
        errdefer path_input.deinit();

        self.pending_keys.clearRetainingCapacity();
        self.prefix_hint.clearRetainingCapacity();
        self.input_mode = .{
            .save_some_buffers = .{
                .dirty_indices = dirty_indices,
                .path_input = path_input,
                .quit_after = quit_after,
            },
        };
    }

    fn saveBufferAtIndex(self: *Editor, idx: usize) !SaveBufferOutcome {
        if (idx >= self.buffers.items.len) return .saved;
        const slot = &self.buffers.items[idx];
        if (!slot.dirty) return .saved;
        const path = slot.file_path orelse return .needs_path;
        try FileIO.saveFromBuffer(path, &slot.buffer);
        slot.dirty = false;
        return .saved;
    }

    fn saveBufferAtIndexWithPath(self: *Editor, idx: usize, path: []const u8) !void {
        if (idx >= self.buffers.items.len) return error.BufferUnavailable;
        const slot = &self.buffers.items[idx];
        try slot.setFilePath(self.allocator, path);
        try FileIO.saveFromBuffer(path, &slot.buffer);
        slot.dirty = false;
    }

    fn beginSaveSomeWritePath(self: *Editor, state: *SaveSomeBuffersState) !void {
        _ = self;
        state.phase = .write_path;
        state.path_input.clearRetainingCapacity();
    }

    fn advanceSaveSomeBuffers(self: *Editor, state: *SaveSomeBuffersState, save_current: bool) !bool {
        if (state.index >= state.dirty_indices.items.len) {
            try self.finishSaveSomeBuffers(state, false);
            return false;
        }

        if (save_current) {
            const idx = state.dirty_indices.items[state.index];
            const outcome = self.saveBufferAtIndex(idx) catch {
                state.failed += 1;
                state.index += 1;
                if (state.index >= state.dirty_indices.items.len) {
                    try self.finishSaveSomeBuffers(state, false);
                    return false;
                }
                return true;
            };
            switch (outcome) {
                .saved => state.saved += 1,
                .needs_path => {
                    try self.beginSaveSomeWritePath(state);
                    return true;
                },
            }
        } else {
            state.skipped += 1;
        }

        state.index += 1;
        if (state.index >= state.dirty_indices.items.len) {
            try self.finishSaveSomeBuffers(state, false);
            return false;
        }
        return true;
    }

    fn finishSaveSomeBuffers(self: *Editor, state: *const SaveSomeBuffersState, canceled: bool) !void {
        if (canceled) {
            try self.setStatusFmt(
                "{s}: {d} saved, {d} skipped, {d} failed",
                .{
                    if (state.quit_after) "Quit canceled" else "Save some buffers canceled",
                    state.saved,
                    state.skipped,
                    state.failed,
                },
            );
            return;
        }
        try self.setStatusFmt(
            "{s}: {d} saved, {d} skipped, {d} failed",
            .{
                if (state.quit_after) "Quit after save" else "Save some buffers done",
                state.saved,
                state.skipped,
                state.failed,
            },
        );
    }

    fn saveSomeCurrentIndex(self: *const Editor, state: *const SaveSomeBuffersState) ?usize {
        _ = self;
        if (state.index >= state.dirty_indices.items.len) return null;
        return state.dirty_indices.items[state.index];
    }

    fn completeSaveSomeWritePath(self: *Editor, state: *SaveSomeBuffersState) !bool {
        const idx = self.saveSomeCurrentIndex(state) orelse {
            try self.finishSaveSomeBuffers(state, false);
            return false;
        };

        const path = std.mem.trim(u8, state.path_input.items, " \t");
        if (path.len == 0) {
            try self.setStatus("Write file path cannot be empty");
            return true;
        }

        self.saveBufferAtIndexWithPath(idx, path) catch |err| {
            state.failed += 1;
            try self.setStatusFmt("Write failed: {s}", .{@errorName(err)});
            return true;
        };

        state.saved += 1;
        state.index += 1;
        state.phase = .decision;
        state.path_input.clearRetainingCapacity();

        if (state.index >= state.dirty_indices.items.len) {
            try self.finishSaveSomeBuffers(state, false);
            return false;
        }

        if (state.auto_yes_remaining) {
            while (state.index < state.dirty_indices.items.len) {
                const keep = try self.advanceSaveSomeBuffers(state, true);
                if (!keep) return false;
                if (state.phase == .write_path) return true;
            }
            return false;
        }

        return true;
    }

    fn handleSaveSomeWritePathKey(self: *Editor, state: *SaveSomeBuffersState, key: Key) !bool {
        switch (key) {
            .escape => {
                state.canceled = true;
                try self.finishSaveSomeBuffers(state, true);
                return false;
            },
            .ctrl => |ch| {
                if (ch == 'g') {
                    state.canceled = true;
                    try self.finishSaveSomeBuffers(state, true);
                    return false;
                }
                return true;
            },
            .backspace => {
                if (state.path_input.items.len > 0) {
                    state.path_input.shrinkRetainingCapacity(state.path_input.items.len - 1);
                }
                return true;
            },
            .enter => return self.completeSaveSomeWritePath(state),
            .char => |ch| {
                if (std.ascii.isPrint(ch)) {
                    try state.path_input.append(ch);
                }
                return true;
            },
            .tab => {
                try state.path_input.append('\t');
                return true;
            },
            else => return true,
        }
    }

    fn handleSaveSomeBuffersKey(self: *Editor, state: *SaveSomeBuffersState, key: Key) !bool {
        if (state.phase == .write_path) {
            return self.handleSaveSomeWritePathKey(state, key);
        }

        switch (key) {
            .escape => {
                state.canceled = true;
                try self.finishSaveSomeBuffers(state, true);
                return false;
            },
            .ctrl => |ch| {
                if (ch == 'g') {
                    state.canceled = true;
                    try self.finishSaveSomeBuffers(state, true);
                    return false;
                }
                return true;
            },
            .char => |ch| {
                switch (ch) {
                    'y', 'Y' => {
                        state.auto_yes_remaining = false;
                        return self.advanceSaveSomeBuffers(state, true);
                    },
                    'n', 'N' => {
                        state.auto_yes_remaining = false;
                        return self.advanceSaveSomeBuffers(state, false);
                    },
                    '!' => {
                        state.auto_yes_remaining = true;
                        while (state.index < state.dirty_indices.items.len) {
                            const keep = try self.advanceSaveSomeBuffers(state, true);
                            if (!keep) return false;
                            if (state.phase == .write_path) return true;
                        }
                        try self.finishSaveSomeBuffers(state, false);
                        return false;
                    },
                    'q', 'Q' => {
                        state.canceled = true;
                        try self.finishSaveSomeBuffers(state, true);
                        return false;
                    },
                    else => return true,
                }
            },
            else => return true,
        }
    }

    fn executeNamedCommand(self: *Editor, name: []const u8) !void {
        const trimmed = std.mem.trim(u8, name, " \t");
        if (trimmed.len == 0) return error.UnknownCommand;
        const spec = self.findCommandSpec(trimmed) orelse return error.UnknownCommand;

        return switch (spec.id) {
            .keyboard_quit => self.executeCommand(.keyboard_quit),
            .find_file => self.startMinibuffer(.open_file),
            .write_file => self.startMinibuffer(.save_as),
            .save_buffer => self.executeCommand(.save),
            .save_some_buffers => self.executeCommand(.save_some_buffers),
            .save_buffers_kill_terminal => self.executeCommand(.save_buffers_kill_terminal),
            .switch_buffer => self.startMinibuffer(.switch_buffer),
            .list_buffers => self.executeCommand(.list_buffers),
            .kill_buffer => self.startMinibuffer(.kill_buffer),
            .next_buffer => self.executeCommand(.next_buffer),
            .previous_buffer => self.executeCommand(.previous_buffer),
            .split_window_below => self.executeCommand(.split_window_below),
            .split_window_right => self.executeCommand(.split_window_right),
            .delete_window => self.executeCommand(.delete_window),
            .delete_other_windows => self.executeCommand(.delete_other_windows),
            .other_window => self.executeCommand(.other_window),
            .beginning_of_line => self.executeCommand(.move_line_start),
            .end_of_line => self.executeCommand(.move_line_end),
            .beginning_of_buffer => self.executeCommand(.move_buffer_start),
            .end_of_buffer => self.executeCommand(.move_buffer_end),
            .scroll_up_command => self.executeCommand(.page_down),
            .scroll_down_command => self.executeCommand(.page_up),
            .goto_line => self.startMinibuffer(.goto_line),
            .replace_string => self.executeCommand(.replace_string_prompt),
            .query_replace => self.executeCommand(.query_replace_prompt),
            .replace_regexp => self.executeCommand(.replace_regexp_prompt),
            .query_replace_regexp => self.executeCommand(.query_replace_regexp_prompt),
            .forward_word => self.executeCommand(.move_word_forward),
            .backward_word => self.executeCommand(.move_word_backward),
            .delete_char => self.executeCommand(.delete_forward),
            .kill_word => self.executeCommand(.kill_word),
            .backward_kill_word => self.executeCommand(.backward_kill_word),
            .open_line => self.executeCommand(.open_line),
            .transpose_chars => self.executeCommand(.transpose_chars),
            .transpose_words => self.executeCommand(.transpose_words),
            .set_mark_command => self.executeCommand(.set_mark),
            .pop_mark => self.executeCommand(.pop_mark),
            .exchange_point_and_mark => self.executeCommand(.exchange_point_and_mark),
            .kill_region => self.executeCommand(.kill_region),
            .copy_region_as_kill => self.executeCommand(.copy_region_as_kill),
            .mark_whole_buffer => self.executeCommand(.mark_whole_buffer),
            .kill_line => self.executeCommand(.kill_line),
            .yank => self.executeCommand(.yank),
            .yank_pop => self.executeCommand(.yank_pop),
            .undo => self.executeCommand(.undo),
            .redo => self.executeCommand(.redo),
            .isearch_forward => self.startISearch(.forward),
            .isearch_backward => self.startISearch(.backward),
            .quoted_insert => self.executeCommand(.quoted_insert),
            .quit => self.executeCommand(.quit),
        };
    }

    fn completeCommandInput(self: *Editor, list_candidates: bool) !void {
        _ = list_candidates;
        const query = std.mem.trim(u8, self.input_buffer.items, " \t");
        if (query.len == 0) return;

        var matches = std.array_list.Managed(CommandMatch).init(self.allocator);
        defer matches.deinit();
        try self.collectCommandMatches(query, &matches);

        if (matches.items.len == 0) return;

        if (matches.items.len == 1) {
            try self.replaceInputBuffer(matches.items[0].spec.name);
            return;
        }

        if (self.findCommandSpec(query)) |exact| {
            try self.replaceInputBuffer(exact.name);
            return;
        }

        const first = matches.items[0].spec.name;
        var lcp_len = first.len;
        for (matches.items[1..]) |match| {
            lcp_len = @min(lcp_len, commonPrefixLen(first, match.spec.name));
        }
        if (lcp_len > query.len) {
            try self.replaceInputBuffer(first[0..lcp_len]);
        }
    }

    fn completePathInput(self: *Editor, state: MinibufferState) !void {
        const input = self.input_buffer.items;
        const path_ctx = splitPathInput(input);

        var matches = std.array_list.Managed(PathMatch).init(self.allocator);
        defer matches.deinit();
        try self.collectPathMatches(path_ctx.base, path_ctx.dir_path, &matches);
        if (matches.items.len == 0) return;

        const idx = @min(state.ido_index, matches.items.len - 1);
        try self.replaceInputWithPathSelection(path_ctx.dir_prefix, &matches.items[idx]);
    }

    fn completeBufferInput(self: *Editor, state: MinibufferState) !void {
        const query = std.mem.trim(u8, self.input_buffer.items, " \t");
        var matches = std.array_list.Managed(BufferMatch).init(self.allocator);
        defer matches.deinit();
        try self.collectBufferMatches(query, &matches);
        if (matches.items.len == 0) return;

        const idx = @min(state.ido_index, matches.items.len - 1);
        try self.replaceInputBuffer(matches.items[idx].name);
    }

    fn replaceInputWithPathSelection(self: *Editor, dir_prefix: []const u8, match: *const PathMatch) !void {
        var out = std.array_list.Managed(u8).init(self.allocator);
        defer out.deinit();
        try out.appendSlice(dir_prefix);
        try out.appendSlice(match.slice());
        if (match.is_dir) try out.append(std.fs.path.sep);
        try self.replaceInputBuffer(out.items);
    }

    fn pathMatchBetter(
        lhs_score: fuzzy_mod.MatchScore,
        lhs_name: []const u8,
        rhs_score: fuzzy_mod.MatchScore,
        rhs_name: []const u8,
    ) bool {
        if (lhs_score.score != rhs_score.score) return lhs_score.score > rhs_score.score;
        if (lhs_score.first_index != rhs_score.first_index) return lhs_score.first_index < rhs_score.first_index;
        if (lhs_name.len != rhs_name.len) return lhs_name.len < rhs_name.len;
        return std.mem.lessThan(u8, lhs_name, rhs_name);
    }

    fn openDirForCompletion(path: []const u8) !std.fs.Dir {
        if (std.fs.path.isAbsolute(path)) {
            return std.fs.openDirAbsolute(path, .{ .iterate = true });
        }
        return std.fs.cwd().openDir(path, .{ .iterate = true });
    }

    fn splitPathInput(input: []const u8) PathInput {
        const sep = std.fs.path.sep;
        const slash_idx_opt = std.mem.lastIndexOfScalar(u8, input, sep);
        const dir_prefix = if (slash_idx_opt) |idx| input[0 .. idx + 1] else "";
        const base = if (slash_idx_opt) |idx| input[idx + 1 ..] else input;
        const dir_path = if (slash_idx_opt) |idx|
            if (idx == 0) input[0..1] else input[0..idx]
        else
            ".";
        return .{
            .dir_prefix = dir_prefix,
            .dir_path = dir_path,
            .base = base,
        };
    }

    fn collectPathMatches(
        self: *Editor,
        base_query: []const u8,
        dir_path: []const u8,
        out: *std.array_list.Managed(PathMatch),
    ) !void {
        _ = self;
        var dir = openDirForCompletion(dir_path) catch return;
        defer dir.close();

        var it = dir.iterate();
        while (try it.next()) |entry| {
            if (entry.name.len > std.fs.max_name_bytes) continue;
            const score = fuzzy_mod.score(entry.name, base_query) orelse continue;

            var match = PathMatch{
                .name_len = entry.name.len,
                .name = undefined,
                .is_dir = entry.kind == .directory,
                .score = score,
            };
            std.mem.copyForwards(u8, match.name[0..entry.name.len], entry.name);
            try out.append(match);
        }

        std.sort.block(PathMatch, out.items, {}, pathStoredMatchLessThan);
    }

    fn pathStoredMatchLessThan(_: void, lhs: PathMatch, rhs: PathMatch) bool {
        return pathMatchBetter(lhs.score, lhs.slice(), rhs.score, rhs.slice());
    }

    fn collectBufferMatches(self: *Editor, query: []const u8, out: *std.array_list.Managed(BufferMatch)) !void {
        for (self.buffers.items, 0..) |slot, idx| {
            const name = slot.displayName();
            const score = fuzzy_mod.score(name, query) orelse continue;
            try out.append(.{
                .buffer_index = idx,
                .name = name,
                .score = score,
            });
        }
        std.sort.block(BufferMatch, out.items, {}, bufferMatchLessThan);
    }

    fn bufferMatchLessThan(_: void, lhs: BufferMatch, rhs: BufferMatch) bool {
        if (lhs.score.score != rhs.score.score) return lhs.score.score > rhs.score.score;
        if (lhs.score.first_index != rhs.score.first_index) return lhs.score.first_index < rhs.score.first_index;
        if (lhs.name.len != rhs.name.len) return lhs.name.len < rhs.name.len;
        return std.mem.lessThan(u8, lhs.name, rhs.name);
    }

    fn resolveBufferForInput(self: *Editor, state: MinibufferState, input: []const u8) !?usize {
        const query = std.mem.trim(u8, input, " \t");
        if (query.len > 0) {
            for (self.buffers.items, 0..) |slot, idx| {
                if (std.ascii.eqlIgnoreCase(slot.displayName(), query)) return idx;
            }
        }

        var matches = std.array_list.Managed(BufferMatch).init(self.allocator);
        defer matches.deinit();
        try self.collectBufferMatches(query, &matches);
        if (matches.items.len == 0) return null;

        const idx = @min(state.ido_index, matches.items.len - 1);
        return matches.items[idx].buffer_index;
    }

    fn resolvePathForInput(self: *Editor, state: MinibufferState, input: []const u8) !?[]u8 {
        const path_ctx = splitPathInput(input);
        var matches = std.array_list.Managed(PathMatch).init(self.allocator);
        defer matches.deinit();
        try self.collectPathMatches(path_ctx.base, path_ctx.dir_path, &matches);
        if (matches.items.len == 0) return null;

        const idx = @min(state.ido_index, matches.items.len - 1);
        var out = std.array_list.Managed(u8).init(self.allocator);
        errdefer out.deinit();
        try out.appendSlice(path_ctx.dir_prefix);
        try out.appendSlice(matches.items[idx].slice());
        if (matches.items[idx].is_dir) try out.append(std.fs.path.sep);
        return try out.toOwnedSlice();
    }

    fn switchToBuffer(self: *Editor, buffer_index: usize) !void {
        if (buffer_index >= self.buffers.items.len) return;
        const win = self.activeWindow();
        win.buffer_id = buffer_index;
        win.cursor = .{};
        win.mark = null;
        win.top_line = 0;
        const slot = self.activeSlot();
        try self.setStatusFmt("Switched to {s}{s}", .{ slot.displayName(), if (slot.dirty) "*" else "" });
    }

    fn killBufferAt(self: *Editor, buffer_index: usize) !bool {
        if (buffer_index >= self.buffers.items.len) return false;
        if (self.buffers.items.len <= 1) {
            try self.setStatus("Cannot kill last buffer");
            return false;
        }

        var removed = self.buffers.orderedRemove(buffer_index);
        const killed_name = try std.fmt.allocPrint(self.allocator, "{s}", .{removed.displayName()});
        defer self.allocator.free(killed_name);
        removed.deinit(self.allocator);

        for (self.windows.items) |*win| {
            if (win.buffer_id == buffer_index) {
                win.buffer_id = if (buffer_index >= self.buffers.items.len)
                    self.buffers.items.len - 1
                else
                    buffer_index;
                win.cursor = .{};
                win.mark = null;
                win.top_line = 0;
            } else if (win.buffer_id > buffer_index) {
                win.buffer_id -= 1;
            }
        }

        try self.setStatusFmt("Killed {s}", .{killed_name});
        return true;
    }

    fn isIdoKind(kind: MinibufferKind) bool {
        return switch (kind) {
            .open_file, .save_as, .switch_buffer, .kill_buffer, .command => true,
            .kill_buffer_confirm,
            .quit_confirm,
            .goto_line,
            .replace_from,
            .replace_to,
            .query_replace_from,
            .query_replace_to,
            .replace_regexp_from,
            .replace_regexp_to,
            .query_replace_regexp_from,
            .query_replace_regexp_to,
            => false,
        };
    }

    fn refreshMinibufferHint(self: *Editor, state: MinibufferState, show_candidates: bool) !void {
        _ = show_candidates;
        self.minibuffer_hint.clearRetainingCapacity();

        switch (state.kind) {
            .command => {
                const query = std.mem.trim(u8, self.input_buffer.items, " \t");
                var matches = std.array_list.Managed(CommandMatch).init(self.allocator);
                defer matches.deinit();
                try self.collectCommandMatches(query, &matches);

                if (matches.items.len == 0) {
                    try self.minibuffer_hint.appendSlice("No matching command");
                    return;
                }

                if (matches.items.len == 1) {
                    try self.appendCommandHint(matches.items[0].spec);
                    return;
                }

                const selected = self.selectedCommandMatchIndex(state, query, matches.items);
                try self.setCommandMinibufferHintMatches(&matches, selected);
            },
            .open_file, .save_as => {
                const path_ctx = splitPathInput(self.input_buffer.items);
                var matches = std.array_list.Managed(PathMatch).init(self.allocator);
                defer matches.deinit();
                try self.collectPathMatches(path_ctx.base, path_ctx.dir_path, &matches);

                if (matches.items.len == 0) {
                    try self.minibuffer_hint.appendSlice("No matching path");
                    return;
                }

                const selected = @min(state.ido_index, matches.items.len - 1);
                try self.setPathMinibufferHintMatches(&matches, path_ctx.dir_prefix, selected);
            },
            .switch_buffer, .kill_buffer => {
                const query = std.mem.trim(u8, self.input_buffer.items, " \t");
                var matches = std.array_list.Managed(BufferMatch).init(self.allocator);
                defer matches.deinit();
                try self.collectBufferMatches(query, &matches);

                if (matches.items.len == 0) {
                    try self.minibuffer_hint.appendSlice("No matching buffer");
                    return;
                }

                const selected = @min(state.ido_index, matches.items.len - 1);
                try self.setBufferMinibufferHintMatches(&matches, selected);
            },
            .kill_buffer_confirm => {
                try self.minibuffer_hint.appendSlice("Buffer modified. Type yes to kill, C-g to cancel");
            },
            .quit_confirm => {
                const dirty_count = self.countDirtyBuffers();
                if (dirty_count == 0) {
                    try self.minibuffer_hint.appendSlice("No modified buffers");
                    return;
                }
                try self.minibuffer_hint.writer().print(
                    "{d} modified buffer{s}: ",
                    .{ dirty_count, if (dirty_count == 1) "" else "s" },
                );
                var shown: usize = 0;
                for (self.buffers.items) |slot| {
                    if (!slot.dirty) continue;
                    if (shown > 0) try self.minibuffer_hint.appendSlice(", ");
                    try self.minibuffer_hint.appendSlice(slot.displayName());
                    shown += 1;
                    if (shown >= 3 and dirty_count > shown) {
                        try self.minibuffer_hint.writer().print(", +{d} more", .{dirty_count - shown});
                        break;
                    }
                }
                try self.minibuffer_hint.appendSlice("  (yes/no)");
            },
            .goto_line => {
                try self.minibuffer_hint.appendSlice("Enter a 1-based line number");
            },
            .replace_from, .query_replace_from => {
                try self.minibuffer_hint.appendSlice("Literal text to search for");
            },
            .replace_to, .query_replace_to => {
                try self.minibuffer_hint.appendSlice("Replacement text (empty allowed)");
            },
            .replace_regexp_from, .query_replace_regexp_from => {
                try self.minibuffer_hint.appendSlice("Regexp pattern");
            },
            .replace_regexp_to, .query_replace_regexp_to => {
                try self.minibuffer_hint.appendSlice("Replacement (supports \\\\&, \\\\1..\\\\9)");
            },
        }
    }

    fn appendCommandHint(self: *Editor, spec: *const CommandSpec) !void {
        try self.minibuffer_hint.writer().print("{s}", .{spec.description});
        if (spec.aliases.len > 0) {
            try self.minibuffer_hint.appendSlice(" | aliases: ");
            for (spec.aliases, 0..) |alias, idx| {
                if (idx > 0) try self.minibuffer_hint.appendSlice(", ");
                try self.minibuffer_hint.appendSlice(alias);
            }
        }
    }

    fn setCommandMinibufferHintMatches(
        self: *Editor,
        matches: *const std.array_list.Managed(CommandMatch),
        selected: usize,
    ) !void {
        try self.appendIdoListHeader(selected, matches.items.len);

        const visible = self.idoVisibleWindow(selected, matches.items.len);
        var start = visible.start;
        const end = visible.end;

        while (start < end) : (start += 1) {
            if (start > visible.start) try self.minibuffer_hint.appendSlice(" | ");
            const name = matches.items[start].spec.name;
            if (start == selected) {
                try self.minibuffer_hint.writer().print("[{s}]", .{name});
            } else {
                try self.minibuffer_hint.appendSlice(name);
            }
        }
        try self.appendIdoWindowTail(visible.start, visible.end, matches.items.len);
    }

    fn setPathMinibufferHintMatches(
        self: *Editor,
        matches: *const std.array_list.Managed(PathMatch),
        dir_prefix: []const u8,
        selected: usize,
    ) !void {
        try self.appendIdoListHeader(selected, matches.items.len);
        const visible = self.idoVisibleWindow(selected, matches.items.len);

        var idx = visible.start;
        while (idx < visible.end) : (idx += 1) {
            if (idx > visible.start) try self.minibuffer_hint.appendSlice(" | ");
            const match = &matches.items[idx];
            if (idx == selected) try self.minibuffer_hint.appendSlice("[");
            try self.minibuffer_hint.appendSlice(dir_prefix);
            try self.minibuffer_hint.appendSlice(match.slice());
            if (match.is_dir) try self.minibuffer_hint.appendSlice("/");
            if (idx == selected) try self.minibuffer_hint.appendSlice("]");
        }
        try self.appendIdoWindowTail(visible.start, visible.end, matches.items.len);
    }

    fn setBufferMinibufferHintMatches(
        self: *Editor,
        matches: *const std.array_list.Managed(BufferMatch),
        selected: usize,
    ) !void {
        try self.appendIdoListHeader(selected, matches.items.len);
        const visible = self.idoVisibleWindow(selected, matches.items.len);

        var idx = visible.start;
        while (idx < visible.end) : (idx += 1) {
            if (idx > visible.start) try self.minibuffer_hint.appendSlice(" | ");
            const name = matches.items[idx].name;
            if (idx == selected) {
                try self.minibuffer_hint.writer().print("[{s}]", .{name});
            } else {
                try self.minibuffer_hint.appendSlice(name);
            }
        }
        try self.appendIdoWindowTail(visible.start, visible.end, matches.items.len);
    }

    fn appendIdoListHeader(self: *Editor, selected: usize, total: usize) !void {
        try self.minibuffer_hint.writer().print("ido {d}/{d}: ", .{ selected + 1, total });
    }

    fn idoVisibleWindow(self: *Editor, selected: usize, total: usize) struct { start: usize, end: usize } {
        _ = self;
        const max_visible: usize = 6;
        var start: usize = if (selected > 2) selected - 2 else 0;
        if (total > max_visible and start + max_visible > total) {
            start = total - max_visible;
        }
        return .{ .start = start, .end = @min(total, start + max_visible) };
    }

    fn appendIdoWindowTail(self: *Editor, start: usize, end: usize, total: usize) !void {
        if (start > 0) {
            try self.minibuffer_hint.writer().print(" (+{d} before)", .{start});
        }
        if (end < total) {
            try self.minibuffer_hint.writer().print(" (+{d} more)", .{total - end});
        }
        try self.minibuffer_hint.appendSlice("  (Tab/C-s/C-r cycle)");
    }

    fn selectedCommandMatchIndex(
        self: *Editor,
        state: MinibufferState,
        query: []const u8,
        matches: []const CommandMatch,
    ) usize {
        if (matches.len == 0) return 0;

        const exact = self.findCommandSpec(query);
        if (exact) |spec| {
            for (matches, 0..) |match, idx| {
                if (match.spec == spec) return idx;
            }
        }

        return @min(state.ido_index, matches.len - 1);
    }

    fn cycleCommandCandidate(self: *Editor, state: *MinibufferState, forward: bool) !void {
        var count: usize = 0;
        switch (state.kind) {
            .command => {
                const query = std.mem.trim(u8, self.input_buffer.items, " \t");
                var matches = std.array_list.Managed(CommandMatch).init(self.allocator);
                defer matches.deinit();
                try self.collectCommandMatches(query, &matches);
                count = matches.items.len;
            },
            .open_file, .save_as => {
                const path_ctx = splitPathInput(self.input_buffer.items);
                var matches = std.array_list.Managed(PathMatch).init(self.allocator);
                defer matches.deinit();
                try self.collectPathMatches(path_ctx.base, path_ctx.dir_path, &matches);
                count = matches.items.len;
            },
            .switch_buffer, .kill_buffer => {
                const query = std.mem.trim(u8, self.input_buffer.items, " \t");
                var matches = std.array_list.Managed(BufferMatch).init(self.allocator);
                defer matches.deinit();
                try self.collectBufferMatches(query, &matches);
                count = matches.items.len;
            },
            .kill_buffer_confirm,
            .quit_confirm,
            .goto_line,
            .replace_from,
            .replace_to,
            .query_replace_from,
            .query_replace_to,
            .replace_regexp_from,
            .replace_regexp_to,
            .query_replace_regexp_from,
            .query_replace_regexp_to,
            => return,
        }
        if (count == 0) return;

        if (forward) {
            state.ido_index = (state.ido_index + 1) % count;
        } else {
            state.ido_index = if (state.ido_index == 0) count - 1 else state.ido_index - 1;
        }
    }

    fn resolveCommandForInput(self: *Editor, state: MinibufferState, input: []const u8) !?*const CommandSpec {
        const query = std.mem.trim(u8, input, " \t");
        if (query.len == 0) return null;

        if (self.findCommandSpec(query)) |exact| {
            return exact;
        }

        var matches = std.array_list.Managed(CommandMatch).init(self.allocator);
        defer matches.deinit();
        try self.collectCommandMatches(query, &matches);
        if (matches.items.len == 0) return null;

        const idx = @min(state.ido_index, matches.items.len - 1);
        return matches.items[idx].spec;
    }

    fn collectCommandMatches(self: *Editor, query: []const u8, out: *std.array_list.Managed(CommandMatch)) !void {
        _ = self;
        for (command_specs, 0..) |_, idx| {
            const spec = &command_specs[idx];
            var best: ?fuzzy_mod.MatchScore = fuzzy_mod.score(spec.name, query);
            for (spec.aliases) |alias| {
                const alias_score = fuzzy_mod.score(alias, query) orelse continue;
                if (best == null or alias_score.score > best.?.score or
                    (alias_score.score == best.?.score and alias_score.first_index < best.?.first_index))
                {
                    best = alias_score;
                }
            }
            if (best) |score| {
                try out.append(.{ .spec = spec, .score = score });
            }
        }
        std.sort.block(CommandMatch, out.items, {}, commandMatchLessThan);
    }

    fn commandMatchLessThan(_: void, lhs: CommandMatch, rhs: CommandMatch) bool {
        if (lhs.score.score != rhs.score.score) return lhs.score.score > rhs.score.score;
        if (lhs.score.first_index != rhs.score.first_index) return lhs.score.first_index < rhs.score.first_index;
        if (lhs.spec.name.len != rhs.spec.name.len) return lhs.spec.name.len < rhs.spec.name.len;
        return std.mem.lessThan(u8, lhs.spec.name, rhs.spec.name);
    }

    fn findCommandSpec(self: *Editor, token: []const u8) ?*const CommandSpec {
        _ = self;
        for (command_specs, 0..) |_, idx| {
            const spec = &command_specs[idx];
            if (std.ascii.eqlIgnoreCase(spec.name, token)) return spec;
            for (spec.aliases) |alias| {
                if (std.ascii.eqlIgnoreCase(alias, token)) return spec;
            }
        }
        return null;
    }

    fn commonPrefixLen(a: []const u8, b: []const u8) usize {
        const max_len = @min(a.len, b.len);
        var idx: usize = 0;
        while (idx < max_len and a[idx] == b[idx]) : (idx += 1) {}
        return idx;
    }

    fn historyList(self: *Editor, kind: MinibufferKind) *std.array_list.Managed([]u8) {
        return switch (kind) {
            .open_file => &self.open_history,
            .save_as => &self.save_history,
            .switch_buffer, .kill_buffer, .kill_buffer_confirm => &self.buffer_history,
            .goto_line,
            .quit_confirm,
            .command,
            .replace_from,
            .replace_to,
            .query_replace_from,
            .query_replace_to,
            .replace_regexp_from,
            .replace_regexp_to,
            .query_replace_regexp_from,
            .query_replace_regexp_to,
            => &self.command_history,
        };
    }

    fn addHistoryEntry(self: *Editor, kind: MinibufferKind, text: []const u8) !void {
        if (text.len == 0) return;

        const list = self.historyList(kind);
        var idx: usize = 0;
        while (idx < list.items.len) : (idx += 1) {
            if (std.mem.eql(u8, list.items[idx], text)) {
                const old = list.orderedRemove(idx);
                self.allocator.free(old);
                break;
            }
        }

        const copy = try self.allocator.alloc(u8, text.len);
        std.mem.copyForwards(u8, copy, text);
        try list.insert(0, copy);

        while (list.items.len > 64) {
            const old = list.pop().?;
            self.allocator.free(old);
        }
    }

    fn minibufferHistoryUp(self: *Editor, state: *MinibufferState) !void {
        const list = self.historyList(state.kind);
        if (list.items.len == 0) return;

        const next_index = if (state.history_index) |idx|
            @min(idx + 1, list.items.len - 1)
        else blk: {
            self.minibuffer_backup.clearRetainingCapacity();
            try self.minibuffer_backup.appendSlice(self.input_buffer.items);
            break :blk @as(usize, 0);
        };

        state.history_index = next_index;
        try self.replaceInputBuffer(list.items[next_index]);
    }

    fn minibufferHistoryDown(self: *Editor, state: *MinibufferState) !void {
        const idx = state.history_index orelse return;
        if (idx == 0) {
            state.history_index = null;
            try self.replaceInputBuffer(self.minibuffer_backup.items);
            return;
        }

        const next_index = idx - 1;
        state.history_index = next_index;
        const list = self.historyList(state.kind);
        try self.replaceInputBuffer(list.items[next_index]);
    }

    fn replaceInputBuffer(self: *Editor, text: []const u8) !void {
        self.input_buffer.clearRetainingCapacity();
        try self.input_buffer.appendSlice(text);
    }

    fn historyFilePath(self: *Editor) !?[]u8 {
        const home = std.process.getEnvVarOwned(self.allocator, "HOME") catch |err| switch (err) {
            error.EnvironmentVariableNotFound => return null,
            else => return err,
        };
        defer self.allocator.free(home);

        return try std.fs.path.join(self.allocator, &.{ home, ".mg-zig-history" });
    }

    fn loadHistory(self: *Editor) !void {
        const path_opt = try self.historyFilePath();
        if (path_opt == null) return;
        const path = path_opt.?;
        defer self.allocator.free(path);

        var file = std.fs.cwd().openFile(path, .{}) catch |err| switch (err) {
            error.FileNotFound => return,
            else => return err,
        };
        defer file.close();

        const raw = try file.readToEndAlloc(self.allocator, 4 * 1024 * 1024);
        defer self.allocator.free(raw);

        var it = std.mem.splitScalar(u8, raw, '\n');
        while (it.next()) |line_raw| {
            const line = std.mem.trimRight(u8, line_raw, "\r");
            if (line.len < 3 or line[1] != '\t') continue;

            const list = switch (line[0]) {
                'o' => &self.open_history,
                's' => &self.save_history,
                'b' => &self.buffer_history,
                'c' => &self.command_history,
                else => continue,
            };

            const payload = line[2..];
            const decoded_len = std.base64.standard.Decoder.calcSizeForSlice(payload) catch continue;
            const decoded = try self.allocator.alloc(u8, decoded_len);
            std.base64.standard.Decoder.decode(decoded, payload) catch {
                self.allocator.free(decoded);
                continue;
            };

            if (list.items.len < 64) {
                try list.append(decoded);
            } else {
                self.allocator.free(decoded);
            }
        }
    }

    fn saveHistory(self: *Editor) !void {
        const path_opt = try self.historyFilePath();
        if (path_opt == null) return;
        const path = path_opt.?;
        defer self.allocator.free(path);

        var file = try std.fs.cwd().createFile(path, .{ .truncate = true });
        defer file.close();

        try self.writeHistoryKind(&file, 'o', &self.open_history);
        try self.writeHistoryKind(&file, 's', &self.save_history);
        try self.writeHistoryKind(&file, 'b', &self.buffer_history);
        try self.writeHistoryKind(&file, 'c', &self.command_history);
    }

    fn writeHistoryKind(self: *Editor, file: *std.fs.File, kind: u8, list: *const std.array_list.Managed([]u8)) !void {
        for (list.items) |entry| {
            const encoded_len = std.base64.standard.Encoder.calcSize(entry.len);
            const encoded = try self.allocator.alloc(u8, encoded_len);
            defer self.allocator.free(encoded);
            const encoded_slice = std.base64.standard.Encoder.encode(encoded, entry);
            const line = try std.fmt.allocPrint(self.allocator, "{c}\t{s}\n", .{ kind, encoded_slice });
            defer self.allocator.free(line);
            try file.writeAll(line);
        }
    }

    fn freeHistory(self: *Editor, list: *std.array_list.Managed([]u8)) void {
        for (list.items) |entry| {
            self.allocator.free(entry);
        }
        list.deinit();
    }

    fn startMinibuffer(self: *Editor, kind: MinibufferKind) !void {
        self.pending_keys.clearRetainingCapacity();
        self.input_buffer.clearRetainingCapacity();
        self.minibuffer_hint.clearRetainingCapacity();
        self.minibuffer_backup.clearRetainingCapacity();
        if (kind != .kill_buffer_confirm) {
            self.pending_kill_buffer_idx = null;
        }
        if (kind == .replace_from and self.pending_replace_from != null) {
            self.allocator.free(self.pending_replace_from.?);
            self.pending_replace_from = null;
        }
        if (kind == .query_replace_from and self.pending_query_replace_from != null) {
            self.allocator.free(self.pending_query_replace_from.?);
            self.pending_query_replace_from = null;
        }
        if (kind == .replace_regexp_from and self.pending_replace_regexp_from != null) {
            self.allocator.free(self.pending_replace_regexp_from.?);
            self.pending_replace_regexp_from = null;
        }
        if (kind == .query_replace_regexp_from and self.pending_query_replace_regexp_from != null) {
            self.allocator.free(self.pending_query_replace_regexp_from.?);
            self.pending_query_replace_regexp_from = null;
        }
        if (kind == .kill_buffer) {
            try self.input_buffer.appendSlice(self.activeSlot().displayName());
        } else if (kind == .goto_line) {
            const win = self.activeWindow();
            const line = self.activeSlot().buffer.byteToLineCol(win.cursor.byte_index).line + 1;
            try self.input_buffer.writer().print("{d}", .{line});
        } else if (kind == .replace_from or
            kind == .query_replace_from or
            kind == .replace_regexp_from or
            kind == .query_replace_regexp_from)
        {
            _ = try self.seedInputFromWordAtCursor();
        }
        const state = MinibufferState{
            .kind = kind,
            .history_index = null,
            .tab_count = 0,
            .ido_index = 0,
        };
        self.input_mode = .{ .minibuffer = state };
        try self.refreshMinibufferHint(state, false);

        switch (kind) {
            .open_file => try self.setStatus("Find file:"),
            .save_as => try self.setStatus("Write file:"),
            .switch_buffer => try self.setStatus("Switch buffer:"),
            .kill_buffer => try self.setStatus("Kill buffer:"),
            .kill_buffer_confirm => try self.setStatus("Kill modified buffer (type yes): "),
            .quit_confirm => try self.setStatus("Modified buffers exist; quit anyway? (yes/no): "),
            .goto_line => try self.setStatus("Goto line:"),
            .command => try self.setStatus("M-x "),
            .replace_from => try self.setStatus("Replace string: "),
            .replace_to => try self.setStatus("Replace string with: "),
            .query_replace_from => try self.setStatus("Query replace: "),
            .query_replace_to => try self.setStatus("Query replace with: "),
            .replace_regexp_from => try self.setStatus("Replace regexp: "),
            .replace_regexp_to => try self.setStatus("Replace regexp with: "),
            .query_replace_regexp_from => try self.setStatus("Query replace regexp: "),
            .query_replace_regexp_to => try self.setStatus("Query replace regexp with: "),
        }
    }

    fn startISearch(self: *Editor, direction: search_mod.Direction) !void {
        const win = self.activeWindow();

        self.pending_keys.clearRetainingCapacity();
        self.input_buffer.clearRetainingCapacity();

        var state = ISearchState{
            .direction = direction,
            .anchor = win.cursor.byte_index,
            .matched = true,
        };

        if (self.last_search.items.len > 0) {
            try self.input_buffer.appendSlice(self.last_search.items);
        } else {
            _ = try self.seedInputFromWordAtCursor();
        }

        if (self.input_buffer.items.len > 0) {
            state.matched = self.isearchFind(state, false);
        }

        self.input_mode = .{ .isearch = state };
    }

    fn isearchFind(self: *Editor, state: ISearchState, from_cursor: bool) bool {
        const win = self.activeWindow();
        const buf = &self.activeSlot().buffer;

        if (self.input_buffer.items.len == 0) {
            win.cursor.byte_index = state.anchor;
            win.cursor.preferred_column = null;
            win.ensureCursorVisible(buf);
            return true;
        }

        const base = if (from_cursor) win.cursor.byte_index else state.anchor;
        const start = switch (state.direction) {
            .forward => @min(buf.len(), base + 1),
            .backward => if (base == 0) 0 else base - 1,
        };

        const found = search_mod.find(buf.bytes(), self.input_buffer.items, start, state.direction, .{ .case_sensitive = false, .wrap = true });
        if (found) |idx| {
            win.cursor.byte_index = idx;
            win.cursor.preferred_column = null;
            win.ensureCursorVisible(buf);
            return true;
        }

        if (!from_cursor) {
            win.cursor.byte_index = state.anchor;
            win.cursor.preferred_column = null;
            win.ensureCursorVisible(buf);
        }
        return false;
    }

    fn seedInputFromWordAtCursor(self: *Editor) !bool {
        const win = self.activeWindow();
        const buf = &self.activeSlot().buffer;
        const text = buf.bytes();

        if (text.len == 0) return false;

        const idx = @min(win.cursor.byte_index, text.len - 1);
        if (!isSearchChar(text[idx])) return false;

        var start = idx;
        while (start > 0 and isSearchChar(text[start - 1])) {
            start -= 1;
        }

        var end = idx + 1;
        while (end < text.len and isSearchChar(text[end])) {
            end += 1;
        }

        try self.input_buffer.appendSlice(text[start..end]);
        return self.input_buffer.items.len > 0;
    }

    fn nextBuffer(self: *Editor) !void {
        if (self.buffers.items.len <= 1) {
            try self.setStatus("Only one buffer");
            return;
        }

        const win = self.activeWindow();
        win.buffer_id = (win.buffer_id + 1) % self.buffers.items.len;
        win.cursor = .{};
        win.mark = null;
        win.top_line = 0;

        const slot = self.activeSlot();
        try self.setStatusFmt("Switched to {s}{s}", .{ slot.displayName(), if (slot.dirty) "*" else "" });
    }

    fn previousBuffer(self: *Editor) !void {
        if (self.buffers.items.len <= 1) {
            try self.setStatus("Only one buffer");
            return;
        }

        const win = self.activeWindow();
        win.buffer_id = if (win.buffer_id == 0) self.buffers.items.len - 1 else win.buffer_id - 1;
        win.cursor = .{};
        win.mark = null;
        win.top_line = 0;

        const slot = self.activeSlot();
        try self.setStatusFmt("Switched to {s}{s}", .{ slot.displayName(), if (slot.dirty) "*" else "" });
    }

    fn splitWindowBelow(self: *Editor) !void {
        try self.splitActiveWindow(.horizontal);
        try self.setStatus("Split window");
    }

    fn splitWindowRight(self: *Editor) !void {
        try self.splitActiveWindow(.vertical);
        try self.setStatus("Split window right");
    }

    fn deleteWindow(self: *Editor) !void {
        if (self.windows.items.len <= 1) {
            try self.setStatus("Cannot delete only window");
            return;
        }

        try self.deleteActiveWindowNode();
        try self.setStatus("Deleted window");
    }

    fn deleteOtherWindows(self: *Editor) !void {
        if (self.windows.items.len <= 1) {
            try self.setStatus("No other windows");
            return;
        }

        const keep = cloneWindow(&self.windows.items[self.active_window]);
        self.windows.clearRetainingCapacity();
        try self.windows.append(keep);
        self.active_window = 0;
        try self.rebuildSplitTreeSingle();
        try self.setStatus("Deleted other windows");
    }

    fn otherWindow(self: *Editor) !void {
        if (self.windows.items.len <= 1) {
            try self.setStatus("Only one window");
            return;
        }

        self.active_window = (self.active_window + 1) % self.windows.items.len;
        try self.setStatusFmt("Window {d}/{d}", .{ self.active_window + 1, self.windows.items.len });
    }

    fn splitActiveWindow(self: *Editor, axis: SplitAxis) !void {
        if (self.active_window >= self.windows.items.len) return;
        const leaf_idx = self.findLeafNodeForWindow(self.active_window) orelse blk: {
            try self.rebuildSplitTreeFromWindows();
            break :blk self.findLeafNodeForWindow(self.active_window) orelse return error.InvalidWindowTree;
        };

        const current = self.activeWindow();
        try self.windows.insert(self.active_window + 1, cloneWindow(current));
        const new_window_index = self.active_window + 1;
        self.bumpLeafWindowIndicesFrom(new_window_index);

        const old_window_index = switch (self.split_nodes.items[leaf_idx].kind) {
            .leaf => |idx| idx,
            .split => return error.InvalidWindowTree,
        };

        const first = try self.appendSplitNode(.{
            .parent = leaf_idx,
            .kind = .{ .leaf = old_window_index },
        });
        const second = try self.appendSplitNode(.{
            .parent = leaf_idx,
            .kind = .{ .leaf = new_window_index },
        });
        self.split_nodes.items[leaf_idx].kind = .{
            .split = .{
                .axis = axis,
                .first = first,
                .second = second,
            },
        };
    }

    fn deleteActiveWindowNode(self: *Editor) !void {
        if (self.active_window >= self.windows.items.len) return;
        const leaf_idx = self.findLeafNodeForWindow(self.active_window) orelse blk: {
            try self.rebuildSplitTreeFromWindows();
            break :blk self.findLeafNodeForWindow(self.active_window) orelse return error.InvalidWindowTree;
        };
        const parent_idx = self.split_nodes.items[leaf_idx].parent orelse return error.InvalidWindowTree;

        const sibling_idx = switch (self.split_nodes.items[parent_idx].kind) {
            .leaf => return error.InvalidWindowTree,
            .split => |s| if (s.first == leaf_idx) s.second else s.first,
        };
        const grand_idx = self.split_nodes.items[parent_idx].parent;

        if (grand_idx) |g| {
            switch (self.split_nodes.items[g].kind) {
                .leaf => return error.InvalidWindowTree,
                .split => |*s| {
                    if (s.first == parent_idx) {
                        s.first = sibling_idx;
                    } else if (s.second == parent_idx) {
                        s.second = sibling_idx;
                    } else {
                        return error.InvalidWindowTree;
                    }
                },
            }
            self.split_nodes.items[sibling_idx].parent = g;
        } else {
            self.split_root = sibling_idx;
            self.split_nodes.items[sibling_idx].parent = null;
        }

        _ = self.windows.orderedRemove(self.active_window);
        self.decrementLeafWindowIndicesAfterRemoval(self.active_window);
        if (self.active_window >= self.windows.items.len) {
            self.active_window = self.windows.items.len - 1;
        }
    }

    fn appendSplitNode(self: *Editor, node: SplitNode) !usize {
        try self.split_nodes.append(node);
        return self.split_nodes.items.len - 1;
    }

    fn rebuildSplitTreeSingle(self: *Editor) !void {
        self.split_nodes.clearRetainingCapacity();
        try self.split_nodes.append(.{ .kind = .{ .leaf = 0 } });
        self.split_root = 0;
    }

    fn rebuildSplitTreeFromWindows(self: *Editor) !void {
        self.split_nodes.clearRetainingCapacity();
        if (self.windows.items.len == 0) {
            self.split_root = 0;
            return;
        }

        try self.split_nodes.append(.{ .kind = .{ .leaf = 0 } });
        self.split_root = 0;
        var idx: usize = 1;
        while (idx < self.windows.items.len) : (idx += 1) {
            const old_root = self.split_root;
            try self.split_nodes.append(.{ .kind = .{ .leaf = idx } });
            const new_leaf_idx = self.split_nodes.items.len - 1;
            try self.split_nodes.append(.{
                .kind = .{
                    .split = .{
                        .axis = .horizontal,
                        .first = old_root,
                        .second = new_leaf_idx,
                    },
                },
            });
            const new_root = self.split_nodes.items.len - 1;
            self.split_nodes.items[old_root].parent = new_root;
            self.split_nodes.items[new_leaf_idx].parent = new_root;
            self.split_nodes.items[new_root].parent = null;
            self.split_root = new_root;
        }
    }

    fn findLeafNodeForWindow(self: *const Editor, window_index: usize) ?usize {
        if (self.split_nodes.items.len == 0) return null;
        return self.findLeafNodeForWindowRec(self.split_root, window_index);
    }

    fn findLeafNodeForWindowRec(self: *const Editor, node_idx: usize, window_index: usize) ?usize {
        if (node_idx >= self.split_nodes.items.len) return null;
        return switch (self.split_nodes.items[node_idx].kind) {
            .leaf => |idx| if (idx == window_index) node_idx else null,
            .split => |s| self.findLeafNodeForWindowRec(s.first, window_index) orelse self.findLeafNodeForWindowRec(s.second, window_index),
        };
    }

    fn bumpLeafWindowIndicesFrom(self: *Editor, start: usize) void {
        for (self.split_nodes.items) |*node| {
            switch (node.kind) {
                .leaf => |*idx| {
                    if (idx.* >= start) idx.* += 1;
                },
                .split => {},
            }
        }
    }

    fn decrementLeafWindowIndicesAfterRemoval(self: *Editor, removed: usize) void {
        for (self.split_nodes.items) |*node| {
            switch (node.kind) {
                .leaf => |*idx| {
                    if (idx.* > removed) idx.* -= 1;
                },
                .split => {},
            }
        }
    }

    fn cloneWindow(src: *const Window) Window {
        var dst = Window.init(src.buffer_id, src.rows, src.cols);
        dst.cursor.byte_index = src.cursor.byte_index;
        dst.cursor.preferred_column = if (src.cursor.preferred_column) |col| col else null;
        dst.mark = if (src.mark) |mark| mark else null;
        dst.top_line = src.top_line;
        return dst;
    }

    fn pageMove(self: *Editor, direction: isize) void {
        const win = self.activeWindow();
        const buf = &self.activeSlot().buffer;
        const line_count = buf.lineCount();
        if (line_count == 0) return;

        const pos = buf.byteToLineCol(win.cursor.byte_index);
        const target_col = win.cursor.preferred_column orelse pos.column;
        const page: usize = if (win.rows > 1) win.rows - 1 else 1;
        const page_delta: isize = @intCast(page);
        const delta = page_delta * direction;

        var next_line_signed: isize = @intCast(pos.line);
        next_line_signed += delta;
        if (next_line_signed < 0) next_line_signed = 0;

        const max_line: isize = @intCast(line_count - 1);
        if (next_line_signed > max_line) next_line_signed = max_line;

        const target_line: usize = @intCast(next_line_signed);
        win.cursor.byte_index = buf.lineColToByte(target_line, target_col);
        win.cursor.preferred_column = target_col;
        win.ensureCursorVisible(buf);
    }

    fn gotoLine(self: *Editor, one_based: usize) !void {
        const win = self.activeWindow();
        const buf = &self.activeSlot().buffer;
        const line_count = buf.lineCount();
        if (line_count == 0) return;

        const clamped = if (one_based == 0) @as(usize, 1) else @min(one_based, line_count);
        const target_line = clamped - 1;
        win.cursor.byte_index = buf.lineColToByte(target_line, 0);
        win.cursor.preferred_column = null;
        win.ensureCursorVisible(buf);
        try self.setStatusFmt("Line {d}", .{clamped});
    }

    fn transposeChars(self: *Editor) !void {
        const win = self.activeWindow();
        const slot = self.activeSlot();
        const buf = &slot.buffer;
        const text = buf.bytes();
        if (text.len < 2) return;

        const at = win.cursor.byte_index;
        const left = if (at == 0)
            @as(usize, 0)
        else if (at >= text.len)
            text.len - 2
        else
            at - 1;
        const right = left + 1;

        // Keep transpose local to the line to avoid surprising newline shuffles.
        if (text[left] == '\n' or text[right] == '\n') {
            try self.setStatus("Cannot transpose across line boundary");
            return;
        }

        const pair = [2]u8{ text[right], text[left] };
        try buf.deleteRange(left, 2);
        try buf.insert(left, pair[0..]);
        slot.dirty = true;

        win.cursor.byte_index = if (at == 0) 2 else if (at >= text.len) at else at + 1;
        win.cursor.preferred_column = null;
        win.ensureCursorVisible(buf);
        try self.setStatus("Transpose chars");
    }

    fn moveWordForward(self: *Editor) !void {
        const win = self.activeWindow();
        const buf = &self.activeSlot().buffer;
        const to = wordForwardTarget(buf.bytes(), win.cursor.byte_index);
        win.cursor.byte_index = to;
        win.cursor.preferred_column = null;
        win.ensureCursorVisible(buf);
    }

    fn moveWordBackward(self: *Editor) !void {
        const win = self.activeWindow();
        const buf = &self.activeSlot().buffer;
        const to = wordBackwardTarget(buf.bytes(), win.cursor.byte_index);
        win.cursor.byte_index = to;
        win.cursor.preferred_column = null;
        win.ensureCursorVisible(buf);
    }

    fn killWordForward(self: *Editor) !bool {
        const win = self.activeWindow();
        const slot = self.activeSlot();
        const buf = &slot.buffer;

        const start = win.cursor.byte_index;
        const end = wordForwardTarget(buf.bytes(), start);
        if (end <= start) {
            try self.setStatus("No word ahead");
            return false;
        }

        const chunk = buf.bytes()[start..end];
        try self.storeKilledText(chunk, false, start);
        try buf.deleteRange(start, end - start);
        self.adjustMarkAfterDelete(win, start, end - start);
        slot.dirty = true;
        win.cursor.preferred_column = null;
        win.ensureCursorVisible(buf);
        try self.setStatus("Killed word");
        return true;
    }

    fn killWordBackward(self: *Editor) !bool {
        const win = self.activeWindow();
        const slot = self.activeSlot();
        const buf = &slot.buffer;

        const end = win.cursor.byte_index;
        const start = wordBackwardTarget(buf.bytes(), end);
        if (start >= end) {
            try self.setStatus("No word behind");
            return false;
        }

        const chunk = buf.bytes()[start..end];
        try self.storeKilledText(chunk, true, end);
        try buf.deleteRange(start, end - start);
        self.adjustMarkAfterDelete(win, start, end - start);
        slot.dirty = true;
        win.cursor.byte_index = start;
        win.cursor.preferred_column = null;
        win.ensureCursorVisible(buf);
        try self.setStatus("Backward killed word");
        return true;
    }

    fn transposeWords(self: *Editor) !void {
        const win = self.activeWindow();
        const slot = self.activeSlot();
        const buf = &slot.buffer;
        const text = buf.bytes();

        const next = findWordAtOrAfter(text, win.cursor.byte_index) orelse {
            try self.setStatus("No word to transpose");
            return;
        };
        const prev = findWordBefore(text, next.start) orelse {
            try self.setStatus("No previous word to transpose");
            return;
        };

        const between = text[prev.end..next.start];
        if (std.mem.indexOfScalar(u8, between, '\n') != null) {
            try self.setStatus("Cannot transpose words across lines");
            return;
        }

        var replacement = std.array_list.Managed(u8).init(self.allocator);
        defer replacement.deinit();
        try replacement.appendSlice(text[next.start..next.end]);
        try replacement.appendSlice(between);
        try replacement.appendSlice(text[prev.start..prev.end]);

        try buf.deleteRange(prev.start, next.end - prev.start);
        self.adjustMarkAfterDelete(win, prev.start, next.end - prev.start);
        try buf.insert(prev.start, replacement.items);
        self.adjustMarkAfterInsert(win, prev.start, replacement.items.len);

        slot.dirty = true;
        win.cursor.byte_index = prev.start + replacement.items.len;
        win.cursor.preferred_column = null;
        win.ensureCursorVisible(buf);
        try self.setStatus("Transpose words");
    }

    fn killRegion(self: *Editor, delete: bool) !bool {
        const win = self.activeWindow();
        const slot = self.activeSlot();
        const buf = &slot.buffer;

        const region = self.regionBounds(win) orelse {
            try self.setStatus("No active region");
            return false;
        };
        const chunk = buf.bytes()[region.start..region.end];
        if (chunk.len == 0) {
            try self.setStatus("No active region");
            return false;
        }

        if (delete) {
            try self.storeKilledText(chunk, false, win.cursor.byte_index);
            try buf.deleteRange(region.start, region.end - region.start);
            self.adjustMarkAfterDelete(win, region.start, region.end - region.start);
            slot.dirty = true;
            win.cursor.byte_index = region.start;
            win.mark = null;
            win.cursor.preferred_column = null;
            win.ensureCursorVisible(buf);
            try self.setStatus("Killed region");
            return true;
        } else {
            try self.kill_ring.push(chunk);
            try self.setStatus("Copied region");
            return false;
        }
    }

    fn regionBounds(self: *Editor, win: *Window) ?ByteRange {
        _ = self;
        const mark = win.mark orelse return null;
        if (mark == win.cursor.byte_index) return null;
        return .{
            .start = @min(mark, win.cursor.byte_index),
            .end = @max(mark, win.cursor.byte_index),
        };
    }

    fn pushActiveMarkRing(self: *Editor, pos: usize) !void {
        const ring = &self.activeSlot().mark_ring;
        const clamped = @min(pos, self.activeSlot().buffer.len());
        if (ring.items.len > 0 and ring.items[0] == clamped) return;

        try ring.insert(0, clamped);
        const mark_ring_max: usize = 32;
        if (ring.items.len > mark_ring_max) {
            _ = ring.pop();
        }
    }

    fn popActiveMarkRing(self: *Editor) ?usize {
        const ring = &self.activeSlot().mark_ring;
        if (ring.items.len == 0) return null;
        return ring.orderedRemove(0);
    }

    fn setMarkCommand(self: *Editor) !void {
        const win = self.activeWindow();
        if (win.mark) |existing| {
            try self.pushActiveMarkRing(existing);
        }
        win.mark = win.cursor.byte_index;
        try self.setStatus("Mark set");
    }

    fn popMarkCommand(self: *Editor) !void {
        const win = self.activeWindow();
        const slot = self.activeSlot();
        const buf = &slot.buffer;
        const popped = self.popActiveMarkRing() orelse {
            try self.setStatus("Mark ring empty");
            return;
        };

        if (win.mark) |existing| {
            try self.pushActiveMarkRing(existing);
        }
        win.mark = win.cursor.byte_index;
        win.cursor.byte_index = @min(popped, buf.len());
        win.cursor.preferred_column = null;
        win.ensureCursorVisible(buf);
        try self.setStatus("Popped mark");
    }

    fn adjustMarkAfterInsert(self: *Editor, win: *Window, at: usize, len: usize) void {
        if (len == 0) return;
        if (win.mark) |m| {
            if (at <= m) win.mark = m + len;
        }
        for (self.activeSlot().mark_ring.items) |*mark| {
            if (at <= mark.*) mark.* += len;
        }
    }

    fn adjustMarkAfterDelete(self: *Editor, win: *Window, at: usize, len: usize) void {
        if (len == 0) return;
        if (win.mark) |m| {
            if (m > at) {
                const end_mark = at + len;
                if (m >= end_mark) {
                    win.mark = m - len;
                } else {
                    win.mark = at;
                }
            }
        }
        const end = at + len;
        for (self.activeSlot().mark_ring.items) |*mark| {
            if (mark.* <= at) continue;
            if (mark.* >= end) {
                mark.* -= len;
            } else {
                mark.* = at;
            }
        }
    }

    fn killLine(self: *Editor) !bool {
        const win = self.activeWindow();
        const slot = self.activeSlot();
        const buf = &slot.buffer;

        if (buf.len() == 0) return false;

        const pos = buf.byteToLineCol(win.cursor.byte_index);
        const line_end = buf.lineColToByte(pos.line, std.math.maxInt(usize));

        var delete_end = line_end;
        if (delete_end == win.cursor.byte_index and delete_end < buf.len() and buf.bytes()[delete_end] == '\n') {
            delete_end += 1;
        }

        if (delete_end <= win.cursor.byte_index) {
            try self.setStatus("Nothing to kill");
            return false;
        }

        const chunk = buf.bytes()[win.cursor.byte_index..delete_end];
        try self.storeKilledText(chunk, false, win.cursor.byte_index);
        try buf.deleteRange(win.cursor.byte_index, delete_end - win.cursor.byte_index);
        self.adjustMarkAfterDelete(win, win.cursor.byte_index, delete_end - win.cursor.byte_index);
        slot.dirty = true;
        win.cursor.preferred_column = null;
        win.ensureCursorVisible(buf);

        try self.setStatus("Killed line segment");
        return true;
    }

    fn yank(self: *Editor) !void {
        const text = self.kill_ring.at(0) orelse {
            try self.setStatus("Kill ring is empty");
            return;
        };

        const win = self.activeWindow();
        const slot = self.activeSlot();
        const buf = &slot.buffer;

        try buf.insert(win.cursor.byte_index, text);
        self.adjustMarkAfterInsert(win, win.cursor.byte_index, text.len);
        const at = win.cursor.byte_index;
        win.cursor.byte_index += text.len;
        win.cursor.preferred_column = null;
        slot.dirty = true;
        win.ensureCursorVisible(buf);
        self.last_yank = .{ .at = at, .len = text.len, .ring_index = 0 };

        try self.setStatus("Yank");
    }

    fn yankPop(self: *Editor) !void {
        const prev_cmd = self.last_command orelse {
            try self.setStatus("Previous command was not a yank");
            return;
        };
        if (prev_cmd != .yank and prev_cmd != .yank_pop) {
            try self.setStatus("Previous command was not a yank");
            return;
        }
        if (self.kill_ring.count() < 2) {
            try self.setStatus("Kill ring has no alternate entry");
            return;
        }

        const state = self.last_yank orelse {
            try self.setStatus("No previous yank to replace");
            return;
        };

        const next_index = (state.ring_index + 1) % self.kill_ring.count();
        const next_text = self.kill_ring.at(next_index).?;

        const win = self.activeWindow();
        const slot = self.activeSlot();
        const buf = &slot.buffer;

        try buf.deleteRange(state.at, state.len);
        self.adjustMarkAfterDelete(win, state.at, state.len);
        try buf.insert(state.at, next_text);
        self.adjustMarkAfterInsert(win, state.at, next_text.len);

        win.cursor.byte_index = state.at + next_text.len;
        win.cursor.preferred_column = null;
        slot.dirty = true;
        win.ensureCursorVisible(buf);
        self.last_yank = .{ .at = state.at, .len = next_text.len, .ring_index = next_index };

        try self.setStatus("Yank-pop");
    }

    fn storeKilledText(self: *Editor, text: []const u8, backward: bool, kill_cursor: usize) !void {
        if (text.len == 0) return;
        const coalesce = self.last_command_was_kill and self.last_kill_cursor != null and self.last_kill_cursor.? == kill_cursor;
        if (coalesce) {
            if (backward) {
                try self.kill_ring.prependToLatest(text);
            } else {
                try self.kill_ring.appendToLatest(text);
            }
        } else {
            try self.kill_ring.push(text);
        }
    }

    fn renderStatusText(self: *Editor, out: *std.array_list.Managed(u8)) ![]const u8 {
        switch (self.input_mode) {
            .normal => {
                if (self.prefix_hint.items.len > 0) {
                    return self.prefix_hint.items;
                }
                return self.status_line.items;
            },
            .minibuffer => |state| {
                const prompt = switch (state.kind) {
                    .open_file => "Find file: ",
                    .save_as => "Write file: ",
                    .switch_buffer => "Switch buffer: ",
                    .kill_buffer => "Kill buffer: ",
                    .kill_buffer_confirm => "Kill modified buffer (type yes): ",
                    .quit_confirm => "Modified buffers exist; quit anyway? (yes/no): ",
                    .goto_line => "Goto line: ",
                    .command => "M-x ",
                    .replace_from => "Replace string: ",
                    .replace_to => "Replace string with: ",
                    .query_replace_from => "Query replace: ",
                    .query_replace_to => "Query replace with: ",
                    .replace_regexp_from => "Replace regexp: ",
                    .replace_regexp_to => "Replace regexp with: ",
                    .query_replace_regexp_from => "Query replace regexp: ",
                    .query_replace_regexp_to => "Query replace regexp with: ",
                };
                try out.appendSlice(prompt);
                try out.appendSlice(self.input_buffer.items);
                if (self.minibuffer_hint.items.len > 0) {
                    try out.appendSlice("  |  ");
                    try out.appendSlice(self.minibuffer_hint.items);
                }
                return out.items;
            },
            .isearch => |state| {
                const prompt = switch (state.direction) {
                    .forward => if (state.matched) "I-search: " else "Failing I-search: ",
                    .backward => if (state.matched) "I-search backward: " else "Failing I-search backward: ",
                };
                try out.appendSlice(prompt);
                try out.appendSlice(self.input_buffer.items);
                return out.items;
            },
            .save_some_buffers => |state| {
                const idx = self.saveSomeCurrentIndex(&state) orelse {
                    return "Save some buffers";
                };
                const total = state.dirty_indices.items.len;
                const at = state.index + 1;
                if (idx < self.buffers.items.len) {
                    const slot = &self.buffers.items[idx];
                    if (state.phase == .write_path) {
                        try out.writer().print(
                            "Write file for {s}: {s}",
                            .{ slot.displayName(), state.path_input.items },
                        );
                        return out.items;
                    }
                    if (slot.file_path != null) {
                        if (state.quit_after) {
                            try out.writer().print(
                                "Save file {s} before quit? (y,n,!,q) [{d}/{d}]",
                                .{ slot.displayName(), at, total },
                            );
                        } else {
                            try out.writer().print(
                                "Save file {s}? (y,n,!,q) [{d}/{d}]",
                                .{ slot.displayName(), at, total },
                            );
                        }
                    } else {
                        if (state.quit_after) {
                            try out.writer().print(
                                "Buffer {s} has no file path (y writes file, n skips then quit). (n,!,q) [{d}/{d}]",
                                .{ slot.displayName(), at, total },
                            );
                        } else {
                            try out.writer().print(
                                "Buffer {s} has no file path (y writes file, n skips). (n,!,q) [{d}/{d}]",
                                .{ slot.displayName(), at, total },
                            );
                        }
                    }
                } else {
                    if (state.quit_after) {
                        try out.writer().print("Save buffer before quit? (y,n,!,q) [{d}/{d}]", .{ at, total });
                    } else {
                        try out.writer().print("Save buffer? (y,n,!,q) [{d}/{d}]", .{ at, total });
                    }
                }
                return out.items;
            },
            .quoted_insert => {
                return "Quoted insert:";
            },
            .query_replace => |state| {
                const label = switch (state.kind) {
                    .literal => "Query replace",
                    .regexp => "Query replace regexp",
                };
                try out.writer().print(
                    "{s} {s} -> {s}  [y,n,!,q] ({d} replaced, {d} skipped)",
                    .{ label, state.from, state.to, state.replaced, state.skipped },
                );
                return out.items;
            },
        }
    }

    fn activeWindow(self: *Editor) *Window {
        return &self.windows.items[self.active_window];
    }

    fn activeSlot(self: *Editor) *BufferSlot {
        const win = self.activeWindow();
        return &self.buffers.items[win.buffer_id];
    }

    fn isSearchChar(byte: u8) bool {
        return std.ascii.isAlphanumeric(byte) or byte == '_';
    }

    fn isWordByte(byte: u8) bool {
        return std.ascii.isAlphanumeric(byte) or byte == '_';
    }

    fn wordForwardTarget(text: []const u8, from: usize) usize {
        var idx = @min(from, text.len);
        if (idx < text.len and isWordByte(text[idx])) {
            while (idx < text.len and isWordByte(text[idx])) : (idx += 1) {}
            return idx;
        }

        while (idx < text.len and !isWordByte(text[idx])) : (idx += 1) {}
        while (idx < text.len and isWordByte(text[idx])) : (idx += 1) {}
        return idx;
    }

    fn wordBackwardTarget(text: []const u8, from: usize) usize {
        var idx = @min(from, text.len);
        if (idx == 0) return 0;

        if (isWordByte(text[idx - 1])) {
            while (idx > 0 and isWordByte(text[idx - 1])) : (idx -= 1) {}
            return idx;
        }

        while (idx > 0 and !isWordByte(text[idx - 1])) : (idx -= 1) {}
        while (idx > 0 and isWordByte(text[idx - 1])) : (idx -= 1) {}
        return idx;
    }

    fn findWordAtOrAfter(text: []const u8, from: usize) ?struct { start: usize, end: usize } {
        var idx = @min(from, text.len);
        while (idx < text.len and !isWordByte(text[idx])) : (idx += 1) {}
        if (idx >= text.len) return null;

        const start = idx;
        while (idx < text.len and isWordByte(text[idx])) : (idx += 1) {}
        return .{ .start = start, .end = idx };
    }

    fn findWordBefore(text: []const u8, from: usize) ?struct { start: usize, end: usize } {
        var idx = @min(from, text.len);
        while (idx > 0 and !isWordByte(text[idx - 1])) : (idx -= 1) {}
        if (idx == 0) return null;

        const end = idx;
        while (idx > 0 and isWordByte(text[idx - 1])) : (idx -= 1) {}
        return .{ .start = idx, .end = end };
    }

    fn setStatus(self: *Editor, text: []const u8) !void {
        self.status_line.clearRetainingCapacity();
        try self.status_line.appendSlice(text);
    }

    fn setStatusFmt(self: *Editor, comptime fmt: []const u8, args: anytype) !void {
        const msg = try std.fmt.allocPrint(self.allocator, fmt, args);
        defer self.allocator.free(msg);
        try self.setStatus(msg);
    }
};

test "editor inserts printable chars" {
    var ed = try Editor.init(std.testing.allocator, .{ .rows = 10, .cols = 40 });
    defer ed.deinit();

    try ed.handleKey(.{ .char = 'h' });
    try ed.handleKey(.{ .char = 'i' });

    const win = ed.windows.items[ed.active_window];
    const buf = ed.buffers.items[win.buffer_id].buffer;
    try std.testing.expectEqualStrings("hi", buf.bytes());
}

test "editor moves to line boundaries with ctrl-a and ctrl-e" {
    var ed = try Editor.init(std.testing.allocator, .{ .rows = 10, .cols = 40 });
    defer ed.deinit();

    try ed.buffers.items[0].buffer.setText("abc\ndef");
    ed.windows.items[0].cursor.byte_index = ed.buffers.items[0].buffer.lineColToByte(1, 2);

    try ed.handleKey(.{ .ctrl = 'a' });
    var pos = ed.buffers.items[0].buffer.byteToLineCol(ed.windows.items[0].cursor.byte_index);
    try std.testing.expectEqual(@as(usize, 1), pos.line);
    try std.testing.expectEqual(@as(usize, 0), pos.column);

    try ed.handleKey(.{ .ctrl = 'e' });
    pos = ed.buffers.items[0].buffer.byteToLineCol(ed.windows.items[0].cursor.byte_index);
    try std.testing.expectEqual(@as(usize, 1), pos.line);
    try std.testing.expectEqual(@as(usize, 3), pos.column);
}

test "editor ctrl-d deletes forward char" {
    var ed = try Editor.init(std.testing.allocator, .{ .rows = 10, .cols = 40 });
    defer ed.deinit();

    try ed.buffers.items[0].buffer.setText("abc");
    ed.windows.items[0].cursor.byte_index = 1;
    try ed.handleKey(.{ .ctrl = 'd' });

    try std.testing.expectEqualStrings("ac", ed.buffers.items[0].buffer.bytes());
    try std.testing.expectEqual(@as(usize, 1), ed.windows.items[0].cursor.byte_index);
}

test "editor ctrl-o opens line and keeps point" {
    var ed = try Editor.init(std.testing.allocator, .{ .rows = 10, .cols = 40 });
    defer ed.deinit();

    try ed.buffers.items[0].buffer.setText("abc");
    ed.windows.items[0].cursor.byte_index = 1;
    try ed.handleKey(.{ .ctrl = 'o' });

    try std.testing.expectEqualStrings("a\nbc", ed.buffers.items[0].buffer.bytes());
    try std.testing.expectEqual(@as(usize, 1), ed.windows.items[0].cursor.byte_index);
}

test "editor ctrl-t transposes chars around point" {
    var ed = try Editor.init(std.testing.allocator, .{ .rows = 10, .cols = 40 });
    defer ed.deinit();

    try ed.buffers.items[0].buffer.setText("ab");
    ed.windows.items[0].cursor.byte_index = 1;
    try ed.handleKey(.{ .ctrl = 't' });

    try std.testing.expectEqualStrings("ba", ed.buffers.items[0].buffer.bytes());
    try std.testing.expectEqual(@as(usize, 2), ed.windows.items[0].cursor.byte_index);
}

test "editor meta-f and meta-b move by words" {
    var ed = try Editor.init(std.testing.allocator, .{ .rows = 10, .cols = 40 });
    defer ed.deinit();

    try ed.buffers.items[0].buffer.setText("foo bar_baz");
    ed.windows.items[0].cursor.byte_index = 0;

    try ed.handleKey(.{ .meta = 'f' });
    try std.testing.expectEqual(@as(usize, 3), ed.windows.items[0].cursor.byte_index);
    try ed.handleKey(.{ .meta = 'f' });
    try std.testing.expectEqual(@as(usize, 11), ed.windows.items[0].cursor.byte_index);
    try ed.handleKey(.{ .meta = 'b' });
    try std.testing.expectEqual(@as(usize, 4), ed.windows.items[0].cursor.byte_index);
}

test "editor meta-d and meta-backspace kill words" {
    var ed = try Editor.init(std.testing.allocator, .{ .rows = 10, .cols = 40 });
    defer ed.deinit();

    try ed.buffers.items[0].buffer.setText("foo bar");
    ed.windows.items[0].cursor.byte_index = 0;
    try ed.handleKey(.{ .meta = 'd' });
    try std.testing.expectEqualStrings(" bar", ed.buffers.items[0].buffer.bytes());
    try std.testing.expectEqualStrings("foo", ed.kill_ring.latest().?);

    ed.windows.items[0].cursor.byte_index = ed.buffers.items[0].buffer.len();
    try ed.handleKey(.meta_backspace);
    try std.testing.expectEqualStrings(" ", ed.buffers.items[0].buffer.bytes());
    try std.testing.expectEqualStrings("bar", ed.kill_ring.latest().?);
}

test "editor meta-t transposes neighboring words" {
    var ed = try Editor.init(std.testing.allocator, .{ .rows = 10, .cols = 40 });
    defer ed.deinit();

    try ed.buffers.items[0].buffer.setText("alpha beta");
    ed.windows.items[0].cursor.byte_index = ed.buffers.items[0].buffer.len();
    try ed.handleKey(.{ .meta = 't' });

    try std.testing.expectEqualStrings("beta alpha", ed.buffers.items[0].buffer.bytes());
}

test "editor set mark and kill region with ctrl-w" {
    var ed = try Editor.init(std.testing.allocator, .{ .rows = 10, .cols = 40 });
    defer ed.deinit();

    try ed.buffers.items[0].buffer.setText("hello");
    ed.windows.items[0].cursor.byte_index = 0;

    try ed.handleKey(.{ .ctrl = '@' });
    try ed.handleKey(.{ .ctrl = 'f' });
    try ed.handleKey(.{ .ctrl = 'f' });
    try ed.handleKey(.{ .ctrl = 'w' });

    try std.testing.expectEqualStrings("llo", ed.buffers.items[0].buffer.bytes());
    try std.testing.expectEqualStrings("he", ed.kill_ring.latest().?);
    try std.testing.expect(ed.windows.items[0].mark == null);
}

test "editor C-u C-@ pops previous mark from mark ring" {
    var ed = try Editor.init(std.testing.allocator, .{ .rows = 10, .cols = 40 });
    defer ed.deinit();

    try ed.buffers.items[0].buffer.setText("hello");
    ed.windows.items[0].cursor.byte_index = 0;

    try ed.handleKey(.{ .ctrl = '@' }); // mark at 0
    try ed.handleKey(.{ .ctrl = 'f' });
    try ed.handleKey(.{ .ctrl = 'f' });
    try ed.handleKey(.{ .ctrl = '@' }); // mark at 2, pushes 0 into ring
    try ed.handleKey(.{ .ctrl = 'f' }); // point at 3

    try ed.handleKey(.{ .ctrl = 'u' });
    try ed.handleKey(.{ .ctrl = '@' }); // pop mark ring => point to 0

    try std.testing.expectEqual(@as(usize, 0), ed.windows.items[0].cursor.byte_index);
    try std.testing.expectEqual(@as(usize, 3), ed.windows.items[0].mark.?);
}

test "editor copy-region-as-kill keeps text" {
    var ed = try Editor.init(std.testing.allocator, .{ .rows = 10, .cols = 40 });
    defer ed.deinit();

    try ed.buffers.items[0].buffer.setText("hello");
    ed.windows.items[0].cursor.byte_index = 0;
    try ed.handleKey(.{ .ctrl = '@' });
    try ed.handleKey(.{ .ctrl = 'f' });
    try ed.handleKey(.{ .ctrl = 'f' });
    try ed.handleKey(.{ .ctrl = 'c' });
    try ed.handleKey(.{ .char = 'w' });

    try std.testing.expectEqualStrings("hello", ed.buffers.items[0].buffer.bytes());
    try std.testing.expectEqualStrings("he", ed.kill_ring.latest().?);
}

test "editor meta-w copies region as kill" {
    var ed = try Editor.init(std.testing.allocator, .{ .rows = 10, .cols = 40 });
    defer ed.deinit();

    try ed.buffers.items[0].buffer.setText("hello");
    ed.windows.items[0].cursor.byte_index = 0;
    try ed.handleKey(.{ .ctrl = '@' });
    try ed.handleKey(.{ .ctrl = 'f' });
    try ed.handleKey(.{ .ctrl = 'f' });
    try ed.handleKey(.{ .meta = 'w' });

    try std.testing.expectEqualStrings("hello", ed.buffers.items[0].buffer.bytes());
    try std.testing.expectEqualStrings("he", ed.kill_ring.latest().?);
}

test "editor exchange point and mark" {
    var ed = try Editor.init(std.testing.allocator, .{ .rows = 10, .cols = 40 });
    defer ed.deinit();

    try ed.buffers.items[0].buffer.setText("hello");
    ed.windows.items[0].cursor.byte_index = 0;
    try ed.handleKey(.{ .ctrl = '@' });
    try ed.handleKey(.{ .ctrl = 'f' });
    try ed.handleKey(.{ .ctrl = 'f' });
    try ed.handleKey(.{ .ctrl = 'x' });
    try ed.handleKey(.{ .ctrl = 'x' });

    try std.testing.expectEqual(@as(usize, 0), ed.windows.items[0].cursor.byte_index);
    try std.testing.expectEqual(@as(usize, 2), ed.windows.items[0].mark.?);
}

test "editor mark whole buffer" {
    var ed = try Editor.init(std.testing.allocator, .{ .rows = 10, .cols = 40 });
    defer ed.deinit();

    try ed.buffers.items[0].buffer.setText("hello");
    ed.windows.items[0].cursor.byte_index = 3;
    try ed.handleKey(.{ .ctrl = 'x' });
    try ed.handleKey(.{ .char = 'h' });

    try std.testing.expectEqual(@as(usize, 0), ed.windows.items[0].cursor.byte_index);
    try std.testing.expectEqual(@as(usize, 5), ed.windows.items[0].mark.?);
}

test "queueHighlightedLine overlays active region" {
    var term = term_mod.Terminal.init(std.testing.allocator);
    defer term.deinit();

    try Editor.queueHighlightedLine(&term, "abc", null, 0, .{ .start = 1, .end = 2 });
    try std.testing.expectEqualStrings("a\x1b[7mb\x1b[27mc", term.out_buffer.items);
}

test "queueHighlightedLine preserves syntax color with region overlay" {
    var term = term_mod.Terminal.init(std.testing.allocator);
    defer term.deinit();

    const styles = [_]syntax_mod.Style{ .keyword, .keyword };
    try Editor.queueHighlightedLine(&term, "ab", styles[0..], 0, .{ .start = 0, .end = 2 });
    try std.testing.expect(std.mem.indexOf(u8, term.out_buffer.items, "\x1b[7m") != null);
    try std.testing.expect(std.mem.indexOf(u8, term.out_buffer.items, "\x1b[34m") != null);
    try std.testing.expect(std.mem.indexOf(u8, term.out_buffer.items, "\x1b[27m") != null);
    try std.testing.expect(std.mem.indexOf(u8, term.out_buffer.items, "\x1b[39m") != null);
}

test "editor kill then yank" {
    var ed = try Editor.init(std.testing.allocator, .{ .rows = 10, .cols = 40 });
    defer ed.deinit();

    for ("hello\n") |ch| {
        try ed.handleKey(.{ .char = ch });
    }

    ed.windows.items[0].cursor.byte_index = 0;
    try ed.handleKey(.{ .ctrl = 'k' });

    try std.testing.expectEqualStrings("\n", ed.buffers.items[0].buffer.bytes());

    try ed.handleKey(.{ .ctrl = 'y' });
    try std.testing.expectEqualStrings("hello\n", ed.buffers.items[0].buffer.bytes());
}

test "editor consecutive kill-word appends to single kill-ring entry" {
    var ed = try Editor.init(std.testing.allocator, .{ .rows = 10, .cols = 40 });
    defer ed.deinit();

    try ed.buffers.items[0].buffer.setText("foo bar baz");
    ed.windows.items[0].cursor.byte_index = 0;

    try ed.handleKey(.{ .meta = 'd' });
    try ed.handleKey(.{ .meta = 'd' });
    try std.testing.expectEqualStrings(" baz", ed.buffers.items[0].buffer.bytes());
    try std.testing.expectEqualStrings("foo bar", ed.kill_ring.latest().?);
}

test "editor consecutive backward-kill-word prepends to keep order" {
    var ed = try Editor.init(std.testing.allocator, .{ .rows = 10, .cols = 40 });
    defer ed.deinit();

    try ed.buffers.items[0].buffer.setText("foo bar baz");
    ed.windows.items[0].cursor.byte_index = ed.buffers.items[0].buffer.len();

    try ed.handleKey(.meta_backspace);
    try ed.handleKey(.meta_backspace);
    try std.testing.expectEqualStrings("foo ", ed.buffers.items[0].buffer.bytes());
    try std.testing.expectEqualStrings("bar baz", ed.kill_ring.latest().?);
}

test "editor yank-pop cycles previously yanked text" {
    var ed = try Editor.init(std.testing.allocator, .{ .rows = 10, .cols = 40 });
    defer ed.deinit();

    try ed.kill_ring.push("one");
    try ed.kill_ring.push("two");
    try ed.kill_ring.push("three");

    try ed.handleKey(.{ .ctrl = 'y' });
    try std.testing.expectEqualStrings("three", ed.buffers.items[0].buffer.bytes());

    try ed.handleKey(.{ .meta = 'y' });
    try std.testing.expectEqualStrings("two", ed.buffers.items[0].buffer.bytes());

    try ed.handleKey(.{ .meta = 'y' });
    try std.testing.expectEqualStrings("one", ed.buffers.items[0].buffer.bytes());
}

test "editor yank-pop requires prior yank" {
    var ed = try Editor.init(std.testing.allocator, .{ .rows = 10, .cols = 40 });
    defer ed.deinit();

    try ed.kill_ring.push("a");
    try ed.kill_ring.push("b");
    try ed.handleKey(.{ .meta = 'y' });

    try std.testing.expectEqualStrings("", ed.buffers.items[0].buffer.bytes());
    try std.testing.expect(std.mem.indexOf(u8, ed.status_line.items, "not a yank") != null);
}

test "editor incremental search and confirm" {
    var ed = try Editor.init(std.testing.allocator, .{ .rows = 10, .cols = 60 });
    defer ed.deinit();

    try ed.buffers.items[0].buffer.setText("alpha beta alpha");
    ed.windows.items[0].cursor.byte_index = 0;

    try ed.handleKey(.{ .ctrl = 's' });
    try ed.handleKey(.{ .char = 'a' });
    try ed.handleKey(.{ .char = 'l' });
    try ed.handleKey(.{ .char = 'p' });
    try ed.handleKey(.{ .char = 'h' });
    try ed.handleKey(.{ .char = 'a' });
    try ed.handleKey(.enter);

    try std.testing.expectEqual(@as(usize, 11), ed.windows.items[0].cursor.byte_index);
    try std.testing.expectEqualStrings("alpha", ed.last_search.items);
}

test "editor open file via minibuffer" {
    const allocator = std.testing.allocator;

    const filename = try std.fmt.allocPrint(allocator, ".mg-zig-open-{d}.txt", .{std.time.microTimestamp()});
    defer allocator.free(filename);
    defer std.fs.cwd().deleteFile(filename) catch {};

    try std.fs.cwd().writeFile(.{ .sub_path = filename, .data = "from-file\n" });

    var ed = try Editor.init(allocator, .{ .rows = 10, .cols = 60 });
    defer ed.deinit();

    try ed.handleKey(.{ .ctrl = 'x' });
    try ed.handleKey(.{ .ctrl = 'f' });
    for (filename) |ch| {
        try ed.handleKey(.{ .char = ch });
    }
    try ed.handleKey(.enter);

    const win = ed.windows.items[ed.active_window];
    const buf = ed.buffers.items[win.buffer_id].buffer;
    try std.testing.expectEqualStrings("from-file\n", buf.bytes());
}

test "editor open missing file creates empty buffer with file path" {
    const allocator = std.testing.allocator;
    const filename = try std.fmt.allocPrint(allocator, ".mg-zig-new-{d}.txt", .{std.time.microTimestamp()});
    defer allocator.free(filename);
    defer std.fs.cwd().deleteFile(filename) catch {};

    var ed = try Editor.init(allocator, .{ .rows = 10, .cols = 60 });
    defer ed.deinit();

    try ed.openFile(filename);

    const slot = ed.activeSlot();
    try std.testing.expectEqualStrings("", slot.buffer.bytes());
    try std.testing.expect(slot.file_path != null);
    try std.testing.expectEqualStrings(filename, slot.file_path.?);
}

test "editor save-as via minibuffer" {
    const allocator = std.testing.allocator;

    const filename = try std.fmt.allocPrint(allocator, ".mg-zig-save-{d}.txt", .{std.time.microTimestamp()});
    defer allocator.free(filename);
    defer std.fs.cwd().deleteFile(filename) catch {};

    var ed = try Editor.init(allocator, .{ .rows = 10, .cols = 60 });
    defer ed.deinit();

    for ("saved-text") |ch| {
        try ed.handleKey(.{ .char = ch });
    }

    try ed.handleKey(.{ .ctrl = 'x' });
    try ed.handleKey(.{ .ctrl = 'w' });
    for (filename) |ch| {
        try ed.handleKey(.{ .char = ch });
    }
    try ed.handleKey(.enter);

    var file = try std.fs.cwd().openFile(filename, .{});
    defer file.close();

    const data = try file.readToEndAlloc(allocator, 1024 * 1024);
    defer allocator.free(data);

    try std.testing.expectEqualStrings("saved-text", data);
}

test "editor M-x command prompt executes next-buffer" {
    const allocator = std.testing.allocator;

    const filename = try std.fmt.allocPrint(allocator, ".mg-zig-cmd-{d}.txt", .{std.time.microTimestamp()});
    defer allocator.free(filename);
    defer std.fs.cwd().deleteFile(filename) catch {};
    try std.fs.cwd().writeFile(.{ .sub_path = filename, .data = "buf2\n" });

    var ed = try Editor.init(allocator, .{ .rows = 10, .cols = 60 });
    defer ed.deinit();
    try ed.openFile(filename);

    const before = ed.windows.items[ed.active_window].buffer_id;
    try ed.handleKey(.{ .meta = 'x' });
    for ("next-buffer") |ch| {
        try ed.handleKey(.{ .char = ch });
    }
    try ed.handleKey(.enter);

    const after = ed.windows.items[ed.active_window].buffer_id;
    try std.testing.expect(before != after);
}

test "editor M-x alias executes command" {
    var ed = try Editor.init(std.testing.allocator, .{ .rows = 10, .cols = 60 });
    defer ed.deinit();

    try ed.handleKey(.{ .meta = 'x' });
    try ed.handleKey(.{ .char = 'q' });
    try ed.handleKey(.enter);

    try std.testing.expect(ed.shouldQuit());
}

test "editor command completion unique match" {
    var ed = try Editor.init(std.testing.allocator, .{ .rows = 10, .cols = 60 });
    defer ed.deinit();

    try ed.handleKey(.{ .meta = 'x' });
    for ("next-b") |ch| {
        try ed.handleKey(.{ .char = ch });
    }
    try ed.handleKey(.tab);

    try std.testing.expectEqualStrings("next-buffer", ed.input_buffer.items);
    try ed.handleKey(.escape);
}

test "editor command completion common prefix" {
    var ed = try Editor.init(std.testing.allocator, .{ .rows = 10, .cols = 60 });
    defer ed.deinit();

    try ed.handleKey(.{ .meta = 'x' });
    try ed.handleKey(.{ .char = 'i' });
    try ed.handleKey(.{ .char = 's' });
    try ed.handleKey(.tab);

    try std.testing.expectEqualStrings("isearch-", ed.input_buffer.items);
    try ed.handleKey(.escape);
}

test "editor command completion fuzzy unique match" {
    var ed = try Editor.init(std.testing.allocator, .{ .rows = 10, .cols = 60 });
    defer ed.deinit();

    try ed.handleKey(.{ .meta = 'x' });
    for ("svb") |ch| {
        try ed.handleKey(.{ .char = ch });
    }
    try ed.handleKey(.tab);

    try std.testing.expectEqualStrings("save-buffer", ed.input_buffer.items);
    try ed.handleKey(.escape);
}

test "editor repeated tab lists fuzzy-ranked command candidates" {
    var ed = try Editor.init(std.testing.allocator, .{ .rows = 10, .cols = 60 });
    defer ed.deinit();

    try ed.handleKey(.{ .meta = 'x' });
    try ed.handleKey(.{ .char = 's' });
    try ed.handleKey(.{ .char = 'b' });
    try ed.handleKey(.tab);
    try ed.handleKey(.tab);

    try std.testing.expect(std.mem.startsWith(u8, ed.minibuffer_hint.items, "ido "));
    try std.testing.expect(std.mem.indexOf(u8, ed.minibuffer_hint.items, "save-buffer") != null);
    try ed.handleKey(.escape);
}

test "editor command hint shows help for unique match" {
    var ed = try Editor.init(std.testing.allocator, .{ .rows = 10, .cols = 60 });
    defer ed.deinit();

    try ed.handleKey(.{ .meta = 'x' });
    for ("undo") |ch| {
        try ed.handleKey(.{ .char = ch });
    }
    try std.testing.expect(std.mem.indexOf(u8, ed.minibuffer_hint.items, "Undo latest edit") != null);
    try ed.handleKey(.escape);
}

test "editor repeated tab lists command candidates" {
    var ed = try Editor.init(std.testing.allocator, .{ .rows = 10, .cols = 60 });
    defer ed.deinit();

    try ed.handleKey(.{ .meta = 'x' });
    try ed.handleKey(.{ .char = 'i' });
    try ed.handleKey(.tab);
    try ed.handleKey(.tab);

    try std.testing.expect(std.mem.startsWith(u8, ed.minibuffer_hint.items, "ido "));
    try ed.handleKey(.escape);
}

test "editor ido list is shown for command query" {
    var ed = try Editor.init(std.testing.allocator, .{ .rows = 10, .cols = 60 });
    defer ed.deinit();

    try ed.handleKey(.{ .meta = 'x' });
    try ed.handleKey(.{ .char = 'i' });
    try ed.handleKey(.{ .char = 's' });

    try std.testing.expect(std.mem.startsWith(u8, ed.minibuffer_hint.items, "ido "));
    try std.testing.expect(std.mem.indexOf(u8, ed.minibuffer_hint.items, "isearch-forward") != null);
    try ed.handleKey(.escape);
}

test "editor ido cycling executes selected candidate on enter" {
    var ed = try Editor.init(std.testing.allocator, .{ .rows = 10, .cols = 60 });
    defer ed.deinit();

    try ed.handleKey(.{ .meta = 'x' });
    try ed.handleKey(.{ .char = 'i' });
    try ed.handleKey(.{ .char = 's' });
    try ed.handleKey(.{ .ctrl = 's' });
    try std.testing.expect(std.mem.indexOf(u8, ed.minibuffer_hint.items, "ido 2/2:") != null);
    try ed.handleKey(.enter);

    switch (ed.input_mode) {
        .isearch => |state| try std.testing.expectEqual(search_mod.Direction.backward, state.direction),
        else => return error.TestUnexpectedResult,
    }
    try ed.handleKey(.escape);
}

test "editor find-file uses ido candidate list" {
    const allocator = std.testing.allocator;
    const base_dir = try std.fmt.allocPrint(allocator, ".mg-zig-ido-files-{d}", .{std.time.microTimestamp()});
    defer allocator.free(base_dir);
    defer std.fs.cwd().deleteTree(base_dir) catch {};
    try std.fs.cwd().makePath(base_dir);

    const f1 = try std.fmt.allocPrint(allocator, "{s}/alpha.txt", .{base_dir});
    defer allocator.free(f1);
    const f2 = try std.fmt.allocPrint(allocator, "{s}/beta.txt", .{base_dir});
    defer allocator.free(f2);
    try std.fs.cwd().writeFile(.{ .sub_path = f1, .data = "x" });
    try std.fs.cwd().writeFile(.{ .sub_path = f2, .data = "x" });

    var ed = try Editor.init(allocator, .{ .rows = 10, .cols = 60 });
    defer ed.deinit();

    try ed.handleKey(.{ .ctrl = 'x' });
    try ed.handleKey(.{ .ctrl = 'f' });
    const query = try std.fmt.allocPrint(allocator, "{s}/", .{base_dir});
    defer allocator.free(query);
    for (query) |ch| {
        try ed.handleKey(.{ .char = ch });
    }

    try std.testing.expect(std.mem.startsWith(u8, ed.minibuffer_hint.items, "ido "));
    try std.testing.expect(std.mem.indexOf(u8, ed.minibuffer_hint.items, "alpha.txt") != null);
    try ed.handleKey(.escape);
}

test "editor switch-buffer uses ido menu and selects match" {
    const allocator = std.testing.allocator;
    const filename = try std.fmt.allocPrint(allocator, ".mg-zig-buf-{d}.txt", .{std.time.microTimestamp()});
    defer allocator.free(filename);
    defer std.fs.cwd().deleteFile(filename) catch {};
    try std.fs.cwd().writeFile(.{ .sub_path = filename, .data = "buf2\n" });

    var ed = try Editor.init(allocator, .{ .rows = 10, .cols = 60 });
    defer ed.deinit();
    try ed.openFile(filename);

    try std.testing.expectEqualStrings(filename, ed.activeSlot().displayName());

    try ed.handleKey(.{ .ctrl = 'x' });
    try ed.handleKey(.{ .ctrl = 'b' });
    try std.testing.expect(std.mem.startsWith(u8, ed.minibuffer_hint.items, "ido "));
    for ("scr") |ch| {
        try ed.handleKey(.{ .char = ch });
    }
    try ed.handleKey(.enter);

    try std.testing.expectEqualStrings("*scratch*", ed.activeSlot().displayName());
}

test "editor C-x b opens switch-buffer prompt" {
    var ed = try Editor.init(std.testing.allocator, .{ .rows = 10, .cols = 60 });
    defer ed.deinit();

    try ed.handleKey(.{ .ctrl = 'x' });
    try ed.handleKey(.{ .char = 'b' });

    switch (ed.input_mode) {
        .minibuffer => |state| try std.testing.expectEqual(MinibufferKind.switch_buffer, state.kind),
        else => return error.TestUnexpectedResult,
    }
}

test "editor C-x left/right cycles previous and next buffer" {
    const allocator = std.testing.allocator;
    const filename = try std.fmt.allocPrint(allocator, ".mg-zig-prevnext-{d}.txt", .{std.time.microTimestamp()});
    defer allocator.free(filename);
    defer std.fs.cwd().deleteFile(filename) catch {};
    try std.fs.cwd().writeFile(.{ .sub_path = filename, .data = "buf2\n" });

    var ed = try Editor.init(allocator, .{ .rows = 10, .cols = 60 });
    defer ed.deinit();
    try ed.openFile(filename);
    try std.testing.expectEqualStrings(filename, ed.activeSlot().displayName());

    try ed.handleKey(.{ .ctrl = 'x' });
    try ed.handleKey(.arrow_left);
    try std.testing.expectEqualStrings("*scratch*", ed.activeSlot().displayName());

    try ed.handleKey(.{ .ctrl = 'x' });
    try ed.handleKey(.arrow_right);
    try std.testing.expectEqualStrings(filename, ed.activeSlot().displayName());
}

test "editor C-x C-b opens *Buffer List* buffer" {
    var ed = try Editor.init(std.testing.allocator, .{ .rows = 12, .cols = 60 });
    defer ed.deinit();

    try ed.handleKey(.{ .ctrl = 'x' });
    try ed.handleKey(.{ .ctrl = 'b' });

    try std.testing.expectEqualStrings("*Buffer List*", ed.activeSlot().buffer.name.items);
    try std.testing.expect(std.mem.indexOf(u8, ed.activeSlot().buffer.bytes(), "*scratch*") != null);
}

test "editor *Buffer List* enter visits selected buffer" {
    const allocator = std.testing.allocator;
    const filename = try std.fmt.allocPrint(allocator, ".mg-zig-buflist-open-{d}.txt", .{std.time.microTimestamp()});
    defer allocator.free(filename);
    defer std.fs.cwd().deleteFile(filename) catch {};
    try std.fs.cwd().writeFile(.{ .sub_path = filename, .data = "buf\n" });

    var ed = try Editor.init(allocator, .{ .rows = 12, .cols = 60 });
    defer ed.deinit();
    try ed.openFile(filename);

    try ed.handleKey(.{ .ctrl = 'x' });
    try ed.handleKey(.{ .ctrl = 'b' });
    try std.testing.expectEqualStrings("*Buffer List*", ed.activeSlot().buffer.name.items);

    var target_entry: ?usize = null;
    for (ed.buffer_list_entries.items, 0..) |buf_idx, idx| {
        if (std.mem.eql(u8, ed.buffers.items[buf_idx].displayName(), filename)) {
            target_entry = idx;
            break;
        }
    }
    try std.testing.expect(target_entry != null);
    ed.moveCursorToBufferListEntry(target_entry.?);

    try ed.handleKey(.enter);
    try std.testing.expectEqualStrings(filename, ed.activeSlot().displayName());
}

test "editor *Buffer List* d then x kills marked buffer" {
    const allocator = std.testing.allocator;
    const filename = try std.fmt.allocPrint(allocator, ".mg-zig-buflist-kill-{d}.txt", .{std.time.microTimestamp()});
    defer allocator.free(filename);
    defer std.fs.cwd().deleteFile(filename) catch {};
    try std.fs.cwd().writeFile(.{ .sub_path = filename, .data = "buf\n" });

    var ed = try Editor.init(allocator, .{ .rows = 12, .cols = 60 });
    defer ed.deinit();
    try ed.openFile(filename);

    try ed.handleKey(.{ .ctrl = 'x' });
    try ed.handleKey(.{ .ctrl = 'b' });
    try std.testing.expectEqualStrings("*Buffer List*", ed.activeSlot().buffer.name.items);

    var target_entry: ?usize = null;
    for (ed.buffer_list_entries.items, 0..) |buf_idx, idx| {
        if (std.mem.eql(u8, ed.buffers.items[buf_idx].displayName(), filename)) {
            target_entry = idx;
            break;
        }
    }
    try std.testing.expect(target_entry != null);
    ed.moveCursorToBufferListEntry(target_entry.?);

    try ed.handleKey(.{ .char = 'd' });
    try ed.handleKey(.{ .char = 'x' });

    var found = false;
    for (ed.buffers.items) |slot| {
        if (std.mem.eql(u8, slot.displayName(), filename)) {
            found = true;
            break;
        }
    }
    try std.testing.expect(!found);
}

test "editor ctrl-x k kills selected buffer" {
    const allocator = std.testing.allocator;
    const filename = try std.fmt.allocPrint(allocator, ".mg-zig-kill-{d}.txt", .{std.time.microTimestamp()});
    defer allocator.free(filename);
    defer std.fs.cwd().deleteFile(filename) catch {};
    try std.fs.cwd().writeFile(.{ .sub_path = filename, .data = "buf2\n" });

    var ed = try Editor.init(allocator, .{ .rows = 10, .cols = 60 });
    defer ed.deinit();
    try ed.openFile(filename);

    const target = std.fs.path.basename(filename);
    try ed.handleKey(.{ .ctrl = 'x' });
    try ed.handleKey(.{ .char = 'k' });
    for (target) |ch| {
        try ed.handleKey(.{ .char = ch });
    }
    try ed.handleKey(.enter);

    try std.testing.expectEqual(@as(usize, 1), ed.buffers.items.len);
    try std.testing.expectEqualStrings("*scratch*", ed.activeSlot().displayName());
}

test "editor ctrl-x k asks confirmation for modified buffer" {
    const allocator = std.testing.allocator;
    const filename = try std.fmt.allocPrint(allocator, ".mg-zig-kill-modified-{d}.txt", .{std.time.microTimestamp()});
    defer allocator.free(filename);
    defer std.fs.cwd().deleteFile(filename) catch {};
    try std.fs.cwd().writeFile(.{ .sub_path = filename, .data = "buf2\n" });

    var ed = try Editor.init(allocator, .{ .rows = 10, .cols = 60 });
    defer ed.deinit();
    try ed.openFile(filename);

    try ed.handleKey(.{ .char = 'x' });
    try std.testing.expect(ed.activeSlot().dirty);

    const target = std.fs.path.basename(filename);
    try ed.handleKey(.{ .ctrl = 'x' });
    try ed.handleKey(.{ .char = 'k' });
    for (target) |ch| {
        try ed.handleKey(.{ .char = ch });
    }
    try ed.handleKey(.enter);

    try std.testing.expectEqual(@as(usize, 2), ed.buffers.items.len);
    try std.testing.expect(ed.pending_kill_buffer_idx != null);
    switch (ed.input_mode) {
        .minibuffer => |state| try std.testing.expectEqual(MinibufferKind.kill_buffer_confirm, state.kind),
        else => try std.testing.expect(false),
    }

    for ("yes") |ch| {
        try ed.handleKey(.{ .char = ch });
    }
    try ed.handleKey(.enter);

    try std.testing.expectEqual(@as(?usize, null), ed.pending_kill_buffer_idx);
    try std.testing.expectEqual(@as(usize, 1), ed.buffers.items.len);
    try std.testing.expectEqualStrings("*scratch*", ed.activeSlot().displayName());
}

test "editor ctrl-x k cancel keeps modified buffer" {
    const allocator = std.testing.allocator;
    const filename = try std.fmt.allocPrint(allocator, ".mg-zig-kill-cancel-{d}.txt", .{std.time.microTimestamp()});
    defer allocator.free(filename);
    defer std.fs.cwd().deleteFile(filename) catch {};
    try std.fs.cwd().writeFile(.{ .sub_path = filename, .data = "buf2\n" });

    var ed = try Editor.init(allocator, .{ .rows = 10, .cols = 60 });
    defer ed.deinit();
    try ed.openFile(filename);
    try ed.handleKey(.{ .char = 'x' });

    const target = std.fs.path.basename(filename);
    try ed.handleKey(.{ .ctrl = 'x' });
    try ed.handleKey(.{ .char = 'k' });
    for (target) |ch| {
        try ed.handleKey(.{ .char = ch });
    }
    try ed.handleKey(.enter);
    try std.testing.expect(ed.pending_kill_buffer_idx != null);

    try ed.handleKey(.{ .ctrl = 'g' });

    try std.testing.expectEqual(@as(?usize, null), ed.pending_kill_buffer_idx);
    try std.testing.expectEqual(@as(usize, 2), ed.buffers.items.len);
    switch (ed.input_mode) {
        .normal => {},
        else => try std.testing.expect(false),
    }

    var found = false;
    for (ed.buffers.items) |slot| {
        if (std.mem.eql(u8, slot.displayName(), target)) {
            found = true;
            break;
        }
    }
    try std.testing.expect(found);
}

test "editor split other and delete window shortcuts" {
    var ed = try Editor.init(std.testing.allocator, .{ .rows = 12, .cols = 60 });
    defer ed.deinit();

    try std.testing.expectEqual(@as(usize, 1), ed.windows.items.len);
    try std.testing.expectEqual(@as(usize, 0), ed.active_window);

    try ed.handleKey(.{ .ctrl = 'x' });
    try ed.handleKey(.{ .char = '2' });
    try std.testing.expectEqual(@as(usize, 2), ed.windows.items.len);
    try std.testing.expectEqual(@as(usize, 0), ed.active_window);
    for (ed.windows.items) |w| {
        try std.testing.expect(w.buffer_id < ed.buffers.items.len);
    }

    try ed.handleKey(.{ .ctrl = 'x' });
    try ed.handleKey(.{ .char = 'o' });
    try std.testing.expectEqual(@as(usize, 1), ed.active_window);

    try ed.handleKey(.{ .ctrl = 'x' });
    try ed.handleKey(.{ .char = '0' });
    try std.testing.expectEqual(@as(usize, 1), ed.windows.items.len);
    try std.testing.expectEqual(@as(usize, 0), ed.active_window);
    for (ed.windows.items) |w| {
        try std.testing.expect(w.buffer_id < ed.buffers.items.len);
    }
}

test "editor split-window-right creates side-by-side windows" {
    var ed = try Editor.init(std.testing.allocator, .{ .rows = 12, .cols = 20 });
    defer ed.deinit();

    try ed.handleKey(.{ .ctrl = 'x' });
    try ed.handleKey(.{ .char = '3' });
    try std.testing.expectEqual(@as(usize, 2), ed.windows.items.len);

    ed.resize(.{ .rows = 12, .cols = 20 });
    try std.testing.expectEqual(@as(usize, 10), ed.windows.items[0].rows);
    try std.testing.expectEqual(@as(usize, 10), ed.windows.items[0].cols);
    try std.testing.expectEqual(@as(usize, 10), ed.windows.items[1].rows);
    try std.testing.expectEqual(@as(usize, 9), ed.windows.items[1].cols);
}

test "editor supports mixed split directions" {
    var ed = try Editor.init(std.testing.allocator, .{ .rows = 12, .cols = 20 });
    defer ed.deinit();

    try ed.handleKey(.{ .ctrl = 'x' });
    try ed.handleKey(.{ .char = '3' });

    try ed.handleKey(.{ .ctrl = 'x' });
    try ed.handleKey(.{ .char = '2' });
    try std.testing.expectEqual(@as(usize, 3), ed.windows.items.len);

    ed.resize(.{ .rows = 12, .cols = 20 });
    // one pane gets split horizontally; resulting windows differ in row geometry
    var saw_tall = false;
    var saw_short = false;
    for (ed.windows.items) |w| {
        if (w.rows >= 9) saw_tall = true;
        if (w.rows <= 5) saw_short = true;
    }
    try std.testing.expect(saw_tall);
    try std.testing.expect(saw_short);
}

test "editor delete-window works after mixed splits" {
    var ed = try Editor.init(std.testing.allocator, .{ .rows = 14, .cols = 30 });
    defer ed.deinit();

    try ed.handleKey(.{ .ctrl = 'x' });
    try ed.handleKey(.{ .char = '3' });
    try ed.handleKey(.{ .ctrl = 'x' });
    try ed.handleKey(.{ .char = '2' });
    try std.testing.expectEqual(@as(usize, 3), ed.windows.items.len);

    try ed.handleKey(.{ .ctrl = 'x' });
    try ed.handleKey(.{ .char = '0' });
    try std.testing.expectEqual(@as(usize, 2), ed.windows.items.len);
    for (ed.windows.items) |w| {
        try std.testing.expect(w.buffer_id < ed.buffers.items.len);
    }
}

test "editor delete-other-windows keeps one window" {
    var ed = try Editor.init(std.testing.allocator, .{ .rows = 12, .cols = 60 });
    defer ed.deinit();

    try ed.handleKey(.{ .ctrl = 'x' });
    try ed.handleKey(.{ .char = '2' });
    try ed.handleKey(.{ .ctrl = 'x' });
    try ed.handleKey(.{ .char = '2' });
    try std.testing.expectEqual(@as(usize, 3), ed.windows.items.len);

    try ed.handleKey(.{ .ctrl = 'x' });
    try ed.handleKey(.{ .char = '1' });
    try std.testing.expectEqual(@as(usize, 1), ed.windows.items.len);
    try std.testing.expectEqual(@as(usize, 0), ed.active_window);
}

test "editor window layout distributes rows" {
    var ed = try Editor.init(std.testing.allocator, .{ .rows = 11, .cols = 80 });
    defer ed.deinit();

    try ed.handleKey(.{ .ctrl = 'x' });
    try ed.handleKey(.{ .char = '2' });
    try ed.handleKey(.{ .ctrl = 'x' });
    try ed.handleKey(.{ .char = '2' });
    try std.testing.expectEqual(@as(usize, 3), ed.windows.items.len);

    ed.resize(.{ .rows = 11, .cols = 80 });
    try std.testing.expectEqual(@as(usize, 3), ed.windows.items[0].rows);
    try std.testing.expectEqual(@as(usize, 2), ed.windows.items[1].rows);
    try std.testing.expectEqual(@as(usize, 2), ed.windows.items[2].rows);
}

test "window layout reserves per-window modeline rows" {
    const a = Editor.windowLayoutFor(.horizontal, 10, 80, 3, 0);
    try std.testing.expectEqual(@as(usize, 1), a.top_row);
    try std.testing.expectEqual(@as(usize, 1), a.left_col);
    try std.testing.expectEqual(@as(usize, 3), a.body_rows);
    try std.testing.expectEqual(@as(usize, 80), a.body_cols);
    try std.testing.expectEqual(@as(?usize, 4), a.modeline_row);
    try std.testing.expectEqual(@as(?usize, null), a.separator_col);

    const b = Editor.windowLayoutFor(.horizontal, 10, 80, 3, 1);
    try std.testing.expectEqual(@as(usize, 5), b.top_row);
    try std.testing.expectEqual(@as(usize, 2), b.body_rows);
    try std.testing.expectEqual(@as(?usize, 7), b.modeline_row);
    try std.testing.expectEqual(@as(?usize, null), b.separator_col);

    const third = Editor.windowLayoutFor(.horizontal, 10, 80, 3, 2);
    try std.testing.expectEqual(@as(usize, 8), third.top_row);
    try std.testing.expectEqual(@as(usize, 2), third.body_rows);
    try std.testing.expectEqual(@as(?usize, 10), third.modeline_row);
    try std.testing.expectEqual(@as(?usize, null), third.separator_col);
}

test "single window layout still has modeline above minibuffer" {
    const only = Editor.windowLayoutFor(.horizontal, 10, 80, 1, 0);
    try std.testing.expectEqual(@as(usize, 1), only.top_row);
    try std.testing.expectEqual(@as(usize, 9), only.body_rows);
    try std.testing.expectEqual(@as(?usize, 10), only.modeline_row);
}

test "two-window layout places first modeline at split boundary" {
    const top = Editor.windowLayoutFor(.horizontal, 10, 80, 2, 0);
    try std.testing.expectEqual(@as(usize, 1), top.top_row);
    try std.testing.expectEqual(@as(usize, 4), top.body_rows);
    try std.testing.expectEqual(@as(?usize, 5), top.modeline_row);

    const bottom = Editor.windowLayoutFor(.horizontal, 10, 80, 2, 1);
    try std.testing.expectEqual(@as(usize, 6), bottom.top_row);
    try std.testing.expectEqual(@as(usize, 4), bottom.body_rows);
    try std.testing.expectEqual(@as(?usize, 10), bottom.modeline_row);
}

test "vertical window layout reserves separator columns" {
    const left = Editor.windowLayoutFor(.vertical, 10, 20, 2, 0);
    try std.testing.expectEqual(@as(usize, 1), left.top_row);
    try std.testing.expectEqual(@as(usize, 1), left.left_col);
    try std.testing.expectEqual(@as(usize, 9), left.body_rows);
    try std.testing.expectEqual(@as(usize, 10), left.body_cols);
    try std.testing.expectEqual(@as(?usize, 10), left.modeline_row);
    try std.testing.expectEqual(@as(?usize, 11), left.separator_col);

    const right = Editor.windowLayoutFor(.vertical, 10, 20, 2, 1);
    try std.testing.expectEqual(@as(usize, 12), right.left_col);
    try std.testing.expectEqual(@as(usize, 9), right.body_cols);
    try std.testing.expectEqual(@as(?usize, null), right.separator_col);
}

test "editor path completion unique match" {
    const allocator = std.testing.allocator;
    const base_dir = try std.fmt.allocPrint(allocator, ".mg-zig-comp-{d}", .{std.time.microTimestamp()});
    defer allocator.free(base_dir);
    defer std.fs.cwd().deleteTree(base_dir) catch {};
    try std.fs.cwd().makePath(base_dir);

    const file_path = try std.fmt.allocPrint(allocator, "{s}/notes.txt", .{base_dir});
    defer allocator.free(file_path);
    try std.fs.cwd().writeFile(.{ .sub_path = file_path, .data = "x" });

    var ed = try Editor.init(allocator, .{ .rows = 10, .cols = 60 });
    defer ed.deinit();

    try ed.handleKey(.{ .ctrl = 'x' });
    try ed.handleKey(.{ .ctrl = 'f' });
    const prefix = try std.fmt.allocPrint(allocator, "{s}/nts", .{base_dir});
    defer allocator.free(prefix);
    for (prefix) |ch| {
        try ed.handleKey(.{ .char = ch });
    }
    try ed.handleKey(.tab);

    try std.testing.expectEqualStrings(file_path, ed.input_buffer.items);
    try ed.handleKey(.escape);
}

test "editor path completion common prefix" {
    const allocator = std.testing.allocator;
    const base_dir = try std.fmt.allocPrint(allocator, ".mg-zig-comp2-{d}", .{std.time.microTimestamp()});
    defer allocator.free(base_dir);
    defer std.fs.cwd().deleteTree(base_dir) catch {};
    try std.fs.cwd().makePath(base_dir);

    const f1 = try std.fmt.allocPrint(allocator, "{s}/alpha.txt", .{base_dir});
    defer allocator.free(f1);
    const f2 = try std.fmt.allocPrint(allocator, "{s}/alpine.txt", .{base_dir});
    defer allocator.free(f2);
    try std.fs.cwd().writeFile(.{ .sub_path = f1, .data = "x" });
    try std.fs.cwd().writeFile(.{ .sub_path = f2, .data = "x" });

    var ed = try Editor.init(allocator, .{ .rows = 10, .cols = 60 });
    defer ed.deinit();

    try ed.handleKey(.{ .ctrl = 'x' });
    try ed.handleKey(.{ .ctrl = 'f' });
    const prefix = try std.fmt.allocPrint(allocator, "{s}/al", .{base_dir});
    defer allocator.free(prefix);
    for (prefix) |ch| {
        try ed.handleKey(.{ .char = ch });
    }
    try ed.handleKey(.tab);

    const expected = try std.fmt.allocPrint(allocator, "{s}/alp", .{base_dir});
    defer allocator.free(expected);
    try std.testing.expectEqualStrings(expected, ed.input_buffer.items);
    try ed.handleKey(.escape);
}

test "editor minibuffer history recalls previous command" {
    var ed = try Editor.init(std.testing.allocator, .{ .rows = 10, .cols = 60 });
    defer ed.deinit();

    try ed.handleKey(.{ .meta = 'x' });
    for ("undo") |ch| {
        try ed.handleKey(.{ .char = ch });
    }
    try ed.handleKey(.enter);

    try ed.handleKey(.{ .meta = 'x' });
    try ed.handleKey(.arrow_up);
    try std.testing.expectEqualStrings("undo", ed.input_buffer.items);
    try ed.handleKey(.arrow_down);
    try std.testing.expectEqualStrings("", ed.input_buffer.items);
    try ed.handleKey(.escape);
}

test "editor minibuffer history supports M-p/M-n and C-p/C-n" {
    var ed = try Editor.init(std.testing.allocator, .{ .rows = 10, .cols = 60 });
    defer ed.deinit();

    try ed.handleKey(.{ .meta = 'x' });
    for ("undo") |ch| {
        try ed.handleKey(.{ .char = ch });
    }
    try ed.handleKey(.enter);

    try ed.handleKey(.{ .meta = 'x' });
    try ed.handleKey(.{ .meta = 'p' });
    try std.testing.expectEqualStrings("undo", ed.input_buffer.items);
    try ed.handleKey(.{ .meta = 'n' });
    try std.testing.expectEqualStrings("", ed.input_buffer.items);
    try ed.handleKey(.escape);

    try ed.handleKey(.{ .meta = 'x' });
    try ed.handleKey(.{ .ctrl = 'p' });
    try std.testing.expectEqualStrings("undo", ed.input_buffer.items);
    try ed.handleKey(.{ .ctrl = 'n' });
    try std.testing.expectEqualStrings("", ed.input_buffer.items);
    try ed.handleKey(.escape);
}

test "editor shows which-key style hint for prefix" {
    var ed = try Editor.init(std.testing.allocator, .{ .rows = 10, .cols = 60 });
    defer ed.deinit();

    try ed.handleKey(.{ .ctrl = 'x' });
    try std.testing.expect(std.mem.startsWith(u8, ed.prefix_hint.items, "C-x: "));
    try std.testing.expect(std.mem.indexOf(u8, ed.prefix_hint.items, "C-f find-file") != null);
    try std.testing.expect(std.mem.indexOf(u8, ed.prefix_hint.items, "C-b list-buffers") != null);
    try std.testing.expect(std.mem.indexOf(u8, ed.prefix_hint.items, "b switch-buffer") != null);
}

test "editor clears prefix hint once key sequence resolves" {
    var ed = try Editor.init(std.testing.allocator, .{ .rows = 10, .cols = 60 });
    defer ed.deinit();

    try ed.handleKey(.{ .ctrl = 'x' });
    try std.testing.expect(ed.prefix_hint.items.len > 0);
    try ed.handleKey(.{ .ctrl = 's' });
    try std.testing.expectEqual(@as(usize, 0), ed.prefix_hint.items.len);
}

test "editor quits on C-x C-c" {
    var ed = try Editor.init(std.testing.allocator, .{ .rows = 10, .cols = 40 });
    defer ed.deinit();

    try ed.handleKey(.{ .ctrl = 'x' });
    try ed.handleKey(.{ .ctrl = 'c' });
    try std.testing.expect(ed.shouldQuit());
}

test "editor C-x C-c prompts when modified buffers exist" {
    var ed = try Editor.init(std.testing.allocator, .{ .rows = 10, .cols = 40 });
    defer ed.deinit();

    try ed.handleKey(.{ .char = 'x' });
    try std.testing.expect(ed.activeSlot().dirty);

    try ed.handleKey(.{ .ctrl = 'x' });
    try ed.handleKey(.{ .ctrl = 'c' });

    try std.testing.expect(!ed.shouldQuit());
    switch (ed.input_mode) {
        .save_some_buffers => |state| try std.testing.expect(state.quit_after),
        else => try std.testing.expect(false),
    }
}

test "editor C-x C-c n skips save and quits" {
    var ed = try Editor.init(std.testing.allocator, .{ .rows = 10, .cols = 40 });
    defer ed.deinit();

    try ed.handleKey(.{ .char = 'x' });
    try ed.handleKey(.{ .ctrl = 'x' });
    try ed.handleKey(.{ .ctrl = 'c' });
    try ed.handleKey(.{ .char = 'n' });

    try std.testing.expect(ed.shouldQuit());
    try std.testing.expect(ed.activeSlot().dirty);
}

test "editor C-x C-c y saves modified file and quits" {
    const allocator = std.testing.allocator;
    const filename = try std.fmt.allocPrint(allocator, ".mg-zig-quit-save-{d}.txt", .{std.time.microTimestamp()});
    defer allocator.free(filename);
    defer std.fs.cwd().deleteFile(filename) catch {};
    try std.fs.cwd().writeFile(.{ .sub_path = filename, .data = "abc\n" });

    var ed = try Editor.init(allocator, .{ .rows = 10, .cols = 40 });
    defer ed.deinit();
    try ed.openFile(filename);
    try ed.handleKey(.{ .char = 'Z' });
    try std.testing.expect(ed.activeSlot().dirty);

    try ed.handleKey(.{ .ctrl = 'x' });
    try ed.handleKey(.{ .ctrl = 'c' });
    try ed.handleKey(.{ .char = 'y' });
    try std.testing.expect(ed.shouldQuit());
    try std.testing.expect(!ed.activeSlot().dirty);

    const data = try std.fs.cwd().readFileAlloc(allocator, filename, 1024);
    defer allocator.free(data);
    try std.testing.expectEqualStrings("Zabc\n", data);
}

test "editor C-x C-c q cancels quit" {
    var ed = try Editor.init(std.testing.allocator, .{ .rows = 10, .cols = 40 });
    defer ed.deinit();

    try ed.handleKey(.{ .char = 'x' });
    try ed.handleKey(.{ .ctrl = 'x' });
    try ed.handleKey(.{ .ctrl = 'c' });
    try ed.handleKey(.{ .char = 'q' });

    try std.testing.expect(!ed.shouldQuit());
    switch (ed.input_mode) {
        .normal => {},
        else => try std.testing.expect(false),
    }
}

test "editor C-x s saves modified buffer with y" {
    const allocator = std.testing.allocator;
    const filename = try std.fmt.allocPrint(allocator, ".mg-zig-save-some-{d}.txt", .{std.time.microTimestamp()});
    defer allocator.free(filename);
    defer std.fs.cwd().deleteFile(filename) catch {};
    try std.fs.cwd().writeFile(.{ .sub_path = filename, .data = "abc\n" });

    var ed = try Editor.init(allocator, .{ .rows = 10, .cols = 40 });
    defer ed.deinit();
    try ed.openFile(filename);
    try ed.handleKey(.{ .char = 'Z' });
    try std.testing.expect(ed.activeSlot().dirty);

    try ed.handleKey(.{ .ctrl = 'x' });
    try ed.handleKey(.{ .char = 's' });
    switch (ed.input_mode) {
        .save_some_buffers => {},
        else => try std.testing.expect(false),
    }
    try ed.handleKey(.{ .char = 'y' });

    try std.testing.expect(!ed.activeSlot().dirty);
    switch (ed.input_mode) {
        .normal => {},
        else => try std.testing.expect(false),
    }

    const data = try std.fs.cwd().readFileAlloc(allocator, filename, 1024);
    defer allocator.free(data);
    try std.testing.expectEqualStrings("Zabc\n", data);
}

test "editor C-x s bang saves all modified buffers" {
    const allocator = std.testing.allocator;
    const f1 = try std.fmt.allocPrint(allocator, ".mg-zig-save-some-all-a-{d}.txt", .{std.time.microTimestamp()});
    defer allocator.free(f1);
    defer std.fs.cwd().deleteFile(f1) catch {};
    const f2 = try std.fmt.allocPrint(allocator, ".mg-zig-save-some-all-b-{d}.txt", .{std.time.microTimestamp()});
    defer allocator.free(f2);
    defer std.fs.cwd().deleteFile(f2) catch {};
    try std.fs.cwd().writeFile(.{ .sub_path = f1, .data = "a\n" });
    try std.fs.cwd().writeFile(.{ .sub_path = f2, .data = "b\n" });

    var ed = try Editor.init(allocator, .{ .rows = 10, .cols = 40 });
    defer ed.deinit();
    try ed.openFile(f1);
    try ed.handleKey(.{ .char = '1' });
    try ed.openFile(f2);
    try ed.handleKey(.{ .char = '2' });

    try ed.handleKey(.{ .ctrl = 'x' });
    try ed.handleKey(.{ .char = 's' });
    try ed.handleKey(.{ .char = '!' });

    for (ed.buffers.items) |slot| {
        if (slot.file_path != null) {
            try std.testing.expect(!slot.dirty);
        }
    }
    switch (ed.input_mode) {
        .normal => {},
        else => try std.testing.expect(false),
    }

    const d1 = try std.fs.cwd().readFileAlloc(allocator, f1, 1024);
    defer allocator.free(d1);
    const d2 = try std.fs.cwd().readFileAlloc(allocator, f2, 1024);
    defer allocator.free(d2);
    try std.testing.expectEqualStrings("1a\n", d1);
    try std.testing.expectEqualStrings("2b\n", d2);
}

test "editor C-x s y on buffer without file path prompts write path" {
    const allocator = std.testing.allocator;
    const out_path = try std.fmt.allocPrint(allocator, ".mg-zig-save-some-nopath-{d}.txt", .{std.time.microTimestamp()});
    defer allocator.free(out_path);
    defer std.fs.cwd().deleteFile(out_path) catch {};

    var ed = try Editor.init(allocator, .{ .rows = 10, .cols = 40 });
    defer ed.deinit();
    try ed.handleKey(.{ .char = 'A' });
    try std.testing.expect(ed.activeSlot().dirty);
    try std.testing.expect(ed.activeSlot().file_path == null);

    try ed.handleKey(.{ .ctrl = 'x' });
    try ed.handleKey(.{ .char = 's' });
    try ed.handleKey(.{ .char = 'y' });

    switch (ed.input_mode) {
        .save_some_buffers => |state| try std.testing.expectEqual(SaveSomeBuffersState.Phase.write_path, state.phase),
        else => try std.testing.expect(false),
    }

    for (out_path) |ch| {
        try ed.handleKey(.{ .char = ch });
    }
    try ed.handleKey(.enter);

    switch (ed.input_mode) {
        .normal => {},
        else => try std.testing.expect(false),
    }
    try std.testing.expect(!ed.activeSlot().dirty);
    try std.testing.expect(ed.activeSlot().file_path != null);

    const data = try std.fs.cwd().readFileAlloc(allocator, out_path, 1024);
    defer allocator.free(data);
    try std.testing.expectEqualStrings("A", data);
}

test "editor C-x C-c on buffer without file path writes then quits" {
    const allocator = std.testing.allocator;
    const out_path = try std.fmt.allocPrint(allocator, ".mg-zig-quit-nopath-{d}.txt", .{std.time.microTimestamp()});
    defer allocator.free(out_path);
    defer std.fs.cwd().deleteFile(out_path) catch {};

    var ed = try Editor.init(allocator, .{ .rows = 10, .cols = 40 });
    defer ed.deinit();
    try ed.handleKey(.{ .char = 'Q' });
    try std.testing.expect(ed.activeSlot().dirty);
    try std.testing.expect(ed.activeSlot().file_path == null);

    try ed.handleKey(.{ .ctrl = 'x' });
    try ed.handleKey(.{ .ctrl = 'c' });
    try ed.handleKey(.{ .char = 'y' });
    for (out_path) |ch| {
        try ed.handleKey(.{ .char = ch });
    }
    try ed.handleKey(.enter);

    try std.testing.expect(ed.shouldQuit());
    try std.testing.expect(!ed.activeSlot().dirty);
    const data = try std.fs.cwd().readFileAlloc(allocator, out_path, 1024);
    defer allocator.free(data);
    try std.testing.expectEqualStrings("Q", data);
}

test "editor ctrl-g cancels prefix and deactivates mark" {
    var ed = try Editor.init(std.testing.allocator, .{ .rows = 10, .cols = 40 });
    defer ed.deinit();

    try ed.buffers.items[0].buffer.setText("abc");
    ed.windows.items[0].cursor.byte_index = 1;
    try ed.handleKey(.{ .ctrl = '@' });
    try std.testing.expect(ed.windows.items[0].mark != null);

    try ed.handleKey(.{ .ctrl = 'x' });
    try std.testing.expect(ed.prefix_hint.items.len > 0);
    try ed.handleKey(.{ .ctrl = 'g' });

    try std.testing.expectEqual(@as(usize, 0), ed.prefix_hint.items.len);
    try std.testing.expectEqual(@as(usize, 0), ed.pending_keys.items.len);
    try std.testing.expect(ed.windows.items[0].mark == null);
}

test "editor meta-angle moves to buffer boundaries" {
    var ed = try Editor.init(std.testing.allocator, .{ .rows = 10, .cols = 40 });
    defer ed.deinit();

    try ed.buffers.items[0].buffer.setText("one\ntwo\nthree");
    ed.windows.items[0].cursor.byte_index = 4;

    try ed.handleKey(.{ .meta = '>' });
    try std.testing.expectEqual(ed.buffers.items[0].buffer.len(), ed.windows.items[0].cursor.byte_index);

    try ed.handleKey(.{ .meta = '<' });
    try std.testing.expectEqual(@as(usize, 0), ed.windows.items[0].cursor.byte_index);
}

test "editor ctrl-v and meta-v page movement" {
    var ed = try Editor.init(std.testing.allocator, .{ .rows = 5, .cols = 40 });
    defer ed.deinit();

    try ed.buffers.items[0].buffer.setText("0\n1\n2\n3\n4\n5\n6\n7\n8\n9\n");
    ed.windows.items[0].cursor.byte_index = 0;

    try ed.handleKey(.{ .ctrl = 'v' });
    var pos = ed.buffers.items[0].buffer.byteToLineCol(ed.windows.items[0].cursor.byte_index);
    try std.testing.expectEqual(@as(usize, 4), pos.line);

    try ed.handleKey(.{ .meta = 'v' });
    pos = ed.buffers.items[0].buffer.byteToLineCol(ed.windows.items[0].cursor.byte_index);
    try std.testing.expectEqual(@as(usize, 0), pos.line);
}

test "editor quoted-insert inserts next control key literally" {
    var ed = try Editor.init(std.testing.allocator, .{ .rows = 10, .cols = 40 });
    defer ed.deinit();

    try ed.handleKey(.{ .ctrl = 'q' });
    try ed.handleKey(.{ .ctrl = 'g' });

    const bytes = ed.buffers.items[0].buffer.bytes();
    try std.testing.expectEqual(@as(usize, 1), bytes.len);
    try std.testing.expectEqual(@as(u8, 0x07), bytes[0]);
}

test "editor M-g g goto-line prompt moves point" {
    var ed = try Editor.init(std.testing.allocator, .{ .rows = 10, .cols = 40 });
    defer ed.deinit();

    try ed.buffers.items[0].buffer.setText("a\nb\nc\nd\ne\n");
    try ed.handleKey(.{ .meta = 'g' });
    try ed.handleKey(.{ .char = 'g' });
    try ed.handleKey(.{ .ctrl = 'u' });
    try ed.handleKey(.{ .char = '4' });
    try ed.handleKey(.enter);

    const pos = ed.buffers.items[0].buffer.byteToLineCol(ed.windows.items[0].cursor.byte_index);
    try std.testing.expectEqual(@as(usize, 3), pos.line);
}

test "editor M-x replace-string replaces all matches from point" {
    var ed = try Editor.init(std.testing.allocator, .{ .rows = 10, .cols = 80 });
    defer ed.deinit();

    try ed.buffers.items[0].buffer.setText("one two one");
    ed.windows.items[0].cursor.byte_index = 0;

    try ed.handleKey(.{ .meta = 'x' });
    for ("replace-string") |ch| {
        try ed.handleKey(.{ .char = ch });
    }
    try ed.handleKey(.enter);
    try ed.handleKey(.{ .ctrl = 'u' });
    for ("one") |ch| {
        try ed.handleKey(.{ .char = ch });
    }
    try ed.handleKey(.enter);
    try ed.handleKey(.{ .char = '1' });
    try ed.handleKey(.enter);

    try std.testing.expectEqualStrings("1 two 1", ed.buffers.items[0].buffer.bytes());
}

test "editor query-replace supports y n and ! choices" {
    var ed = try Editor.init(std.testing.allocator, .{ .rows = 10, .cols = 80 });
    defer ed.deinit();

    try ed.buffers.items[0].buffer.setText("cat cat cat");
    ed.windows.items[0].cursor.byte_index = 0;

    try ed.handleKey(.{ .meta = '%' });
    try ed.handleKey(.enter);
    for ("dog") |ch| {
        try ed.handleKey(.{ .char = ch });
    }
    try ed.handleKey(.enter);

    try ed.handleKey(.{ .char = 'y' });
    try ed.handleKey(.{ .char = 'n' });
    try ed.handleKey(.{ .char = '!' });

    try std.testing.expectEqualStrings("dog cat dog", ed.buffers.items[0].buffer.bytes());
    switch (ed.input_mode) {
        .normal => {},
        else => return error.TestUnexpectedResult,
    }
}

test "editor query-replace q exits without applying current match" {
    var ed = try Editor.init(std.testing.allocator, .{ .rows = 10, .cols = 80 });
    defer ed.deinit();

    try ed.buffers.items[0].buffer.setText("aa aa");
    ed.windows.items[0].cursor.byte_index = 0;

    try ed.handleKey(.{ .meta = '%' });
    try ed.handleKey(.{ .ctrl = 'u' });
    for ("aa") |ch| {
        try ed.handleKey(.{ .char = ch });
    }
    try ed.handleKey(.enter);
    for ("bb") |ch| {
        try ed.handleKey(.{ .char = ch });
    }
    try ed.handleKey(.enter);
    try ed.handleKey(.{ .char = 'q' });

    try std.testing.expectEqualStrings("aa aa", ed.buffers.items[0].buffer.bytes());
    switch (ed.input_mode) {
        .normal => {},
        else => return error.TestUnexpectedResult,
    }
}

test "editor M-x replace-regexp replaces regex matches" {
    var ed = try Editor.init(std.testing.allocator, .{ .rows = 10, .cols = 80 });
    defer ed.deinit();

    try ed.buffers.items[0].buffer.setText("cat cot cut");
    ed.windows.items[0].cursor.byte_index = 0;

    try ed.handleKey(.{ .meta = 'x' });
    for ("replace-regexp") |ch| {
        try ed.handleKey(.{ .char = ch });
    }
    try ed.handleKey(.enter);
    for ("c.t") |ch| {
        try ed.handleKey(.{ .char = ch });
    }
    try ed.handleKey(.enter);
    for ("dog") |ch| {
        try ed.handleKey(.{ .char = ch });
    }
    try ed.handleKey(.enter);

    try std.testing.expectEqualStrings("dog dog dog", ed.buffers.items[0].buffer.bytes());
}

test "editor replace-regexp supports backreferences in replacement" {
    var ed = try Editor.init(std.testing.allocator, .{ .rows = 10, .cols = 80 });
    defer ed.deinit();

    try ed.buffers.items[0].buffer.setText("aaabbb");
    ed.windows.items[0].cursor.byte_index = 0;

    try ed.handleKey(.{ .meta = 'x' });
    for ("replace-regexp") |ch| {
        try ed.handleKey(.{ .char = ch });
    }
    try ed.handleKey(.enter);
    for ("(a+)(b+)") |ch| {
        try ed.handleKey(.{ .char = ch });
    }
    try ed.handleKey(.enter);
    for ("\\2-\\1") |ch| {
        try ed.handleKey(.{ .char = ch });
    }
    try ed.handleKey(.enter);

    try std.testing.expectEqualStrings("bbb-aaa", ed.buffers.items[0].buffer.bytes());
}

test "editor query-replace-regexp supports y n and !" {
    var ed = try Editor.init(std.testing.allocator, .{ .rows = 10, .cols = 80 });
    defer ed.deinit();

    try ed.buffers.items[0].buffer.setText("cat cot cut");
    ed.windows.items[0].cursor.byte_index = 0;

    try ed.handleKey(.{ .meta = 'x' });
    for ("query-replace-regexp") |ch| {
        try ed.handleKey(.{ .char = ch });
    }
    try ed.handleKey(.enter);
    for ("c.t") |ch| {
        try ed.handleKey(.{ .char = ch });
    }
    try ed.handleKey(.enter);
    for ("dog") |ch| {
        try ed.handleKey(.{ .char = ch });
    }
    try ed.handleKey(.enter);

    try ed.handleKey(.{ .char = 'y' });
    try ed.handleKey(.{ .char = 'n' });
    try ed.handleKey(.{ .char = '!' });

    try std.testing.expectEqualStrings("dog cot dog", ed.buffers.items[0].buffer.bytes());
    switch (ed.input_mode) {
        .normal => {},
        else => return error.TestUnexpectedResult,
    }
}

test "editor C-M-% opens query-replace-regexp prompt" {
    var ed = try Editor.init(std.testing.allocator, .{ .rows = 10, .cols = 80 });
    defer ed.deinit();

    try ed.handleKey(.{ .meta_ctrl = '5' });
    switch (ed.input_mode) {
        .minibuffer => |state| try std.testing.expectEqual(MinibufferKind.query_replace_regexp_from, state.kind),
        else => return error.TestUnexpectedResult,
    }
}
