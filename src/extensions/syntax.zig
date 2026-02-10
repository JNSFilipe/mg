const builtin = @import("builtin");
const std = @import("std");

const c = @cImport({
    @cInclude("tree_sitter/api.h");
    @cInclude("dlfcn.h");
});

const Allocator = std.mem.Allocator;

pub const Style = enum(u8) {
    normal,
    comment,
    string,
    keyword,
    type_name,
    function_name,
    number,
    constant,
    operator,
    property,
};

const LanguageId = enum {
    zig,
    c,
    cpp,
    typescript,
    tsx,
    rust,
    go,
    python,
    javascript,
    json,
    bash,
    toml,
    yaml,
    html,
    css,
};

const GrammarSpec = struct {
    id: LanguageId,
    short_name: []const u8,
    repo_name: []const u8,
    repo_url: []const u8,
    grammar_subdir: ?[]const u8 = null,
    query_from_repo_root: bool = false,
    symbol: [:0]const u8,
    extensions: []const []const u8,
};

const grammar_specs = [_]GrammarSpec{
    .{ .id = .zig, .short_name = "zig", .repo_name = "tree-sitter-zig", .repo_url = "https://github.com/maxxnino/tree-sitter-zig", .symbol = "tree_sitter_zig", .extensions = &.{"zig"} },
    .{ .id = .c, .short_name = "c", .repo_name = "tree-sitter-c", .repo_url = "https://github.com/tree-sitter/tree-sitter-c", .symbol = "tree_sitter_c", .extensions = &.{ "c", "h" } },
    .{ .id = .cpp, .short_name = "cpp", .repo_name = "tree-sitter-cpp", .repo_url = "https://github.com/tree-sitter/tree-sitter-cpp", .symbol = "tree_sitter_cpp", .extensions = &.{ "cc", "cpp", "cxx", "hpp", "hh", "hxx" } },
    .{
        .id = .typescript,
        .short_name = "typescript",
        .repo_name = "tree-sitter-typescript",
        .repo_url = "https://github.com/tree-sitter/tree-sitter-typescript",
        .grammar_subdir = "typescript",
        .query_from_repo_root = true,
        .symbol = "tree_sitter_typescript",
        .extensions = &.{ "ts", "mts", "cts" },
    },
    .{
        .id = .tsx,
        .short_name = "tsx",
        .repo_name = "tree-sitter-typescript",
        .repo_url = "https://github.com/tree-sitter/tree-sitter-typescript",
        .grammar_subdir = "tsx",
        .query_from_repo_root = true,
        .symbol = "tree_sitter_tsx",
        .extensions = &.{"tsx"},
    },
    .{ .id = .rust, .short_name = "rust", .repo_name = "tree-sitter-rust", .repo_url = "https://github.com/tree-sitter/tree-sitter-rust", .symbol = "tree_sitter_rust", .extensions = &.{"rs"} },
    .{ .id = .go, .short_name = "go", .repo_name = "tree-sitter-go", .repo_url = "https://github.com/tree-sitter/tree-sitter-go", .symbol = "tree_sitter_go", .extensions = &.{"go"} },
    .{ .id = .python, .short_name = "python", .repo_name = "tree-sitter-python", .repo_url = "https://github.com/tree-sitter/tree-sitter-python", .symbol = "tree_sitter_python", .extensions = &.{"py"} },
    .{ .id = .javascript, .short_name = "javascript", .repo_name = "tree-sitter-javascript", .repo_url = "https://github.com/tree-sitter/tree-sitter-javascript", .symbol = "tree_sitter_javascript", .extensions = &.{ "js", "mjs", "cjs", "jsx" } },
    .{ .id = .json, .short_name = "json", .repo_name = "tree-sitter-json", .repo_url = "https://github.com/tree-sitter/tree-sitter-json", .symbol = "tree_sitter_json", .extensions = &.{"json"} },
    .{ .id = .bash, .short_name = "bash", .repo_name = "tree-sitter-bash", .repo_url = "https://github.com/tree-sitter/tree-sitter-bash", .symbol = "tree_sitter_bash", .extensions = &.{ "sh", "bash" } },
    .{ .id = .toml, .short_name = "toml", .repo_name = "tree-sitter-toml", .repo_url = "https://github.com/tree-sitter-grammars/tree-sitter-toml", .symbol = "tree_sitter_toml", .extensions = &.{"toml"} },
    .{ .id = .yaml, .short_name = "yaml", .repo_name = "tree-sitter-yaml", .repo_url = "https://github.com/tree-sitter-grammars/tree-sitter-yaml", .symbol = "tree_sitter_yaml", .extensions = &.{ "yaml", "yml" } },
    .{ .id = .html, .short_name = "html", .repo_name = "tree-sitter-html", .repo_url = "https://github.com/tree-sitter/tree-sitter-html", .symbol = "tree_sitter_html", .extensions = &.{ "html", "htm" } },
    .{ .id = .css, .short_name = "css", .repo_name = "tree-sitter-css", .repo_url = "https://github.com/tree-sitter/tree-sitter-css", .symbol = "tree_sitter_css", .extensions = &.{"css"} },
};

const LoadedGrammar = struct {
    language: *const c.TSLanguage,
    query: ?*c.TSQuery,
    lib_handle: *anyopaque,
};

pub const SyntaxEngine = struct {
    allocator: Allocator,
    parser: ?*c.TSParser,
    grammars: std.AutoHashMap(LanguageId, LoadedGrammar),
    failed_grammars: std.AutoHashMap(LanguageId, void),
    cache_root: ?[]u8,
    enabled: bool,

    pub fn init(allocator: Allocator) SyntaxEngine {
        return .{
            .allocator = allocator,
            .parser = null,
            .grammars = std.AutoHashMap(LanguageId, LoadedGrammar).init(allocator),
            .failed_grammars = std.AutoHashMap(LanguageId, void).init(allocator),
            .cache_root = null,
            .enabled = !builtin.is_test,
        };
    }

    pub fn deinit(self: *SyntaxEngine) void {
        var it = self.grammars.iterator();
        while (it.next()) |entry| {
            const loaded = entry.value_ptr.*;
            if (loaded.query) |q| c.ts_query_delete(q);
            _ = c.dlclose(loaded.lib_handle);
        }
        self.grammars.deinit();
        self.failed_grammars.deinit();

        if (self.cache_root) |root| {
            self.allocator.free(root);
        }
        if (self.parser) |p| {
            c.ts_parser_delete(p);
        }
    }

    pub fn buildStyleMap(
        self: *SyntaxEngine,
        file_path: ?[]const u8,
        source: []const u8,
    ) !?[]Style {
        if (!self.enabled) return null;
        if (file_path == null or source.len == 0) return null;
        if (source.len > std.math.maxInt(u32)) return null;

        const language = detectLanguage(file_path.?) orelse return null;
        const loaded = self.ensureLoadedGrammar(language) catch return null;
        const query = loaded.query orelse return null;

        if (self.parser == null) {
            self.parser = c.ts_parser_new() orelse return error.OutOfMemory;
        }
        if (!c.ts_parser_set_language(self.parser.?, loaded.language)) {
            return null;
        }

        const tree = c.ts_parser_parse_string(
            self.parser.?,
            null,
            @ptrCast(source.ptr),
            @intCast(source.len),
        ) orelse return null;
        defer c.ts_tree_delete(tree);

        const root = c.ts_tree_root_node(tree);
        const cursor = c.ts_query_cursor_new() orelse return error.OutOfMemory;
        defer c.ts_query_cursor_delete(cursor);
        c.ts_query_cursor_exec(cursor, query, root);

        const styles = try self.allocator.alloc(Style, source.len);
        errdefer self.allocator.free(styles);
        @memset(styles, .normal);

        var saw_non_default = false;
        var match: c.TSQueryMatch = undefined;
        var capture_index: u32 = 0;
        while (c.ts_query_cursor_next_capture(cursor, &match, &capture_index)) {
            const cap = match.captures[capture_index];

            var name_len: u32 = 0;
            const cap_name_ptr = c.ts_query_capture_name_for_id(query, cap.index, &name_len);
            const cap_name = @as([*]const u8, @ptrCast(cap_name_ptr))[0..name_len];
            const style = styleForCapture(cap_name);
            if (style == .normal) continue;

            const start = @as(usize, @intCast(c.ts_node_start_byte(cap.node)));
            const end = @as(usize, @intCast(c.ts_node_end_byte(cap.node)));
            if (start >= source.len) continue;
            if (start >= end) continue;
            const clamped_end = @min(end, source.len);

            var idx = start;
            while (idx < clamped_end) : (idx += 1) {
                if (stylePriority(style) >= stylePriority(styles[idx])) {
                    styles[idx] = style;
                    saw_non_default = true;
                }
            }
        }

        if (!saw_non_default) {
            self.allocator.free(styles);
            return null;
        }
        return styles;
    }

    fn ensureLoadedGrammar(self: *SyntaxEngine, id: LanguageId) !*LoadedGrammar {
        if (self.grammars.getPtr(id)) |loaded| return loaded;
        if (self.failed_grammars.contains(id)) return error.GrammarUnavailable;

        const spec = grammarSpec(id);
        const loaded = self.installAndLoad(spec) catch |err| {
            self.failed_grammars.put(id, {}) catch {};
            std.log.warn("tree-sitter disabled for {s}: {s}", .{ spec.short_name, @errorName(err) });
            return err;
        };

        try self.grammars.put(id, loaded);
        return self.grammars.getPtr(id).?;
    }

    fn installAndLoad(self: *SyntaxEngine, spec: GrammarSpec) !LoadedGrammar {
        const cache_root = try self.ensureCacheRoot();
        const repo_dir = try std.fs.path.join(self.allocator, &.{ cache_root, spec.repo_name });
        defer self.allocator.free(repo_dir);

        if (!try dirExists(repo_dir)) {
            try self.runCommand(
                null,
                &.{ "git", "clone", "--depth", "1", spec.repo_url, repo_dir },
            );
        }

        const grammar_root = try grammarRootPath(self.allocator, spec, repo_dir);
        defer self.allocator.free(grammar_root);

        const lib_path = try self.grammarLibraryPath(spec, repo_dir);
        defer self.allocator.free(lib_path);

        if (!try fileExists(lib_path)) {
            try self.buildGrammarLibrary(spec, grammar_root, lib_path);
        }

        const lib_handle = try openDynLib(self.allocator, lib_path);
        errdefer _ = c.dlclose(lib_handle);

        const lang = try loadLanguageSymbol(lib_handle, spec.symbol);
        const query_root = if (spec.query_from_repo_root) repo_dir else grammar_root;
        const query = try self.loadHighlightQuery(query_root, lang);

        return .{
            .language = lang,
            .query = query,
            .lib_handle = lib_handle,
        };
    }

    fn grammarLibraryPath(self: *SyntaxEngine, spec: GrammarSpec, repo_dir: []const u8) ![]u8 {
        const ext = sharedLibraryExtension();
        const filename = try std.fmt.allocPrint(self.allocator, "libmgts-{s}{s}", .{ spec.short_name, ext });
        defer self.allocator.free(filename);
        return try std.fs.path.join(self.allocator, &.{ repo_dir, filename });
    }

    fn buildGrammarLibrary(self: *SyntaxEngine, spec: GrammarSpec, grammar_root: []const u8, lib_path: []const u8) !void {
        const src_dir = try std.fs.path.join(self.allocator, &.{ grammar_root, "src" });
        defer self.allocator.free(src_dir);
        const parser_c = try std.fs.path.join(self.allocator, &.{ src_dir, "parser.c" });
        defer self.allocator.free(parser_c);
        if (!try fileExists(parser_c)) return error.ParserSourceMissing;

        const build_dir = try std.fs.path.join(self.allocator, &.{ grammar_root, ".mg-zig-build" });
        defer self.allocator.free(build_dir);
        try std.fs.cwd().makePath(build_dir);

        const parser_obj = try std.fs.path.join(self.allocator, &.{ build_dir, "parser.o" });
        defer self.allocator.free(parser_obj);
        const scanner_c = try std.fs.path.join(self.allocator, &.{ src_dir, "scanner.c" });
        defer self.allocator.free(scanner_c);
        const scanner_cc = try std.fs.path.join(self.allocator, &.{ src_dir, "scanner.cc" });
        defer self.allocator.free(scanner_cc);
        const scanner_cpp = try std.fs.path.join(self.allocator, &.{ src_dir, "scanner.cpp" });
        defer self.allocator.free(scanner_cpp);
        const scanner_c_obj = try std.fs.path.join(self.allocator, &.{ build_dir, "scanner-c.o" });
        defer self.allocator.free(scanner_c_obj);
        const scanner_cpp_obj = try std.fs.path.join(self.allocator, &.{ build_dir, "scanner-cpp.o" });
        defer self.allocator.free(scanner_cpp_obj);

        try self.runCommand(
            grammar_root,
            &.{ "cc", "-O2", "-fPIC", "-I", src_dir, "-c", parser_c, "-o", parser_obj },
        );

        var object_files = std.array_list.Managed([]const u8).init(self.allocator);
        defer object_files.deinit();
        try object_files.append(parser_obj);

        var needs_cpp_linker = false;
        if (try fileExists(scanner_c)) {
            try self.runCommand(
                grammar_root,
                &.{ "cc", "-O2", "-fPIC", "-I", src_dir, "-c", scanner_c, "-o", scanner_c_obj },
            );
            try object_files.append(scanner_c_obj);
        }

        const cpp_scanner = if (try fileExists(scanner_cc))
            scanner_cc
        else if (try fileExists(scanner_cpp))
            scanner_cpp
        else
            null;

        if (cpp_scanner) |scanner_path| {
            needs_cpp_linker = true;
            try self.runCommand(
                grammar_root,
                &.{ "c++", "-std=c++17", "-O2", "-fPIC", "-I", src_dir, "-c", scanner_path, "-o", scanner_cpp_obj },
            );
            try object_files.append(scanner_cpp_obj);
        }

        var link_args = std.array_list.Managed([]const u8).init(self.allocator);
        defer link_args.deinit();

        try link_args.append(if (needs_cpp_linker) "c++" else "cc");
        if (builtin.os.tag == .macos) {
            try link_args.append("-dynamiclib");
            try link_args.append("-Wl,-undefined,dynamic_lookup");
        } else {
            try link_args.append("-shared");
        }
        try link_args.append("-o");
        try link_args.append(lib_path);
        for (object_files.items) |obj| try link_args.append(obj);

        try self.runCommand(grammar_root, link_args.items);
        _ = spec;
    }

    fn loadHighlightQuery(self: *SyntaxEngine, grammar_root: []const u8, language: *const c.TSLanguage) !?*c.TSQuery {
        const query_path = try std.fs.path.join(self.allocator, &.{ grammar_root, "queries", "highlights.scm" });
        defer self.allocator.free(query_path);
        if (!try fileExists(query_path)) return null;

        const raw = try readWholeFile(self.allocator, query_path, 512 * 1024);
        defer self.allocator.free(raw);
        if (raw.len == 0 or raw.len > std.math.maxInt(u32)) return null;

        var err_offset: u32 = 0;
        var err_type: c.TSQueryError = c.TSQueryErrorNone;
        const query = c.ts_query_new(
            language,
            @ptrCast(raw.ptr),
            @intCast(raw.len),
            &err_offset,
            &err_type,
        );
        if (query == null) {
            std.log.warn("tree-sitter query compile failed at byte {d} ({d})", .{ err_offset, @as(u32, @intCast(err_type)) });
            return null;
        }
        return query;
    }

    fn ensureCacheRoot(self: *SyntaxEngine) ![]const u8 {
        if (self.cache_root) |root| return root;

        const from_env = std.process.getEnvVarOwned(self.allocator, "MG_ZIG_TS_CACHE") catch |err| switch (err) {
            error.EnvironmentVariableNotFound => null,
            else => return err,
        };

        const root = if (from_env) |env_path|
            env_path
        else blk: {
            const home = try std.process.getEnvVarOwned(self.allocator, "HOME");
            defer self.allocator.free(home);
            break :blk try std.fs.path.join(self.allocator, &.{ home, ".mg-zig", "tree-sitter" });
        };

        try std.fs.cwd().makePath(root);
        self.cache_root = root;
        return root;
    }

    fn runCommand(self: *SyntaxEngine, cwd: ?[]const u8, argv: []const []const u8) !void {
        const result = try std.process.Child.run(.{
            .allocator = self.allocator,
            .argv = argv,
            .cwd = cwd,
            .max_output_bytes = 2 * 1024 * 1024,
        });
        defer self.allocator.free(result.stdout);
        defer self.allocator.free(result.stderr);

        switch (result.term) {
            .Exited => |code| {
                if (code == 0) return;
            },
            else => {},
        }

        if (result.stderr.len > 0) {
            std.log.warn("command failed: {s}", .{result.stderr});
        } else {
            std.log.warn("command failed: {s}", .{argv[0]});
        }
        return error.CommandFailed;
    }
};

pub fn ansiForStyle(style: Style) []const u8 {
    return switch (style) {
        .normal => "\x1b[39m",
        .comment => "\x1b[90m",
        .string => "\x1b[32m",
        .keyword => "\x1b[34m",
        .type_name => "\x1b[36m",
        .function_name => "\x1b[33m",
        .number => "\x1b[35m",
        .constant => "\x1b[31m",
        .operator => "\x1b[37m",
        .property => "\x1b[96m",
    };
}

fn stylePriority(style: Style) u8 {
    return switch (style) {
        .normal => 0,
        .operator => 1,
        .property => 2,
        .number => 3,
        .constant => 4,
        .function_name => 5,
        .type_name => 6,
        .keyword => 7,
        .string => 8,
        .comment => 9,
    };
}

fn styleForCapture(name: []const u8) Style {
    if (captureHas(name, "comment")) return .comment;
    if (captureHas(name, "string") or captureHas(name, "character")) return .string;
    if (captureHas(name, "keyword") or captureHas(name, "conditional") or captureHas(name, "repeat") or captureHas(name, "exception")) return .keyword;
    if (captureHas(name, "type") or captureHas(name, "namespace") or captureHas(name, "module")) return .type_name;
    if (captureHas(name, "function") or captureHas(name, "method") or captureHas(name, "constructor")) return .function_name;
    if (captureHas(name, "number") or captureHas(name, "float")) return .number;
    if (captureHas(name, "constant") or captureHas(name, "macro") or captureHas(name, "boolean")) return .constant;
    if (captureHas(name, "operator") or captureHas(name, "punctuation")) return .operator;
    if (captureHas(name, "property") or captureHas(name, "field") or captureHas(name, "attribute")) return .property;
    return .normal;
}

fn captureHas(name: []const u8, needle: []const u8) bool {
    return std.mem.indexOf(u8, name, needle) != null;
}

fn detectLanguage(path: []const u8) ?LanguageId {
    const ext = std.fs.path.extension(path);
    if (ext.len > 1) {
        const ext_name = ext[1..];
        for (grammar_specs) |spec| {
            for (spec.extensions) |candidate| {
                if (std.ascii.eqlIgnoreCase(ext_name, candidate)) return spec.id;
            }
        }
    }

    const base = std.fs.path.basename(path);
    if (std.ascii.eqlIgnoreCase(base, "Makefile")) return .bash;
    return null;
}

fn grammarSpec(id: LanguageId) GrammarSpec {
    for (grammar_specs) |spec| {
        if (spec.id == id) return spec;
    }
    unreachable;
}

fn grammarRootPath(allocator: Allocator, spec: GrammarSpec, repo_dir: []const u8) ![]u8 {
    if (spec.grammar_subdir) |subdir| {
        return std.fs.path.join(allocator, &.{ repo_dir, subdir });
    }
    return allocator.dupe(u8, repo_dir);
}

fn fileExists(path: []const u8) !bool {
    var file = openFileAny(path) catch |err| switch (err) {
        error.FileNotFound => return false,
        else => return err,
    };
    defer file.close();
    return true;
}

fn dirExists(path: []const u8) !bool {
    var dir = openDirAny(path) catch |err| switch (err) {
        error.FileNotFound => return false,
        else => return err,
    };
    defer dir.close();
    return true;
}

fn openFileAny(path: []const u8) !std.fs.File {
    if (std.fs.path.isAbsolute(path)) {
        return std.fs.openFileAbsolute(path, .{});
    }
    return std.fs.cwd().openFile(path, .{});
}

fn openDirAny(path: []const u8) !std.fs.Dir {
    if (std.fs.path.isAbsolute(path)) {
        return std.fs.openDirAbsolute(path, .{});
    }
    return std.fs.cwd().openDir(path, .{});
}

fn readWholeFile(allocator: Allocator, path: []const u8, max_bytes: usize) ![]u8 {
    var file = try openFileAny(path);
    defer file.close();
    return file.readToEndAlloc(allocator, max_bytes);
}

fn sharedLibraryExtension() []const u8 {
    return switch (builtin.os.tag) {
        .macos => ".dylib",
        .windows => ".dll",
        else => ".so",
    };
}

fn openDynLib(allocator: Allocator, path: []const u8) !*anyopaque {
    const zpath = try std.mem.concatWithSentinel(allocator, u8, &.{path}, 0);
    defer allocator.free(zpath);

    const handle = c.dlopen(zpath.ptr, c.RTLD_NOW | c.RTLD_LOCAL) orelse return error.DlopenFailed;
    return handle;
}

fn loadLanguageSymbol(handle: *anyopaque, symbol_name: [:0]const u8) !*const c.TSLanguage {
    const raw = c.dlsym(handle, symbol_name.ptr) orelse return error.MissingLanguageSymbol;
    const LangFn = *const fn () callconv(.c) ?*const c.TSLanguage;
    const lang_fn: LangFn = @ptrCast(@alignCast(raw));
    const lang = lang_fn() orelse return error.NullLanguageSymbol;
    return lang;
}

test "detect language by file extension" {
    try std.testing.expectEqual(LanguageId.zig, detectLanguage("/tmp/main.zig").?);
    try std.testing.expectEqual(LanguageId.python, detectLanguage("script.py").?);
    try std.testing.expectEqual(LanguageId.cpp, detectLanguage("vector.hpp").?);
    try std.testing.expectEqual(LanguageId.typescript, detectLanguage("index.ts").?);
    try std.testing.expectEqual(LanguageId.tsx, detectLanguage("component.tsx").?);
    try std.testing.expect(detectLanguage("README") == null);
}

test "style classification from capture names" {
    try std.testing.expectEqual(Style.keyword, styleForCapture("keyword"));
    try std.testing.expectEqual(Style.string, styleForCapture("string.special"));
    try std.testing.expectEqual(Style.comment, styleForCapture("comment.line"));
    try std.testing.expectEqual(Style.function_name, styleForCapture("function.call"));
}
