const std = @import("std");
const buffer_mod = @import("buffer.zig");

const Allocator = std.mem.Allocator;
const Buffer = buffer_mod.Buffer;

/// File I/O is isolated so editor logic stays testable.
/// mg intermixes command and file code in C modules; Zig keeps a narrow API.
pub const FileIO = struct {
    pub fn loadIntoBuffer(allocator: Allocator, path: []const u8, buffer: *Buffer) !void {
        var file = try std.fs.cwd().openFile(path, .{});
        defer file.close();

        const bytes = try file.readToEndAlloc(allocator, std.math.maxInt(usize));
        defer allocator.free(bytes);

        try buffer.setText(bytes);
    }

    pub fn saveFromBuffer(path: []const u8, buffer: *const Buffer) !void {
        var file = try std.fs.cwd().createFile(path, .{ .truncate = true });
        defer file.close();

        try file.writeAll(buffer.bytes());
    }
};

test "fileio save and load roundtrip" {
    const allocator = std.testing.allocator;

    var source = try Buffer.init(allocator, "source");
    defer source.deinit();
    try source.setText("hello\nworld\n");

    const filename = try std.fmt.allocPrint(allocator, ".mg-zig-fileio-{d}.txt", .{std.time.microTimestamp()});
    defer allocator.free(filename);
    defer std.fs.cwd().deleteFile(filename) catch {};

    try FileIO.saveFromBuffer(filename, &source);

    var loaded = try Buffer.init(allocator, "loaded");
    defer loaded.deinit();
    try FileIO.loadIntoBuffer(allocator, filename, &loaded);

    try std.testing.expectEqualStrings(source.bytes(), loaded.bytes());
}
