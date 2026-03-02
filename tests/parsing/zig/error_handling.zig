const std = @import("std");

// =====================
// Error sets
// =====================

const FileOpenError = error{
    AccessDenied,
    FileNotFound,
    OutOfMemory,
};

const AllocationError = error{OutOfMemory};

// =====================
// Error union return types
// =====================

fn parseNumber(buf: []const u8) !u64 {
    return std.fmt.parseInt(u64, buf, 10);
}

fn openFile(path: []const u8) FileOpenError!std.fs.File {
    return std.fs.cwd().openFile(path, .{}) catch |err| {
        return err;
    };
}

// =====================
// try — unwrap or propagate error
// =====================

fn readConfig(path: []const u8) ![]const u8 {
    const file = try openFile(path);
    defer file.close();
    return try file.readToEndAlloc(std.heap.page_allocator, 1024 * 1024);
}

// =====================
// catch — handle errors
// =====================

fn getValueOrDefault() u64 {
    const result = parseNumber("42") catch |err| {
        std.log.err("parse failed: {}", .{err});
        return 0;
    };
    return result;
}

fn getValueOrZero() u64 {
    return parseNumber("bad") catch 0;
}

// =====================
// errdefer — cleanup on error return
// =====================

fn createResource(allocator: std.mem.Allocator) !*Resource {
    const ptr = try allocator.create(Resource);
    errdefer allocator.destroy(ptr);

    ptr.* = try initResource();
    return ptr;
}

// =====================
// Merging error sets
// =====================

const CombinedError = FileOpenError || AllocationError;
