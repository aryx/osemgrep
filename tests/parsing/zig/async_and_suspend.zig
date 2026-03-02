const std = @import("std");

// =====================
// async / await
// =====================

fn fetchData(url: []const u8) ![]const u8 {
    _ = url;
    return "data";
}

fn asyncFetchExample() void {
    var frame = async fetchData("https://example.com");
    // do other work...
    const result = await frame;
    _ = result;
}

// =====================
// suspend / resume
// =====================

fn generator() void {
    suspend {}
    std.debug.print("resumed first time\n", .{});
    suspend {}
    std.debug.print("resumed second time\n", .{});
}

fn driveGenerator() void {
    var gen = async generator();
    resume gen;
    resume gen;
}

// =====================
// nosuspend
// =====================

fn guaranteedNonSuspending() void {
    const result = nosuspend fetchData("local");
    _ = result;
}
