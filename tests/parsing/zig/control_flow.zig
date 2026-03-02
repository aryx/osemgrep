const std = @import("std");

// =====================
// if / else expressions
// =====================

fn abs(x: i32) i32 {
    return if (x < 0) -x else x;
}

// =====================
// if with optional unwrap
// =====================

fn processOptional(opt: ?u32) u32 {
    if (opt) |value| {
        return value * 2;
    } else {
        return 0;
    }
}

// =====================
// if with error union unwrap
// =====================

fn processResult(result: anyerror!u32) u32 {
    if (result) |value| {
        return value;
    } else |err| {
        std.log.err("got error: {}", .{err});
        return 0;
    }
}

// =====================
// while loops
// =====================

fn countUp(limit: u32) u32 {
    var i: u32 = 0;
    var sum: u32 = 0;
    while (i < limit) : (i += 1) {
        sum += i;
    }
    return sum;
}

// while with optional
fn consumeIterator(iter: *Iterator) void {
    while (iter.next()) |item| {
        process(item);
    }
}

// =====================
// for loops
// =====================

fn sumSlice(items: []const u32) u32 {
    var total: u32 = 0;
    for (items) |item| {
        total += item;
    }
    return total;
}

fn indexedIteration(items: []const u8) void {
    for (items, 0..) |item, index| {
        std.debug.print("[{}] = {}\n", .{ index, item });
    }
}

// =====================
// switch
// =====================

fn describe(val: u8) []const u8 {
    return switch (val) {
        0 => "zero",
        1...9 => "single digit",
        10, 100 => "power of ten",
        else => "other",
    };
}

fn handleToken(token: Token) void {
    switch (token) {
        .identifier => |name| std.debug.print("id: {s}\n", .{name}),
        .number => |n| std.debug.print("num: {}\n", .{n}),
        .plus, .minus => std.debug.print("operator\n", .{}),
        else => {},
    }
}

// =====================
// Labeled blocks and break
// =====================

fn findFirst(items: []const u32, target: u32) ?usize {
    return blk: {
        for (items, 0..) |item, index| {
            if (item == target) break :blk index;
        }
        break :blk null;
    };
}

// =====================
// defer and errdefer
// =====================

fn openAndProcess(path: []const u8) !void {
    const file = try std.fs.cwd().openFile(path, .{});
    defer file.close();

    var buf: [4096]u8 = undefined;
    const bytes_read = try file.read(&buf);
    _ = bytes_read;
}
