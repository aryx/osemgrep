const std = @import("std");
const testing = std.testing;

// =====================
// Basic test blocks
// =====================

test "addition" {
    const result = 2 + 2;
    try testing.expectEqual(@as(u32, 4), result);
}

test "string equality" {
    const a = "hello";
    const b = "hello";
    try testing.expectEqualStrings(a, b);
}

// =====================
// Test with error handling
// =====================

fn parseU32(buf: []const u8) !u32 {
    return std.fmt.parseInt(u32, buf, 10);
}

test "parseU32 valid" {
    const result = try parseU32("123");
    try testing.expectEqual(@as(u32, 123), result);
}

test "parseU32 invalid" {
    const result = parseU32("abc");
    try testing.expectError(error.InvalidCharacter, result);
}

// =====================
// Test with allocators
// =====================

test "arraylist" {
    var list = std.ArrayList(u32).init(testing.allocator);
    defer list.deinit();

    try list.append(1);
    try list.append(2);
    try list.append(3);

    try testing.expectEqual(@as(usize, 3), list.items.len);
}

// =====================
// Unnamed tests
// =====================

test {
    try testing.expect(true);
}

// =====================
// comptime test
// =====================

test "comptime fibonacci" {
    const fib = comptime blk: {
        var a: u32 = 0;
        var b: u32 = 1;
        var i: u32 = 0;
        while (i < 10) : (i += 1) {
            const temp = a + b;
            a = b;
            b = temp;
        }
        break :blk a;
    };
    try testing.expectEqual(@as(u32, 55), fib);
}
