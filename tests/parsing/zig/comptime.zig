const std = @import("std");

// =====================
// comptime variables
// =====================

const len = comptime blk: {
    const arr = [_]u8{ 1, 2, 3, 4, 5 };
    break :blk arr.len;
};

// =====================
// comptime function evaluation
// =====================

fn fibonacci(n: u32) u32 {
    if (n == 0) return 0;
    if (n == 1) return 1;
    return fibonacci(n - 1) + fibonacci(n - 2);
}

const fib_10 = comptime fibonacci(10);

// =====================
// comptime parameters (generic programming)
// =====================

fn max(comptime T: type, a: T, b: T) T {
    return if (a > b) a else b;
}

fn List(comptime T: type) type {
    return struct {
        items: []T,
        len: usize,

        const Self = @This();

        fn append(self: *Self, item: T) void {
            self.items[self.len] = item;
            self.len += 1;
        }
    };
}

// =====================
// @typeInfo — comptime type reflection
// =====================

fn isSignedInt(comptime T: type) bool {
    return switch (@typeInfo(T)) {
        .int => |info| info.signedness == .signed,
        else => false,
    };
}

// =====================
// comptime string formatting
// =====================

fn fieldName(comptime prefix: []const u8, comptime index: usize) []const u8 {
    return comptime std.fmt.comptimePrint("{s}_{d}", .{ prefix, index });
}

// =====================
// inline for (comptime loop unrolling)
// =====================

fn sum(values: [4]u32) u32 {
    var result: u32 = 0;
    inline for (values) |v| {
        result += v;
    }
    return result;
}
