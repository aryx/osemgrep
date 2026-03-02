const std = @import("std");

// =====================
// @import
// =====================

const os = @import("std").os;
const c = @cImport({
    @cInclude("stdio.h");
});

// =====================
// @as — explicit type coercion
// =====================

fn coercionExample() void {
    const x: u8 = 42;
    const wide = @as(u32, x);
    _ = wide;
}

// =====================
// @intCast, @floatCast, @ptrCast
// =====================

fn castExample(big: u64) u8 {
    return @intCast(big);
}

fn ptrCastExample(ptr: *anyopaque) *u32 {
    return @ptrCast(@alignCast(ptr));
}

// =====================
// @sizeOf, @alignOf, @bitSizeOf
// =====================

const Point = struct { x: f32, y: f32 };

const point_size = @sizeOf(Point);
const point_align = @alignOf(Point);
const u32_bits = @bitSizeOf(u32);

// =====================
// @memcpy, @memset
// =====================

fn bufferOps(dest: []u8, src: []const u8) void {
    @memcpy(dest[0..src.len], src);
    @memset(dest[src.len..], 0);
}

// =====================
// @panic, @trap
// =====================

fn assertPositive(x: i32) u32 {
    if (x <= 0) @panic("expected positive value");
    return @intCast(x);
}

// =====================
// @TypeOf, @typeName, @This
// =====================

fn printType(val: anytype) void {
    const T = @TypeOf(val);
    std.debug.print("type: {s}\n", .{@typeName(T)});
}

const SelfRef = struct {
    const Self = @This();
    data: u32,

    fn clone(self: Self) Self {
        return .{ .data = self.data };
    }
};

// =====================
// @embedFile
// =====================

const embedded_data = @embedFile("data.bin");

// =====================
// @tagName — enum to string
// =====================

const Color = enum { red, green, blue };

fn colorName(c_val: Color) []const u8 {
    return @tagName(c_val);
}
