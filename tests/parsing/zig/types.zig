// =====================
// Primitive types
// =====================

const a: u8 = 255;
const b: i32 = -42;
const c: f64 = 3.14;
const d: bool = true;
const e: usize = 0;
const f: comptime_int = 100;

// =====================
// Pointer types
// =====================

const single: *u32 = undefined;
const many: [*]u8 = undefined;
const c_ptr: [*c]u8 = undefined;
const const_ptr: *const u8 = undefined;

// =====================
// Optional types
// =====================

const maybe_val: ?u32 = null;
const has_val: ?u32 = 42;

fn unwrap(opt: ?u32) u32 {
    return opt orelse 0;
}

fn unwrapPtr(opt: ?*u32) *u32 {
    return opt.?;
}

// =====================
// Slices and arrays
// =====================

const fixed: [5]u8 = .{ 1, 2, 3, 4, 5 };
const slice: []const u8 = "hello";
const sentinel: [:0]const u8 = "null-terminated";

// =====================
// Struct types
// =====================

const Point = struct {
    x: f32,
    y: f32,

    fn distanceTo(self: Point, other: Point) f32 {
        const dx = self.x - other.x;
        const dy = self.y - other.y;
        return @sqrt(dx * dx + dy * dy);
    }
};

const PackedHeader = packed struct {
    flags: u4,
    version: u4,
    length: u16,
    checksum: u8,
};

// =====================
// Enum types
// =====================

const Color = enum {
    red,
    green,
    blue,
    custom,
};

const HttpStatus = enum(u16) {
    ok = 200,
    not_found = 404,
    internal_error = 500,
};

// =====================
// Tagged union types
// =====================

const Token = union(enum) {
    identifier: []const u8,
    number: f64,
    string_literal: []const u8,
    plus,
    minus,
    eof,
};

// =====================
// Error union types
// =====================

const ParseError = error{
    InvalidCharacter,
    Overflow,
    EndOfStream,
};

const Result = ParseError!u64;

// =====================
// Function types
// =====================

const Callback = *const fn (u32) void;
const Predicate = *const fn (item: u8) bool;
