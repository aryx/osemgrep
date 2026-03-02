Zig Parsing Test Files
======================

hello_world.zig
  @import, pub fn, std.debug.print, anonymous struct literals

error_handling.zig
  Error sets, error union types (!), try, catch, errdefer,
  error set merging (||)

comptime.zig
  comptime blocks/variables, comptime function params (generics),
  @typeInfo reflection, @This(), inline for, comptimePrint

types.zig
  Primitives, pointers (*, [*], [*c]), optionals (?), slices,
  arrays, sentinel-terminated, structs (inc. packed struct),
  enums (with explicit backing type), tagged unions, error unions,
  function pointer types

control_flow.zig
  if/else expressions, optional unwrap (if (opt) |val|),
  error unwrap, while with continue expr, for loops (indexed),
  switch with ranges, labeled blocks + break :label, defer

async_and_suspend.zig
  async, await, suspend {}, resume, nosuspend

builtins.zig
  @import, @cImport/@cInclude, @as, @intCast, @ptrCast,
  @alignCast, @sizeOf/@alignOf/@bitSizeOf, @memcpy/@memset,
  @panic, @TypeOf/@typeName/@This, @embedFile, @tagName

testing.zig
  test "name" blocks, testing.expectEqual/expectError/expect,
  allocator in tests, unnamed tests, comptime inside tests
