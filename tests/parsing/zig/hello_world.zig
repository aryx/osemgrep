// AST_zig mapping for this file:
//
// program = [
//
//   (* const std = @import("std"); *)
//   DVar {
//     vd_kind = (Const, "const");
//     vd_name = ("std", ...);
//     vd_type = None;
//     vd_init = Some (
//       BuiltinCall (("@import", ...), ("(", [String ("std", ...)], ")"))
//     );
//     ...
//   };
//
//   (* pub fn main() void { ... } *)
//   DFunc {
//     fd_pub = Some "pub";
//     fd_name = ("main", ...);
//     fd_type = {
//       ftok = "fn";
//       fparams = ("(", [], ")");
//       freturn = Some (TName ("void", ...));
//       fcalling_convention = None;
//     };
//     fd_body = Some ("{", [
//
//       (* std.debug.print("Hello, {s}!\n", .{"world"}); *)
//       ExprSt (
//         Call (
//           FieldAccess (
//             FieldAccess (Id ("std", ...), ".", ("debug", ...)),
//             ".",
//             ("print", ...)
//           ),
//           ("(", [
//             String ("Hello, {s}!\n", ...);
//             ArrayInit (None, ("{", [String ("world", ...)], "}"))
//           ], ")")
//         ),
//         ";"
//       )
//
//     ], "}");
//     ...
//   }
//
// ]

const std = @import("std");

pub fn main() void {
    std.debug.print("Hello, {s}!\n", .{"world"});
}
