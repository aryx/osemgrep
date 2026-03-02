(* Claude Code
 *
 * Copyright (C) 2024-2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public License
 * version 2.1 as published by the Free Software Foundation, with the
 * special exception on linking described in file LICENSE.
 *
 * This library is distributed in the hope that it will be useful, but
 * WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the file
 * LICENSE for more details.
 *)

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(* Abstract Syntax Tree for Zig.
 *
 * This AST is designed to be populated by a tree-sitter-zig parser (future)
 * and translated to AST_generic for semgrep matching.
 *
 * Reference: https://ziglang.org/documentation/master/
 * Reference: tree-sitter-zig grammar
 *
 * Design draws from AST_go.ml (modern systems language, reuses
 * AST_generic.operator) and AST_c.ml (C-like systems language).
 *)

(*****************************************************************************)
(* Names *)
(*****************************************************************************)

(* ------------------------------------------------------------------------- *)
(* Token/info *)
(* ------------------------------------------------------------------------- *)
type tok = Tok.t [@@deriving show]
type 'a wrap = 'a * tok [@@deriving show]
type 'a bracket = tok * 'a * tok [@@deriving show]

(* ------------------------------------------------------------------------- *)
(* Ident *)
(* ------------------------------------------------------------------------- *)
type ident = string wrap [@@deriving show]

(*****************************************************************************)
(* Type *)
(*****************************************************************************)
type type_ =
  (* named types, including builtins: u8, i32, usize, bool, void, etc. *)
  | TName of ident
  (* pointer types: *T, [*]T, [*c]T *)
  | TPointer of pointer_kind * tok * type_
  (* [N]T — fixed-size array *)
  | TArray of expr bracket * type_
  (* []T — slice *)
  | TSlice of tok bracket * type_
  (* ?T — optional *)
  | TOptional of tok * type_
  (* E!T — error union *)
  | TErrorUnion of type_ * tok (* ! *) * type_
  (* anyerror!T — inferred error set *)
  | TAnyErrorUnion of tok (* anyerror *) * tok (* ! *) * type_
  (* fn (...) ReturnType *)
  | TFunc of func_type
  (* struct { ... } *)
  | TStruct of tok * container_field list bracket
  (* enum { ... } or enum(TagType) { ... } *)
  | TEnum of tok * type_ option * enum_field list bracket
  (* union { ... } or union(TagType) { ... } *)
  | TUnion of tok * type_ option * container_field list bracket
  (* opaque {} *)
  | TOpaque of tok * tok bracket
  (* std.fs.File — qualified type access *)
  | TDotAccess of type_ * tok (* . *) * ident
  (* @Type("name") — builtin type functions *)
  | TBuiltinCall of ident (* @-prefixed *) * expr list bracket
  (* sgrep-ext: *)
  | TEllipsis of tok
[@@deriving show { with_path = false }]

and pointer_kind =
  | PtrSingle (* *T *)
  | PtrMany   (* [*]T *)
  | PtrC      (* [*c]T — C pointer interop *)
[@@deriving show { with_path = false }]

and func_type = {
  ftok : tok; (* 'fn' *)
  fparams : param list bracket;
  freturn : type_ option; (* None for 'void' or inferred *)
  fcalling_convention : ident option;
}

and param =
  | Param of param_classic
  (* sgrep-ext: *)
  | ParamEllipsis of tok

and param_classic = {
  p_comptime : tok option;
  p_noalias : tok option;
  p_name : ident option;
  p_type : type_;
}
[@@deriving show { with_path = false }]

(*****************************************************************************)
(* Expression *)
(*****************************************************************************)
and expr =
  (* Literals *)
  | Int of Parsed_int.t
  | Float of float option wrap
  | String of string wrap (* includes the quotes *)
  | MultilineString of string wrap
  | Char of string wrap
  | Bool of bool wrap
  | Null of tok
  | Undefined of tok
  (* Name *)
  | Id of ident
  (* x.y — field/namespace access *)
  | FieldAccess of expr * tok * ident
  (* x.* — pointer dereference *)
  | Deref of expr * tok (* .* *)
  (* x.? — optional unwrap *)
  | OptionalUnwrap of expr * tok (* .? *)
  (* x[i] *)
  | Index of expr * expr bracket
  (* x[a..b] — slice *)
  | SliceExpr of expr * slice_bounds bracket
  (* f(args) *)
  | Call of expr * expr list bracket
  (* @name(args) — builtin function call *)
  | BuiltinCall of ident (* @-prefixed *) * expr list bracket
  (* Operators — reuse AST_generic.operator like Go does *)
  | Unary of AST_generic.operator wrap * expr
  | Binary of expr * AST_generic.operator wrap * expr
  (* x = e, x += e, etc. *)
  | Assign of expr * tok * expr
  | AssignOp of expr * AST_generic.operator wrap * expr
  (* .{ field = val, ... } — anonymous struct init *)
  | StructInit of type_ option * field_init list bracket
  (* .{ val, val, ... } — array/tuple init *)
  | ArrayInit of type_ option * expr list bracket
  (* if (cond) |capture| then_expr else else_expr — expression form *)
  | IfExpr of tok * expr * capture option * expr * (tok * expr) option
  (* switch (operand) { prongs... } *)
  | SwitchExpr of tok * expr * switch_prong list bracket
  (* for (items) |captures| expr — expression form *)
  | ForExpr of tok * for_input list * capture list * expr * (tok * expr) option
  (* while (cond) |capture| : (continue_expr) body (else ...) — expr form *)
  | WhileExpr of tok * expr * capture option * expr option * expr * (tok * expr) option
  (* try expr *)
  | Try of tok * expr
  (* expr catch |err| expr *)
  | Catch of expr * tok * capture option * expr
  (* expr orelse |val| expr *)
  | Orelse of expr * tok * capture option * expr
  (* comptime expr *)
  | Comptime of tok * expr
  (* nosuspend expr *)
  | Nosuspend of tok * expr
  (* async expr *)
  | Async of tok * expr
  (* await expr *)
  | Await of tok * expr
  (* resume expr *)
  | Resume of tok * expr
  (* suspend (with optional body) *)
  | Suspend of tok * expr option
  (* label: { ... } — labeled block expression *)
  | BlockExpr of ident option * stmt list bracket
  (* a..b — range, used in slicing *)
  | Range of expr option * tok (* .. *) * expr option
  (* error.Name *)
  | ErrorValue of tok (* error *) * tok (* . *) * ident
  (* anonymous function / closure: fn(params) type { body } *)
  | FuncLit of func_type * stmt list bracket
  (* type used as expression, e.g., [_]u8 in [_]u8{ 1, 2, 3 } *)
  | TypeExpr of type_
  (* sgrep-ext: *)
  | Ellipsis of tok
  | DeepEllipsis of expr bracket
  | TypedMetavar of ident * tok * type_
  | DotAccessEllipsis of expr * tok
[@@deriving show { with_path = false }]

and slice_bounds = {
  sl_start : expr option;
  sl_end : expr option;
  sl_sentinel : expr option; (* :sentinel *)
}

and field_init =
  | FieldInit of ident * tok (* = *) * expr
  | FieldInitEllipsis of tok (* sgrep *)

and capture = ident list (* |a, b| payload captures *)

and for_input = {
  fi_expr : expr;
  fi_sentinel : expr option;
}

and switch_prong = {
  sp_cases : switch_case list;
  sp_capture : capture option;
  sp_body : expr;
}

and switch_case =
  | SCExpr of expr
  | SCRange of expr * tok (* ... *) * expr
  | SCDefault of tok (* else *)
  (* sgrep-ext: *)
  | SCEllipsis of tok
[@@deriving show { with_path = false }]

(*****************************************************************************)
(* Statement *)
(*****************************************************************************)
and stmt =
  | ExprSt of expr * tok (* ; *)
  | Block of stmt list bracket
  | VarDecl of var_decl
  (* if (cond) |capture| body else ... — statement form *)
  | If of tok * expr * capture option * stmt * stmt option
  (* while (cond) |capture| : (continue) body (else ...) *)
  | While of tok * expr * capture option * expr option * stmt * stmt option
  (* for (items) |captures| body (else ...) *)
  | For of tok * for_input list * capture list * stmt * stmt option
  (* switch (operand) { prongs... } — statement form *)
  | Switch of tok * expr * switch_prong list bracket
  | Return of tok * expr option
  | Break of tok * ident option (* label *) * expr option
  | Continue of tok * ident option (* label *)
  (* defer body *)
  | Defer of tok * stmt
  (* errdefer |err| body *)
  | Errdefer of tok * capture option * stmt
  (* asm volatile { ... } *)
  | Asm of tok * expr list bracket
  (* empty statement *)
  | Empty of tok
[@@deriving show { with_path = false }]

(*****************************************************************************)
(* Declarations *)
(*****************************************************************************)
and var_decl = {
  vd_pub : tok option;
  vd_extern : tok option;
  vd_comptime : tok option;
  vd_threadlocal : tok option;
  vd_kind : var_kind wrap; (* 'const' or 'var' *)
  vd_name : ident;
  vd_type : type_ option;
  vd_align : expr option;
  vd_init : expr option;
}

and var_kind = Const | Var
[@@deriving show { with_path = false }]

(*****************************************************************************)
(* Container fields *)
(*****************************************************************************)
and container_field = {
  cf_name : ident;
  cf_type : type_;
  cf_default : expr option;
  cf_align : expr option;
}

and enum_field =
  | EnumField of ident * expr option (* explicit value *)
  (* sgrep-ext: *)
  | EnumFieldEllipsis of tok
[@@deriving show { with_path = false }]

(*****************************************************************************)
(* Toplevel *)
(*****************************************************************************)
type function_def = {
  fd_pub : tok option;
  fd_export : tok option;
  fd_extern : tok option;
  fd_inline : tok option;
  fd_name : ident;
  fd_type : func_type;
  fd_body : stmt list bracket option; (* None for extern fn *)
}
[@@deriving show { with_path = false }]

type toplevel =
  | DFunc of function_def
  | DVar of var_decl
  | DTest of tok * string wrap option (* test name *) * stmt list bracket
  | DComptime of tok * stmt list bracket
  | DUsingNamespace of tok option (* pub *) * tok * expr
  (* container type definitions at toplevel:
     const Foo = struct { ... }; is parsed as DVar,
     but we might also see standalone type defs *)
  | STop of stmt
[@@deriving show { with_path = false }]

type program = toplevel list [@@deriving show { with_path = false }]

(*****************************************************************************)
(* Any *)
(*****************************************************************************)
type any =
  | E of expr
  | S of stmt
  | T of type_
  | D of toplevel
  | P of program
  | Ss of stmt list
[@@deriving show { with_path = false }]

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)
let str_of_ident (s, _) = s
