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
(* Parsing hacks for Zig.
 *
 * Similar to Parsing_hacks_go.ml, this module rewrites token streams
 * between lexing and parsing to resolve LR(1) conflicts.
 *
 * Main hack:
 * 1. COMPTIME followed by CONST/VAR → COMPTIME_MOD
 *    In Zig, `comptime` can be a declaration modifier (`comptime var x = 5`)
 *    or an expression prefix (`comptime foo()`). The parser has an S/R
 *    conflict because `pub_opt extern_opt comptime_opt` are all optional
 *    and COMPTIME could start either path.
 *    We look ahead: if COMPTIME is followed by CONST or VAR, we rewrite
 *    it to COMPTIME_MOD so the grammar can use the modifier form.
 *)

open Parser_zig

(*****************************************************************************)
(* Fix tokens *)
(*****************************************************************************)

let fix_tokens (toks : Parser_zig.token list) : Parser_zig.token list =
  let rec aux = function
    | [] -> []
    | COMPTIME ii :: ((CONST _ | VAR _) :: _ as rest) ->
        COMPTIME_MOD ii :: aux rest
    | tok :: rest ->
        tok :: aux rest
  in
  aux toks
