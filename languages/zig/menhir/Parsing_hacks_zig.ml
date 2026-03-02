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
 * Similar to parsing_hacks_go.ml, this module rewrites token streams
 * between lexing and parsing to resolve LR(1) conflicts.
 *
 * Hacks (all operate on the flat token list, no Lib_ast_fuzzy needed):
 * 1. COMPTIME before CONST/VAR → COMPTIME_MOD
 * 2. IF/SWITCH after expr-context tokens (=, return, =>) → IF_EXPR/SWITCH_EXPR
 * 3. LBRACE after RBRACKET/IDENT/RPAREN (typed init) → LBRACE_INIT
 *)

open Parser_zig

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)

(* After these tokens, if/switch is an expression, not a statement *)
let is_expr_context = function
  | EQ _ | RETURN _ | FATARROW _ | COMMA _
  | PLUSEQ _ | MINUSEQ _ | STAREQ _ | SLASHEQ _ | PERCENTEQ _
  | AMPEQ _ | PIPEEQ _ | CARETEQ _ | LSHIFTEQ _ | RSHIFTEQ _
  -> true
  | _ -> false

let is_lbracket = function
  | Some (LBRACKET _) -> true
  | _ -> false

(*****************************************************************************)
(* Fix tokens *)
(*****************************************************************************)

let fix_tokens (toks : Parser_zig.token list) : Parser_zig.token list =
  let rec aux prev = function
    | [] -> []
    (* Hack 1: COMPTIME before CONST/VAR → COMPTIME_MOD *)
    | COMPTIME ii :: ((CONST _ | VAR _) :: _ as rest) ->
        COMPTIME_MOD ii :: aux (Some (COMPTIME_MOD ii)) rest
    (* Hack 2: keywords in expression context → *_EXPR variants *)
    | (IF ii as tok) :: rest when is_expr_ctx prev ->
        IF_EXPR ii :: aux (Some tok) rest
    | (SWITCH ii as tok) :: rest when is_expr_ctx prev ->
        SWITCH_EXPR ii :: aux (Some tok) rest
    | (STRUCT ii as tok) :: rest when is_expr_ctx prev ->
        STRUCT_EXPR ii :: aux (Some tok) rest
    | (ENUM ii as tok) :: rest when is_expr_ctx prev ->
        ENUM_EXPR ii :: aux (Some tok) rest
    | (UNION ii as tok) :: rest when is_expr_ctx prev ->
        UNION_EXPR ii :: aux (Some tok) rest
    | (ERROR ii as tok) :: rest when is_expr_ctx prev ->
        ERROR_EXPR ii :: aux (Some tok) rest
    (* Hack 2b: packed/extern before struct in expr context *)
    | (PACKED _ as p) :: (STRUCT ii as tok) :: rest when is_expr_ctx prev ->
        p :: STRUCT_EXPR ii :: aux (Some tok) rest
    | (EXTERN _ as e) :: (STRUCT ii as tok) :: rest when is_expr_ctx prev ->
        e :: STRUCT_EXPR ii :: aux (Some tok) rest
    (* Hack 3: ]type{ → LBRACE_INIT for typed array init like [_]u8{ ... }
     * Only when brackets are non-empty (prev != LBRACKET), so []u8{ is NOT
     * retagged (it's a slice type followed by a block, not a typed init). *)
    | (RBRACKET _ as rb) :: (IDENT _ as t1) :: (LBRACE ii) :: rest
      when not (is_lbracket prev) ->
        rb :: t1 :: LBRACE_INIT ii :: aux (Some (LBRACE_INIT ii)) rest
    | (RBRACKET _ as rb) :: (CONST _ as c) :: (IDENT _ as t1) :: (LBRACE ii) :: rest
      when not (is_lbracket prev) ->
        rb :: c :: t1 :: LBRACE_INIT ii :: aux (Some (LBRACE_INIT ii)) rest
    | tok :: rest ->
        tok :: aux (Some tok) rest
  and is_expr_ctx = function
    | None -> false
    | Some tok -> is_expr_context tok
  in
  aux None toks
