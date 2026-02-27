(* Yoann Padioleau
 *
 * Copyright (C) 2024 Semgrep Inc.
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
open Fpath_.Operators
module CST = Tree_sitter_haskell.CST
module H = Parse_tree_sitter_helpers
open AST_haskell

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(* Haskell parser using tree-sitter-lang/semgrep-haskell and converting
 * to AST_haskell.
 *
 * The resulting AST can then be converted to the generic AST by using
 * Haskell_to_generic.ml
 *
 * TODO:
 *  - Complete the CST-to-AST conversion based on Boilerplate.ml
 *  - Handle all Haskell extensions
 *)

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)

type env = unit H.env

let _token = H.token
let _str = H.str

(*****************************************************************************)
(* Boilerplate converter *)
(*****************************************************************************)
(* This was started by copying semgrep-haskell/lib/Boilerplate.ml.
 *
 * TODO: Complete CST-to-AST mapping. The Haskell tree-sitter grammar
 * has ~200+ CST types. A full implementation would be ~3000 lines based
 * on the Boilerplate.ml template. Below are starter helpers for the
 * most common CST node types.
 *)

let _map_variable (env : env) (x : CST.variable) : ident =
  match x with
  | `Varid tok -> _str env tok
  | `Semg_meta tok -> _str env tok

let _map_constructor (env : env) (x : CST.constructor) : ident =
  match x with
  | `Conid tok -> _str env tok
  | `Semg_meta tok -> _str env tok

let _map_integer (env : env) (x : CST.integer) : literal =
  match x with
  | `Bin_lit tok
  | `Int_lit tok
  | `Octal_lit tok
  | `Hex_lit tok ->
      let s, t = _str env tok in
      Int (Parsed_int.parse (s, t))

let _map_number (env : env) (x : CST.number) : literal =
  match x with
  | `Int x -> _map_integer env x
  | `Float tok ->
      let s, t = _str env tok in
      Float (float_of_string_opt s, t)

let _map_stringly (env : env) (x : CST.stringly) : literal =
  match x with
  | `Str tok -> String (_str env tok)
  | `Char tok -> Char (_str env tok)

let _map_literal (env : env) (x : CST.literal) : literal =
  match x with
  | `Choice_str x -> _map_stringly env x
  | `Choice_int x -> _map_number env x

let map_module (env : env) (_x : CST.haskell) : program =
  ignore env;
  { p_module = None; p_imports = []; p_decls = [] }

(*****************************************************************************)
(* Entry point *)
(*****************************************************************************)

let parse file =
  H.wrap_parser
    (fun () -> Tree_sitter_haskell.Parse.file !!file)
    (fun cst _extras ->
      let env = { H.file; conv = H.line_col_to_pos file; extra = () } in
      map_module env cst)

let parse_pattern str =
  H.wrap_parser
    (fun () -> Tree_sitter_haskell.Parse.string str)
    (fun cst _extras ->
      let file = Fpath.v "<pattern>" in
      let env = { H.file; conv = H.line_col_to_pos_pattern str; extra = () } in
      let prog = map_module env cst in
      match prog.p_decls with
      | [ d ] -> D d
      | _ -> Pr prog)
