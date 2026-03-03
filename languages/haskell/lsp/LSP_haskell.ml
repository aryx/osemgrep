(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public License
 * version 2.1 as published by the Free Software Foundation, with the
 * special exception on linking described in file license.txt.
 *
 * This library is distributed in the hope that it will be useful, but
 * WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the file
 * license.txt for more details.
 *)

(* Haskell (HLS) helpers for the LSP client.
 *
 * Haskell typed metavars use ($X :: TYPE) syntax (double-colon,
 * matching Haskell's type annotation operator).
 *)

open Common
module G = AST_generic

let project_root_marker = "*.cabal"
let language_id = "haskell"

let server_cmd (_caps : < Cap.exec ; .. >) ~root =
  let home = Sys.getenv "HOME" in
  let ghcup_bin = Filename.concat home ".ghcup/bin/haskell-language-server-wrapper" in
  let bin =
    if Sys.file_exists ghcup_bin then ghcup_bin
    else "haskell-language-server-wrapper"
  in
  (* HLS wrapper uses CWD (not rootUri) to discover the project cradle,
   * so we must cd to the project root before starting it. *)
  spf "cd %s && %s --lsp" (Filename.quote root) bin

(* Extract the type string from an HLS hover response.
 *
 * coupling: https://haskell-language-server.readthedocs.io/
 *
 * HLS returns MarkupContent with ```haskell code fences.
 *
 * For variables like 'x = 10':
 *   "```haskell\nx :: Int\n```"  ->  "Int"
 * For functions like 'add :: Int -> Int -> Int':
 *   "```haskell\nadd :: Int -> Int -> Int\n```"  ->  "Int -> Int -> Int"
 *)
let clean_hover s =
  let s = String.trim s in

  (* Extract content from ```haskell ... ``` code fences *)
  let s =
    if s =~ "^```haskell\n\\(.*\\)" then Common.matched1 s
    else s
  in
  let s = Str.global_replace (Str.regexp "```$") "" s in

  (* Remove documentation after separators *)
  let s =
    match Str.bounded_split (Str.regexp "\n---\n\\|\n\n") s 2 with
    | type_part :: _ -> type_part
    | [] -> s
  in

  (* Collapse newlines and trim *)
  let s = Str.global_replace (Str.regexp "\n") " " s in
  let s = String.trim s in

  (* Extract the type after :: *)
  if s =~ "^.* :: \\(.*\\)$" then
    String.trim (Common.matched1 s)
  else
    s

(* Parse a cleaned hover type string by wrapping it as "($X :: TYPE)"
 * and using the Haskell tree-sitter parser, which produces a TypedExpr
 * that Haskell_to_generic converts to TypedMetavar for metavar names. *)
let parse_type s =
  let wrapped = "($X :: " ^ s ^ ")" in
  let res = Parse_haskell_tree_sitter.parse_pattern wrapped in
  let any =
    match res.Tree_sitter_run.Parsing_result.program with
    | Some x -> x
    | None -> failwith (spf "LSP_client: cannot parse Haskell pattern: %s" wrapped)
  in
  let any = Haskell_to_generic.any any in
  match any with
  | G.E { e = G.TypedMetavar (_, _, ty); _ } -> ty
  | G.S { s = G.ExprStmt ({ e = G.TypedMetavar (_, _, ty); _ }, _); _ } -> ty
  | _ ->
      failwith (spf "LSP_client: cannot parse Haskell type from: %s" s)

let lsp_lang : LSP_lang.t = {
  server_cmd = (fun caps ~root -> server_cmd caps ~root);
  language_id;
  project_root_marker;
  clean_hover;
  parse_type;
  needs_warmup = true;
  init_options = None;
}
