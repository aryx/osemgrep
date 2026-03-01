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

(* Rust (rust-analyzer) helpers for the LSP client. *)

open Common
module G = AST_generic

let project_root_marker = "Cargo.toml"
let language_id = "rust"

let server_cmd (_caps : < Cap.exec ; .. >) =
  let cargo_path = Filename.concat (Sys.getenv "HOME") ".cargo/bin/rust-analyzer" in
  if Sys.file_exists cargo_path then cargo_path
  else "rust-analyzer"

(* Extract the type string from a rust-analyzer hover response.
 *
 * rust-analyzer hover format (markdown with code fences):
 *
 * For variables like 'let x: i32 = 42':
 *   "*x*\n\n```rust\nlet x: i32\n```"  ->  "i32"
 * For functions like 'fn foo() -> u32':
 *   "*foo*\n\n```rust\nmod_path\n```\n\n```rust\npub fn foo() -> u32\n```"
 *   ->  "pub fn foo() -> u32"  (the last code block)
 * For structs:
 *   "*Foo*\n\n```rust\nstruct Foo { field: u32 }\n```\n\n---\nsize = 4..."
 *   ->  "struct Foo { field: u32 }"
 *)
let clean_hover s =
  (* Step 1: extract the last ```rust ... ``` code block.
   * rust-analyzer may emit multiple code blocks; the first is often the
   * module path, and the last is the actual declaration. *)
  let blocks = ref [] in
  let re = Str.regexp "```rust\n\\([^`]*\\)```" in
  let pos = ref 0 in
  (try
    while true do
      let _ = Str.search_forward re s !pos in
      blocks := Str.matched_group 1 s :: !blocks;
      pos := Str.match_end ()
    done
  with Not_found -> ());
  let s =
    match !blocks with
    | block :: _ -> String.trim block  (* last block found = head of reversed list *)
    | [] ->
        (* No code fences.  rust-analyzer sometimes returns plain text
         * with sections separated by \n\n:
         *   "module_path\n\npub fn foo(a: i32) -> i32\n\n\nDoc comment"
         *   "let x: i32"
         * Split into sections and find the declaration (the first section
         * that looks like Rust code, not a bare module name). *)
        let sections = Str.split (Str.regexp "\n---\n\\|\n\n") s in
        let is_decl sec =
          let sec = String.trim sec in
          (* A declaration starts with a Rust keyword *)
          sec =~ "^\\(pub \\|fn \\|let \\|struct \\|enum \\|type \\|const \\|static \\|impl \\|trait \\|mod \\|use \\)"
        in
        let decl =
          match List.filter is_decl sections with
          | d :: _ -> String.trim d
          | [] ->
              (* No keyword found — take the first section as-is *)
              (match sections with
               | part :: _ -> String.trim part
               | [] -> String.trim s)
        in
        decl
  in

  (* Step 2: collapse newlines *)
  let s = Str.global_replace (Str.regexp "\n") " " s in
  let s = String.trim s in

  (* Step 3: extract the type from a let binding.
   * "let x: i32"  -> "i32"
   * "let mut y: Vec<u8>"  -> "Vec<u8>"
   *)
  if s =~ "^let mut [a-zA-Z_][a-zA-Z_0-9]*: \\(.*\\)" then
    Common.matched1 s
  else if s =~ "^let [a-zA-Z_][a-zA-Z_0-9]*: \\(.*\\)" then
    Common.matched1 s
  else s

(* Try to parse a Rust type string by wrapping it as "let _x: TYPE;"
 * and extracting the type from the resulting variable declaration. *)
let extract_type_from_defstmt def =
  match def with
  | G.VarDef { G.vtype = Some ty; _ } -> Some ty
  | G.FuncDef { G.frettype = Some ty; _ } -> Some ty
  | _ -> None

let extract_type_from_any (any : G.any) =
  match any with
  | G.S { G.s = G.DefStmt (_, def); _ } -> extract_type_from_defstmt def
  | G.Ss [{ G.s = G.DefStmt (_, def); _ }] -> extract_type_from_defstmt def
  | G.Pr [{ G.s = G.DefStmt (_, def); _ }] -> extract_type_from_defstmt def
  | _ -> None

let try_parse_rust str =
  try
    let res = Parse_rust_tree_sitter.parse_pattern str in
    match res.Tree_sitter_run.Parsing_result.program with
    | Some any -> extract_type_from_any any
    | None -> None
  with _exn -> None

let try_parse_rust_type str =
  (* First try wrapping as "let _x: TYPE;" for simple types *)
  match try_parse_rust ("let _x: " ^ str ^ ";") with
  | Some _ as result -> result
  | None ->
      (* Try parsing the string directly (e.g. function declarations) *)
      try_parse_rust str

(* Parse a cleaned rust-analyzer hover string into AST_generic.type_. *)
let parse_type s =
  match try_parse_rust_type s with
  | Some ty -> ty
  | None ->
      failwith (spf "LSP_client: cannot parse Rust type from: %s" s)

let lsp_lang : LSP_lang.t = {
  server_cmd = (fun caps ~root:_ -> server_cmd caps);
  language_id;
  project_root_marker;
  clean_hover;
  parse_type;
  needs_warmup = true;
  init_options = None;
}
