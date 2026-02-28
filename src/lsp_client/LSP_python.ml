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

(* Python (ty / pyright) helpers for the LSP client.
 *
 * Extract the type string from a Python LSP server hover response and
 * parse it into AST_generic.type_.
 *
 * Supported language servers (tried in order by server_cmd):
 *
 *  | Server              | Hover types? | Install                          | Notes                                      |
 *  |---------------------|-------------|----------------------------------|--------------------------------------------|
 *  | ty (Astral)         | yes         | uv tool install ty               | Extremely fast (Rust), beta since Dec 2025 |
 *  | pyright (Microsoft) | yes         | npm i -g pyright / pipx pyright  | Gold standard, written in TypeScript/Node   |
 *  | pylsp + jedi        | limited     | pip install python-lsp-server    | Less precise type inference                |
 *  | jedi-language-server | limited    | pip install jedi-language-server | Lightweight, static analysis only          |
 *
 * We prefer ty (fastest, Rust-native) and fall back to pyright.
 * pylsp/jedi are not tried because their hover responses lack the
 * "(variable) x: int" format needed for reliable type extraction.
 *
 * Hover format (both ty and pyright use markdown with code fences):
 *
 * For variables like 'x = 42':
 *   "```python\n(variable) x: int\n```"
 *   We extract: "int"  (strip "(variable) NAME: ")
 *
 * For functions like 'def add(a: int, b: int) -> int':
 *   "```python\n(function) def add(a: int, b: int) -> int\n```\n---\nDocstring..."
 *   We extract: "def add(a: int, b: int) -> int"  (the function signature)
 *
 * For classes:
 *   "```python\n(class) Foo\n```"
 *   We keep: "Foo"
 *
 * For methods:
 *   "```python\n(method) def bar(self, x: int) -> str\n```"
 *   We extract: "def bar(self, x: int) -> str"
 *
 * coupling: pyright hover format is defined in:
 *   https://github.com/microsoft/pyright/blob/main/packages/pyright-internal/src/analyzer/hoverProvider.ts
 *)

open Common
module G = AST_generic

let project_root_marker = "pyproject.toml"
let language_id = "python"

let server_cmd (_caps : < Cap.exec ; .. >) =
  (* Prefer ty (Astral's fast Rust type checker / language server) *)
  let home = Sys.getenv "HOME" in
  let ty_local = Filename.concat home ".local/bin/ty" in
  let ty_cargo = Filename.concat home ".cargo/bin/ty" in
  if Sys.file_exists ty_local then ty_local ^ " server"
  else if Sys.file_exists ty_cargo then ty_cargo ^ " server"
  else
    (* Fall back to pyright *)
    let pyright_local = Filename.concat home ".local/bin/pyright-langserver" in
    let pyright_npm_local = Filename.concat home ".local/share/npm/bin/pyright-langserver" in
    let npx_global = Filename.concat home ".npm-global/bin/pyright-langserver" in
    if Sys.file_exists pyright_local then pyright_local ^ " --stdio"
    else if Sys.file_exists pyright_npm_local then pyright_npm_local ^ " --stdio"
    else if Sys.file_exists npx_global then npx_global ^ " --stdio"
    else
      (* Last resort: hope it's on PATH *)
      "pyright-langserver --stdio"

let clean_hover s =
  (* Step 1: extract content from ```python ... ``` code fences.
   * Both ty and pyright wrap type info in code fences.
   * e.g. "```python\n(variable) x: int\n```\n---\nDocstring..."
   *   -> "(variable) x: int"
   *)
  let s =
    if s =~ "^```python\n\\(.*\\)" then Common.matched1 s else s
  in
  let s = Str.global_replace (Str.regexp "```$") "" s in

  (* Step 2: remove documentation after separators.
   * e.g. "(variable) x: int\n---\nDoc..."
   *   -> "(variable) x: int"
   *)
  let s =
    match Str.bounded_split (Str.regexp "\n---\n\\|\n\n") s 2 with
    | type_part :: _ -> type_part
    | [] -> s
  in

  (* Step 3: collapse newlines *)
  let s = Str.global_replace (Str.regexp "\n") " " s in
  let s = String.trim s in

  (* Step 4: strip the kind prefix and extract the type.
   *
   * "(variable) x: int"                -> "int"
   * "(variable) x: list[int]"          -> "list[int]"
   * "(parameter) x: int"               -> "int"
   * "(constant) PI: float"             -> "float"
   * "(function) def add(a: int, b: int) -> int"
   *   -> "def add(a: int, b: int) -> int"   (keep as function sig)
   * "(method) def bar(self, x: int) -> str"
   *   -> "def bar(self, x: int) -> str"
   * "(class) Foo"                       -> "Foo"
   * "(type alias) Foo = int | str"      -> "int | str"
   *
   * If no parenthesized prefix, try bare "name: type" or keep as-is.
   *)
  let s =
    if s =~ "^([a-z ]+) \\(.*\\)" then Common.matched1 s
    else s
  in
  let s = String.trim s in

  (* Step 5: for variable/parameter/constant bindings, extract the type.
   * "x: int"                 -> "int"
   * "PI: float"              -> "float"
   * But NOT "def add(...)..." — those are function signatures.
   *)
  if s =~ "^def \\|^class \\|^async def " then
    (* Function/method/class — keep as-is for parse_type *)
    s
  else if s =~ "^[a-zA-Z_][a-zA-Z_0-9]*: \\(.*\\)" then
    Common.matched1 s
  else
    s

(* Extract type from a parsed generic AST node *)
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

(* Try to parse a Python type string by wrapping it as "x: TYPE"
 * and extracting the type from the resulting variable annotation.
 * This is the same approach used by Parse_metavariable_type for Python. *)
let try_parse_python str =
  try
    let any = Parse_python.any_of_string str in
    let generic = Python_to_generic.any any in
    extract_type_from_any generic
  with _exn -> None

let try_parse_python_type str =
  (* First try wrapping as "x: TYPE" for simple types *)
  match try_parse_python ("x: " ^ str) with
  | Some _ as result -> result
  | None ->
      (* Try parsing directly (e.g. function definitions) *)
      try_parse_python str

(* Parse a cleaned hover string into AST_generic.type_. *)
let parse_type s =
  match try_parse_python_type s with
  | Some ty -> ty
  | None ->
      failwith (spf "LSP_client: cannot parse Python type from: %s" s)

let lsp_lang : LSP_lang.t = {
  server_cmd = (fun caps ~root:_ -> server_cmd caps);
  language_id;
  project_root_marker;
  clean_hover;
  parse_type;
  needs_warmup = true;
}
