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

(* TypeScript/JavaScript (typescript-language-server) helpers for the LSP client.
 *
 * Extract the type string from a typescript-language-server hover response
 * and parse it into AST_generic.type_.
 *
 * Both TypeScript and JavaScript use the same language server
 * (typescript-language-server --stdio), similar to how C/C++ share clangd.
 * The language_id ("typescript" vs "javascript") is set based on the
 * --lang flag.
 *
 * Note: typed metavars for TS use ($X : TYPE) syntax (parsed by the
 * menhir JS parser as TypedMetavar nodes). Lang.Js has no typed metavar
 * support in Parse_metavariable_type.ml, so typed metavar matching only
 * works with --lang ts.
 *
 * typescript-language-server hover format (markdown with code fences):
 *
 * For variables like 'const x: string = "hello"':
 *   "\n```typescript\nconst x: string\n```"
 *   We extract: "string"
 *
 * For properties like 'x.length':
 *   "\n```typescript\n(property) x: string\n```"
 *   We extract: "string"
 *
 * For functions like 'function add(a: number, b: number): number':
 *   "\n```typescript\nfunction add(a: number, b: number): number\n```"
 *   We extract the return type: "number"
 *
 * For consts like 'const bar = 42':
 *   "\n```typescript\nconst bar: number\n```"
 *   We extract: "number"
 *)

open Common
module G = AST_generic

let project_root_marker = "tsconfig.json"

let language_id lang =
  match lang with
  | Lang.Ts -> "typescript"
  | Lang.Js -> "javascript"
  | _ -> failwith "LSP_typescript: unsupported language"

let server_cmd (_caps : < Cap.exec ; .. >) =
  let home = Sys.getenv "HOME" in
  let npm_local = Filename.concat home ".local/share/npm/bin/typescript-language-server" in
  let npm_global = Filename.concat home ".npm-global/bin/typescript-language-server" in
  if Sys.file_exists npm_local then npm_local ^ " --stdio"
  else if Sys.file_exists npm_global then npm_global ^ " --stdio"
  else "typescript-language-server --stdio"

let clean_hover s =
  (* Step 1: strip leading/trailing whitespace.
   * The hover response starts with \n before the code fence:
   *   "\n```typescript\nconst x: number\n```"
   *   -> "```typescript\nconst x: number\n```"
   *)
  let s = String.trim s in

  (* Step 2: extract content from ```typescript ... ``` or ```javascript ... ```
   * code fences.
   *   "```typescript\nconst x: number\n```\n---\nDocs..."
   *   -> "const x: number\n```\n---\nDocs..."
   * (the closing ``` is removed next)
   *)
  let s =
    if s =~ "^```typescript\n\\(.*\\)" then Common.matched1 s
    else if s =~ "^```javascript\n\\(.*\\)" then Common.matched1 s
    else s
  in
  (* Remove closing ``` at end of lines:
   *   "const x: number\n```\n---\nDocs..."
   *   -> "const x: number\n\n---\nDocs..."
   *)
  let s = Str.global_replace (Str.regexp "```$") "" s in

  (* Step 3: remove documentation after separators.
   *   "const x: number\n\n---\nDocs..."
   *   -> "const x: number"
   *)
  let s =
    match Str.bounded_split (Str.regexp "\n---\n\\|\n\n") s 2 with
    | type_part :: _ -> type_part
    | [] -> s
  in

  (* Step 4: collapse newlines and trim.
   *   "const x: number"  (already single-line in this case)
   *   -> "const x: number"
   *)
  let s = Str.global_replace (Str.regexp "\n") " " s in
  let s = String.trim s in

  (* Step 5: strip kind prefixes like (variable), (property), (parameter), etc.
   *   "(property) x: string"  -> "x: string"
   *   "(variable) x: number"  -> "x: number"
   *   "(method) Console.log(...data: any[]): void"  -> "Console.log(...data: any[]): void"
   *)
  let s =
    if s =~ "^([a-z ]+) \\(.*\\)" then Common.matched1 s
    else s
  in
  let s = String.trim s in

  (* Step 6: extract the type from the declaration.
   *   "const x: string"                              -> "string"
   *   "let x: number"                                -> "number"
   *   "var x: boolean"                               -> "boolean"
   *   "x: string"                                    -> "string"  (after prefix strip)
   *   "function add(a: number, b: number): number"   -> kept as-is for parse_type
   *)
  if s =~ "^function \\|^class " then
    (* Function or class — keep as-is for parse_type *)
    s
  else if s =~ "^const [a-zA-Z_][a-zA-Z_0-9]*: \\(.*\\)" then
    Common.matched1 s
  else if s =~ "^let [a-zA-Z_][a-zA-Z_0-9]*: \\(.*\\)" then
    Common.matched1 s
  else if s =~ "^var [a-zA-Z_][a-zA-Z_0-9]*: \\(.*\\)" then
    Common.matched1 s
  else if s =~ "^[a-zA-Z_][a-zA-Z_0-9]*: \\(.*\\)" then
    Common.matched1 s
  else
    s

(* Try to parse a type string by wrapping it as "($X : TYPE)" and using
 * the menhir JS parser, which produces a TypedMetavar node.
 * This ensures we get the same AST_generic.type_ as the pattern parser
 * produces for ($X : number) etc. *)
let try_parse_type_via_menhir str =
  try
    let wrapped = "($X : " ^ str ^ ")" in
    let js_any = Parse_js.any_of_string wrapped in
    let generic = Js_to_generic.any js_any in
    (match generic with
    | G.E { e = G.TypedMetavar (_, _, ty); _ } -> Some ty
    | _ -> None)
  with _exn -> None

(* Try to parse a function signature via tree-sitter for function types. *)
let try_parse_func_sig str =
  try
    let res = Parse_typescript_tree_sitter.parse_pattern str in
    match res.Tree_sitter_run.Parsing_result.program with
    | Some any ->
        let generic = Js_to_generic.any any in
        (match generic with
        | G.S { G.s = G.DefStmt (_, G.FuncDef { G.frettype = Some ty; _ }); _ } ->
            Some ty
        | _ -> None)
    | None -> None
  with _exn -> None

(* Parse a cleaned hover string into AST_generic.type_.
 * Uses the menhir JS parser (same as the pattern parser) to ensure
 * type AST compatibility with ($X : TYPE) patterns. *)
let parse_type s =
  (* First try parsing as a simple type via menhir *)
  match try_parse_type_via_menhir s with
  | Some ty -> ty
  | None ->
      (* Try parsing function signatures via tree-sitter *)
      (match try_parse_func_sig s with
      | Some ty -> ty
      | None ->
          failwith (spf "LSP_client: cannot parse TypeScript type from: %s" s))

let lsp_lang lang : LSP_lang.t = {
  server_cmd = (fun caps ~root:_ -> server_cmd caps);
  language_id = language_id lang;
  project_root_marker;
  clean_hover;
  parse_type;
  needs_warmup = true;
  init_options = None;
}
