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

(* C/C++ (clangd) helpers for the LSP client.
 *
 * Extract the type string from a clangd hover response and parse it
 * into AST_generic.type_.
 *
 * coupling: the HoverInfo struct is defined in:
 *   https://github.com/llvm/llvm-project/blob/main/clang-tools-extra/clangd/Hover.h
 * The text is rendered by HoverInfo::present() / presentDefault() in:
 *   https://github.com/llvm/llvm-project/blob/main/clang-tools-extra/clangd/Hover.cpp
 * presentDefault() formats the HoverInfo fields:
 * - "Type: TYPE" from the HoverInfo::Type field (for variables)
 * - "→ TYPE" from the HoverInfo::ReturnType field (for functions)
 * - "Parameters:" with bullet list from HoverInfo::Parameters
 *
 * clangd hover format (MarkupKind = plaintext, not markdown fences):
 *
 * For variables like 'int x = 42':
 *   "variable x\n\nType: int\nValue = 42 (0x2a)\n\n// In main\nint x = 42"
 *   We extract: "int"  (from the "Type: " line)
 *
 * For pointers like 'char *p':
 *   "variable p\n\nType: char *\n\n// In main\nchar *p"
 *   We extract: "char *"
 *
 * For functions like 'int add(int a, int b)':
 *   "function add\n\n→ int\nParameters:\n- int a\n- int b\n..."
 *   We extract: "int"  (from the "→ " line = return type)
 *
 * For macros or other symbols without type info:
 *   "macro FOO\n\n#define FOO 42"
 *   We return: the first line as fallback
 *)

open Common
module G = AST_generic

let project_root_marker = "compile_commands.json"

let language_id lang =
  match lang with
  | Lang.C -> "c"
  | Lang.Cpp -> "cpp"
  | _ -> failwith "LSP_c: unsupported language"

(* clangd is typically in PATH; uses compile_commands.json for
 * project-specific flags. Handles both C and C++. *)
let server_cmd (_caps : < Cap.exec ; .. >) = "clangd"

let clean_hover s =
  let lines = String.split_on_char '\n' s in
  (* Look for "Type: TYPE" (variables) or "→ TYPE" (functions) *)
  let type_line =
    List_.find_some_opt (fun line ->
      let line = String.trim line in
      if line =~ "^Type: \\(.*\\)" then Some (Common.matched1 line)
      (* clangd uses the arrow "→" (UTF-8: \xe2\x86\x92) for return types *)
      else if line =~ "^\xe2\x86\x92 \\(.*\\)" then Some (Common.matched1 line)
      else None
    ) lines
  in
  match type_line with
  | Some s -> String.trim s
  | None ->
      (* Fallback: return the first line, which is "variable x" or
       * "function foo" — parse_type will likely fail on this but
       * at least the debug output will be informative *)
      (match lines with
      | first :: _ -> String.trim first
      | [] -> s)

(* Try to parse a C declaration string and extract the type.
 * Returns None if the string cannot be parsed. *)
let try_parse_c_decl str =
  try
    let any = Parse_c.any_of_string str in
    match any with
    | Ast_c.Stmt (Ast_c.Vars ({ v_type; _ } :: _)) ->
        (match C_to_generic.any (Ast_c.Type v_type) with
        | G.T ty -> Some ty
        | _ -> None)
    | Ast_c.Stmt (Ast_c.DefStmt (Ast_c.VarDef { v_type; _ })) ->
        (match C_to_generic.any (Ast_c.Type v_type) with
        | G.T ty -> Some ty
        | _ -> None)
    | Ast_c.Stmt (Ast_c.DefStmt (Ast_c.Prototype { f_type; _ })) ->
        let ftype = Ast_c.TFunction f_type in
        (match C_to_generic.any (Ast_c.Type ftype) with
        | G.T ty -> Some ty
        | _ -> None)
    | _ -> None
  with _exn -> None

(* Parse a cleaned clangd hover string into AST_generic.type_.
 * First tries "DECL;" (e.g. "int x;"), then falls back to
 * "TYPE _x;" (e.g. "int _x;") for bare type strings. *)
let parse_type s =
  match try_parse_c_decl (s ^ ";") with
  | Some ty -> ty
  | None ->
      (* Fallback: wrap as "TYPE _x;" for bare type strings *)
      (match try_parse_c_decl (s ^ " _x;") with
      | Some ty -> ty
      | None ->
          failwith (spf "LSP_client: cannot parse C type from: %s" s))

let lsp_lang lang : LSP_lang.t = {
  server_cmd = (fun caps ~root:_ -> server_cmd caps);
  language_id = language_id lang;
  project_root_marker;
  clean_hover;
  parse_type;
  needs_warmup = false;
  init_options = None;
}
