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

(* Java (jdtls / Eclipse JDT Language Server) helpers for the LSP client.
 *
 * Extract the type string from a jdtls hover response and parse it
 * into AST_generic.type_.
 *
 * Java typed metavars use (TYPE $X) syntax — parsed by the menhir Java
 * parser as TypedMetavar nodes (see Parse_metavariable_type.ml).
 *
 * jdtls hover format: MarkedString list (not MarkupContent).
 * The first MarkedString has language="java" and value like:
 *
 * For variables like 'int a = 10':
 *   "int a - Main.main(String[])"
 *   We extract: "int"
 *
 * For method calls like 'Lib.add(a, b)' hovering on 'add':
 *   "int Lib.add(int a, int b)"
 *   We extract: "int"
 *
 * For static method references like 'Lib.add':
 *   "int Lib.add(int a, int b)"
 *   We extract: "int"
 *
 * Empty string for some expressions (e.g. hovering on 'Lib' in 'Lib.add').
 *)

open Common
module G = AST_generic

let project_root_marker = "pom.xml"
let language_id = "java"

let server_cmd (_caps : < Cap.exec ; .. >) = "jdtls"

let clean_hover s =
  (* Step 1: strip leading/trailing whitespace. *)
  let s = String.trim s in

  (* Step 2: extract content from ```java ... ``` code fences,
   * in case jdtls wraps in markdown. *)
  let s =
    if s =~ "^```java\n\\(.*\\)" then Common.matched1 s
    else s
  in
  let s = Str.global_replace (Str.regexp "```$") "" s in

  (* Step 3: remove the " - Context" suffix.
   * jdtls appends " - Class.method(params)" after variable declarations.
   *   "int a - Main.main(String[])"  ->  "int a"
   *)
  let s =
    match Str.bounded_split (Str.regexp " - ") s 2 with
    | decl :: _ -> decl
    | [] -> s
  in

  (* Step 4: remove documentation after separators (javadoc, etc.). *)
  let s =
    match Str.bounded_split (Str.regexp "\n---\n\\|\n\n") s 2 with
    | type_part :: _ -> type_part
    | [] -> s
  in

  (* Step 5: collapse newlines and trim. *)
  let s = Str.global_replace (Str.regexp "\n") " " s in
  let s = String.trim s in

  (* Step 6: extract the type from the declaration.
   *
   * Method signatures: "int Lib.add(int a, int b)" -> "int"
   *   (return type is everything before the qualified method name)
   *
   * Variable: "int a" -> "int"
   *   (type is the first token(s) before the identifier)
   *
   * Simple type (just "int", "String", etc.) -> kept as-is
   *)
  (* Method signature: "RetType Name.method(...)" or "RetType method(...)" *)
  if s =~ "^\\(.*\\) [a-zA-Z_][a-zA-Z_0-9.]*(" then
    String.trim (Common.matched1 s)
  (* Variable/field: "Type name" *)
  else if s =~ "^\\(.*\\) [a-zA-Z_][a-zA-Z_0-9]*$" then
    String.trim (Common.matched1 s)
  else
    s

(* Parse a cleaned hover type string by wrapping it as "(TYPE _x)" and
 * using the menhir Java parser, which produces a TypedMetavar node.
 * This matches how Parse_metavariable_type.ml wraps Java types. *)
let try_parse_type_via_menhir str =
  try
    let wrapped = "(" ^ str ^ " _x)" in
    let java_any = Parse_java.any_of_string wrapped in
    let generic = Java_to_generic.any java_any in
    (match generic with
    | G.E { e = G.TypedMetavar (_, _, ty); _ } -> Some ty
    | _ -> None)
  with _exn -> None

(* Parse a cleaned hover string into AST_generic.type_. *)
let parse_type s =
  match try_parse_type_via_menhir s with
  | Some ty -> ty
  | None ->
      failwith (spf "LSP_client: cannot parse Java type from: %s" s)

let lsp_lang : LSP_lang.t = {
  server_cmd = (fun caps ~root:_ -> server_cmd caps);
  language_id;
  project_root_marker;
  clean_hover;
  parse_type;
  needs_warmup = true;
  init_options = None;
}
