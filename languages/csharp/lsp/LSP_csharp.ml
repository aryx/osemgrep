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

(* C# (OmniSharp) helpers for the LSP client. *)

open Common
module G = AST_generic

(* C# projects use *.csproj or *.sln with varying names, so we use
 * a glob pattern. find_marker_upward in LSP_client.ml supports
 * glob patterns containing '*'. *)
let project_root_marker = "*.csproj"
let language_id = "csharp"

let server_cmd (_caps : < Cap.exec ; .. >) ~root =
  let home = Sys.getenv "HOME" in
  let local_share = Filename.concat home ".local/share/omnisharp/OmniSharp" in
  let dotnet_tools = Filename.concat home ".dotnet/tools/OmniSharp" in
  let bin =
    if Sys.file_exists local_share then local_share
    else if Sys.file_exists dotnet_tools then dotnet_tools
    else "OmniSharp"
  in
  spf "%s -lsp -s %s" bin root

(* Extract the type string from an OmniSharp hover response.
 *
 * coupling: https://github.com/OmniSharp/omnisharp-roslyn/blob/master/src/OmniSharp.Roslyn.CSharp/Services/Navigation/QuickInfoProvider.cs
 *
 * OmniSharp hover format: MarkupContent with ```csharp ... ``` fences.
 *
 * For variables like 'int a = 10':
 *   "```csharp\nint a\n```"  ->  "int"
 * For method calls like 'Lib.Add(a, b)' hovering on 'Add':
 *   "```csharp\nint Lib.Add(int a, int b)\n```"  ->  "int"
 * For properties:
 *   "```csharp\nstring Name { get; set; }\n```"  ->  "string"
 *)
let clean_hover s =
  (* Step 1: strip leading/trailing whitespace. *)
  let s = String.trim s in

  (* Step 2: extract content from ```csharp ... ``` code fences. *)
  let s =
    if s =~ "^```csharp\n\\(.*\\)" then Common.matched1 s
    else s
  in
  let s = Str.global_replace (Str.regexp "```$") "" s in

  (* Step 3: remove documentation after separators. *)
  let s =
    match Str.bounded_split (Str.regexp "\n---\n\\|\n\n") s 2 with
    | type_part :: _ -> type_part
    | [] -> s
  in

  (* Step 4: collapse newlines and trim. *)
  let s = Str.global_replace (Str.regexp "\n") " " s in
  let s = String.trim s in

  (* Step 5: strip kind prefixes like (local variable), (parameter), (field).
   *   "(local variable) int a"  -> "int a"
   *   "(parameter) string name" -> "string name"
   *)
  let s =
    if s =~ "^([a-z ]+) \\(.*\\)" then Common.matched1 s
    else s
  in
  let s = String.trim s in

  (* Step 6: extract the type from the declaration.
   *
   * Method signatures: "int Lib.Add(int a, int b)" -> "int"
   *   (return type is everything before the qualified method name)
   *
   * Variable: "int a" -> "int"
   *   (type is the first token(s) before the identifier)
   *
   * Property: "string Name { get; set; }" -> "string"
   *   (type is before the property name)
   *)
  (* Property: "Type Name { ... }" *)
  if s =~ "^\\(.*\\) [a-zA-Z_][a-zA-Z_0-9]* {" then
    String.trim (Common.matched1 s)
  (* Method signature: "RetType Name.Method(...)" or "RetType Method(...)" *)
  else if s =~ "^\\(.*\\) [a-zA-Z_][a-zA-Z_0-9.]*(" then
    String.trim (Common.matched1 s)
  (* Variable/field: "Type name" *)
  else if s =~ "^\\(.*\\) [a-zA-Z_][a-zA-Z_0-9]*$" then
    String.trim (Common.matched1 s)
  else
    s

(* Parse a cleaned hover type string by wrapping it as "(TYPE $X)" and
 * using the tree-sitter C# parser, which produces a TypedMetavar node.
 * The tree-sitter C# grammar requires a $-prefixed semgrep metavariable
 * for typed_metavariable nodes, so we use "$X" (not a bare identifier). *)
let parse_type s =
  let wrapped = "(" ^ s ^ " $X)" in
  let res = Parse_csharp_tree_sitter.parse_pattern wrapped in
  match res.Tree_sitter_run.Parsing_result.program with
  | Some (G.E { e = G.TypedMetavar (_, _, ty); _ }) -> ty
  | _ ->
      failwith (spf "LSP_client: cannot parse C# type from: %s" s)

let lsp_lang : LSP_lang.t = {
  server_cmd = (fun caps ~root -> server_cmd caps ~root);
  language_id;
  project_root_marker;
  clean_hover;
  parse_type;
  needs_warmup = true;
  init_options = None;
}
