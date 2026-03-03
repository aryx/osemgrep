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

(* PHP (Intelephense) helpers for the LSP client.
 *
 * PHP typed metavars use ($X : TYPE) syntax (colon-separated, like TypeScript)
 * to avoid ambiguity with PHP cast expressions like (int) $x.
 *)

open Common
module G = AST_generic

let project_root_marker = "composer.json"
let language_id = "php"

let server_cmd (_caps : < Cap.exec ; .. >) ~root:_ =
  let home = Sys.getenv "HOME" in
  let npm_local = Filename.concat home ".local/share/npm/bin/intelephense" in
  let npm_global = Filename.concat home ".npm-global/bin/intelephense" in
  if Sys.file_exists npm_local then npm_local ^ " --stdio"
  else if Sys.file_exists npm_global then npm_global ^ " --stdio"
  else "intelephense --stdio"

(* Extract the type string from an Intelephense hover response.
 *
 * coupling: https://intelephense.com/
 *
 * For variables like '$a = 10':
 *   "_@var_ `int $a`"  ->  "int"
 * For method calls like 'Lib::add($a, $b)' hovering on 'add':
 *   "__Lib::add__\n\n```php\n<?php\npublic static function add(int $a, int $b): int { }\n```\n..."
 *   ->  "int" (return type)
 *)
let clean_hover s =
  let s = String.trim s in

  (* Case 1: Variable hover — "_@var_ `TYPE $name`"
   * Intelephense returns this for variables like '$a = 10'. *)
  if s =~ "_@var_ `\\([^ ]+\\) " then
    Common.matched1 s

  (* Case 2: Function/method hover — contains a ```php code fence.
   * E.g. "__Lib::add__\n\n```php\n<?php\npublic static function add(int $a, int $b): int { }\n```\n..."
   * We extract the signature line from the code fence and get the return type.
   * Note: Str.regexp '.' does not match newlines, so we split on lines. *)
  else
    (* Find the line after "<?php" inside the code fence *)
    let lines = String.split_on_char '\n' s in
    let rec find_sig = function
      | "<?php" :: sig_line :: _ -> Some (String.trim sig_line)
      | _ :: rest -> find_sig rest
      | [] -> None
    in
    match find_sig lines with
    | Some sig_ ->
      (* Strip visibility/static modifiers: "public static function ..." -> "function ..." *)
      let sig_ =
        if sig_ =~ "^.*function \\(.*\\)" then
          "function " ^ Common.matched1 sig_
        else sig_
      in
      (* Extract return type from "function name(...): RetType { }" *)
      if sig_ =~ "^function .*): \\([^ {]+\\)" then
        String.trim (Common.matched1 sig_)
      (* Variable declaration: "Type $name" *)
      else if sig_ =~ "^\\(.*\\) \\$[a-zA-Z_]" then
        String.trim (Common.matched1 sig_)
      else
        sig_
    | None -> s

(* Parse a cleaned hover type string by wrapping it as "function foo(TYPE $x) {}"
 * and using the PHP menhir parser pipeline, matching the approach in
 * Parse_metavariable_type.ml line 25. *)
let parse_type s =
  let wrapped = spf "function foo(%s $x) {}" s in
  let any_cst = Parse_php.any_of_string wrapped in
  let any_ast = Ast_php_build.any any_cst in
  let any = Php_to_generic.any any_ast in
  match any with
  | G.S { s = G.DefStmt (_, FuncDef { fparams = (_, Param { ptype = Some t; _ } :: _, _); _ }); _ } -> t
  | _ ->
      failwith (spf "LSP_client: cannot parse PHP type from: %s" s)

let lsp_lang : LSP_lang.t = {
  server_cmd = (fun caps ~root -> server_cmd caps ~root);
  language_id;
  project_root_marker;
  clean_hover;
  parse_type;
  needs_warmup = true;
  init_options = None;
}
