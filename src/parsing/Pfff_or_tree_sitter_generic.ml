(* Yoann Padioleau
 *
 * Copyright (C) 2019-2023 Semgrep Inc.
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
open Pfff_or_tree_sitter
module Flag = Flag_semgrep

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(* claude: the AST_generic-dependent part of what used to be
 * src/parsing/Pfff_or_tree_sitter.ml. The rest (run_either, run_pattern,
 * throw_tokens, etc.) now lives in semgrep-pfff-langs in
 * parsing_infra/parsing_tree_sitter/Pfff_or_tree_sitter.ml, which does not
 * depend on AST_generic nor Parsing_result2 (see also the similar 'run'
 * local helper in semgrep-pfff-langs/parsing/Parse_generic.ml).
 *
 * Note that we still use Flag_semgrep.tree_sitter_only and pfff_only
 * (set by -tree_sitter_only and -pfff_only in Core_CLI.ml and used in
 * tests) to filter the parsers, instead of the flags with the same name
 * in Pfff_or_tree_sitter, so that src/configuring/ does not have to depend
 * on semgrep-pfff-langs.
 *)

(*****************************************************************************)
(* Entry point *)
(*****************************************************************************)

let (run :
      Fpath.t ->
      'ast parser list ->
      ('ast -> AST_generic.program) ->
      Parsing_result2.t) =
 fun file xs fconvert ->
  let xs =
    match () with
    | () when !Flag.tree_sitter_only ->
        xs
        |> List_.exclude (function
             | Pfff _ -> true
             | TreeSitter _ -> false)
    | () when !Flag.pfff_only ->
        xs
        |> List_.exclude (function
             | TreeSitter _ -> true
             | Pfff _ -> false)
    | () -> xs
  in
  match run_either_filtered file xs with
  | ResOk (ast, stat, tolerable_errors) ->
      Parsing_result2.ok (fconvert ast) stat tolerable_errors
  | ResPartial (ast, stat, errors) ->
      Parsing_result2.partial (fconvert ast) stat errors
  | ResError e -> Exception.reraise e

(*****************************************************************************)
(* Other helpers *)
(*****************************************************************************)

(* Simplified version of 'run' that allows for plugins to hide the
   intermediate AST type. *)
let run_external_parser (file : Fpath.t)
    (parse :
      Fpath.t -> (AST_generic.program, unit) Tree_sitter_run.Parsing_result.t) :
    Parsing_result2.t =
  run file [ TreeSitter parse ] (fun ast -> ast)
