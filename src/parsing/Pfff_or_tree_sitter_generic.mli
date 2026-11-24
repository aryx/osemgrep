(* claude: the AST_generic-dependent part of what used to be
 * src/parsing/Pfff_or_tree_sitter.mli; the rest is now in
 * Pfff_or_tree_sitter in semgrep-pfff-langs (parsing_tree_sitter library).
 *)

(* usage:
    run file [
        TreeSitter (Parse_typescript_tree_sitter.parse);
        Pfff (throw_tokens Parse_js.parse);
     ] Js_to_generic.program
*)
val run :
  Fpath.t ->
  'ast Pfff_or_tree_sitter.parser list ->
  ('ast -> AST_generic.program) ->
  Parsing_result2.t

val run_external_parser :
  Fpath.t ->
  (Fpath.t -> (AST_generic.program, unit) Tree_sitter_run.Parsing_result.t) ->
  Parsing_result2.t
