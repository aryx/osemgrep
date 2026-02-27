(* Per-language LSP helpers record type.
 *
 * Each supported language (OCaml, C, Go, ...) provides a value of this
 * type so that LSP_client can dispatch without repeating per-language
 * match statements.
 *)

type t = {
  server_cmd : unit -> string;
  language_id : string;
  project_root_marker : string;
  clean_hover : string -> string;
  parse_type : string -> AST_generic.type_;
}
