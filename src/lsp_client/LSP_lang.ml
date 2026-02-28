(* Per-language LSP helpers record type.
 *
 * Each supported language (OCaml, C, Go, ...) provides a value of this
 * type so that LSP_client can dispatch without repeating per-language
 * match statements.
 *
 * coupling: when adding a new language, also update the LSP server
 * detection section at the end of ./configure so that it checks for
 * the new server binary (and any non-PATH locations like ~/.cargo/bin).
 *)

type t = {
  (* Command to start the language server, e.g. "ocamllsp", "clangd", "gopls".
   * Takes an exec capability because some implementations (e.g. LSP_ocaml)
   * need to run shell commands to locate the server binary. *)
  server_cmd : < Cap.exec > -> string;
  (* LSP languageId for didOpen notifications, e.g. "ocaml", "c", "go" *)
  language_id : string;
  (* File whose presence marks the project root, e.g. "dune-project", "go.mod" *)
  project_root_marker : string;
  (* Extract the type string from a hover response.
   * e.g. "```ocaml\nval f : int -> int\n```\n---\ndoc" -> "val f : int -> int" *)
  clean_hover : string -> string;
  (* Parse a cleaned type string into the generic AST.
   * e.g. "int -> int" -> TyFun([TyN "int"], TyN "int") *)
  parse_type : string -> AST_generic.type_;
  (* Whether the server needs time to index after initialization.
   * When true, LSP_client will drain progress notifications after
   * didOpen before sending hover requests.  Needed for rust-analyzer
   * which loads cargo metadata asynchronously. *)
  needs_warmup : bool;
}
