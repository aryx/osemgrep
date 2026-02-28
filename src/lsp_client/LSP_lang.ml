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
   * need to run shell commands to locate the server binary.
   * The second argument is the project root directory, needed by some
   * servers (e.g. OmniSharp uses -s to scope project scanning). *)
  server_cmd : < Cap.exec > -> root:string -> string;
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
  (* Optional JSON to pass as initializationOptions in the Initialize
   * request.  Used by Metals to suppress browser opens (doctorProvider)
   * and other server-specific settings.  Default: None. *)
  init_options : Yojson.Safe.t option;
}
