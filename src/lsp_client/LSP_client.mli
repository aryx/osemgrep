(** LSP client: connect to a language server to get type information.
 *
 * This module spawns a language server process (e.g., ocamllsp, clangd)
 * and communicates with it over JSON-RPC to resolve types. The resolved
 * types are wired into {!Core_hooks.get_type} (and optionally
 * {!Core_hooks.get_type_of_expr}) so the matching engine can use them.
 *
 * Supported languages: OCaml (ocamllsp), C (clangd), C++ (clangd), Go (gopls).
 *)

val debug : bool ref
(** Enable debug output to stderr. *)

val init : ?lang:Lang.t -> ?expr:bool -> ?roots:string list -> unit -> unit
(** Connect to the LSP server for [lang] and register the
    {!Core_hooks.get_type} hook.
    [lang] defaults to [Lang.Ocaml].
    If [~expr:true], also register {!Core_hooks.get_type_of_expr} to resolve
    types for arbitrary expressions (not just identifiers).
    [~roots] are the scanning roots used to find the project root
    (e.g., the directory containing [go.mod] for Go, [dune-project] for
    OCaml). Defaults to CWD if empty or no marker is found.
    Also registers a cleanup function in {!Core_hooks.exit} to shut down
    the server on exit. *)

val get_type : AST_generic.ident -> AST_generic.type_ option
(** Query the LSP server for the type of an identifier at its source location.
    Returns [None] if the server has no type information. *)

val get_type_of_expr : AST_generic.expr -> AST_generic.type_ option
(** Query the LSP server for the type of an expression. Uses the first
    original token of the expression to determine the source position.
    Returns [None] if the server has no type information. *)
