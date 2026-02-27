(** LSP client: connect to ocamllsp to get type information.
 *
 * This module spawns an ocamllsp process and communicates with it over
 * JSON-RPC to resolve types. The resolved types are wired into
 * {!Core_hooks.get_type} (and optionally {!Core_hooks.get_type_of_expr})
 * so the matching engine can use them.
 *)

val debug : bool ref
(** Enable debug output to stderr. *)

val init : ?expr:bool -> unit -> unit
(** Connect to the LSP server and register the {!Core_hooks.get_type} hook.
    If [~expr:true], also register {!Core_hooks.get_type_of_expr} to resolve
    types for arbitrary expressions (not just identifiers).
    Also registers a cleanup function in {!Core_hooks.exit} to shut down
    the server on exit. *)

val get_type : AST_generic.ident -> AST_generic.type_ option
(** Query the LSP server for the type of an identifier at its source location.
    Returns [None] if the server has no type information. *)

val get_type_of_expr : AST_generic.expr -> AST_generic.type_ option
(** Query the LSP server for the type of an expression. Uses the first
    original token of the expression to determine the source position.
    Returns [None] if the server has no type information. *)
