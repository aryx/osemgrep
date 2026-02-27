(** LSP client: connect to ocamllsp to get type information for identifiers.
 *
 * This module spawns an ocamllsp process and communicates with it over
 * JSON-RPC to resolve types. The resolved types are wired into
 * {!Core_hooks.get_type} so the matching engine can use them.
 *)

val debug : bool ref
(** Enable debug output to stderr. *)

val init : unit -> unit
(** Connect to the LSP server and register the {!Core_hooks.get_type} hook.
    Also registers a cleanup function in {!Core_hooks.exit} to shut down
    the server on exit. *)

val get_type : AST_generic.ident -> AST_generic.type_ option
(** Query the LSP server for the type of an identifier at its source location.
    Returns [None] if the server has no type information. *)
