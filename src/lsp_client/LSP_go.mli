(** Go (gopls) helpers for the LSP client. *)

val project_root_marker : string
val language_id : string
val server_cmd : unit -> string
val clean_hover : string -> string
val parse_type : string -> AST_generic.type_
