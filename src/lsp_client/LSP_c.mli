(** C/C++ (clangd) helpers for the LSP client. *)

val clean_hover : string -> string
val parse_type : string -> AST_generic.type_
