(** C# (OmniSharp) helpers for the LSP client. *)

val project_root_marker : string
val language_id : string
val server_cmd : < Cap.exec ; .. > -> root:string -> string
val clean_hover : string -> string
val parse_type : string -> AST_generic.type_

val lsp_lang : LSP_lang.t
