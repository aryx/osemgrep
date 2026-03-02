val parse : Fpath.t -> (AST_zig.program, Parser_zig.token) Parsing_result.t
val parse_program : Fpath.t -> AST_zig.program
val any_of_string : string -> AST_zig.any
