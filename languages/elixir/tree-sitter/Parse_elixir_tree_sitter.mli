val parse :
  Fpath.t -> (AST_elixir.program, unit) Tree_sitter_run.Parsing_result.t

val parse_pattern : string -> (AST_elixir.any, unit) Tree_sitter_run.Parsing_result.t
