val parse :
  Fpath.t -> (AST_haskell.program, unit) Tree_sitter_run.Parsing_result.t

val parse_pattern :
  string -> (AST_haskell.any, unit) Tree_sitter_run.Parsing_result.t
