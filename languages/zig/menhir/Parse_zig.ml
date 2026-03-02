(* Claude Code
 *
 * Copyright (C) 2024-2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public License
 * version 2.1 as published by the Free Software Foundation, with the
 * special exception on linking described in file LICENSE.
 *
 * This library is distributed in the hope that it will be useful, but
 * WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the file
 * LICENSE for more details.
 *)
open Fpath_.Operators
module Flag = Flag_parsing
module TH = Token_helpers_zig
module Lexer = Lexer_zig
module Log = Log_lib_parsing.Log

(*****************************************************************************)
(* Error diagnostic *)
(*****************************************************************************)
let error_msg_tok tok = Parsing_helpers.error_message_info (TH.info_of_tok tok)

(*****************************************************************************)
(* Lexing only *)
(*****************************************************************************)

let tokens input_source =
  let token lexbuf = Lexer.token lexbuf in
  Parsing_helpers.tokenize_all_and_adjust_pos input_source token
    TH.visitor_info_of_tok TH.is_eof
[@@profiling]

(*****************************************************************************)
(* Main entry point *)
(*****************************************************************************)
let parse filename =
  let toks_orig = tokens (Parsing_helpers.file !!filename) in
  let toks = List_.exclude TH.is_comment_or_space toks_orig in
  let toks = Parsing_hacks_zig.fix_tokens toks in
  let tr, lexer, lexbuf_fake =
    Parsing_helpers.mk_lexer_for_yacc toks TH.is_irrelevant
  in

  try
    let xs =
      Profiling.profile_code "Parser_zig.file" (fun () ->
          Parser_zig.file lexer lexbuf_fake)
    in
    {
      Parsing_result.ast = xs;
      tokens = toks_orig;
      stat = Parsing_stat.correct_stat filename;
    }
  with
  | Parsing.Parse_error ->
      let cur = tr.Parsing_helpers.current in
      if not !Flag.error_recovery then
        raise (Parsing_error.Syntax_error (TH.info_of_tok cur));

      if !Flag.show_parsing_error then (
        Log.err (fun m -> m "parse error \n = %s" (error_msg_tok cur));
        let filelines = UFile.cat_array filename in
        let checkpoint2 = UFile.cat filename |> List.length in
        let line_error = Tok.line_of_tok (TH.info_of_tok cur) in
        Log.err (fun m ->
            m "%s"
              (Parsing_helpers.show_parse_error_line line_error (0, checkpoint2)
                 filelines)));
      {
        Parsing_result.ast = [];
        tokens = toks_orig;
        stat = Parsing_stat.bad_stat filename;
      }
[@@profiling]

let parse_program file =
  let res = parse file in
  res.Parsing_result.ast

(*****************************************************************************)
(* Sub parsers *)
(*****************************************************************************)

let any_of_string s =
  Common.save_excursion Flag_parsing.sgrep_mode true (fun () ->
      let toks_orig = tokens (Parsing_helpers.Str s) in
      let toks = List_.exclude TH.is_comment_or_space toks_orig in
      let toks = Parsing_hacks_zig.fix_tokens toks in
      let tr, lexer, lexbuf_fake =
        Parsing_helpers.mk_lexer_for_yacc toks TH.is_irrelevant
      in
      try Parser_zig.sgrep_spatch_pattern lexer lexbuf_fake with
      | Parsing.Parse_error ->
          let cur = tr.Parsing_helpers.current in
          Log.err (fun m -> m "parse error \n = %s" (error_msg_tok cur));
          raise (Parsing_error.Syntax_error (TH.info_of_tok cur)))
