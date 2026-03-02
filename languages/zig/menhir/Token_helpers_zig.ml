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
open Parser_zig
module PI = Lib_ast_fuzzy

(*****************************************************************************)
(* Token Helpers *)
(*****************************************************************************)
let is_eof = function
  | EOF _ -> true
  | _ -> false

let is_irrelevant = function
  | TComment _
  | TCommentSpace _
  | TCommentNewline _ ->
      true
  | _ -> false

let is_comment_or_space = function
  | TComment _
  | TCommentSpace _ ->
      true
  | _ -> false

let token_kind_of_tok t =
  match t with
  | LBRACE _ -> PI.LBrace
  | RBRACE _ -> PI.RBrace
  | LPAREN _ -> PI.LPar
  | RPAREN _ -> PI.RPar
  | LBRACKET _ -> PI.LBracket
  | RBRACKET _ -> PI.RBracket
  | TComment _ -> PI.Esthet PI.Comment
  | TCommentSpace _ -> PI.Esthet PI.Space
  | TCommentNewline _ -> PI.Esthet PI.Newline
  | _ -> PI.Other

(*****************************************************************************)
(* Visitors *)
(*****************************************************************************)

let visitor_info_of_tok f = function
  | TUnknown ii -> TUnknown (f ii)
  | EOF ii -> EOF (f ii)
  | TCommentSpace ii -> TCommentSpace (f ii)
  | TComment ii -> TComment (f ii)
  | TCommentNewline ii -> TCommentNewline (f ii)
  (* Literals *)
  | LINT pi -> LINT (Parsed_int.map_tok f pi)
  | LFLOAT (s, ii) -> LFLOAT (s, f ii)
  | LSTR (s, ii) -> LSTR (s, f ii)
  | LCHAR (s, ii) -> LCHAR (s, f ii)
  (* Identifiers *)
  | IDENT (s, ii) -> IDENT (s, f ii)
  | BUILTIN (s, ii) -> BUILTIN (s, f ii)
  (* Keywords *)
  | CONST ii -> CONST (f ii)
  | VAR ii -> VAR (f ii)
  | PUB ii -> PUB (f ii)
  | FN ii -> FN (f ii)
  | RETURN ii -> RETURN (f ii)
  | IF ii -> IF (f ii)
  | ELSE ii -> ELSE (f ii)
  | WHILE ii -> WHILE (f ii)
  | FOR ii -> FOR (f ii)
  | SWITCH ii -> SWITCH (f ii)
  | BREAK ii -> BREAK (f ii)
  | CONTINUE ii -> CONTINUE (f ii)
  | DEFER ii -> DEFER (f ii)
  | ERRDEFER ii -> ERRDEFER (f ii)
  | TRY ii -> TRY (f ii)
  | CATCH ii -> CATCH (f ii)
  | ORELSE ii -> ORELSE (f ii)
  | COMPTIME ii -> COMPTIME (f ii)
  | TEST ii -> TEST (f ii)
  | STRUCT ii -> STRUCT (f ii)
  | ENUM ii -> ENUM (f ii)
  | UNION ii -> UNION (f ii)
  | ERROR ii -> ERROR (f ii)
  | TRUE ii -> TRUE (f ii)
  | FALSE ii -> FALSE (f ii)
  | NULL ii -> NULL (f ii)
  | UNDEFINED ii -> UNDEFINED (f ii)
  | UNREACHABLE ii -> UNREACHABLE (f ii)
  | INLINE ii -> INLINE (f ii)
  | EXTERN ii -> EXTERN (f ii)
  | EXPORT ii -> EXPORT (f ii)
  | NOALIAS ii -> NOALIAS (f ii)
  | ASM ii -> ASM (f ii)
  | VOLATILE ii -> VOLATILE (f ii)
  | USINGNAMESPACE ii -> USINGNAMESPACE (f ii)
  | AND ii -> AND (f ii)
  | OR ii -> OR (f ii)
  | ASYNC ii -> ASYNC (f ii)
  | AWAIT ii -> AWAIT (f ii)
  | RESUME ii -> RESUME (f ii)
  | SUSPEND ii -> SUSPEND (f ii)
  | NOSUSPEND ii -> NOSUSPEND (f ii)
  | THREADLOCAL ii -> THREADLOCAL (f ii)
  | OPAQUE ii -> OPAQUE (f ii)
  | ANYERROR ii -> ANYERROR (f ii)
  | PACKED ii -> PACKED (f ii)
  | ANYTYPE ii -> ANYTYPE (f ii)
  | TYPE ii -> TYPE (f ii)
  | COMPTIME_MOD ii -> COMPTIME_MOD (f ii)
  | IF_EXPR ii -> IF_EXPR (f ii)
  | SWITCH_EXPR ii -> SWITCH_EXPR (f ii)
  | LBRACE_INIT ii -> LBRACE_INIT (f ii)
  | STRUCT_EXPR ii -> STRUCT_EXPR (f ii)
  | ENUM_EXPR ii -> ENUM_EXPR (f ii)
  | UNION_EXPR ii -> UNION_EXPR (f ii)
  | ERROR_EXPR ii -> ERROR_EXPR (f ii)
  (* Operators and punctuation *)
  | PLUS ii -> PLUS (f ii)
  | MINUS ii -> MINUS (f ii)
  | STAR ii -> STAR (f ii)
  | SLASH ii -> SLASH (f ii)
  | PERCENT ii -> PERCENT (f ii)
  | PLUSEQ ii -> PLUSEQ (f ii)
  | MINUSEQ ii -> MINUSEQ (f ii)
  | STAREQ ii -> STAREQ (f ii)
  | SLASHEQ ii -> SLASHEQ (f ii)
  | PERCENTEQ ii -> PERCENTEQ (f ii)
  | AMP ii -> AMP (f ii)
  | PIPE ii -> PIPE (f ii)
  | CARET ii -> CARET (f ii)
  | TILDE ii -> TILDE (f ii)
  | AMPEQ ii -> AMPEQ (f ii)
  | PIPEEQ ii -> PIPEEQ (f ii)
  | CARETEQ ii -> CARETEQ (f ii)
  | LSHIFT ii -> LSHIFT (f ii)
  | RSHIFT ii -> RSHIFT (f ii)
  | LSHIFTEQ ii -> LSHIFTEQ (f ii)
  | RSHIFTEQ ii -> RSHIFTEQ (f ii)
  | EQEQ ii -> EQEQ (f ii)
  | BANGEQ ii -> BANGEQ (f ii)
  | LE ii -> LE (f ii)
  | GE ii -> GE (f ii)
  | LT ii -> LT (f ii)
  | GT ii -> GT (f ii)
  | EQ ii -> EQ (f ii)
  | BANG ii -> BANG (f ii)
  | AMPAMP ii -> AMPAMP (f ii)
  | PIPEPIPE ii -> PIPEPIPE (f ii)
  | PLUSPLUS ii -> PLUSPLUS (f ii)
  | LPAREN ii -> LPAREN (f ii)
  | RPAREN ii -> RPAREN (f ii)
  | LBRACKET ii -> LBRACKET (f ii)
  | RBRACKET ii -> RBRACKET (f ii)
  | LBRACE ii -> LBRACE (f ii)
  | RBRACE ii -> RBRACE (f ii)
  | COLON ii -> COLON (f ii)
  | SEMICOLON ii -> SEMICOLON (f ii)
  | DOT ii -> DOT (f ii)
  | COMMA ii -> COMMA (f ii)
  | QUESTION ii -> QUESTION (f ii)
  | DOTDOT ii -> DOTDOT (f ii)
  | FATARROW ii -> FATARROW (f ii)
  | ELLIPSIS ii -> ELLIPSIS (f ii)
  | LDots ii -> LDots (f ii)
  | RDots ii -> RDots (f ii)

let info_of_tok tok =
  let res = ref None in
  visitor_info_of_tok
    (fun ii ->
      res := Some ii;
      ii)
    tok
  |> ignore;
  match !res with
  | Some x -> x
  | None -> Tok.unsafe_fake_tok "NOTOK"
