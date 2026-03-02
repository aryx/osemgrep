{
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
module Flag = Flag_parsing

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)

let tok = Lexing.lexeme
let tokinfo = Tok.tok_of_lexbuf
let error = Parsing_error.lexical_error

}

(*****************************************************************************)
(* UTF-8 boilerplate *)
(*****************************************************************************)

(* 0xxxxxxx *)
let ascii = ['\000'-'\127']

(* 110xxxxx *)
let utf8_head_byte2 = ['\192'-'\223']

(* 1110xxxx *)
let utf8_head_byte3 = ['\224'-'\239']

(* 11110xxx *)
let utf8_head_byte4 = ['\240'-'\247']

(* 10xxxxxx *)
let utf8_tail_byte = ['\128'-'\191']

let utf8_1 = ascii
let utf8_2 = utf8_head_byte2 utf8_tail_byte
let utf8_3 = utf8_head_byte3 utf8_tail_byte utf8_tail_byte
let utf8_4 = utf8_head_byte4 utf8_tail_byte utf8_tail_byte utf8_tail_byte

let utf8 = utf8_1 | utf8_2 | utf8_3 | utf8_4
let utf8_nonascii = utf8_2 | utf8_3 | utf8_4

(*****************************************************************************)
(* Regexp aliases *)
(*****************************************************************************)

let newline = ('\n' | "\r\n")
let whitespace = [' ' '\t']

let unicode_digit = ['0'-'9']
let unicode_letter = ['a'-'z' 'A'-'Z']

let unicode_char_no_double_quote =
  ascii # ['\n' '\r' '"' '\\']
| utf8_nonascii

let letter = unicode_letter | '_'
let identifier = letter (letter | unicode_digit)*

let decimal_digit = ['0'-'9']
let hex_digit = ['0'-'9' 'a'-'f' 'A'-'F']
let octal_digit = ['0'-'7']
let binary_digit = ['0'-'1']

let decimal_digits = decimal_digit ('_'? decimal_digit)*
let hex_digits = hex_digit ('_'? hex_digit)*
let octal_digits = octal_digit ('_'? octal_digit)*
let binary_digits = binary_digit ('_'? binary_digit)*

let decimal_lit = "0" | ['1'-'9'] ('_'? decimal_digits)?
let hex_lit = "0" ['x' 'X'] '_'? hex_digits
let octal_lit = "0" ['o' 'O'] '_'? octal_digits
let binary_lit = "0" ['b' 'B'] '_'? binary_digits

let int_lit =
   decimal_lit
 | hex_lit
 | octal_lit
 | binary_lit

let decimal_exponent = ['e' 'E'] ['+' '-']? decimal_digits
let float_lit =
   decimal_digits '.' decimal_digits decimal_exponent?
 | decimal_digits decimal_exponent

let escaped_char = '\\' ['n' 'r' 't' '\\' '\'' '"' '0']
let hex_escape = '\\' 'x' hex_digit hex_digit
let unicode_escape = '\\' 'u' '{' hex_digit+ '}'

let string_char = unicode_char_no_double_quote | escaped_char | hex_escape | unicode_escape

(* semgrep: support any escape in sgrep mode *)
let semgrep_escapeseq = '\\' _

(*****************************************************************************)
(* Rule initial *)
(*****************************************************************************)

rule token = parse

  (* ----------------------------------------------------------------------- *)
  (* spacing/comments *)
  (* ----------------------------------------------------------------------- *)
  | "//" [^ '\r' '\n']* { TComment (tokinfo lexbuf) }
  | newline     { TCommentNewline (tokinfo lexbuf) }
  | whitespace+ { TCommentSpace (tokinfo lexbuf) }

  (* ----------------------------------------------------------------------- *)
  (* symbols *)
  (* ----------------------------------------------------------------------- *)

  | '+'     { PLUS (tokinfo lexbuf) }
  | '-'     { MINUS (tokinfo lexbuf) }
  | '*'     { STAR (tokinfo lexbuf) }
  | '/'     { SLASH (tokinfo lexbuf) }
  | '%'     { PERCENT (tokinfo lexbuf) }

  | "+="    { PLUSEQ (tokinfo lexbuf) }
  | "-="    { MINUSEQ (tokinfo lexbuf) }
  | "*="    { STAREQ (tokinfo lexbuf) }
  | "/="    { SLASHEQ (tokinfo lexbuf) }
  | "%="    { PERCENTEQ (tokinfo lexbuf) }

  | '&'     { AMP (tokinfo lexbuf) }
  | '|'     { PIPE (tokinfo lexbuf) }
  | '^'     { CARET (tokinfo lexbuf) }
  | '~'     { TILDE (tokinfo lexbuf) }

  | "&="    { AMPEQ (tokinfo lexbuf) }
  | "|="    { PIPEEQ (tokinfo lexbuf) }
  | "^="    { CARETEQ (tokinfo lexbuf) }

  | "<<"    { LSHIFT (tokinfo lexbuf) }
  | ">>"    { RSHIFT (tokinfo lexbuf) }
  | "<<="   { LSHIFTEQ (tokinfo lexbuf) }
  | ">>="   { RSHIFTEQ (tokinfo lexbuf) }

  | "=="    { EQEQ (tokinfo lexbuf) }
  | "!="    { BANGEQ (tokinfo lexbuf) }
  | "<="    { LE (tokinfo lexbuf) }
  | ">="    { GE (tokinfo lexbuf) }
  | '<'     { LT (tokinfo lexbuf) }
  | '>'     { GT (tokinfo lexbuf) }

  | "=>"    { FATARROW (tokinfo lexbuf) }
  | '='     { EQ (tokinfo lexbuf) }

  | '!'     { BANG (tokinfo lexbuf) }

  | "&&"    { AMPAMP (tokinfo lexbuf) }
  | "||"    { PIPEPIPE (tokinfo lexbuf) }

  | "++"    { PLUSPLUS (tokinfo lexbuf) }

  | '(' { LPAREN (tokinfo lexbuf) }   | ')' { RPAREN (tokinfo lexbuf) }
  | '[' { LBRACKET (tokinfo lexbuf) } | ']' { RBRACKET (tokinfo lexbuf) }
  | '{' { LBRACE (tokinfo lexbuf) }   | '}' { RBRACE (tokinfo lexbuf) }

  | ':'     { COLON (tokinfo lexbuf) }
  | ';'     { SEMICOLON (tokinfo lexbuf) }
  | '.'     { DOT (tokinfo lexbuf) }
  | ','     { COMMA (tokinfo lexbuf) }
  | '?'     { QUESTION (tokinfo lexbuf) }

  | ".."    { DOTDOT (tokinfo lexbuf) }

  (* sgrep-ext: *)
  | "..."   { ELLIPSIS (tokinfo lexbuf) }
  | "<..."  { Flag_parsing.sgrep_guard (LDots (tokinfo lexbuf)) }
  | "...>"  { Flag_parsing.sgrep_guard (RDots (tokinfo lexbuf)) }

  (* ----------------------------------------------------------------------- *)
  (* Keywords and ident *)
  (* ----------------------------------------------------------------------- *)
  | identifier as id
      { match id with
        | "const"       -> CONST (tokinfo lexbuf)
        | "var"         -> VAR (tokinfo lexbuf)
        | "pub"         -> PUB (tokinfo lexbuf)
        | "fn"          -> FN (tokinfo lexbuf)
        | "return"      -> RETURN (tokinfo lexbuf)
        | "if"          -> IF (tokinfo lexbuf)
        | "else"        -> ELSE (tokinfo lexbuf)
        | "while"       -> WHILE (tokinfo lexbuf)
        | "for"         -> FOR (tokinfo lexbuf)
        | "switch"      -> SWITCH (tokinfo lexbuf)
        | "break"       -> BREAK (tokinfo lexbuf)
        | "continue"    -> CONTINUE (tokinfo lexbuf)
        | "defer"       -> DEFER (tokinfo lexbuf)
        | "errdefer"    -> ERRDEFER (tokinfo lexbuf)
        | "try"         -> TRY (tokinfo lexbuf)
        | "catch"       -> CATCH (tokinfo lexbuf)
        | "orelse"      -> ORELSE (tokinfo lexbuf)
        | "comptime"    -> COMPTIME (tokinfo lexbuf)
        | "test"        -> TEST (tokinfo lexbuf)
        | "struct"      -> STRUCT (tokinfo lexbuf)
        | "enum"        -> ENUM (tokinfo lexbuf)
        | "union"       -> UNION (tokinfo lexbuf)
        | "error"       -> ERROR (tokinfo lexbuf)
        | "true"        -> TRUE (tokinfo lexbuf)
        | "false"       -> FALSE (tokinfo lexbuf)
        | "null"        -> NULL (tokinfo lexbuf)
        | "undefined"   -> UNDEFINED (tokinfo lexbuf)
        | "unreachable" -> UNREACHABLE (tokinfo lexbuf)
        | "inline"      -> INLINE (tokinfo lexbuf)
        | "extern"      -> EXTERN (tokinfo lexbuf)
        | "export"      -> EXPORT (tokinfo lexbuf)
        | "noalias"     -> NOALIAS (tokinfo lexbuf)
        | "asm"         -> ASM (tokinfo lexbuf)
        | "volatile"    -> VOLATILE (tokinfo lexbuf)
        | "usingnamespace" -> USINGNAMESPACE (tokinfo lexbuf)
        | "and"         -> AND (tokinfo lexbuf)
        | "or"          -> OR (tokinfo lexbuf)
        | "async"       -> ASYNC (tokinfo lexbuf)
        | "await"       -> AWAIT (tokinfo lexbuf)
        | "resume"      -> RESUME (tokinfo lexbuf)
        | "suspend"     -> SUSPEND (tokinfo lexbuf)
        | "nosuspend"   -> NOSUSPEND (tokinfo lexbuf)
        | "threadlocal" -> THREADLOCAL (tokinfo lexbuf)
        | "opaque"      -> OPAQUE (tokinfo lexbuf)
        | "anyerror"    -> ANYERROR (tokinfo lexbuf)
        | "packed"      -> PACKED (tokinfo lexbuf)
        | "anytype"     -> ANYTYPE (tokinfo lexbuf)
        | "anyopaque"   -> IDENT ("anyopaque", tokinfo lexbuf)
        | "type"        -> TYPE (tokinfo lexbuf)
        | _             -> IDENT (id, tokinfo lexbuf)
    }

  (* @builtin identifiers *)
  | '@' (identifier as id)
      { BUILTIN (id, tokinfo lexbuf) }

  (* sgrep-ext: metavariables *)
  | '$' identifier
    { let s = tok lexbuf in
      if not !Flag_parsing.sgrep_mode
      then error ("identifier with dollar: " ^ s) lexbuf;
      IDENT (s, tokinfo lexbuf)
    }
  | '$' "..." ['A'-'Z''_']['A'-'Z''_''0'-'9']*
     { Flag.sgrep_guard (IDENT (tok lexbuf, tokinfo lexbuf)) }

  (* ----------------------------------------------------------------------- *)
  (* Constants *)
  (* ----------------------------------------------------------------------- *)
  | "0" (octal_digits as n)
      { LINT (Parsed_int.parse ("0o" ^ n, tokinfo lexbuf)) }

  | int_lit as n
      { LINT (Parsed_int.parse (n, tokinfo lexbuf)) }

  | float_lit as n
      { LFLOAT (float_of_string_opt n, tokinfo lexbuf) }

  (* ----------------------------------------------------------------------- *)
  (* Strings *)
  (* ----------------------------------------------------------------------- *)
  | '"' (string_char* as s) '"'
      { LSTR (s, tokinfo lexbuf) }
  | '"' ((string_char | semgrep_escapeseq)* as s) '"'
      { Flag.sgrep_guard (LSTR (s, tokinfo lexbuf)) }

  (* char literals *)
  | '\'' ([^ '\\' '\''] as c) '\''
      { LCHAR (String.make 1 c, tokinfo lexbuf) }
  | '\'' (escaped_char as s) '\''
      { LCHAR (s, tokinfo lexbuf) }
  | '\'' (hex_escape as s) '\''
      { LCHAR (s, tokinfo lexbuf) }
  | '\'' (unicode_escape as s) '\''
      { LCHAR (s, tokinfo lexbuf) }

  (* ----------------------------------------------------------------------- *)
  (* eof *)
  (* ----------------------------------------------------------------------- *)
  | eof { EOF (tokinfo lexbuf) }

  | _ { error (Common.spf "unrecognized symbol: %s" (tok lexbuf)) lexbuf;
        TUnknown (tokinfo lexbuf)
      }
