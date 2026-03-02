%{
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
open AST_zig

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)

let mk_bin e1 op tok e2 =
  Binary (e1, (op, tok), e2)

let mk_unary op tok e =
  Unary ((op, tok), e)

(* The Zig grammar is not LALR(1)-friendly in all places because of
 * constructs like `.{ ... }` which could be struct or array inits,
 * and `expr { ... }` for typed struct inits. We keep the grammar
 * minimal and resolve these in a few places via the AST. *)

%}

(*****************************************************************************)
(* Tokens *)
(*****************************************************************************)

(* Whitespace/comment *)
%token <Tok.t> TCommentSpace TCommentNewline TComment
%token <Tok.t> TUnknown
%token <Tok.t> EOF

(* Literals *)
%token <Parsed_int.t> LINT
%token <float option * Tok.t> LFLOAT
%token <string * Tok.t> LSTR
%token <string * Tok.t> LCHAR

(* Identifiers *)
%token <string * Tok.t> IDENT
%token <string * Tok.t> BUILTIN   (* @import etc. *)

(* Keywords *)
%token <Tok.t> CONST VAR PUB FN RETURN
%token <Tok.t> IF ELSE WHILE FOR SWITCH
%token <Tok.t> BREAK CONTINUE DEFER ERRDEFER
%token <Tok.t> TRY CATCH ORELSE
%token <Tok.t> COMPTIME TEST
%token <Tok.t> STRUCT ENUM UNION ERROR
%token <Tok.t> TRUE FALSE NULL UNDEFINED UNREACHABLE
%token <Tok.t> INLINE EXTERN EXPORT NOALIAS
%token <Tok.t> ASM VOLATILE USINGNAMESPACE
%token <Tok.t> AND OR
%token <Tok.t> ASYNC AWAIT RESUME SUSPEND NOSUSPEND
%token <Tok.t> THREADLOCAL OPAQUE ANYERROR

(* Tokens inserted by Parsing_hacks_zig *)
%token <Tok.t> COMPTIME_MOD  (* COMPTIME used as declaration modifier *)

(* Operators and punctuation *)
%token <Tok.t> PLUS MINUS STAR SLASH PERCENT
%token <Tok.t> PLUSEQ MINUSEQ STAREQ SLASHEQ PERCENTEQ
%token <Tok.t> AMP PIPE CARET TILDE
%token <Tok.t> AMPEQ PIPEEQ CARETEQ
%token <Tok.t> LSHIFT RSHIFT LSHIFTEQ RSHIFTEQ
%token <Tok.t> EQEQ BANGEQ LE GE LT GT
%token <Tok.t> EQ BANG
%token <Tok.t> AMPAMP PIPEPIPE
%token <Tok.t> PLUSPLUS
%token <Tok.t> LPAREN RPAREN LBRACKET RBRACKET LBRACE RBRACE
%token <Tok.t> COLON SEMICOLON DOT COMMA QUESTION
%token <Tok.t> DOTDOT

(* sgrep-ext *)
%token <Tok.t> ELLIPSIS LDots RDots

(*****************************************************************************)
(* Precedence *)
(*****************************************************************************)

(* Using layered grammar for the expression hierarchy, so no
 * precedence declarations needed for operators. The remaining
 * S/R and R/R conflicts (5 total) are all resolved correctly
 * by menhir's defaults (shift preference, first-rule preference). *)

(*****************************************************************************)
(* Top-level *)
(*****************************************************************************)

%start <AST_zig.program> file
%start <AST_zig.any> sgrep_spatch_pattern

%%

(*****************************************************************************)
(* File *)
(*****************************************************************************)

file:
 | top_decls EOF { $1 }

top_decls:
 | (* empty *)            { [] }
 | top_decl top_decls     { $1 :: $2 }

top_decl:
 | var_decl_full SEMICOLON
     { DVar $1 }
 | fn_def
     { DFunc $1 }
 | COMPTIME block
     { DComptime ($1, $2) }
 | TEST LSTR block
     { DTest ($1, Some $2, $3) }
 | TEST block
     { DTest ($1, None, $2) }
 | pub_opt USINGNAMESPACE expr SEMICOLON
     { DUsingNamespace ($1, $2, $3) }

(*****************************************************************************)
(* sgrep *)
(*****************************************************************************)

sgrep_spatch_pattern:
 | expr EOF              { E $1 }
 | stmt_with_semi EOF    { S $1 }
 | stmt_no_semi EOF      { S $1 }
 | top_decl EOF          { D $1 }

(*****************************************************************************)
(* Declarations *)
(*****************************************************************************)

var_decl_full:
 | pub_opt extern_opt comptime_opt threadlocal_opt var_kind IDENT type_annotation_opt
   var_init_opt
     { { vd_pub = $1; vd_extern = $2; vd_comptime = $3; vd_threadlocal = $4;
         vd_kind = $5; vd_name = $6; vd_type = $7; vd_align = None; vd_init = $8 } }

var_kind:
 | CONST { (Const, $1) }
 | VAR   { (Var, $1) }

type_annotation_opt:
 | (* empty *)    { None }
 | COLON type_    { Some $2 }

var_init_opt:
 | (* empty *)    { None }
 | EQ expr        { Some $2 }

pub_opt:
 | (* empty *) { None }
 | PUB         { Some $1 }

extern_opt:
 | (* empty *) { None }
 | EXTERN      { Some $1 }

comptime_opt:
 | (* empty *)     { None }
 | COMPTIME_MOD    { Some $1 }

threadlocal_opt:
 | (* empty *)    { None }
 | THREADLOCAL    { Some $1 }

(*****************************************************************************)
(* Functions *)
(*****************************************************************************)

fn_def:
 | pub_opt export_opt extern_opt inline_opt FN IDENT
   LPAREN params RPAREN return_type fn_body
     { { fd_pub = $1; fd_export = $2; fd_extern = $3; fd_inline = $4;
         fd_name = $6;
         fd_type = { ftok = $5; fparams = ($7, $8, $9); freturn = $10;
                     fcalling_convention = None };
         fd_body = $11 } }

export_opt:
 | (* empty *) { None }
 | EXPORT      { Some $1 }

inline_opt:
 | (* empty *) { None }
 | INLINE      { Some $1 }

params:
 | (* empty *)                 { [] }
 | param_list                  { $1 }
 | param_list COMMA            { $1 }

param_list:
 | param                       { [$1] }
 | param_list COMMA param      { $1 @ [$3] }

(* Params: IDENT COLON type_ is a named param, bare type_ is unnamed.
 * To avoid S/R conflict with IDENT (name vs type), we parse uniformly
 * and disambiguate in the action. *)
param:
 | type_
     { Param { p_comptime = None; p_noalias = None; p_name = None; p_type = $1 } }
 | IDENT COLON type_
     { Param { p_comptime = None; p_noalias = None; p_name = Some $1; p_type = $3 } }
 | COMPTIME_MOD IDENT COLON type_
     { Param { p_comptime = Some $1; p_noalias = None; p_name = Some $2; p_type = $4 } }
 | COMPTIME_MOD type_
     { Param { p_comptime = Some $1; p_noalias = None; p_name = None; p_type = $2 } }
 | NOALIAS type_
     { Param { p_comptime = None; p_noalias = Some $1; p_name = None; p_type = $2 } }
 | NOALIAS IDENT COLON type_
     { Param { p_comptime = None; p_noalias = Some $1; p_name = Some $2; p_type = $4 } }
 | ELLIPSIS
     { ParamEllipsis $1 }

return_type:
 | (* empty *)  { None }
 | type_        { Some $1 }

fn_body:
 | block        { Some $1 }
 | SEMICOLON    { None }

(*****************************************************************************)
(* Types *)
(*****************************************************************************)

type_:
 | type_name             { $1 }
 | STAR type_            { TPointer (PtrSingle, $1, $2) }
 | QUESTION type_        { TOptional ($1, $2) }
 | LBRACKET RBRACKET type_
     { TSlice (($1, $2, $2), $3) }
 | LBRACKET expr RBRACKET type_
     { TArray (($1, $2, $3), $4) }
 | FN LPAREN type_params RPAREN type_
     { TFunc { ftok = $1; fparams = ($2, $3, $4); freturn = Some $5;
               fcalling_convention = None } }
 | STRUCT LBRACE container_fields RBRACE
     { TStruct ($1, ($2, $3, $4)) }
 | ENUM LBRACE enum_fields RBRACE
     { TEnum ($1, None, ($2, $3, $4)) }
 | ENUM LPAREN type_ RPAREN LBRACE enum_fields RBRACE
     { TEnum ($1, Some $3, ($4, $6, $7)) }
 | UNION LBRACE container_fields RBRACE
     { TUnion ($1, None, ($2, $3, $4)) }
 | OPAQUE LBRACE RBRACE
     { TOpaque ($1, ($2, $3, $3)) }
 | BUILTIN LPAREN expr_comma_list RPAREN
     { TBuiltinCall ($1, ($2, $3, $4)) }
 | ELLIPSIS
     { TEllipsis $1 }

type_name:
 | IDENT       { TName $1 }
 | ANYERROR    { TName ("anyerror", $1) }
 | ERROR       { TName ("error", $1) }

type_params:
 | (* empty *)                   { [] }
 | type_param_list               { $1 }
 | type_param_list COMMA         { $1 }

type_param_list:
 | type_
     { [Param { p_comptime = None; p_noalias = None; p_name = None; p_type = $1 }] }
 | type_param_list COMMA type_
     { $1 @ [Param { p_comptime = None; p_noalias = None; p_name = None; p_type = $3 }] }

container_fields:
 | (* empty *)                              { [] }
 | container_field_list                     { $1 }
 | container_field_list COMMA               { $1 }

container_field_list:
 | container_field                              { [$1] }
 | container_field_list COMMA container_field   { $1 @ [$3] }

container_field:
 | IDENT COLON type_
     { { cf_name = $1; cf_type = $3; cf_default = None; cf_align = None } }
 | IDENT COLON type_ EQ expr
     { { cf_name = $1; cf_type = $3; cf_default = Some $5; cf_align = None } }

enum_fields:
 | (* empty *)                    { [] }
 | enum_field_list                { $1 }
 | enum_field_list COMMA          { $1 }

enum_field_list:
 | enum_field                          { [$1] }
 | enum_field_list COMMA enum_field    { $1 @ [$3] }

enum_field:
 | IDENT                  { EnumField ($1, None) }
 | IDENT EQ expr          { EnumField ($1, Some $3) }
 | ELLIPSIS               { EnumFieldEllipsis $1 }

(*****************************************************************************)
(* Expressions — layered grammar for LR(1) *)
(*****************************************************************************)

(* Top-level expression *)
expr:
 | assign_expr           { $1 }

(* Assignment: right-associative, lowest precedence *)
assign_expr:
 | or_expr                         { $1 }
 | postfix_expr EQ assign_expr     { Assign ($1, $2, $3) }
 | postfix_expr PLUSEQ assign_expr { AssignOp ($1, (AST_generic.Plus, $2), $3) }
 | postfix_expr MINUSEQ assign_expr { AssignOp ($1, (AST_generic.Minus, $2), $3) }
 | postfix_expr STAREQ assign_expr { AssignOp ($1, (AST_generic.Mult, $2), $3) }
 | postfix_expr SLASHEQ assign_expr { AssignOp ($1, (AST_generic.Div, $2), $3) }
 | postfix_expr PERCENTEQ assign_expr { AssignOp ($1, (AST_generic.Mod, $2), $3) }
 | postfix_expr AMPEQ assign_expr { AssignOp ($1, (AST_generic.BitAnd, $2), $3) }
 | postfix_expr PIPEEQ assign_expr { AssignOp ($1, (AST_generic.BitOr, $2), $3) }
 | postfix_expr CARETEQ assign_expr { AssignOp ($1, (AST_generic.BitXor, $2), $3) }
 | postfix_expr LSHIFTEQ assign_expr { AssignOp ($1, (AST_generic.LSL, $2), $3) }
 | postfix_expr RSHIFTEQ assign_expr { AssignOp ($1, (AST_generic.LSR, $2), $3) }

(* Boolean OR *)
or_expr:
 | and_expr                      { $1 }
 | or_expr PIPEPIPE and_expr     { mk_bin $1 AST_generic.Or $2 $3 }

(* Boolean AND *)
and_expr:
 | cmp_expr                      { $1 }
 | and_expr AMPAMP cmp_expr      { mk_bin $1 AST_generic.And $2 $3 }

(* Comparison *)
cmp_expr:
 | bitor_expr                    { $1 }
 | bitor_expr EQEQ bitor_expr   { mk_bin $1 AST_generic.Eq $2 $3 }
 | bitor_expr BANGEQ bitor_expr { mk_bin $1 AST_generic.NotEq $2 $3 }
 | bitor_expr LT bitor_expr     { mk_bin $1 AST_generic.Lt $2 $3 }
 | bitor_expr GT bitor_expr     { mk_bin $1 AST_generic.Gt $2 $3 }
 | bitor_expr LE bitor_expr     { mk_bin $1 AST_generic.LtE $2 $3 }
 | bitor_expr GE bitor_expr     { mk_bin $1 AST_generic.GtE $2 $3 }

(* Bitwise OR *)
bitor_expr:
 | bitxor_expr                     { $1 }
 | bitor_expr PIPE bitxor_expr     { mk_bin $1 AST_generic.BitOr $2 $3 }

(* Bitwise XOR *)
bitxor_expr:
 | bitand_expr                       { $1 }
 | bitxor_expr CARET bitand_expr     { mk_bin $1 AST_generic.BitXor $2 $3 }

(* Bitwise AND *)
bitand_expr:
 | shift_expr                       { $1 }
 | bitand_expr AMP shift_expr       { mk_bin $1 AST_generic.BitAnd $2 $3 }

(* Shift *)
shift_expr:
 | add_expr                         { $1 }
 | shift_expr LSHIFT add_expr       { mk_bin $1 AST_generic.LSL $2 $3 }
 | shift_expr RSHIFT add_expr       { mk_bin $1 AST_generic.LSR $2 $3 }

(* Addition-level *)
add_expr:
 | mul_expr                         { $1 }
 | add_expr PLUS mul_expr           { mk_bin $1 AST_generic.Plus $2 $3 }
 | add_expr MINUS mul_expr          { mk_bin $1 AST_generic.Minus $2 $3 }
 | add_expr PLUSPLUS mul_expr       { mk_bin $1 AST_generic.Concat $2 $3 }

(* Multiplication-level *)
mul_expr:
 | prefix_expr                      { $1 }
 | mul_expr STAR prefix_expr        { mk_bin $1 AST_generic.Mult $2 $3 }
 | mul_expr SLASH prefix_expr       { mk_bin $1 AST_generic.Div $2 $3 }
 | mul_expr PERCENT prefix_expr     { mk_bin $1 AST_generic.Mod $2 $3 }

(* Unary prefix *)
prefix_expr:
 | postfix_expr                     { $1 }
 | MINUS prefix_expr                { mk_unary AST_generic.Minus $1 $2 }
 | BANG prefix_expr                 { mk_unary AST_generic.Not $1 $2 }
 | TILDE prefix_expr                { mk_unary AST_generic.BitNot $1 $2 }
 | AMP prefix_expr                  { mk_unary AST_generic.BitAnd $1 $2 }
 | TRY prefix_expr                  { Try ($1, $2) }
 | COMPTIME prefix_expr             { Comptime ($1, $2) }
 | NOSUSPEND prefix_expr            { Nosuspend ($1, $2) }
 | ASYNC prefix_expr                { Async ($1, $2) }
 | AWAIT prefix_expr                { Await ($1, $2) }
 | RESUME prefix_expr               { Resume ($1, $2) }

(* Postfix: field access, calls, indexing *)
postfix_expr:
 | atom_expr                        { $1 }
 | postfix_expr DOT IDENT           { FieldAccess ($1, $2, $3) }
 | postfix_expr DOT STAR            { Deref ($1, $2) }
 | postfix_expr DOT QUESTION        { OptionalUnwrap ($1, $2) }
 | postfix_expr LPAREN call_args RPAREN
     { Call ($1, ($2, $3, $4)) }
 | postfix_expr LBRACKET expr RBRACKET
     { Index ($1, ($2, $3, $4)) }
 (* catch/orelse are postfix-ish: expr catch expr, expr orelse expr *)
 | postfix_expr CATCH prefix_expr
     { Catch ($1, $2, None, $3) }
 | postfix_expr ORELSE prefix_expr
     { Orelse ($1, $2, None, $3) }
 (* sgrep *)
 | postfix_expr DOT ELLIPSIS
     { DotAccessEllipsis ($1, $3) }

(* Atoms: leaves and parenthesized expressions *)
atom_expr:
 | LINT                  { Int $1 }
 | LFLOAT                { Float $1 }
 | LSTR                  { String $1 }
 | LCHAR                 { Char $1 }
 | TRUE                  { Bool (true, $1) }
 | FALSE                 { Bool (false, $1) }
 | NULL                  { Null $1 }
 | UNDEFINED             { Undefined $1 }
 | UNREACHABLE           { Id ("unreachable", $1) }
 | IDENT                 { Id $1 }
 | LPAREN expr RPAREN    { $2 }
 (* builtin call: @import("std") *)
 | BUILTIN LPAREN call_args RPAREN
     { BuiltinCall ($1, ($2, $3, $4)) }
 (* anonymous init: .{ } or .{ .x = 1 } or .{ "world" } *)
 | DOT LBRACE anon_init_body RBRACE
     { $3 ($2, $4) }
 (* block expression *)
 | LBRACE stmts RBRACE
     { BlockExpr (None, ($1, $2, $3)) }
 (* error.Name *)
 | ERROR DOT IDENT
     { ErrorValue ($1, $2, $3) }
 (* sgrep-ext: *)
 | ELLIPSIS
     { Ellipsis $1 }
 | LDots expr RDots
     { DeepEllipsis ($1, $2, $3) }

call_args:
 | (* empty *)                { [] }
 | call_arg_list              { $1 }
 | call_arg_list COMMA        { $1 }

call_arg_list:
 | expr                       { [$1] }
 | call_arg_list COMMA expr   { $1 @ [$3] }

expr_comma_list:
 | (* empty *)                { [] }
 | expr_comma_list1           { $1 }
 | expr_comma_list1 COMMA     { $1 }

expr_comma_list1:
 | expr                           { [$1] }
 | expr_comma_list1 COMMA expr    { $1 @ [$3] }

(* Anonymous init body: disambiguates .{} vs .{field_inits} vs .{exprs} *)
anon_init_body:
 | (* empty *)
     { fun (lb, rb) -> ArrayInit (None, (lb, [], rb)) }
 | field_init_list
     { fun (lb, rb) -> StructInit (None, (lb, $1, rb)) }
 | field_init_list COMMA
     { fun (lb, rb) -> StructInit (None, (lb, $1, rb)) }
 | anon_expr_list
     { fun (lb, rb) -> ArrayInit (None, (lb, $1, rb)) }
 | anon_expr_list COMMA
     { fun (lb, rb) -> ArrayInit (None, (lb, $1, rb)) }

field_init_list:
 | field_init                          { [$1] }
 | field_init_list COMMA field_init    { $1 @ [$3] }

field_init:
 | DOT IDENT EQ expr
     { FieldInit ($2, $3, $4) }

(* For array inits inside .{}, a non-empty list of expressions.
 * We can use regular expr here because field_init always starts
 * with DOT IDENT EQ, which is unambiguous. *)
anon_expr_list:
 | expr                            { [$1] }
 | anon_expr_list COMMA expr       { $1 @ [$3] }

(*****************************************************************************)
(* Statements *)
(*****************************************************************************)

stmts:
 | (* empty *)        { [] }
 | stmt stmts         { $1 :: $2 }

stmt:
 | stmt_with_semi     { $1 }
 | stmt_no_semi       { $1 }

stmt_with_semi:
 | expr SEMICOLON
     { ExprSt ($1, $2) }
 | var_decl_full SEMICOLON
     { VarDecl $1 }
 | RETURN SEMICOLON
     { Return ($1, None) }
 | RETURN expr SEMICOLON
     { Return ($1, Some $2) }
 | BREAK SEMICOLON
     { Break ($1, None, None) }
 | CONTINUE SEMICOLON
     { Continue ($1, None) }
 | SEMICOLON
     { Empty $1 }

stmt_no_semi:
 | if_stmt            { $1 }
 | while_stmt         { $1 }
 | for_stmt           { $1 }
 | block_stmt         { $1 }
 | defer_stmt         { $1 }
 | errdefer_stmt      { $1 }

if_stmt:
 | IF LPAREN expr RPAREN block_or_stmt
     { If ($1, $3, None, $5, None) }
 | IF LPAREN expr RPAREN block_or_stmt ELSE block_or_stmt
     { If ($1, $3, None, $5, Some $7) }
 | IF LPAREN expr RPAREN block_or_stmt ELSE if_stmt
     { If ($1, $3, None, $5, Some $7) }

while_stmt:
 | WHILE LPAREN expr RPAREN block_or_stmt
     { While ($1, $3, None, None, $5, None) }

for_stmt:
 | FOR LPAREN expr_comma_list RPAREN block_or_stmt
     { let inputs = List.map (fun e -> { fi_expr = e; fi_sentinel = None }) $3 in
       For ($1, inputs, [], $5, None) }

block_stmt:
 | block { Block $1 }

block:
 | LBRACE stmts RBRACE { ($1, $2, $3) }

block_or_stmt:
 | block          { Block $1 }
 | stmt_with_semi { $1 }

defer_stmt:
 | DEFER block_or_stmt
     { Defer ($1, $2) }

errdefer_stmt:
 | ERRDEFER block_or_stmt
     { Errdefer ($1, None, $2) }
