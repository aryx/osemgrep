(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
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
module CST = Tree_sitter_haskell.CST
module H = Parse_tree_sitter_helpers
open AST_haskell

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(* Haskell parser using tree-sitter-lang/semgrep-haskell and converting
 * to AST_haskell.
 *
 * The resulting AST can then be converted to the generic AST by using
 * Haskell_to_generic.ml
 *)

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)

type env = unit H.env

let token = H.token
let str = H.str
let fb = Tok.unsafe_fake_bracket

let todo_expr (_env : env) _x cat =
  ExprTodo (cat, [])

let todo_pat (_env : env) _x cat =
  PatTodo (cat, [])

let todo_type (_env : env) _x cat =
  TyTodo (cat, [])

let todo_decl (_env : env) _x cat =
  DeclTodo (cat, [])

(*****************************************************************************)
(* Token helpers *)
(*****************************************************************************)

let map_arrow (env : env) (x : CST.arrow) : Tok.t =
  match x with
  | `UNKUNKUNK tok
  | `DASHGT tok ->
      token env tok

let map_larrow (env : env) (x : CST.larrow) : Tok.t =
  match x with
  | `UNKUNKUNK tok
  | `LTDASH tok ->
      token env tok

let map_carrow (env : env) (x : CST.carrow) : Tok.t =
  match x with
  | `UNKUNKUNK tok
  | `EQGT tok ->
      token env tok

let map_colon2 (env : env) (x : CST.colon2) : Tok.t =
  match x with
  | `UNKUNKUNK tok
  | `COLONCOLON tok ->
      token env tok

let map_do_keyword (env : env) (x : CST.do_keyword) : Tok.t =
  match x with
  | `Mdo tok
  | `Do tok ->
      token env tok

(*****************************************************************************)
(* Names *)
(*****************************************************************************)

let map_variable (env : env) (x : CST.variable) : ident =
  match x with
  | `Varid tok -> str env tok
  | `Semg_meta tok -> str env tok

let map_constructor (env : env) (x : CST.constructor) : ident =
  match x with
  | `Conid tok -> str env tok
  | `Semg_meta tok -> str env tok

let map_tyconid = map_constructor
let map_modid = map_constructor

let map_qualifying_module (env : env) (xs : CST.qualifying_module) : qualifier =
  List_.map (fun (modid, _dot) ->
    map_modid env modid
  ) xs

let map_operator_minus (env : env) (x : CST.operator_minus) : op =
  match x with
  | `Op tok -> str env tok
  | `Minus tok -> str env tok

let map_type_operator (env : env) (x : CST.type_operator) : op =
  match x with
  | `Tyco tok -> str env tok
  | `Cons_op tok -> str env tok

let map_pat_name (env : env) (x : CST.pat_name) : ident =
  match x with
  | `Var x -> map_variable env x
  | `LPAR_choice_op_RPAR (_lp, op, _rp) ->
      map_operator_minus env op

let map_con (env : env) (x : CST.con) : ident =
  match x with
  | `Cons x -> map_tyconid env x
  | `LPAR_cons_op_RPAR (_lp, tok, _rp) ->
      str env tok

let map_fun_name (env : env) (x : CST.fun_name) : ident =
  match x with
  | `Choice_var x -> map_pat_name env x
  | `Impl_parid tok -> str env tok

let map_simple_tycon (env : env) (x : CST.simple_tycon) : ident =
  match x with
  | `Cons x -> map_tyconid env x
  | `LPAR_type_op_RPAR (_lp, op, _rp) ->
      let s, t = map_type_operator env op in
      (s, t)

(* Qualified variable references *)
let map_qvarid (env : env) (x : CST.qvarid) : name =
  match x with
  | `Qual_var (qual, v) ->
      let q = map_qualifying_module env qual in
      let id = map_variable env v in
      (q, id)
  | `Var v ->
      let id = map_variable env v in
      ([], id)

let map_qtyconid (env : env) (x : CST.qtyconid) : name =
  match x with
  | `Qual_type (qual, c) ->
      let q = map_qualifying_module env qual in
      let id = map_tyconid env c in
      (q, id)
  | `Cons c ->
      let id = map_tyconid env c in
      ([], id)

let map_qconid (env : env) (x : CST.qconid) : name =
  match x with
  | `Qual_cons (qual, c) ->
      let q = map_qualifying_module env qual in
      let id = map_tyconid env c in
      (q, id)
  | `Cons c ->
      let id = map_tyconid env c in
      ([], id)

let map_qvarsym (env : env) (x : CST.qvarsym) : name =
  match x with
  | `Qual_op (qual, op) ->
      let q = map_qualifying_module env qual in
      let id = map_operator_minus env op in
      (q, id)
  | `Choice_op op ->
      let id = map_operator_minus env op in
      ([], id)

let map_qconsym (env : env) (x : CST.qconsym) : name =
  match x with
  | `Qual_cons_op (qual, tok) ->
      let q = map_qualifying_module env qual in
      let id = str env tok in
      (q, id)
  | `Cons_op tok ->
      let id = str env tok in
      ([], id)

let map_qvar (env : env) (x : CST.qvar) : name =
  match x with
  | `Choice_qual_var x -> map_qvarid env x
  | `LPAR_choice_qual_op_RPAR (_lp, x, _rp) ->
      map_qvarsym env x

let map_qcon (env : env) (x : CST.qcon) : name =
  match x with
  | `Choice_qual_cons x -> map_qconid env x
  | `LPAR_choice_qual_cons_op_RPAR (_lp, x, _rp) ->
      map_qconsym env x

let map_qmodid (env : env) (x : CST.qmodid) : name =
  match x with
  | `Qual_module (qual, m) ->
      let q = map_qualifying_module env qual in
      let id = map_modid env m in
      (q, id)
  | `Modid m ->
      let id = map_modid env m in
      ([], id)

let map_qtycon (env : env) (x : CST.qtycon) : name =
  match x with
  | `Choice_qual_type x -> map_qtyconid env x
  | `LPAR_choice_qual_type_op__RPAR (_lp, x, _rp) ->
      (match x with
       | `Qual_type_op_ x ->
           (match x with
            | `Qual_type_op (qual, tok) ->
                let q = map_qualifying_module env qual in
                let id = str env tok in
                (q, id)
            | `Qual_cons_op (qual, tok) ->
                let q = map_qualifying_module env qual in
                let id = str env tok in
                (q, id))
       | `Type_op x ->
           let id = map_type_operator env x in
           ([], id))

(* Operators for infix expressions *)
let map_qvarsym_nominus (env : env) (x : CST.qvarsym_nominus) : name =
  match x with
  | `Qual_op (qual, op) ->
      let q = map_qualifying_module env qual in
      let id = map_operator_minus env op in
      (q, id)
  | `Op tok ->
      ([], str env tok)

let map_varop (env : env) (x : CST.varop) : op =
  match x with
  | `Choice_op x -> map_operator_minus env x
  | `BQUOT_var_BQUOT (_bt, v, _bt2) ->
      map_variable env v

let map_conop (env : env) (x : CST.conop) : op =
  match x with
  | `Cons_op tok -> str env tok
  | `BQUOT_cons_BQUOT (_bt, c, _bt2) ->
      map_tyconid env c

let map_qop (env : env) (x : CST.qop) : op =
  match x with
  | `Qvarop x ->
      (match x with
       | `Choice_qual_op x ->
           let _q, id = map_qvarsym env x in
           id
       | `BQUOT_choice_qual_var_BQUOT (_bt, x, _bt2) ->
           let _q, id = map_qvarid env x in
           id)
  | `Choice_choice_qual_cons_op x ->
      (match x with
       | `Choice_qual_cons_op x ->
           let _q, id = map_qconsym env x in
           id
       | `BQUOT_choice_qual_cons_BQUOT (_bt, x, _bt2) ->
           let _q, id = map_qconid env x in
           id)

let map_qop_nominus (env : env) (x : CST.qop_nominus) : op =
  match x with
  | `Qvarop_nominus x ->
      (match x with
       | `Choice_qual_op x ->
           let _q, id = map_qvarsym_nominus env x in
           id
       | `BQUOT_choice_qual_var_BQUOT (_bt, x, _bt2) ->
           let _q, id = map_qvarid env x in
           id)
  | `Choice_choice_qual_cons_op x ->
      (match x with
       | `Choice_qual_cons_op x ->
           let _q, id = map_qconsym env x in
           id
       | `BQUOT_choice_qual_cons_BQUOT (_bt, x, _bt2) ->
           let _q, id = map_qconid env x in
           id)

let map_op (env : env) (x : CST.op) : op =
  match x with
  | `Varop x -> map_varop env x
  | `Choice_cons_op x -> map_conop env x

(*****************************************************************************)
(* Literals *)
(*****************************************************************************)

let map_integer (env : env) (x : CST.integer) : literal =
  match x with
  | `Bin_lit tok
  | `Int_lit tok
  | `Octal_lit tok
  | `Hex_lit tok ->
      let s, t = str env tok in
      Int (Parsed_int.parse (s, t))

let map_number (env : env) (x : CST.number) : literal =
  match x with
  | `Int x -> map_integer env x
  | `Float tok ->
      let s, t = str env tok in
      Float (float_of_string_opt s, t)

let map_stringly (env : env) (x : CST.stringly) : literal =
  match x with
  | `Str tok -> String (str env tok)
  | `Char tok -> Char (str env tok)

let map_literal (env : env) (x : CST.literal) : literal =
  match x with
  | `Choice_str x -> map_stringly env x
  | `Choice_int x -> map_number env x

(*****************************************************************************)
(* List/layout helpers *)
(*****************************************************************************)

(* Many CST types have two variants: braced {x;y;z} or layout-based.
 * These helpers extract the list of items from both variants. *)

let extract_semi_list (first : 'a) (rest : ('b * 'a) list) : 'a list =
  first :: List_.map snd rest

let rec map_decl_list_braced (env : env)
    ((_lbrace, opt, _semi_opt, _rbrace) :
      CST.anon_LCURL_opt_decl_rep_SEMI_decl_opt_SEMI_RCURL_2255345) :
    decl list =
  match opt with
  | None -> []
  | Some (first, rest) ->
      let decls = extract_semi_list first rest in
      List_.map (map_decl env) decls

and map_decl_list_layout (env : env)
    (opt : CST.anon_decl_rep_choice_SEMI_decl_opt_choice_SEMI_896561c option) :
    decl list =
  match opt with
  | None -> []
  | Some (first, rest, _semi_opt) ->
      let decls = extract_semi_list first (List_.map (fun (_, d) -> ((), d)) rest) in
      List_.map (map_decl env) decls

and map_decls (env : env) (x : CST.decls) : decl list =
  match x with
  | `LCURL_opt_decl_rep_SEMI_decl_opt_SEMI_RCURL x ->
      map_decl_list_braced env x
  | `Layout_start_opt_decl_rep_choice_SEMI_decl_opt_choice_SEMI_layout_end
      (_ls, opt, _le) ->
      map_decl_list_layout env opt

and map_let_decls (env : env) (x : CST.let_decls) : decl list =
  match x with
  | `LCURL_opt_decl_rep_SEMI_decl_opt_SEMI_RCURL x ->
      map_decl_list_braced env x
  | `Layout_start_opt_decl_rep_choice_SEMI_decl_opt_choice_SEMI (_ls, opt) ->
      map_decl_list_layout env opt

(*****************************************************************************)
(* Types *)
(*****************************************************************************)

and map_tyvar (env : env) (x : CST.tyvar) : ident =
  match x with
  | `Anno_type_var (_lp, v, _ann, _rp) ->
      str env v
  | `Type_var v ->
      str env v

and map_type_name (env : env) (x : CST.type_name) : type_ =
  match x with
  | `Choice_anno_type_var tv ->
      let id = map_tyvar env tv in
      TyVar id
  | `Choice_prom_tycon gt ->
      map_gtycon env gt

and map_gtycon (env : env) (x : CST.gtycon) : type_ =
  match x with
  | `Prom_tycon (_tick, qt) ->
      let n = map_qtycon env qt in
      TyName n
  | `Choice_choice_qual_type qt ->
      let n = map_qtycon env qt in
      TyName n
  | `Tycon_arrow (_lp, _arr, _rp) ->
      TyName ([], ("(->)", Tok.unsafe_fake_tok ""))

and map_type_literal (env : env) (x : CST.type_literal) : type_ =
  match x with
  | `Lit lit ->
      let _lit = map_literal env lit in
      todo_type env x ("TyLiteral", Tok.unsafe_fake_tok "")
  | `Con_unit (_lp, _rp) ->
      TyName ([], ("()", Tok.unsafe_fake_tok ""))
  | `Con_list (_lb, _rb) ->
      TyName ([], ("[]", Tok.unsafe_fake_tok ""))
  | `Con_tuple (_lp, _commas, _rp) ->
      let arity = List.length _commas + 1 in
      let name = "(" ^ String.make (arity - 1) ',' ^ ")" in
      TyName ([], (name, Tok.unsafe_fake_tok ""))

and map_type_literal_ (env : env) (x : CST.type_literal_) : type_ =
  match x with
  | `Type_prom_lit_c26b94c (_tick, x) ->
      map_type_promotable_literal env x
  | `Type_prom_lit_af79c83 x ->
      map_type_promotable_literal env x

and map_type_promotable_literal (env : env) (x : CST.type_promotable_literal) : type_ =
  match x with
  | `Type_lit x -> map_type_literal env x
  | `Type_tuple_ (_lp, (t1, rest), _rp) ->
      let t1 = map_type_or_implicit env t1 in
      let rest = List_.map (fun (_comma, t) -> map_type_or_implicit env t) rest in
      TyTuple (fb (t1 :: rest))
  | `Type_list (lb, t1, rest, rb) ->
      let t1 = map_type_or_implicit env t1 in
      let _rest = List_.map (fun (_comma, t) -> map_type_or_implicit env t) rest in
      (* Type-level list, approximate as list type *)
      TyList (token env lb, t1, token env rb)

and map_atype (env : env) (x : CST.atype) : type_ =
  match x with
  | `Type_name x -> map_type_name env x
  | `Type_star x ->
      (match x with
       | `STAR tok | `UNKUNKUNK tok ->
           TyName ([], str env tok))
  | `Type_lit_ x -> map_type_literal_ env x
  | `Type_parens (lp, t, rp) ->
      let t = map_type_or_implicit env t in
      TyParen (token env lp, t, token env rp)
  | `Type_unbo_tuple _ ->
      todo_type env x ("TyUnboxedTuple", Tok.unsafe_fake_tok "")
  | `Type_unbo_sum _ ->
      todo_type env x ("TyUnboxedSum", Tok.unsafe_fake_tok "")
  | `Splice _ ->
      todo_type env x ("TySplice", Tok.unsafe_fake_tok "")
  | `Quas _ ->
      todo_type env x ("TyQuasiquote", Tok.unsafe_fake_tok "")

and map_btype (env : env) (x : CST.btype) : type_ =
  match x with
  | `Atype x -> map_atype env x
  | `Type_apply (t1, ts) ->
      let t1 = map_atype env t1 in
      let ts = List_.map (map_atype env) ts in
      List.fold_left (fun acc t -> TyApp (acc, t)) t1 ts

and map_type_infix_ (env : env) ((t1, _op, t2) : CST.type_infix_) : type_ =
  let t1 = map_btype env t1 in
  let t2 = map_type_infix env t2 in
  TyApp (TyApp (todo_type env t1 ("TyInfixOp", Tok.unsafe_fake_tok ""), t1), t2)

and map_type_infix (env : env) (x : CST.type_infix) : type_ =
  match x with
  | `Type_infix_ x -> map_type_infix_ env x
  | `Btype x -> map_btype env x

and map_type_ (env : env) (x : CST.type_) : type_ =
  match x with
  | `Type_quants ((_forall_kw, tyvars), _dot, body) ->
      let forall_tok = Tok.unsafe_fake_tok "forall" in
      let tyvars = List_.map (map_tyvar env) tyvars in
      let body = map_type_ env body in
      TyForall (forall_tok, tyvars, body)
  | `Type_cont (ctx, body) ->
      let constraints, arrow_tok = map_context env ctx in
      let body = map_type_ env body in
      TyContext (constraints, arrow_tok, body)
  | `Type_fun (lhs, (_mod_opt, arr), rhs) ->
      let lhs = map_type_infix env lhs in
      let arr = map_arrow env arr in
      let rhs = map_type_ env rhs in
      TyFun (lhs, arr, rhs)
  | `Type_infix x ->
      map_type_infix env x

and map_type_or_implicit (env : env) (x : CST.type_or_implicit) : type_ =
  match x with
  | `Impl_param (_parid, _ann) ->
      todo_type env x ("TyImplicitParam", Tok.unsafe_fake_tok "")
  | `Type x -> map_type_ env x

and _map_type_annotation (env : env) ((col, ty) : CST.type_annotation) : Tok.t * type_ =
  let col = map_colon2 env col in
  let ty = map_type_or_implicit env ty in
  (col, ty)

(*****************************************************************************)
(* Context / Constraints *)
(*****************************************************************************)

and map_constraint_ (env : env) (x : CST.constraint_) : constraint_ =
  match x with
  | `Type_name_rep_atype (tn, atypes) ->
      let n = match map_type_name env tn with
        | TyName n -> n
        | TyVar id -> ([], id)
        | _ -> ([], ("_", Tok.unsafe_fake_tok ""))
      in
      let types = List_.map (map_atype env) atypes in
      ClassConstraint (n, types)
  | `Type_infix_ _ ->
      ClassConstraint (([], ("_infix", Tok.unsafe_fake_tok "")), [])

and map_constraint__ (env : env) (x : CST.constraint__) : constraint_ =
  match x with
  | `Quan_cons (_forall, _dot, c) -> map_constraint__ env c
  | `Cons_cont (_ctx, c) -> map_constraint__ env c
  | `LPAR_cons__RPAR (_lp, c, _rp) -> map_constraint__ env c
  | `Cons c -> map_constraint_ env c

and map_context_constraints (env : env) (x : CST.context_constraints) : context =
  match x with
  | `Cons c ->
      [ map_constraint_ env c ]
  | `LPAR_opt_choice_cons__rep_comma_choice_cons__RPAR (_lp, opt, _rp) ->
      (match opt with
       | None -> []
       | Some (first, rest) ->
           let map_one x =
             match x with
             | `Cons_ c -> map_constraint__ env c
             | `Impl_param _ -> ClassConstraint (([], ("?implicit", Tok.unsafe_fake_tok "")), [])
           in
           let first = map_one first in
           let rest = List_.map (fun (_comma, c) -> map_one c) rest in
           first :: rest)

and map_context (env : env) ((constraints, carr) : CST.context) : context * Tok.t =
  let ctx = map_context_constraints env constraints in
  let arrow = map_carrow env carr in
  (ctx, arrow)

(*****************************************************************************)
(* Patterns *)
(*****************************************************************************)

and map_pat_constructor (env : env) (x : CST.pat_constructor) : name =
  map_qcon env x

and map_apat (env : env) (x : CST.apat) : pattern =
  match x with
  | `Pat_name x ->
      let id = map_pat_name env x in
      PatVar id
  | `Pat_as (v, at, p) ->
      let id = map_variable env v in
      let at_tok = token env at in
      let p = map_apat env p in
      PatAs (p, at_tok, id)
  | `Pat_cons c ->
      let n = map_pat_constructor env c in
      PatConstructor (n, [])
  | `Pat_record (c, fields) ->
      let n = map_pat_constructor env c in
      let fields = map_pat_fields env fields in
      PatRecord (n, fields)
  | `Lit_ x ->
      let lit = map_literal_ env x in
      PatLiteral lit
  | `Pat_wild tok ->
      PatWildcard (token env tok)
  | `Pat_parens (lp, np, rp) ->
      let p = map_nested_pat env np in
      PatParen (token env lp, p, token env rp)
  | `Pat_tuple (lp, first, rest, rp) ->
      let first = map_nested_pat env first in
      let rest = List_.map (fun (_comma, np) -> map_nested_pat env np) rest in
      PatTuple (token env lp, first :: rest, token env rp)
  | `Pat_unbo_tuple _ ->
      todo_pat env x ("PatUnboxedTuple", Tok.unsafe_fake_tok "")
  | `Pat_unbo_sum_ _ ->
      todo_pat env x ("PatUnboxedSum", Tok.unsafe_fake_tok "")
  | `Pat_list (lb, first, rest, rb) ->
      let first = map_nested_pat env first in
      let rest = List_.map (fun (_comma, np) -> map_nested_pat env np) rest in
      PatList (token env lb, first :: rest, token env rb)
  | `Pat_strict (bang, p) ->
      PatBang (token env bang, map_apat env p)
  | `Pat_irre (tilde, p) ->
      PatIrrefutable (token env tilde, map_apat env p)
  | `Splice _ ->
      todo_pat env x ("PatSplice", Tok.unsafe_fake_tok "")
  | `Quas _ ->
      todo_pat env x ("PatQuasiquote", Tok.unsafe_fake_tok "")

and map_literal_ (env : env) (x : CST.literal_) : literal =
  match x with
  | `Lit x -> map_literal env x
  | `Choice_con_unit x ->
      (match x with
       | `Con_unit _ -> String (("()", Tok.unsafe_fake_tok ""))
       | `Con_list _ -> String (("[]", Tok.unsafe_fake_tok ""))
       | `Con_tuple _ -> String (("(,)", Tok.unsafe_fake_tok "")))

and map_nested_pat (env : env) (x : CST.nested_pat) : pattern =
  match x with
  | `Typed_pat x -> map_typed_pat env x
  | `Pat_view (_e, _arr, _np) ->
      todo_pat env x ("PatView", Tok.unsafe_fake_tok "")

and map_typed_pat (env : env) (x : CST.typed_pat) : pattern =
  match x with
  | `Pat p -> map_pat env p
  | `Pat_typed (p, (col, ty)) ->
      let p = map_pat env p in
      let col = map_colon2 env col in
      let ty = map_type_or_implicit env ty in
      PatTyped (p, col, ty)

and map_lpat (env : env) (x : CST.lpat) : pattern =
  match x with
  | `Apat x -> map_apat env x
  | `Pat_nega (minus, p) ->
      let p = map_apat env p in
      (match p with
       | PatLiteral lit -> PatNeg (token env minus, lit)
       | _ -> todo_pat env x ("PatNegNonLit", Tok.unsafe_fake_tok ""))
  | `Pat_apply (con, pats) ->
      let n = map_pat_constructor env con in
      let pats = List_.map (map_apat env) pats in
      PatConstructor (n, pats)

and map_pat (env : env) (x : CST.pat) : pattern =
  match x with
  | `Pat_infix (lp, cop, rp) ->
      let lp = map_lpat env lp in
      let op =
        match cop with
        | `Choice_qual_cons_op x ->
            let _q, id = map_qconsym env x in
            id
        | `BQUOT_choice_qual_cons_BQUOT (_bt, x, _bt2) ->
            let _q, id = map_qconid env x in
            id
      in
      let rp = map_pat env rp in
      PatInfix (lp, op, rp)
  | `Lpat x -> map_lpat env x

and map_pat_fields (env : env) ((lb, opt, rb) : CST.pat_fields) :
    field_pat list bracket =
  let fields =
    match opt with
    | None -> []
    | Some (first, rest) ->
        let map_field f =
          match f with
          | `DOTDOT _tok -> None
          | `Choice_choice_qual_var_opt_EQ_nested_pat (qv, opt_pat) ->
              let _q, id = map_qvar env qv in
              let pat =
                match opt_pat with
                | None -> None
                | Some (_eq, np) -> Some (map_nested_pat env np)
              in
              Some (id, pat)
        in
        let first = map_field first in
        let rest = List_.map (fun (_comma, f) -> map_field f) rest in
        List.filter_map Fun.id (first :: rest)
  in
  (token env lb, fields, token env rb)

(*****************************************************************************)
(* Expressions *)
(*****************************************************************************)

and map_exp_name (env : env) (x : CST.exp_name) : expr =
  match x with
  | `Choice_choice_qual_var x ->
      let n = map_qvar env x in
      Name n
  | `Qcon x ->
      let n = map_qcon env x in
      Constructor n
  | `Impl_parid tok ->
      Name ([], str env tok)
  | `Label tok ->
      Name ([], str env tok)

and map_aexp (env : env) (x : CST.aexp) : expr =
  match x with
  | `Exp_name x -> map_exp_name env x
  | `Exp_parens (lp, e, rp) ->
      let e = map_exp env e in
      Paren (token env lp, e, token env rp)
  | `Exp_tuple_ (lp, tup, rp) ->
      let exprs = map_exp_tuple env tup in
      Tuple (token env lp, exprs, token env rp)
  | `Exp_list (lb, first, rest, rb) ->
      let first = map_exp env first in
      let rest = List_.map (fun (_comma, e) -> map_exp env e) rest in
      List (token env lb, first :: rest, token env rb)
  | `Exp_th_quoted_name _ ->
      todo_expr env x ("ExprTHQuote", Tok.unsafe_fake_tok "")
  | `Exp_type_app _ ->
      todo_expr env x ("ExprTypeApp", Tok.unsafe_fake_tok "")
  | `Exp_lambda_case _ ->
      todo_expr env x ("ExprLambdaCase", Tok.unsafe_fake_tok "")
  | `Exp_do (do_kw, stmts) ->
      let do_tok =
        match do_kw with
        | `Do_module (_qual, kw) -> map_do_keyword env kw
        | `Do_kw kw -> map_do_keyword env kw
      in
      let stmts = map_stmt_list env stmts in
      Do (do_tok, fb stmts)
  | `Exp_record (ae, _lb, first_field, rest_fields, _rb) ->
      let base = map_aexp env ae in
      let fields = map_exp_fields env first_field rest_fields in
      (match base with
       | Constructor n ->
           RecordCon (n, fb fields)
       | _ ->
           RecordUpdate (base, fb fields))
  | `Exp_arit_seq (lb, from_, opt_then, _dotdot, opt_to, rb) ->
      let from_ = map_exp env from_ in
      let then_ = Option.map (fun (_comma, e) -> map_exp env e) opt_then in
      let to_ = Option.map (map_exp env) opt_to in
      let seq =
        match then_, to_ with
        | None, None -> FromSeq from_
        | Some then_, None -> FromThenSeq (from_, then_)
        | None, Some to_ -> FromToSeq (from_, to_)
        | Some then_, Some to_ -> FromThenToSeq (from_, then_, to_)
      in
      ArithSeq (token env lb, seq, token env rb)
  | `Exp_list_comp (lb, e, _bar, first_qual, rest_quals, rb) ->
      let e = map_exp env e in
      let first = map_qual env first_qual in
      let rest = List_.map (fun (_comma, q) -> map_qual env q) rest_quals in
      ListComp (e, (token env lb, first :: rest, token env rb))
  | `Exp_sect_left (lp, e, op, rp) ->
      let e = map_top_splice env e in
      let op = map_qop env op in
      Section (token env lp, SectionL (e, op), token env rp)
  | `Exp_sect_right (lp, op, e, rp) ->
      let op = map_qop_nominus env op in
      let e = map_top_splice env e in
      Section (token env lp, SectionR (op, e), token env rp)
  | `Exp_unbo_tuple _ ->
      todo_expr env x ("ExprUnboxedTuple", Tok.unsafe_fake_tok "")
  | `Exp_unbo_sum_ _ ->
      todo_expr env x ("ExprUnboxedSum", Tok.unsafe_fake_tok "")
  | `Splice _ ->
      todo_expr env x ("ExprSplice", Tok.unsafe_fake_tok "")
  | `Quas _ ->
      todo_expr env x ("ExprQuasiquote", Tok.unsafe_fake_tok "")
  | `Lit_ x ->
      let lit = map_literal_ env x in
      L lit

and map_exp_fields (env : env) (first : CST.exp_field)
    (rest : (CST.comma * CST.exp_field) list) : field_bind list =
  let map_field f =
    match f with
    | `DOTDOT _tok -> None
    | `Choice_choice_qual_var_opt_EQ_exp (qv, opt_exp) ->
        let _q, id = map_qvar env qv in
        let e = Option.map (fun (_eq, e) -> map_exp env e) opt_exp in
        Some (id, e)
  in
  let first = map_field first in
  let rest = List_.map (fun (_comma, f) -> map_field f) rest in
  List.filter_map Fun.id (first :: rest)

and map_exp_tuple (env : env) ((v1, v2) : CST.exp_tuple) : expr list =
  let first_part =
    match v1 with
    | `Rep1_comma_exp (_commas, e) ->
        [ map_exp env e ]
    | `Exp_comma_opt_exp (e1, _comma, opt_e2) ->
        let e1 = map_exp env e1 in
        (match opt_e2 with
         | Some e2 -> [ e1; map_exp env e2 ]
         | None -> [ e1 ])
  in
  let rest = List_.map (fun (_comma, opt_e) ->
    match opt_e with
    | Some e -> map_exp env e
    | None -> ExprTodo (("TupleMissing", Tok.unsafe_fake_tok ""), [])
  ) v2 in
  first_part @ rest

and map_fexp (env : env) (x : CST.fexp) : expr =
  match x with
  | `Aexp x -> map_aexp env x
  | `Exp_apply x -> map_exp_apply env x

and map_exp_apply (env : env) (x : CST.exp_apply) : expr =
  match x with
  | `Aexp x -> map_aexp env x
  | `Aexp_exp_apply (ae, ea) ->
      let f = map_aexp env ae in
      let arg = map_exp_apply env ea in
      App (f, arg)
  | `Aexp_exp_lambda (ae, lam) ->
      let f = map_aexp env ae in
      let arg = map_exp_lambda env lam in
      App (f, arg)
  | `Aexp_exp_let_in (ae, letin) ->
      let f = map_aexp env ae in
      let arg = map_exp_let_in env letin in
      App (f, arg)
  | `Aexp_exp_cond (ae, cond) ->
      let f = map_aexp env ae in
      let arg = map_exp_cond env cond in
      App (f, arg)
  | `Aexp_exp_case (ae, case) ->
      let f = map_aexp env ae in
      let arg = map_exp_case env case in
      App (f, arg)

and map_lexp (env : env) (x : CST.lexp) : expr =
  match x with
  | `Exp_let_in x -> map_exp_let_in env x
  | `Exp_cond x -> map_exp_cond env x
  | `Exp_if_guard (_if_tok, _gdpats) ->
      todo_expr env x ("ExprIfGuard", Tok.unsafe_fake_tok "")
  | `Exp_case x -> map_exp_case env x
  | `Exp_nega (minus, ae) ->
      let ae = map_aexp env ae in
      Prefix (str env minus, ae)
  | `Fexp x -> map_fexp env x
  | `Exp_lambda x -> map_exp_lambda env x

and map_exp_lambda (env : env) ((lam, pats, arr, body) : CST.exp_lambda) : expr =
  let lam_tok = token env lam in
  let pats = List_.map (map_apat env) pats in
  let arr_tok = map_arrow env arr in
  let body = map_exp env body in
  Lambda (lam_tok, pats, arr_tok, body)

and map_exp_let_in (env : env) ((let_part, in_part) : CST.exp_let_in) : expr =
  let let_tok, decls = map_exp_let env let_part in
  let in_tok, body = map_exp_in env in_part in
  Let (let_tok, decls, in_tok, body)

and map_exp_let (env : env) ((let_tok, opt_decls) : CST.exp_let) : Tok.t * decl list =
  let t = token env let_tok in
  let decls =
    match opt_decls with
    | None -> []
    | Some x -> map_let_decls env x
  in
  (t, decls)

and map_exp_in (env : env) ((in_tok, e) : CST.exp_in) : Tok.t * expr =
  let t = token env in_tok in
  let e = map_exp env e in
  (t, e)

and map_exp_cond (env : env)
    ((if_tok, cond, _semi1, _then_tok, then_e, _semi2, _else_tok, else_e) :
      CST.exp_cond) : expr =
  let if_tok = token env if_tok in
  let cond = map_exp env cond in
  let then_e = map_exp env then_e in
  let else_e = map_exp env else_e in
  If (if_tok, cond, then_e, else_e)

and map_exp_case (env : env)
    ((case_tok, scrut, _of_tok, opt_alts) : CST.exp_case) : expr =
  let case_tok = token env case_tok in
  let scrut = map_exp env scrut in
  let alts =
    match opt_alts with
    | None -> []
    | Some x -> map_alts env x
  in
  Case (case_tok, scrut, Tok.unsafe_fake_tok "of", fb alts)

and map_exp_infix (env : env) (x : CST.exp_infix) : expr =
  match x with
  | `Exp_infix_ (lhs, op, rhs) ->
      let lhs = map_top_splice env lhs in
      let op = map_qop env op in
      let rhs = map_lexp env rhs in
      Infix (lhs, op, rhs)
  | `Lexp x -> map_lexp env x

and map_top_splice (env : env) (x : CST.top_splice) : expr =
  map_exp_infix env x

and map_exp (env : env) ((splice, opt_ann) : CST.exp) : expr =
  let e = map_top_splice env splice in
  match opt_ann with
  | None -> e
  | Some (col, ty) ->
      let col = map_colon2 env col in
      let ty = map_type_or_implicit env ty in
      TypedExpr (e, col, ty)

(*****************************************************************************)
(* Statements (do-notation) *)
(*****************************************************************************)

and map_stmt (env : env) (x : CST.stmt) : stmt =
  match x with
  | `Exp e ->
      let e = map_exp env e in
      StmtExpr e
  | `Bind_pat (tp, larr, e) ->
      let p = map_typed_pat env tp in
      let larr = map_larrow env larr in
      let e = map_exp env e in
      StmtBind (p, larr, e)
  | `Let (let_tok, opt_decls) ->
      let let_tok = token env let_tok in
      let decls =
        match opt_decls with
        | None -> []
        | Some x -> map_decls env x
      in
      StmtLet (let_tok, decls)
  | `Rec (_rec_tok, stmts) ->
      let stmts = map_stmt_list env stmts in
      (match stmts with
       | [ s ] -> s
       | _ -> StmtExpr (ExprTodo (("RecStmt", Tok.unsafe_fake_tok ""), [])))

and map_stmt_list (env : env)
    (x : CST.anon_choice_LCURL_opt_stmt_rep_SEMI_stmt_opt_SEMI_RCURL_9605efc) :
    stmt list =
  match x with
  | `LCURL_opt_stmt_rep_SEMI_stmt_opt_SEMI_RCURL (_lb, opt, _semi, _rb) ->
      (match opt with
       | None -> []
       | Some (first, rest) ->
           let first = map_stmt env first in
           let rest = List_.map (fun (_semi, s) -> map_stmt env s) rest in
           first :: rest)
  | `Layout_start_opt_stmt_rep_choice_SEMI_stmt_opt_choice_SEMI_layout_end
      (_ls, opt, _le) ->
      (match opt with
       | None -> []
       | Some (first, rest, _semi) ->
           let first = map_stmt env first in
           let rest = List_.map (fun (_semi, s) -> map_stmt env s) rest in
           first :: rest)

(*****************************************************************************)
(* Guards *)
(*****************************************************************************)

and map_guard (env : env) (x : CST.guard) : guard =
  match x with
  | `Pat_guard (p, larr, e) ->
      let p = map_pat env p in
      let larr = map_larrow env larr in
      let e = map_top_splice env e in
      GuardBind (p, larr, e)
  | `Let (let_tok, opt_decls) ->
      let let_tok = token env let_tok in
      let decls =
        match opt_decls with
        | None -> []
        | Some x -> map_decls env x
      in
      GuardLet (let_tok, decls)
  | `Exp_infix e ->
      let e = map_top_splice env e in
      GuardExpr e

and map_guards (env : env) ((bar, first, rest) : CST.guards) :
    Tok.t * guard list =
  let bar = token env bar in
  let first = map_guard env first in
  let rest = List_.map (fun (_comma, g) -> map_guard env g) rest in
  (bar, first :: rest)

and map_guard_equation (env : env) ((guards, eq, body) : CST.guard_equation) :
    guarded_rhs =
  let bar, guards = map_guards env guards in
  let eq = token env eq in
  let body = map_exp env body in
  (bar, guards, eq, body)

and map_gdpat (env : env) ((guards, arr, body) : CST.gdpat) : guarded_rhs =
  let bar, guards = map_guards env guards in
  let arr = map_arrow env arr in
  let body = map_exp env body in
  (bar, guards, arr, body)

(*****************************************************************************)
(* Alternatives (case branches) *)
(*****************************************************************************)

and map_alt_variants (env : env) (x : CST.alt_variants) : rhs =
  match x with
  | `Arrow_exp (arr, e) ->
      let arr = map_arrow env arr in
      let e = map_exp env e in
      UnguardedRhs (arr, e)
  | `Rep1_gdpat gdpats ->
      let gdpats = List_.map (map_gdpat env) gdpats in
      GuardedRhss gdpats

and map_where_opt (env : env)
    (x : CST.anon_opt_where_opt_decls_4a349ec) : where option =
  match x with
  | None -> None
  | Some (where_tok, opt_decls) ->
      let where_tok = token env where_tok in
      let decls =
        match opt_decls with
        | None -> []
        | Some x -> map_decls env x
      in
      Some (where_tok, decls)

and map_alt (env : env) ((p, rhs, wh) : CST.alt) : alt =
  let p = map_pat env p in
  let rhs = map_alt_variants env rhs in
  let wh = map_where_opt env wh in
  (p, rhs, wh)

and map_alts (env : env) (x : CST.alts) : alt list =
  match x with
  | `LCURL_opt_alt_rep_SEMI_alt_opt_SEMI_RCURL (_lb, opt, _semi, _rb) ->
      (match opt with
       | None -> []
       | Some (first, rest) ->
           let first = map_alt env first in
           let rest = List_.map (fun (_semi, a) -> map_alt env a) rest in
           first :: rest)
  | `Layout_start_opt_alt_rep_choice_SEMI_alt_opt_choice_SEMI_layout_end
      (_ls, opt, _le) ->
      (match opt with
       | None -> []
       | Some (first, rest, _semi) ->
           let first = map_alt env first in
           let rest = List_.map (fun (_semi, a) -> map_alt env a) rest in
           first :: rest)

(*****************************************************************************)
(* Qualifiers (list comprehension) *)
(*****************************************************************************)

and map_qual (env : env) (x : CST.qual) : qual =
  match x with
  | `Bind_pat (tp, larr, e) ->
      let p = map_typed_pat env tp in
      let larr = map_larrow env larr in
      let e = map_exp env e in
      QualBind (p, larr, e)
  | `Let (let_tok, opt_decls) ->
      let let_tok = token env let_tok in
      let decls =
        match opt_decls with
        | None -> []
        | Some x -> map_decls env x
      in
      QualLet (let_tok, decls)
  | `Tran _ ->
      QualExpr (ExprTodo (("TransformQual", Tok.unsafe_fake_tok ""), []))
  | `Exp e ->
      let e = map_exp env e in
      QualExpr e

(*****************************************************************************)
(* Declarations *)
(*****************************************************************************)

and map_signature (env : env)
    ((first_name, rest_names, (col, ty)) : CST.signature) :
    decl =
  let first = map_pat_name env first_name in
  let rest = List_.map (fun (_comma, n) -> map_pat_name env n) rest_names in
  let col = map_colon2 env col in
  let ty = map_type_or_implicit env ty in
  TypeSig (first :: rest, col, ty)

and map_funrhs (env : env) ((rhs_val, where_opt) : CST.funrhs) :
    rhs * where option =
  let rhs =
    match rhs_val with
    | `EQ_exp (eq, e) ->
        let eq = token env eq in
        let e = map_exp env e in
        UnguardedRhs (eq, e)
    | `Fun_guards guards ->
        let guards = List_.map (map_guard_equation env) guards in
        GuardedRhss guards
  in
  let wh = map_where_opt env where_opt in
  (rhs, wh)

and map_funlhs (env : env) (x : CST.funlhs) : ident * pattern list =
  match x with
  | `Funvar (fname, opt_pats) ->
      let name = map_fun_name env fname in
      let pats =
        match opt_pats with
        | None -> []
        | Some ps -> List_.map (map_apat env) ps
      in
      (name, pats)
  | `Funpat_infix (lp, vop, rp) ->
      let lp = map_pat env lp in
      let name = map_varop env vop in
      let rp = map_pat env rp in
      (name, [ lp; rp ])

and map_function_ (env : env) ((lhs, rhs_and_where) : CST.function_) : decl =
  let name, pats = map_funlhs env lhs in
  let rhs, wh = map_funrhs env rhs_and_where in
  FunBind
    [
      {
        fm_name = name;
        fm_pats = pats;
        fm_rhs = rhs;
        fm_where = wh;
      };
    ]

and map_gendecl (env : env) (x : CST.gendecl) : decl =
  match x with
  | `Sign x -> map_signature env x
  | `Fixity (fix_kw, opt_prec, first_op, rest_ops) ->
      let fix_tok =
        match fix_kw with
        | `Infixl tok
        | `Infixr tok
        | `Infix tok ->
            token env tok
      in
      let prec =
        match opt_prec with
        | None -> None
        | Some x ->
            let lit = map_integer env x in
            (match lit with
             | Int pi -> Some pi
             | _ -> None)
      in
      let first_op = map_op env first_op in
      let rest_ops = List_.map (fun (_comma, o) -> map_op env o) rest_ops in
      Fixity
        {
          fix_tok;
          fix_prec = prec;
          fix_ops = first_op :: rest_ops;
        }

and map_decl_fun (env : env) (x : CST.decl_fun) : decl =
  match x with
  | `Func x -> map_function_ env x
  | `Funpat (tp, rhs_and_where) ->
      let p = map_typed_pat env tp in
      let rhs, wh = map_funrhs env rhs_and_where in
      PatBind (p, rhs, wh)

and map_decl (env : env) (x : CST.decl) : decl =
  match x with
  | `Gend x -> map_gendecl env x
  | `Decl_fun x -> map_decl_fun env x

(*****************************************************************************)
(* Top-level declarations *)
(*****************************************************************************)

and map_simpletype (env : env) (x : CST.simpletype) : ident * ident list =
  match x with
  | `Choice_cons_rep_choice_anno_type_var (tycon, tyvars) ->
      let name = map_simple_tycon env tycon in
      let tyvars = List_.map (map_tyvar env) tyvars in
      (name, tyvars)
  | `LPAR_simp_RPAR (_lp, head, _rp) ->
      map_simpletype env head
  | `Simp_infix (_tv1, _op, _tv2) ->
      (("_infix", Tok.unsafe_fake_tok ""), [])

and map_constructors (env : env)
    ((opt_forall, opt_ctx, con_decl, rest) : CST.constructors) :
    constr_decl list =
  let _forall = opt_forall in
  let _ctx = opt_ctx in
  let map_one _forall _ctx x =
    match x with
    | `Data_cons (tycon, args) ->
        let name = map_tyconid env tycon in
        let args = List_.map (fun arg ->
          match arg with
          | `Strict_type (_bang, at) -> map_atype env at
          | `Atype at -> map_atype env at
        ) args in
        { cd_name = name; cd_args = PosConstrArgs args }
    | `Data_cons_infix (_lhs, _cop, _rhs) ->
        { cd_name = ("_infix", Tok.unsafe_fake_tok "");
          cd_args = PosConstrArgs [] }
    | `Data_cons_record (tycon, (lb, first_field, rest_fields, rb)) ->
        let name = map_tyconid env tycon in
        let map_field (vars, _comma_vars, _col, field_ty) =
          let vars =
            let first = map_variable env vars in
            first :: List_.map (fun (_comma, v) -> map_variable env v) _comma_vars
          in
          let ty =
            match field_ty with
            | `Strict_type (_bang, at) -> map_atype env at
            | `Type t -> map_type_ env t
          in
          (vars, ty)
        in
        let first = map_field first_field in
        let rest = List_.map (fun (_comma, f) -> map_field f) rest_fields in
        { cd_name = name;
          cd_args = RecConstrArgs (token env lb, first :: rest, token env rb) }
  in
  let first = map_one opt_forall opt_ctx con_decl in
  let rest = List_.map (fun (_bar, f, c, d) -> map_one f c d) rest in
  first :: rest

and map_deriving (env : env)
    ((deriv_tok, _strat, classes, _via) : CST.deriving) : deriving =
  let deriv_tok = token env deriv_tok in
  let names =
    match classes with
    | `Choice_qual_type x ->
        let n = map_qtyconid env x in
        fb [ n ]
    | `LPAR_opt_cons__rep_comma_cons__RPAR (lp, opt, rp) ->
        let names =
          match opt with
          | None -> []
          | Some (first, rest) ->
              let map_c c =
                match map_constraint__ env c with
                | ClassConstraint (n, _) -> n
              in
              let first = map_c first in
              let rest = List_.map (fun (_comma, c) -> map_c c) rest in
              first :: rest
        in
        (token env lp, names, token env rp)
  in
  (deriv_tok, names)

and map_adt (env : env) (x : CST.adt) : data_body * deriving list =
  match x with
  | `Adt_rhs (eq, constrs, derivs) ->
      let eq = token env eq in
      let constrs = map_constructors env constrs in
      let derivs = List_.map (map_deriving env) derivs in
      (DataConstrs (eq, constrs), derivs)
  | `Gadt_rhs (where_tok, _opt_body) ->
      let where_tok = token env where_tok in
      (DataGADT (where_tok, []), [])

and map_cdecl (env : env) (x : CST.cdecl) : decl =
  match x with
  | `Gend x -> map_gendecl env x
  | `Defa_sign (_default, sig_) ->
      map_signature env sig_
  | `Func x -> map_function_ env x
  | `Class_tyfam _ ->
      todo_decl env x ("ClassTyFam", Tok.unsafe_fake_tok "")
  | `Inst_tyinst _ ->
      todo_decl env x ("InstTyInst", Tok.unsafe_fake_tok "")
  | `Class_data _ ->
      todo_decl env x ("ClassData", Tok.unsafe_fake_tok "")

and map_class_body (env : env)
    ((_where_tok, opt_body) : CST.class_body) : (Tok.t * decl list) option =
  let where_tok = Tok.unsafe_fake_tok "where" in
  match opt_body with
  | None -> Some (where_tok, [])
  | Some body ->
      let decls =
        match body with
        | `LCURL_opt_cdecl_rep_SEMI_cdecl_opt_SEMI_RCURL
            (_lb, opt, _semi, _rb) ->
            (match opt with
             | None -> []
             | Some (first, rest) ->
                 let first = map_cdecl env first in
                 let rest = List_.map (fun (_semi, d) -> map_cdecl env d) rest in
                 first :: rest)
        | `Layout_start_opt_cdecl_rep_choice_SEMI_cdecl_opt_choice_SEMI_layout_end
            (_ls, opt, _le) ->
            (match opt with
             | None -> []
             | Some (first, rest, _semi) ->
                 let first = map_cdecl env first in
                 let rest = List_.map (fun (_semi, d) -> map_cdecl env d) rest in
                 first :: rest)
      in
      Some (where_tok, decls)

and map_idecl (env : env) (x : CST.idecl) : decl =
  match x with
  | `Func x -> map_function_ env x
  | `Sign x -> map_signature env x
  | `Inst_data _ ->
      todo_decl env x ("InstData", Tok.unsafe_fake_tok "")
  | `Inst_tyinst _ ->
      todo_decl env x ("InstTyInst", Tok.unsafe_fake_tok "")

and map_inst_body (env : env) opt : (Tok.t * decl list) option =
  match opt with
  | None -> None
  | Some (where_tok, opt_body) ->
      let where_tok = token env where_tok in
      let decls =
        match opt_body with
        | None -> []
        | Some body ->
            (match body with
             | `LCURL_opt_idecl_rep_SEMI_idecl_opt_SEMI_RCURL
                 (_lb, opt, _semi, _rb) ->
                 (match opt with
                  | None -> []
                  | Some (first, rest) ->
                      let first = map_idecl env first in
                      let rest = List_.map (fun (_semi, d) -> map_idecl env d) rest in
                      first :: rest)
             | `Layout_start_opt_idecl_rep_choice_SEMI_idecl_opt_choice_SEMI_layout_end
                 (_ls, opt, _le) ->
                 (match opt with
                  | None -> []
                  | Some (first, rest, _semi) ->
                      let first = map_idecl env first in
                      let rest = List_.map (fun (_semi, d) -> map_idecl env d) rest in
                      first :: rest))
      in
      Some (where_tok, decls)

and map_instance (env : env)
    ((_inst_tok, opt_forall, opt_ctx, constraint_) : CST.instance) :
    name * type_ list =
  let _forall = opt_forall in
  let _ctx = opt_ctx in
  let c = map_constraint_ env constraint_ in
  match c with
  | ClassConstraint (n, tys) -> (n, tys)

(*****************************************************************************)
(* Imports *)
(*****************************************************************************)

and map_import_item (env : env) ((_ns_opt, item) : CST.import_item) : import_spec =
  match item with
  | `Choice_var x ->
      let id = map_pat_name env x in
      ImportVar id
  | `Choice_cons_opt_import_con_names (tycon, opt_names) ->
      let id = map_simple_tycon env tycon in
      let subs =
        match opt_names with
        | None -> None
        | Some (lp, opt, rp) ->
            (match opt with
             | None -> None
             | Some x ->
                 (match x with
                  | `DOTDOT tok ->
                      Some (ExportAll (token env tok))
                  | `Import_name_rep_comma_import_name (first, rest) ->
                      let map_name n =
                        match (n : CST.import_name) with
                        | `Choice_cons x -> map_con env x
                        | `Choice_var x -> map_pat_name env x
                      in
                      let first = map_name first in
                      let rest = List_.map (fun (_comma, n) -> map_name n) rest in
                      Some (ExportSome (token env lp, first :: rest, token env rp))))
      in
      ImportType (id, subs)

and map_import_list (env : env)
    ((hiding, _lp, opt_items, _rp) : CST.import_list) :
    Tok.t option * import_spec list bracket =
  let hiding = Option.map (token env) hiding in
  let specs =
    match opt_items with
    | None -> []
    | Some (first, rest, _comma) ->
        let first = map_import_item env first in
        let rest = List_.map (fun (_comma, i) -> map_import_item env i) rest in
        first :: rest
  in
  (hiding, fb specs)

and map_topdecl_import (env : env)
    ((imp_tok, qual1, _pkg, modname, qual2, as_opt, imp_list) :
      CST.decl_import) : import =
  let imp_tok = token env imp_tok in
  let qualified =
    match qual1, qual2 with
    | Some tok, _ | _, Some tok -> Some (token env tok)
    | None, None -> None
  in
  let mod_name = map_qmodid env modname in
  let as_alias =
    match as_opt with
    | None -> None
    | Some (as_tok, m) ->
        let _q, id = map_qmodid env m in
        Some (token env as_tok, id)
  in
  let hiding, specs =
    match imp_list with
    | None -> (None, None)
    | Some x ->
        let h, s = map_import_list env x in
        (h, Some s)
  in
  {
    imp_tok;
    imp_qualified = qualified;
    imp_module = mod_name;
    imp_as = as_alias;
    imp_hiding = hiding;
    imp_specs = specs;
  }

(*****************************************************************************)
(* Exports *)
(*****************************************************************************)

and map_export (env : env) (x : CST.export) : export =
  match x with
  | `Choice_choice_qual_var x ->
      let n = map_qvar env x in
      ExportVar n
  | `Opt_name_choice_choice_qual_type_opt_export_names (_ns, tycon, opt_names) ->
      let n = map_qtycon env tycon in
      let subs =
        match opt_names with
        | None -> None
        | Some (lp, opt, rp) ->
            (match opt with
             | None -> Some (ExportSome (token env lp, [], token env rp))
             | Some x ->
                 (match x with
                  | `DOTDOT tok ->
                      Some (ExportAll (token env tok))
                  | `Opt_name_rep_comma_name opt_names ->
                      (match opt_names with
                       | None -> Some (ExportSome (token env lp, [], token env rp))
                       | Some (first, rest) ->
                           let map_name (n : CST.name) =
                             match n with
                             | `Choice_var x -> map_pat_name env x
                             | `Choice_cons x -> map_con env x
                           in
                           let first = map_name first in
                           let rest = List_.map (fun (_comma, n) -> map_name n) rest in
                           Some (ExportSome (token env lp, first :: rest, token env rp)))))
      in
      ExportType (n, subs)
  | `Module_qmodid (_mod_tok, m) ->
      let n = map_qmodid env m in
      ExportModule (Tok.unsafe_fake_tok "module", n)

and map_exports (env : env) ((lp, opt, _comma, rp) : CST.exports) :
    export list bracket =
  let exports =
    match opt with
    | None -> []
    | Some (first, rest) ->
        let first = map_export env first in
        let rest = List_.map (fun (_comma, e) -> map_export env e) rest in
        first :: rest
  in
  (token env lp, exports, token env rp)

(*****************************************************************************)
(* Top declarations *)
(*****************************************************************************)

and map_topdecl (env : env) (x : CST.topdecl) : decl list =
  match x with
  | `Decl_type (type_tok, head, rhs) ->
      let type_tok = token env type_tok in
      let name, tparams = map_simpletype env head in
      (match rhs with
       | `EQ_type_or_impl (eq, ty) ->
           let eq = token env eq in
           let ty = map_type_or_implicit env ty in
           [ TypeAlias (type_tok, name, tparams, eq, ty) ]
       | `Type_anno (_col, _ty) ->
           [ todo_decl env x ("TypeFamilyDecl", Tok.unsafe_fake_tok "") ])
  | `Decl_tyfam _ ->
      [ todo_decl env x ("TypeFamily", Tok.unsafe_fake_tok "") ]
  | `Decl_tyinst _ ->
      [ todo_decl env x ("TypeInstance", Tok.unsafe_fake_tok "") ]
  | `Decl_role _ ->
      [ todo_decl env x ("TypeRole", Tok.unsafe_fake_tok "") ]
  | `Decl_adt (data_tok, opt_ctx, head, _opt_ann, opt_body) ->
      let data_tok = token env data_tok in
      let _ctx = opt_ctx in
      let name, tparams = map_simpletype env head in
      let body, derivs =
        match opt_body with
        | None -> (DataConstrs (Tok.unsafe_fake_tok "=", []), [])
        | Some x ->
            (match x with
             | `Adt adt -> map_adt env adt
             | `Rep_deri derivs ->
                 let derivs = List_.map (map_deriving env) derivs in
                 (DataConstrs (Tok.unsafe_fake_tok "=", []), derivs))
      in
      [ DataDecl {
          d_tok = data_tok;
          d_name = name;
          d_tparams = tparams;
          d_body = body;
          d_deriving = derivs;
        } ]
  | `Decl_newt (newt_tok, ctx_newt, body) ->
      let newt_tok = token env newt_tok in
      let name, tparams =
        match ctx_newt with
        | `Cont__simp (_ctx, head) -> map_simpletype env head
        | `Simp head -> map_simpletype env head
      in
      let body, derivs =
        match body with
        | `Newt (eq, (con_id, con_arg), derivs) ->
            let eq = token env eq in
            let con_name = map_tyconid env con_id in
            let con_type =
              match con_arg with
              | `Atype at -> map_atype env at
              | `Record_field (_lb, _field, _rb) ->
                  todo_type env body ("NewtypeRecord", Tok.unsafe_fake_tok "")
            in
            let constrs = [ { cd_name = con_name; cd_args = PosConstrArgs [ con_type ] } ] in
            let derivs = List_.map (map_deriving env) derivs in
            (DataConstrs (eq, constrs), derivs)
        | `Opt_type_anno_gadt_rhs (_opt_ann, (where_tok, _body)) ->
            let where_tok = token env where_tok in
            (DataGADT (where_tok, []), [])
      in
      [ DataDecl {
          d_tok = newt_tok;
          d_name = name;
          d_tparams = tparams;
          d_body = body;
          d_deriving = derivs;
        } ]
  | `Decl_data_8db362d _ ->
      [ todo_decl env x ("DataFamily", Tok.unsafe_fake_tok "") ]
  | `Decl_data_2e645bc _ ->
      [ todo_decl env x ("DataInstance", Tok.unsafe_fake_tok "") ]
  | `Decl_import imp ->
      (* Imports are handled separately, but in case they appear as topdecl *)
      let _imp = map_topdecl_import env imp in
      []
  | `Decl_class (class_tok, opt_ctx, constraint_, _fundeps, opt_body) ->
      let class_tok = token env class_tok in
      let ctx =
        match opt_ctx with
        | None -> None
        | Some ctx ->
            let constraints, arrow = map_context env ctx in
            Some (constraints, arrow)
      in
      let cls_constraint = map_constraint_ env constraint_ in
      let name, tparams =
        match cls_constraint with
        | ClassConstraint (n, tys) ->
            let tparams = List_.map (fun ty ->
              match ty with
              | TyVar id -> id
              | _ -> ("_", Tok.unsafe_fake_tok "")
            ) tys in
            (snd n, tparams)
      in
      let where =
        match opt_body with
        | None -> None
        | Some body -> map_class_body env body
      in
      [ ClassDecl {
          cl_tok = class_tok;
          cl_ctx = ctx;
          cl_name = name;
          cl_tparams = tparams;
          cl_where = where;
        } ]
  | `Decl_inst (inst, opt_body) ->
      let inst_name, inst_types = map_instance env inst in
      let in_tok = Tok.unsafe_fake_tok "instance" in
      let ctx = None in
      let where = map_inst_body env opt_body in
      [ InstanceDecl {
          in_tok;
          in_ctx = ctx;
          in_name = inst_name;
          in_types = inst_types;
          in_where = where;
        } ]
  | `Decl_defa (default_tok, _lp, _opt_types, _rp) ->
      [ DefaultDecl (token env default_tok, fb []) ]
  | `Decl_fore x ->
      (match x with
       | `Decl_fore_import (foreign_tok, _imp, _pre, _str, _sig) ->
           [ ForeignDecl (str env foreign_tok) ]
       | `Decl_fore_export (foreign_tok, _exp, _pre, _str, _sig) ->
           [ ForeignDecl (str env foreign_tok) ])
  | `Decl_deri (deriv_tok, _opt, _inst) ->
      [ DerivDecl (token env deriv_tok, Tok.unsafe_fake_tok "instance",
                   TyName ([], ("_", Tok.unsafe_fake_tok ""))) ]
  | `Decl x ->
      [ map_decl env x ]
  | `Decl_pat _ ->
      [ todo_decl env x ("PatternSynonym", Tok.unsafe_fake_tok "") ]
  | `Top_splice e ->
      let _e = map_top_splice env e in
      []

(*****************************************************************************)
(* Module *)
(*****************************************************************************)

let collect_topdecls (env : env) (all_topdecls : CST.topdecl list) :
    import list * decl list =
  let imports = ref [] in
  let decls = ref [] in
  List.iter (fun td ->
    match td with
    | `Decl_import imp ->
        imports := map_topdecl_import env imp :: !imports
    | _ ->
        decls := map_topdecl env td @ !decls
  ) all_topdecls;
  (List.rev !imports, List.rev !decls)

let map_topdecl_list_layout (env : env) (topdecls :
    CST.anon_topd_rep_choice_SEMI_topd_opt_choice_SEMI_eb02f02) :
    import list * decl list =
  let (first, rest, _semi) = topdecls in
  let all_topdecls = first :: List_.map snd rest in
  collect_topdecls env all_topdecls

let map_module_ (env : env) (x : CST.haskell) : program =
  match x with
  | `Empty_file _tok ->
      { p_module = None; p_imports = []; p_decls = [] }
  | `Module (mod_tok, modname, opt_exports, where_tok, opt_body) ->
      let mod_tok = token env mod_tok in
      let mod_name = map_qmodid env modname in
      let exports = Option.map (map_exports env) opt_exports in
      let where_tok = token env where_tok in
      let imports, decls =
        match opt_body with
        | None -> ([], [])
        | Some body ->
            (match body with
             | `LCURL_opt_topd_rep_SEMI_topd_opt_SEMI_RCURL
                 (_lb, opt, _semi, _rb) ->
                 (match opt with
                  | None -> ([], [])
                  | Some (first, rest) ->
                      let all = first :: List_.map snd rest in
                      collect_topdecls env all)
             | `Layout_start_opt_topd_rep_choice_SEMI_topd_opt_choice_SEMI_layout_end
                 (_ls, opt, _le) ->
                 (match opt with
                  | None -> ([], [])
                  | Some x -> map_topdecl_list_layout env x))
      in
      let module_head =
        { m_tok = mod_tok;
          m_name = mod_name;
          m_exports = exports;
          m_where = where_tok }
      in
      { p_module = Some module_head; p_imports = imports; p_decls = decls }
  | `Topd_rep_choice_SEMI_topd_opt_choice_SEMI x ->
      let imports, decls = map_topdecl_list_layout env x in
      { p_module = None; p_imports = imports; p_decls = decls }

(*****************************************************************************)
(* Entry point *)
(*****************************************************************************)

let parse file =
  H.wrap_parser
    (fun () -> Tree_sitter_haskell.Parse.file !!file)
    (fun cst _extras ->
      let env = { H.file; conv = H.line_col_to_pos file; extra = () } in
      map_module_ env cst)

let parse_pattern str =
  H.wrap_parser
    (fun () -> Tree_sitter_haskell.Parse.string str)
    (fun cst _extras ->
      let file = Fpath.v "<pattern>" in
      let env = { H.file; conv = H.line_col_to_pos_pattern str; extra = () } in
      let prog = map_module_ env cst in
      match prog.p_decls with
      | [ d ] -> D d
      | _ -> Pr prog)
