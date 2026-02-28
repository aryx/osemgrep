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
open Common
open AST_haskell
module G = AST_generic
module H = AST_generic_helpers

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(* AST_haskell to AST_generic.
 *
 * See AST_generic.ml for more information.
 *)

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)
let id x = x
let _option = Option.map
let list = List_.map
let string = id
let fake = G.fake
let fb = Tok.unsafe_fake_bracket

(*****************************************************************************)
(* Entry point helpers *)
(*****************************************************************************)

let info x = x
let tok v = info v

let wrap _of_a (v1, v2) =
  let v1 = _of_a v1 and v2 = info v2 in
  (v1, v2)

let bracket of_a (t1, x, t2) = (info t1, of_a x, info t2)

let rec ident v = wrap string v

and name_ (v1, v2) =
  let v2 = ident v2 in
  match v1 with
  | [] -> G.Id (v2, G.empty_id_info ())
  | _ ->
      let v1 = list ident v1 in
      H.name_of_ids (v1 @ [ v2 ])

and dotted_ident_of_name (v1, v2) =
  let v1 = list ident v1 and v2 = ident v2 in
  v1 @ [ v2 ]

and todo_category v = ident v

(*****************************************************************************)
(* Types *)
(*****************************************************************************)

and type_ x =
  let tk = type_kind x in
  tk |> G.t

and type_kind = function
  | TyEllipsis v1 ->
      let v1 = tok v1 in
      G.TyEllipsis v1
  | TyName v1 ->
      let v1 = name_ v1 in
      G.TyN v1
  | TyVar v1 ->
      let v1 = ident v1 in
      G.TyVar v1
  | TyApp (v1, v2) ->
      let v1 = type_ v1 and v2 = type_ v2 in
      G.TyApply (v1, fb [ G.TA v2 ])
  | TyFun (v1, _tarrow, v2) ->
      let v1 = type_ v1 and v2 = type_ v2 in
      G.TyFun ([ G.Param (G.param_of_type v1) ], v2)
  | TyTuple v1 ->
      let v1 = bracket (list type_) v1 in
      G.TyTuple v1
  | TyList (l, v1, r) ->
      let v1 = type_ v1 in
      G.TyApply
        ( G.TyN (G.Id (("[]", l), G.empty_id_info ())) |> G.t,
          (l, [ G.TA v1 ], r) )
  | TyForall (_tforall, tvars, ty) ->
      let _tvars = list ident tvars in
      let ty = type_ ty in
      (* TODO: encode forall properly *)
      ty.G.t
  | TyContext (ctx, _tarrow, ty) ->
      let _ctx = context ctx in
      let ty = type_ ty in
      (* TODO: encode context/constraints *)
      ty.G.t
  | TyBang (_tbang, ty) ->
      let ty = type_ ty in
      (* strict annotation, just pass through *)
      ty.G.t
  | TyParen (_l, ty, _r) -> type_kind ty
  | TyTodo (t, v1) ->
      let t = todo_category t in
      let v1 = list type_ v1 in
      G.OtherType (t, list (fun x -> G.T x) v1)

and context xs = list constraint_ xs

and constraint_ = function
  | ClassConstraint (n, tys) ->
      let _n = name_ n in
      let _tys = list type_ tys in
      ()

(*****************************************************************************)
(* Literals *)
(*****************************************************************************)

and literal = function
  | Int v1 -> G.Int v1
  | Float v1 ->
      let v1 = wrap id v1 in
      G.Float v1
  | Char v1 ->
      let v1 = wrap string v1 in
      G.Char v1
  | String v1 ->
      let v1 = wrap string v1 in
      G.String (fb v1)

(*****************************************************************************)
(* Patterns *)
(*****************************************************************************)

and pattern = function
  | PatEllipsis v1 ->
      let v1 = tok v1 in
      G.PatEllipsis v1
  | PatVar v1 ->
      let v1 = ident v1 in
      G.PatId (v1, G.empty_id_info ())
  | PatLiteral v1 ->
      let v1 = literal v1 in
      G.PatLiteral v1
  | PatConstructor (v1, v2) ->
      let v1 = name_ v1 in
      let v2 = list pattern v2 in
      G.PatConstructor (v1, v2)
  | PatInfix (v1, v2, v3) ->
      let v1 = pattern v1 in
      let v2 = ident v2 in
      let v3 = pattern v3 in
      let n = H.name_of_ids [ v2 ] in
      G.PatConstructor (n, [ v1; v3 ])
  | PatTuple v1 ->
      let v1 = bracket (list pattern) v1 in
      G.PatTuple v1
  | PatList v1 ->
      let v1 = bracket (list pattern) v1 in
      G.PatList v1
  | PatWildcard v1 ->
      let v1 = tok v1 in
      G.PatWildcard v1
  | PatAs (v1, _tat, v2) ->
      let v1 = pattern v1 and v2 = ident v2 in
      G.PatAs (v1, (v2, G.empty_id_info ()))
  | PatRecord (n, flds) ->
      let _n = name_ n in
      let flds =
        bracket
          (list (fun (id, pat_opt) ->
               let id = ident id in
               let pat =
                 match pat_opt with
                 | Some p -> pattern p
                 | None -> G.PatId (id, G.empty_id_info ())
               in
               ([ id ], pat)))
          flds
      in
      G.PatRecord flds
  | PatTyped (v1, _, v2) ->
      let v1 = pattern v1 and v2 = type_ v2 in
      G.PatTyped (v1, v2)
  | PatIrrefutable (_ttilde, v1) ->
      let v1 = pattern v1 in
      v1
  | PatBang (_tbang, v1) ->
      let v1 = pattern v1 in
      v1
  | PatNeg (_tminus, lit) ->
      let lit = literal lit in
      G.PatLiteral lit
  | PatParen (_l, p, _r) -> pattern p
  | PatTodo (t, xs) ->
      let t = todo_category t in
      let xs = list pattern xs in
      G.OtherPat (t, list (fun x -> G.P x) xs)

(*****************************************************************************)
(* Expressions *)
(*****************************************************************************)

and expr e =
  (match e with
  | Ellipsis v1 ->
      let v1 = tok v1 in
      G.Ellipsis v1
  | DeepEllipsis (v1, v2, v3) ->
      let v1 = tok v1 in
      let v2 = expr v2 in
      let v3 = tok v3 in
      G.DeepEllipsis (v1, v2, v3)
  | L v1 ->
      let v1 = literal v1 in
      G.L v1
  | Name v1 ->
      let v1 = name_ v1 in
      G.N v1
  | Constructor v1 ->
      let v1 = name_ v1 in
      G.N v1
  | App (v1, v2) ->
      let v1 = expr v1 and v2 = expr v2 in
      G.Call (v1, fb [ G.Arg v2 ])
  | Infix (v1, v2, v3) ->
      let v1 = expr v1 in
      let v2 = ident v2 in
      let v3 = expr v3 in
      G.Call
        ( G.N (G.Id (v2, G.empty_id_info ())) |> G.e,
          fb [ G.Arg v1; G.Arg v3 ] )
  | Prefix (v1, v2) ->
      let v1 = ident v1 and v2 = expr v2 in
      G.Call (G.N (G.Id (v1, G.empty_id_info ())) |> G.e, fb [ G.Arg v2 ])
  | Lambda (tlambda, pats, _tarrow, body) ->
      let pats = list param_of_pattern pats in
      let body = expr body in
      G.Lambda
        {
          G.fparams = fb pats;
          frettype = None;
          fkind = (G.LambdaKind, tlambda);
          fbody = G.FBExpr body;
        }
  | Let (_tlet, binds, _tin, body) ->
      let defs = list decl_to_stmts binds |> List_.flatten in
      let body = expr body in
      let st = G.Block (fb (defs @ [ G.exprstmt body ])) |> G.s in
      G.stmt_to_expr st |> fun x -> x.G.e
  | If (tif, cond, then_, else_) ->
      let cond = expr cond in
      let then_ = expr then_ in
      let else_ = expr else_ in
      let s =
        G.If (tif, G.Cond cond, G.exprstmt then_, Some (G.exprstmt else_))
        |> G.s
      in
      G.stmt_to_expr s |> fun x -> x.G.e
  | Case (tcase, scrut, _tof, (l, alts, r)) ->
      let scrut = expr scrut in
      let alts =
        list (fun a -> a |> alt_to_match_case |> G.case_of_pat_and_expr) alts
      in
      let s = G.Switch (tcase, Some (G.Cond scrut), alts) |> G.s in
      G.stmt_to_expr s |> fun x -> x.G.e
      |> fun e ->
      ignore (l, r);
      e
  | Do (_tdo, (l, stmts, r)) ->
      let stmts = list stmt_to_stmt stmts in
      let s = G.Block (l, stmts, r) |> G.s in
      G.stmt_to_expr s |> fun x -> x.G.e
  | ListComp (e, (_l, quals, _r)) ->
      let e = expr e in
      let comps = list qual_to_comp quals in
      G.Comprehension (G.List, fb (e, comps))
  | ArithSeq (l, seq, r) ->
      let args = arith_seq_to_exprs seq in
      G.Call
        ( G.N (G.Id (("enumFromTo", l), G.empty_id_info ())) |> G.e,
          (l, list (fun x -> G.Arg x) args, r) )
  | RecordCon (n, flds) ->
      let _n = name_ n in
      let flds =
        bracket
          (list (fun (id, e_opt) ->
               let id = ident id in
               let e =
                 match e_opt with
                 | Some e -> expr e
                 | None -> G.N (G.Id (id, G.empty_id_info ())) |> G.e
               in
               G.basic_field id (Some e) None))
          flds
      in
      G.Record flds
  | RecordUpdate (e, flds) ->
      let e = expr e in
      let flds =
        bracket
          (list (fun (id, e_opt) ->
               let id = ident id in
               let e2 =
                 match e_opt with
                 | Some e -> expr e
                 | None -> G.N (G.Id (id, G.empty_id_info ())) |> G.e
               in
               G.basic_field id (Some e2) None))
          flds
      in
      let obj = G.Record flds |> G.e in
      G.OtherExpr (("With", fake ""), [ G.E e; G.E obj ])
  | Section (_l, sec, _r) -> (
      match sec with
      | SectionL (e, op) ->
          let e = expr e in
          let op = ident op in
          G.Call
            ( G.N (G.Id (op, G.empty_id_info ())) |> G.e,
              fb [ G.Arg e ] )
      | SectionR (op, e) ->
          let e = expr e in
          let op = ident op in
          G.Lambda
            {
              G.fparams = fb [];
              frettype = None;
              fkind = (G.LambdaKind, snd op);
              fbody =
                G.FBExpr
                  (G.Call
                     ( G.N (G.Id (op, G.empty_id_info ())) |> G.e,
                       fb [ G.Arg e ] )
                  |> G.e);
            })
  | TypedExpr (v1, v2, v3) -> (
      let v1 = expr v1 in
      let v2 = tok v2 in
      let v3 = type_ v3 in
      match v1.G.e with
      | G.N (G.Id (id, _idinfo)) when AST_generic.is_metavar_name (fst id) ->
          G.TypedMetavar (id, v2, v3)
      | _ -> G.Cast (v3, v2, v1))
  | Tuple v1 ->
      let v1 = bracket (list expr) v1 in
      G.Container (G.Tuple, v1)
  | List v1 ->
      let v1 = bracket (list expr) v1 in
      G.Container (G.List, v1)
  | Paren (_l, e, _r) -> (expr e).G.e
  | ExprTodo (t, xs) ->
      let t = todo_category t in
      let xs = list expr xs in
      G.OtherExpr (t, list (fun x -> G.E x) xs))
  |> G.e

and param_of_pattern pat =
  let p = pattern pat in
  match p with
  | G.PatEllipsis t -> G.ParamEllipsis t
  | G.PatId (id, _idinfo) -> G.Param (G.param_of_id id)
  | G.PatTyped (G.PatId (id, _idinfo), ty) ->
      G.Param { (G.param_of_id id) with G.ptype = Some ty }
  | _ -> G.ParamPattern p

(*****************************************************************************)
(* Statements (do-notation) *)
(*****************************************************************************)

and stmt_to_stmt = function
  | StmtExpr e ->
      let e = expr e in
      G.exprstmt e
  | StmtBind (p, _tarrow, e) ->
      let p = pattern p in
      let e = expr e in
      let exp = G.LetPattern (p, e) |> G.e in
      G.exprstmt exp
  | StmtLet (_tlet, decls) ->
      let stmts = list decl_to_stmts decls |> List_.flatten in
      G.Block (fb stmts) |> G.s

(*****************************************************************************)
(* Alternatives *)
(*****************************************************************************)

and alt_to_match_case (pat, rhs, where_opt) : G.pattern * G.expr =
  let pat = pattern pat in
  let e = rhs_to_expr rhs in
  let e =
    match where_opt with
    | None -> e
    | Some (_twhere, decls) ->
        let defs = list decl_to_stmts decls |> List_.flatten in
        let st = G.Block (fb (defs @ [ G.exprstmt e ])) |> G.s in
        G.stmt_to_expr st
  in
  (pat, e)

and rhs_to_expr = function
  | UnguardedRhs (_tok, e) -> expr e
  | GuardedRhss grhss ->
      let rec guards_to_expr = function
        | [] -> G.OtherExpr (("GuardFail", fake ""), []) |> G.e
        | (_, guards, _tok, e) :: rest ->
            let cond = guards_to_cond guards in
            let then_ = expr e in
            let else_ = guards_to_expr rest in
            let s =
              G.If
                ( fake "if",
                  G.Cond cond,
                  G.exprstmt then_,
                  Some (G.exprstmt else_) )
              |> G.s
            in
            G.stmt_to_expr s
      in
      guards_to_expr grhss

and guards_to_cond = function
  | [] -> G.L (G.Bool (true, fake "True")) |> G.e
  | [ GuardExpr e ] -> expr e
  | GuardExpr e :: rest ->
      let e = expr e in
      let rest_cond = guards_to_cond rest in
      G.Call
        ( G.Special (G.Op G.And, fake "&&") |> G.e,
          fb [ G.Arg e; G.Arg rest_cond ] )
      |> G.e
  | GuardBind (_, _, _) :: rest | GuardLet _ :: rest ->
      (* TODO: proper handling of pattern guards *)
      guards_to_cond rest

(*****************************************************************************)
(* Qualifiers *)
(*****************************************************************************)

and qual_to_comp = function
  | QualExpr e ->
      let e = expr e in
      G.CompIf (fake "if", e)
  | QualBind (p, tarrow, e) ->
      let p = pattern p in
      let e = expr e in
      G.CompFor (tarrow, p, fake "in", e)
  | QualLet (_tlet, _decls) ->
      (* TODO: let bindings in list comprehensions *)
      G.CompIf (fake "if", G.L (G.Bool (true, fake "True")) |> G.e)

(*****************************************************************************)
(* Arithmetic sequences *)
(*****************************************************************************)

and arith_seq_to_exprs = function
  | FromSeq e ->
      let e = expr e in
      [ e ]
  | FromThenSeq (e1, e2) ->
      let e1 = expr e1 and e2 = expr e2 in
      [ e1; e2 ]
  | FromToSeq (e1, e2) ->
      let e1 = expr e1 and e2 = expr e2 in
      [ e1; e2 ]
  | FromThenToSeq (e1, e2, e3) ->
      let e1 = expr e1 and e2 = expr e2 and e3 = expr e3 in
      [ e1; e2; e3 ]

(*****************************************************************************)
(* Declarations *)
(*****************************************************************************)

and decl_to_stmts decl : G.stmt list =
  match decl with
  | DeclEllipsis t -> [ G.exprstmt (G.Ellipsis t |> G.e) ]
  | FunBind fms ->
      let first = List_.hd_exn "FunBind should have at least one match" fms in
      let id = ident first.fm_name in
      let ent = G.basic_entity id in
      let fdef = fun_matches_to_funcdef fms in
      [ G.DefStmt (ent, G.FuncDef fdef) |> G.s ]
  | TypeSig (ids, _tcolon, ty) ->
      let ty = type_ ty in
      list
        (fun id ->
          let id = ident id in
          let ent = G.basic_entity id in
          let def = G.Signature { sig_tok = snd id; sig_type = ty } in
          G.DefStmt (ent, def) |> G.s)
        ids
  | PatBind (pat, rhs, where_opt) ->
      let pat = pattern pat in
      let e = rhs_to_expr rhs in
      let e =
        match where_opt with
        | None -> e
        | Some (_twhere, decls) ->
            let defs = list decl_to_stmts decls |> List_.flatten in
            let st = G.Block (fb (defs @ [ G.exprstmt e ])) |> G.s in
            G.stmt_to_expr st
      in
      (match pat with
      | G.PatId (id, _idinfo) ->
          let ent = G.basic_entity id in
          let def = G.VarDef { G.vinit = Some e; vtype = None; vtok = G.no_sc } in
          [ G.DefStmt (ent, def) |> G.s ]
      | _ ->
          let exp = G.LetPattern (pat, e) |> G.e in
          [ G.exprstmt exp ])
  | DataDecl d -> data_decl_to_stmts d
  | TypeAlias (_ttype, name, tparams, _teq, ty) ->
      let name = ident name in
      let _tparams = list ident tparams in
      let ty = type_ ty in
      let ent = G.basic_entity name in
      let def = { G.tbody = G.AliasType ty } in
      [ G.DefStmt (ent, G.TypeDef def) |> G.s ]
  | ClassDecl c -> class_decl_to_stmts c
  | InstanceDecl i -> instance_decl_to_stmts i
  | DefaultDecl _ -> []
  | Fixity _ -> []
  | DerivDecl _ -> []
  | ForeignDecl t ->
      let t = todo_category t in
      [ G.OtherStmt (G.OS_Todo, [ G.TodoK t ]) |> G.s ]
  | DeclTodo (t, xs) ->
      let t = todo_category t in
      let xs = list decl_to_stmts xs |> List_.flatten in
      [ G.OtherStmt (G.OS_Todo, [ G.TodoK t ] @ List_.map (fun x -> G.S x) xs)
        |> G.s ]

and fun_matches_to_funcdef (fms : fun_match list) : G.function_definition =
  match fms with
  | [ { fm_name; fm_pats; fm_rhs; fm_where } ] ->
      let _name = ident fm_name in
      let params = list param_of_pattern fm_pats in
      let body = rhs_to_expr fm_rhs in
      let body =
        match fm_where with
        | None -> body
        | Some (_twhere, decls) ->
            let defs = list decl_to_stmts decls |> List_.flatten in
            let st = G.Block (fb (defs @ [ G.exprstmt body ])) |> G.s in
            G.stmt_to_expr st
      in
      {
        G.fparams = fb params;
        frettype = None;
        fkind = (G.Function, snd fm_name);
        fbody = G.FBExpr body;
      }
  | _ :: _ ->
      (* Multiple equations => convert to case expression over implicit params *)
      let first = List_.hd_exn "unexpected empty list" fms in
      let arity = List.length first.fm_pats in
      let param_ids =
        List.init arity (fun i ->
            let s = Printf.sprintf "_arg%d" i in
            (s, snd first.fm_name))
      in
      let params =
        list (fun id -> G.Param (G.param_of_id id)) param_ids
      in
      let cases =
        list
          (fun { fm_pats; fm_rhs; fm_where; _ } ->
            let pats = list pattern fm_pats in
            let pat =
              match pats with
              | [ p ] -> p
              | _ -> G.PatTuple (fb pats)
            in
            let e = rhs_to_expr fm_rhs in
            let e =
              match fm_where with
              | None -> e
              | Some (_twhere, decls) ->
                  let defs = list decl_to_stmts decls |> List_.flatten in
                  let st = G.Block (fb (defs @ [ G.exprstmt e ])) |> G.s in
                  G.stmt_to_expr st
            in
            G.case_of_pat_and_expr (pat, e))
          fms
      in
      let scrut_expr =
        match param_ids with
        | [ id ] -> G.N (G.Id (id, G.empty_id_info ())) |> G.e
        | _ ->
            G.Container
              ( G.Tuple,
                fb
                  (list
                     (fun id -> G.N (G.Id (id, G.empty_id_info ())) |> G.e)
                     param_ids) )
            |> G.e
      in
      let body_stmt =
        G.Switch (snd first.fm_name, Some (G.Cond scrut_expr), cases) |> G.s
      in
      {
        G.fparams = fb params;
        frettype = None;
        fkind = (G.Function, snd first.fm_name);
        fbody = G.FBStmt body_stmt;
      }
  | [] -> raise Impossible

(*****************************************************************************)
(* Data declarations *)
(*****************************************************************************)

and data_decl_to_stmts d : G.stmt list =
  let name = ident d.d_name in
  let _tparams = list ident d.d_tparams in
  let ent = G.basic_entity name in
  let tbody =
    match d.d_body with
    | DataConstrs (_teq, constrs) ->
        let constrs =
          list
            (fun c ->
              let cname = ident c.cd_name in
              let args =
                match c.cd_args with
                | PosConstrArgs tys -> list type_ tys
                | RecConstrArgs _flds -> [] (* TODO: inline records *)
              in
              G.OrConstructor (cname, args))
            constrs
        in
        G.OrType constrs
    | DataGADT (_twhere, decls) ->
        let constrs =
          list
            (fun (name, _tcolon, ty) ->
              let name = ident name in
              let ty = type_ ty in
              G.OrConstructor (name, [ ty ]))
            decls
        in
        G.OrType constrs
  in
  let def = { G.tbody } in
  [ G.DefStmt (ent, G.TypeDef def) |> G.s ]

(*****************************************************************************)
(* Class declarations *)
(*****************************************************************************)

and class_decl_to_stmts c : G.stmt list =
  let name = ident c.cl_name in
  let ent = G.basic_entity name in
  let _tparams = list ident c.cl_tparams in
  let cbody =
    match c.cl_where with
    | None -> fb []
    | Some (_twhere, decls) ->
        let stmts = list decl_to_stmts decls |> List_.flatten in
        let flds = list (fun s -> G.F s) stmts in
        fb flds
  in
  let cdef =
    G.
      {
        ckind = (Interface, c.cl_tok);
        cextends = [];
        cimplements = [];
        cmixins = [];
        cparams = fb [];
        cbody;
      }
  in
  [ G.DefStmt (ent, G.ClassDef cdef) |> G.s ]

(*****************************************************************************)
(* Instance declarations *)
(*****************************************************************************)

and instance_decl_to_stmts i : G.stmt list =
  let _name = name_ i.in_name in
  let _types = list type_ i.in_types in
  let stmts =
    match i.in_where with
    | None -> []
    | Some (_twhere, decls) -> list decl_to_stmts decls |> List_.flatten
  in
  let todo_name = ("instance", i.in_tok) in
  [
    G.OtherStmt
      (G.OS_Todo, [ G.TodoK todo_name ] @ List_.map (fun s -> G.S s) stmts)
    |> G.s;
  ]

(*****************************************************************************)
(* Module *)
(*****************************************************************************)

and import_to_stmt (imp : import) : G.stmt =
  let mod_dotted = dotted_ident_of_name imp.imp_module in
  let dir =
    match imp.imp_specs with
    | None ->
        let d =
          G.ImportAll (imp.imp_tok, G.DottedName mod_dotted, fake "*")
        in
        { G.d; d_attrs = [] }
    | Some (l, specs, r) ->
        let ids =
          List_.filter_map
            (function
              | ImportVar id ->
                  let id = ident id in
                  Some (G.Direct (id, G.empty_id_info ()))
              | ImportType (id, _subs) ->
                  let id = ident id in
                  Some (G.Direct (id, G.empty_id_info ()))
              | ImportTodo _ -> None)
            specs
        in
        let d =
          G.ImportFrom (imp.imp_tok, G.DottedName mod_dotted, ids)
        in
        ignore (l, r);
        { G.d; d_attrs = [] }
  in
  G.DirectiveStmt dir |> G.s

(*****************************************************************************)
(* Program entry point *)
(*****************************************************************************)

let program (prog : AST_haskell.program) : AST_generic.program =
  let imports = list import_to_stmt prog.p_imports in
  let decls = list decl_to_stmts prog.p_decls |> List_.flatten in
  imports @ decls

let any = function
  | E x -> (
      let x = expr x in
      match x.G.e with
      | G.StmtExpr s -> G.S s
      | _ -> G.E x)
  | D x -> (
      match decl_to_stmts x with
      | [] -> raise Impossible
      | xs -> G.Ss xs)
  | T x ->
      let x = type_ x in
      G.T x
  | P x ->
      let x = pattern x in
      G.P x
  | S x ->
      let x = stmt_to_stmt x in
      G.S x
  | Im x ->
      let x = import_to_stmt x in
      G.S x
  | Id x ->
      let x = ident x in
      G.I x
  | Pr xs ->
      let xs = program xs in
      G.Ss xs
