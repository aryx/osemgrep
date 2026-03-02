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
module G = AST_generic
module H = AST_generic_helpers

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(* AST_zig to AST_generic.
 *
 * See AST_generic.ml for more information.
 *)

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)
let id x = x
let string = id
let list = List_.map
let option = Option.map
let fb = Tok.unsafe_fake_bracket
let fake tok s = Tok.fake_tok tok s
let unsafe_fake s = Tok.unsafe_fake_tok s

(*****************************************************************************)
(* Entry point *)
(*****************************************************************************)

let tok v = v

let wrap _of_a (v1, v2) =
  let v1 = _of_a v1 and v2 = tok v2 in
  (v1, v2)

let bracket of_a (t1, x, t2) = (tok t1, of_a x, tok t2)
let ident v = wrap string v

let top_func () =
  let rec type_ x = type_kind x |> G.t
  and type_kind = function
    | TName v1 ->
        let v1 = ident v1 in
        G.TyN (G.Id (v1, G.empty_id_info ()))
    | TPointer (_kind, t, v1) ->
        let v1 = type_ v1 in
        G.TyPointer (t, v1)
    | TArray (v1, v2) ->
        let (l, e, r) = bracket expr v1 and v2 = type_ v2 in
        G.TyArray ((l, Some e, r), v2)
    | TSlice ((l, _, r), v2) ->
        let v2 = type_ v2 in
        G.TyArray ((l, None, r), v2)
    | TOptional (t, v1) ->
        let v1 = type_ v1 in
        G.TyQuestion (v1, t)
    | TErrorUnion (v1, t, v2) ->
        let v1 = type_ v1 and v2 = type_ v2 in
        G.OtherType (("ErrorUnion", t), [ G.T v1; G.T v2 ])
    | TAnyErrorUnion (t1, t2, v1) ->
        let v1 = type_ v1 in
        G.OtherType (("AnyErrorUnion", t1), [ G.Tk t2; G.T v1 ])
    | TFunc v1 ->
        let params, ret = func_type_to_generic v1 in
        let ret =
          match ret with
          | None -> G.ty_builtin ("void", v1.ftok)
          | Some t -> t
        in
        let (_, param_list, _) = params in
        G.TyFun (param_list, ret)
    | TStruct (t, v1) ->
        let v1 = bracket (list container_field_to_field) v1 in
        G.TyRecordAnon ((G.Class, t), v1)
    | TEnum (t, _tag_opt, v1) ->
        let or_types =
          list
            (fun ef ->
              match ef with
              | EnumField (id, eopt) ->
                  let id = ident id in
                  let eopt = option expr eopt in
                  G.OrEnum (id, eopt)
              | EnumFieldEllipsis _t ->
                  G.OrEnum (("...", unsafe_fake "..."), None))
            (let _, fs, _ = v1 in
             fs)
        in
        let ent = G.basic_entity ("!enum!", t) in
        let typedef = G.TypeDef { G.tbody = G.OrType or_types } in
        (* Wrap as an expression containing the type def *)
        G.OtherType (("Enum", t), [ G.S (G.DefStmt (ent, typedef) |> G.s) ])
    | TUnion (t, _tag_opt, v1) ->
        let v1 = bracket (list container_field_to_field) v1 in
        G.TyRecordAnon ((G.Class, t), v1)
    | TOpaque (t, _v1) ->
        G.OtherType (("Opaque", t), [])
    | TBuiltinCall (name, args) ->
        let name = ident name in
        let args = bracket (list expr) args in
        let args_any = list (fun e -> G.E e) (let _, es, _ = args in es) in
        G.OtherType (("BuiltinType", snd name), args_any)
    | TEllipsis t -> G.TyEllipsis t

  and container_field_to_field cf =
    let id = ident cf.cf_name in
    let vtype = Some (type_ cf.cf_type) in
    let vinit = option expr cf.cf_default in
    let entity =
      G.{ name = EN (Id (id, empty_id_info ())); attrs = []; tparams = None }
    in
    G.(fld (entity, VarDef { vinit; vtype; vtok = no_sc }))

  and func_type_to_generic ft :
      G.parameters * G.type_ option =
    let params = list param_to_generic (let _, ps, _ = ft.fparams in ps) in
    let ret = option type_ ft.freturn in
    let l, _, r = ft.fparams in
    ((l, params, r), ret)

  and param_to_generic = function
    | Param pc ->
        let pname = option ident pc.p_name in
        let ptype = type_ pc.p_type in
        let attrs =
          (match pc.p_comptime with
           | Some t -> [ G.attr G.Static t (* repurpose Static for comptime *) ]
           | None -> [])
          @
          (match pc.p_noalias with
           | Some t -> [ G.OtherAttribute (("noalias", t), []) ]
           | None -> [])
        in
        G.Param
          {
            G.pname;
            ptype = Some ptype;
            pdefault = None;
            pattrs = attrs;
            pinfo = G.empty_id_info ();
          }
    | ParamEllipsis t -> G.ParamEllipsis t

  and expr e =
    (match e with
    | Int v1 -> G.L (G.Int v1)
    | Float v1 ->
        let v1 = wrap id v1 in
        G.L (G.Float v1)
    | String v1 ->
        let v1 = wrap string v1 in
        G.L (G.String (fb v1))
    | MultilineString v1 ->
        let v1 = wrap string v1 in
        G.L (G.String (fb v1))
    | Char v1 ->
        let v1 = wrap string v1 in
        G.L (G.Char v1)
    | Bool v1 ->
        let v1 = wrap id v1 in
        G.L (G.Bool v1)
    | Null t -> G.L (G.Null t)
    | Undefined t ->
        G.N (G.Id (("undefined", t), G.empty_id_info ()))
    | Id v1 ->
        let v1 = ident v1 in
        G.N (G.Id (v1, G.empty_id_info ()))
    | FieldAccess (v1, t, v3) ->
        let v1 = expr v1 and v3 = ident v3 in
        G.DotAccess (v1, t, G.FN (Id (v3, G.empty_id_info ())))
    | Deref (v1, t) ->
        let v1 = expr v1 in
        G.DeRef (t, v1)
    | OptionalUnwrap (v1, t) ->
        let v1 = expr v1 in
        G.OtherExpr (("OptionalUnwrap", t), [ G.E v1 ])
    | Index (v1, v2) ->
        let v1 = expr v1 and v2 = bracket expr v2 in
        G.ArrayAccess (v1, v2)
    | SliceExpr (v1, (t1, bounds, t2)) ->
        let v1 = expr v1 in
        let start = option expr bounds.sl_start in
        let end_ = option expr bounds.sl_end in
        let _sentinel = option expr bounds.sl_sentinel in
        G.SliceAccess (v1, (t1, (start, end_, None), t2))
    | Call (v1, v2) ->
        let v1 = expr v1 and v2 = bracket (list expr) v2 in
        let l, args, r = v2 in
        G.Call (v1, (l, list (fun e -> G.Arg e) args, r))
    | BuiltinCall (name, args) ->
        let name = ident name in
        let l, args, r = bracket (list expr) args in
        let fn = G.N (G.Id (name, G.empty_id_info ())) |> G.e in
        G.Call (fn, (l, list (fun e -> G.Arg e) args, r))
    | Unary ((op, t), v1) ->
        let v1 = expr v1 in
        G.Call (G.Special (G.Op op, t) |> G.e, fb [ G.arg v1 ])
    | Binary (v1, (op, t), v3) ->
        let v1 = expr v1 and v3 = expr v3 in
        G.Call
          (G.Special (G.Op op, t) |> G.e, fb ([ v1; v3 ] |> list G.arg))
    | Assign (v1, t, v3) ->
        let v1 = expr v1 and v3 = expr v3 in
        G.Assign (v1, t, v3)
    | AssignOp (v1, (op, t), v3) ->
        let v1 = expr v1 and v3 = expr v3 in
        G.AssignOp (v1, (op, t), v3)
    | StructInit (ty_opt, v1) ->
        let ty = option type_ ty_opt in
        let l, fields, r = bracket (list field_init_to_generic) v1 in
        (match ty with
         | Some ty ->
             G.New (fake l "new", ty, G.empty_id_info (), (l, fields, r))
         | None ->
             let record_fields =
               List_.filter_map
                 (fun arg ->
                    match arg with
                    | G.ArgKwd (id, e) -> Some (G.basic_field id (Some e) None)
                    | _ -> None)
                 fields
             in
             if List.length record_fields = List.length fields then
               G.Record (l, record_fields, r)
             else
               G.Container (G.Dict, (l, List_.filter_map (fun arg ->
                   match arg with
                   | G.Arg e -> Some e
                   | G.ArgKwd (id, e) -> Some (G.keyval (G.N (G.Id (id, G.empty_id_info ())) |> G.e) (fake (snd id) ":") e)
                   | _ -> None) fields, r)))
    | ArrayInit (ty_opt, v1) ->
        let _ty = option type_ ty_opt in
        let v1 = bracket (list expr) v1 in
        G.Container (G.Array, v1)
    | IfExpr (t, cond, _capture, then_e, else_opt) ->
        let cond = expr cond in
        let then_e = expr then_e in
        let else_opt =
          option (fun (_t, e) -> expr e) else_opt
        in
        G.Conditional (cond, then_e,
          match else_opt with
          | Some e -> e
          | None ->
              G.L (G.Null (fake t "null")) |> G.e)
    | SwitchExpr (t, operand, prongs) ->
        let operand = expr operand in
        let l, prongs, r = bracket (list switch_prong_to_case) prongs in
        G.StmtExpr
          (G.Switch (t, Some (G.Cond operand), prongs) |> G.s)
        (* wrap in braces for range *)
        |> (fun e -> ignore (l, r); e)
    | ForExpr (t, _inputs, _captures, body, _else_opt) ->
        let body = expr body in
        G.OtherExpr (("ForExpr", t), [ G.E body ])
    | WhileExpr (t, cond, _capture, _cont, body, _else_opt) ->
        let cond = expr cond and body = expr body in
        G.OtherExpr (("WhileExpr", t), [ G.E cond; G.E body ])
    | Try (t, v1) ->
        let v1 = expr v1 in
        G.OtherExpr (("Try", t), [ G.E v1 ])
    | Catch (v1, t, _capture, v3) ->
        let v1 = expr v1 and v3 = expr v3 in
        G.OtherExpr (("Catch", t), [ G.E v1; G.E v3 ])
    | Orelse (v1, t, _capture, v3) ->
        let v1 = expr v1 and v3 = expr v3 in
        G.OtherExpr (("Orelse", t), [ G.E v1; G.E v3 ])
    | Comptime (t, v1) ->
        let v1 = expr v1 in
        G.OtherExpr (("Comptime", t), [ G.E v1 ])
    | Nosuspend (t, v1) ->
        let v1 = expr v1 in
        G.OtherExpr (("Nosuspend", t), [ G.E v1 ])
    | Async (t, v1) ->
        let v1 = expr v1 in
        G.Await (t, v1) (* close enough; or OtherExpr *)
    | Await (t, v1) ->
        let v1 = expr v1 in
        G.Await (t, v1)
    | Resume (t, v1) ->
        let v1 = expr v1 in
        G.OtherExpr (("Resume", t), [ G.E v1 ])
    | Suspend (t, body_opt) ->
        let body_any =
          match body_opt with
          | None -> []
          | Some e -> [ G.E (expr e) ]
        in
        G.OtherExpr (("Suspend", t), body_any)
    | BlockExpr (label_opt, stmts) ->
        let stmts = bracket (list stmt) stmts in
        let _label = option ident label_opt in
        G.StmtExpr (G.Block stmts |> G.s)
    | Range (v1, t, v2) ->
        let v1 = option expr v1 and v2 = option expr v2 in
        let args =
          (match v1 with Some e -> [ G.E e ] | None -> [])
          @ [ G.Tk t ]
          @ (match v2 with Some e -> [ G.E e ] | None -> [])
        in
        G.OtherExpr (("Range", t), args)
    | ErrorValue (t1, t2, name) ->
        let name = ident name in
        let err = G.N (G.Id (("error", t1), G.empty_id_info ())) |> G.e in
        G.DotAccess (err, t2, G.FN (Id (name, G.empty_id_info ())))
    | FuncLit (ft, body) ->
        let params, ret = func_type_to_generic ft in
        let body = bracket (list stmt) body in
        G.Lambda
          {
            G.fparams = params;
            frettype = ret;
            fbody = G.FBStmt (G.Block body |> G.s);
            fkind = (G.LambdaKind, ft.ftok);
          }
    | Ellipsis t -> G.Ellipsis t
    | DeepEllipsis v1 ->
        let v1 = bracket expr v1 in
        G.DeepEllipsis v1
    | TypedMetavar (v1, t, v3) ->
        let v1 = ident v1 and v3 = type_ v3 in
        G.TypedMetavar (v1, t, v3)
    | DotAccessEllipsis (v1, t) ->
        let v1 = expr v1 in
        G.DotAccessEllipsis (v1, t))
    |> G.e

  and field_init_to_generic = function
    | FieldInit (id, _t, e) ->
        let id = ident id and e = expr e in
        G.ArgKwd (id, e)
    | FieldInitEllipsis t ->
        G.Arg (G.Ellipsis t |> G.e)

  and switch_prong_to_case prong =
    let cases =
      list
        (fun sc ->
          match sc with
          | SCExpr e ->
              let e = expr e in
              G.Case (unsafe_fake "=>", H.expr_to_pattern e)
          | SCRange (e1, _t, e2) ->
              let e1 = expr e1 and e2 = expr e2 in
              let pat =
                G.PatDisj
                  (H.expr_to_pattern e1, H.expr_to_pattern e2)
              in
              G.Case (unsafe_fake "=>", pat)
          | SCDefault t -> G.Default t
          | SCEllipsis t -> G.Case (t, G.PatEllipsis t))
        prong.sp_cases
    in
    let body = expr prong.sp_body in
    G.CasesAndBody (cases, G.exprstmt body)

  and stmt x = G.stmt1 (stmt_aux x)
  and stmt_aux = function
    | ExprSt (v1, _sc) ->
        let v1 = expr v1 in
        [ G.exprstmt v1 ]
    | Block v1 ->
        let v1 = bracket (fun xs -> list stmt_aux xs |> List_.flatten) v1 in
        [ G.Block v1 |> G.s ]
    | VarDecl vd ->
        [ var_decl_to_stmt vd ]
    | If (t, cond, _capture, then_s, else_opt) ->
        let cond = expr cond in
        let then_s = stmt then_s in
        let else_opt = option stmt else_opt in
        [ G.If (t, G.Cond cond, then_s, else_opt) |> G.s ]
    | While (t, cond, _capture, _cont, body, _else_opt) ->
        let cond = expr cond in
        let body = stmt body in
        [ G.While (t, G.Cond cond, body) |> G.s ]
    | For (t, inputs, _captures, body, _else_opt) ->
        let body = stmt body in
        let exprs =
          list (fun fi -> expr fi.fi_expr) inputs
        in
        let iter =
          match exprs with
          | [ e ] -> e
          | es -> G.Container (G.Tuple, fb es) |> G.e
        in
        let pat = G.PatWildcard (fake t "_") in
        [ G.For (t, G.ForEach (pat, fake t "in", iter), body) |> G.s ]
    | Switch (t, operand, prongs) ->
        let operand = expr operand in
        let prongs =
          list switch_prong_to_case
            (let _, ps, _ = prongs in ps)
        in
        [ G.Switch (t, Some (G.Cond operand), prongs) |> G.s ]
    | Return (t, v1) ->
        let v1 = option expr v1 in
        [ G.Return (t, v1, G.sc) |> G.s ]
    | Break (t, label_opt, v1) ->
        let label = option ident label_opt in
        let _v1 = option expr v1 in
        [ G.Break (t, H.opt_to_label_ident label, G.sc) |> G.s ]
    | Continue (t, label_opt) ->
        let label = option ident label_opt in
        [ G.Continue (t, H.opt_to_label_ident label, G.sc) |> G.s ]
    | Defer (_t, body) ->
        let body = stmt body in
        [ G.OtherStmt (G.OS_Defer, [ G.S body ]) |> G.s ]
    | Errdefer (t, _capture, body) ->
        let body = stmt body in
        [ G.OtherStmt (G.OS_Todo, [ G.Tk t; G.S body ]) |> G.s ]
    | Asm (t, _v1) ->
        [ G.OtherStmt (G.OS_Asm, [ G.Tk t ]) |> G.s ]
    | Empty t ->
        [ G.Block (t, [], t) |> G.s ]

  and var_decl_to_stmt vd =
    let id = ident vd.vd_name in
    let vtype = option type_ vd.vd_type in
    let vinit = option expr vd.vd_init in
    let attrs =
      (match vd.vd_pub with
       | Some t -> [ G.attr G.Public t ]
       | None -> [])
      @
      (match vd.vd_extern with
       | Some t -> [ G.attr G.Extern t ]
       | None -> [])
      @
      (match vd.vd_comptime with
       | Some t -> [ G.attr G.Static t ]
       | None -> [])
      @
      (match vd.vd_kind with
       | (Const, t) -> [ G.attr G.Const t ]
       | (Var, t) -> [ G.attr G.Var t ])
    in
    let ent = G.basic_entity id ~attrs in
    G.DefStmt (ent, G.VarDef { G.vinit; vtype; vtok = G.no_sc }) |> G.s

  and function_def_to_stmt fd =
    let id = ident fd.fd_name in
    let params, ret = func_type_to_generic fd.fd_type in
    let body =
      match fd.fd_body with
      | Some b ->
          let b = bracket (list stmt) b in
          G.FBStmt (G.Block b |> G.s)
      | None -> G.FBDecl G.sc
    in
    let attrs =
      (match fd.fd_pub with
       | Some t -> [ G.attr G.Public t ]
       | None -> [])
      @
      (match fd.fd_export with
       | Some t -> [ G.OtherAttribute (("export", t), []) ]
       | None -> [])
      @
      (match fd.fd_extern with
       | Some t -> [ G.attr G.Extern t ]
       | None -> [])
      @
      (match fd.fd_inline with
       | Some t -> [ G.attr G.Inline t ]
       | None -> [])
    in
    let ent = G.basic_entity id ~attrs in
    G.DefStmt
      ( ent,
        G.FuncDef
          {
            G.fparams = params;
            frettype = ret;
            fbody = body;
            fkind = (G.Function, fd.fd_type.ftok);
          } )
    |> G.s

  and top_decl = function
    | DFunc fd -> function_def_to_stmt fd
    | DVar vd -> var_decl_to_stmt vd
    | DTest (t, name_opt, body) ->
        let id =
          match name_opt with
          | Some (s, tk) -> (s, tk)
          | None -> ("test", t)
        in
        let body = bracket (list stmt) body in
        let ent = G.basic_entity id in
        G.DefStmt
          ( ent,
            G.FuncDef
              {
                G.fparams = fb [];
                frettype = None;
                fbody = G.FBStmt (G.Block body |> G.s);
                fkind = (G.Function, t);
              } )
        |> G.s
    | DComptime (t, body) ->
        let body = bracket (list stmt) body in
        G.OtherStmt (G.OS_Todo, [ G.Tk t; G.S (G.Block body |> G.s) ])
        |> G.s
    | DUsingNamespace (pub_opt, t, e) ->
        let e = expr e in
        let attrs =
          match pub_opt with
          | Some t -> [ G.Tk t ]
          | None -> []
        in
        G.OtherStmt (G.OS_Todo, [ G.Tk t; G.E e ] @ attrs) |> G.s
    | STop s -> stmt s

  and program xs = list top_decl xs
  and any x =
    match x with
    | E v1 ->
        let v1 = expr v1 in
        G.E v1
    | S v1 ->
        let v1 = stmt v1 in
        G.S v1
    | T v1 ->
        let v1 = type_ v1 in
        G.T v1
    | D v1 ->
        let v1 = top_decl v1 in
        G.S v1
    | P v1 ->
        let v1 = program v1 in
        G.Pr v1
    | Ss v1 ->
        let v1 = list stmt_aux v1 |> List_.flatten in
        G.Ss v1
  in

  (program, any)

let program x =
  let p, _ = top_func () in
  p x

let any x =
  let _, a = top_func () in
  a x
