(* Yoann Padioleau
 *
 * Copyright (C) 2024 Semgrep Inc.
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

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(* An Abstract Syntax Tree (AST) for Haskell.
 *
 * Modeled after AST_ocaml.ml and AST_scala.ml.
 *
 * TODO:
 *  - Template Haskell
 *  - GADTs
 *  - Type families
 *  - Pattern synonyms
 *  - More extensions
 *)

(*****************************************************************************)
(* Names *)
(*****************************************************************************)

(* a shortcut to annotate some information with token/position information *)
type 'a wrap = 'a * Tok.t [@@deriving show]

(* round(), square[], curly{} brackets *)
type 'a bracket = Tok.t * 'a * Tok.t [@@deriving show]

type ident = string wrap [@@deriving show]

(* qualified name: e.g., Data.Map.lookup *)
type qualifier = ident list [@@deriving show]
type name = qualifier * ident [@@deriving show]

(* operators: e.g., +, >>=, <$> *)
type op = string wrap [@@deriving show]

type todo_category = string wrap [@@deriving show]

(*****************************************************************************)
(* Types *)
(*****************************************************************************)

type type_ =
  | TyName of name
  | TyVar of ident (* lowercase type variable: a, b *)
  | TyApp of type_ * type_ (* type application: Maybe Int *)
  | TyFun of type_ * Tok.t (* -> *) * type_
  | TyTuple of type_ list bracket (* (Int, String) *)
  | TyList of type_ bracket (* [Int] *)
  | TyForall of Tok.t * ident list * type_ (* forall a b. ... *)
  | TyContext of context * Tok.t (* => *) * type_ (* Eq a => a -> ... *)
  | TyBang of Tok.t * type_ (* strict field: !Int *)
  | TyParen of type_ bracket
  (* sgrep-ext: *)
  | TyEllipsis of Tok.t
  | TyTodo of todo_category * type_ list
[@@deriving show { with_path = false }]

(* type class constraint: Eq a, (Show a, Ord a) *)
and context = constraint_ list

and constraint_ =
  | ClassConstraint of name * type_ list (* Eq a *)
[@@deriving show { with_path = false }]

(*****************************************************************************)
(* Literals *)
(*****************************************************************************)

type literal =
  | Int of Parsed_int.t
  | Float of float option wrap
  | Char of string wrap
  | String of string wrap
[@@deriving show { with_path = false }]

(*****************************************************************************)
(* Patterns *)
(*****************************************************************************)

type pattern =
  | PatVar of ident
  | PatLiteral of literal
  | PatConstructor of name * pattern list (* Just x, Left a *)
  | PatInfix of pattern * op * pattern (* x : xs *)
  | PatTuple of pattern list bracket
  | PatList of pattern list bracket
  | PatWildcard of Tok.t (* _ *)
  | PatAs of pattern * Tok.t (* @ *) * ident (* x@(Just y) *)
  | PatRecord of name * field_pat list bracket
  | PatTyped of pattern * Tok.t (* :: *) * type_
  | PatIrrefutable of Tok.t (* ~ *) * pattern
  | PatBang of Tok.t (* ! *) * pattern
  | PatNeg of Tok.t (* - *) * literal
  | PatParen of pattern bracket
  (* sgrep-ext: *)
  | PatEllipsis of Tok.t
  | PatTodo of todo_category * pattern list

and field_pat = ident * pattern option (* Nothing means punning: Rec{x} *)
[@@deriving show { with_path = false }]

(*****************************************************************************)
(* Expressions *)
(*****************************************************************************)

type expr =
  | L of literal
  | Name of name
  | Constructor of name
  | App of expr * expr
  | Infix of expr * op * expr
  | Prefix of op * expr (* unary minus *)
  | Lambda of Tok.t (* \ *) * pattern list * Tok.t (* -> *) * expr
  | Let of Tok.t * decl list * Tok.t (* in *) * expr
  | If of Tok.t * expr * expr * expr
  | Case of Tok.t * expr * Tok.t (* of *) * alt list bracket
  | Do of Tok.t * stmt list bracket
  | ListComp of expr * qual list bracket (* [ e | quals ] *)
  | ArithSeq of arith_seq bracket (* [1..10], [1,3..10] *)
  | RecordCon of name * field_bind list bracket (* Rec { x = 1, y = 2 } *)
  | RecordUpdate of expr * field_bind list bracket (* r { x = 1 } *)
  | Section of section bracket
  | TypedExpr of expr * Tok.t (* :: *) * type_
  | Tuple of expr list bracket
  | List of expr list bracket
  | Paren of expr bracket
  (* sgrep-ext: *)
  | Ellipsis of Tok.t
  | DeepEllipsis of expr bracket
  | ExprTodo of todo_category * expr list
[@@deriving show { with_path = false }]

and section =
  | SectionL of expr * op (* (2+) *)
  | SectionR of op * expr (* (+2) *)

and arith_seq =
  | FromSeq of expr (* [1..] *)
  | FromThenSeq of expr * expr (* [1,3..] *)
  | FromToSeq of expr * expr (* [1..10] *)
  | FromThenToSeq of expr * expr * expr (* [1,3..10] *)

and field_bind = ident * expr option (* Nothing means punning: Rec{x} *)

(*****************************************************************************)
(* Statements (do-notation) *)
(*****************************************************************************)
and stmt =
  | StmtExpr of expr
  | StmtBind of pattern * Tok.t (* <- *) * expr
  | StmtLet of Tok.t * decl list

(*****************************************************************************)
(* Alternatives (case branches) *)
(*****************************************************************************)
and alt = pattern * rhs * where option

(*****************************************************************************)
(* Guards and RHS *)
(*****************************************************************************)
and rhs =
  | UnguardedRhs of Tok.t (* -> or = *) * expr
  | GuardedRhss of guarded_rhs list

and guarded_rhs = Tok.t (* | *) * guard list * Tok.t (* -> or = *) * expr

and guard =
  | GuardExpr of expr
  | GuardBind of pattern * Tok.t (* <- *) * expr
  | GuardLet of Tok.t * decl list

and where = Tok.t (* where *) * decl list

(*****************************************************************************)
(* Qualifiers (list comprehension) *)
(*****************************************************************************)
and qual =
  | QualExpr of expr (* boolean guard *)
  | QualBind of pattern * Tok.t (* <- *) * expr
  | QualLet of Tok.t * decl list

(*****************************************************************************)
(* Declarations *)
(*****************************************************************************)
and decl =
  | FunBind of fun_match list (* one or more equations *)
  | TypeSig of ident list * Tok.t (* :: *) * type_
  | PatBind of pattern * rhs * where option
  | DataDecl of data_decl
  | TypeAlias of Tok.t (* type *) * ident * ident list * Tok.t (* = *) * type_
  | ClassDecl of class_decl
  | InstanceDecl of instance_decl
  | DefaultDecl of Tok.t * type_ list bracket
  | Fixity of fixity_decl
  | DerivDecl of Tok.t (* deriving *) * Tok.t (* instance *) * type_
  | ForeignDecl of todo_category
  (* sgrep-ext: *)
  | DeclEllipsis of Tok.t
  | DeclTodo of todo_category * decl list

and fun_match = {
  fm_name : ident;
  fm_pats : pattern list;
  fm_rhs : rhs;
  fm_where : where option;
}

and data_decl = {
  d_tok : Tok.t; (* 'data' or 'newtype' *)
  d_name : ident;
  d_tparams : ident list;
  d_body : data_body;
  d_deriving : deriving list;
}

and data_body =
  | DataConstrs of Tok.t (* = *) * constr_decl list
  | DataGADT of Tok.t (* where *) * gadt_decl list

and constr_decl = {
  cd_name : ident;
  cd_args : constr_args;
}

and constr_args =
  | PosConstrArgs of type_ list
  | RecConstrArgs of field_decl list bracket

and gadt_decl = ident * Tok.t (* :: *) * type_

and field_decl = ident list * type_

and deriving = Tok.t (* deriving *) * name list bracket

and class_decl = {
  cl_tok : Tok.t; (* 'class' *)
  cl_ctx : (context * Tok.t (* => *)) option;
  cl_name : ident;
  cl_tparams : ident list;
  cl_where : (Tok.t * decl list) option;
}

and instance_decl = {
  in_tok : Tok.t; (* 'instance' *)
  in_ctx : (context * Tok.t (* => *)) option;
  in_name : name;
  in_types : type_ list;
  in_where : (Tok.t * decl list) option;
}

and fixity_decl = {
  fix_tok : Tok.t; (* 'infixl' | 'infixr' | 'infix' *)
  fix_prec : Parsed_int.t option;
  fix_ops : op list;
}
[@@deriving show { with_path = false }]

(*****************************************************************************)
(* Module *)
(*****************************************************************************)

type export =
  | ExportVar of name
  | ExportType of name * export_subs option
  | ExportModule of Tok.t (* module *) * name
  | ExportTodo of todo_category

and export_subs =
  | ExportAll of Tok.t (* (..) *)
  | ExportSome of ident list bracket
[@@deriving show { with_path = false }]

type import = {
  imp_tok : Tok.t; (* 'import' *)
  imp_qualified : Tok.t option;
  imp_module : name;
  imp_as : (Tok.t * ident) option;
  imp_hiding : Tok.t option;
  imp_specs : import_spec list bracket option;
}

and import_spec =
  | ImportVar of ident
  | ImportType of ident * export_subs option
  | ImportTodo of todo_category
[@@deriving show { with_path = false }]

type module_head = {
  m_tok : Tok.t; (* 'module' *)
  m_name : name;
  m_exports : export list bracket option;
  m_where : Tok.t; (* 'where' *)
}
[@@deriving show { with_path = false }]

type program = {
  p_module : module_head option;
  p_imports : import list;
  p_decls : decl list;
}
[@@deriving show { with_path = false }]

(*****************************************************************************)
(* Any *)
(*****************************************************************************)

type any =
  | E of expr
  | P of pattern
  | T of type_
  | D of decl
  | S of stmt
  | Pr of program
  | Im of import
  | Id of ident
[@@deriving show { with_path = false }]
