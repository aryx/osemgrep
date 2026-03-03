(* Yoann Padioleau
 *
 * Copyright (C) 2014 Facebook
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public License
 * version 2.1 as published by the Free Software Foundation.
 *
 * This library is distributed in the hope that it will be useful, but
 * WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the file
 * license.txt for more details.
 *
 *)

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(* Abstract Syntax Tree for Lisp (Scheme/Common Lisp).
 *
 * Three distinctive Lisp features reflected in this AST:
 *
 * 1. S-expressions (code is data): the entire syntax is uniform
 *    nested lists—there is no separate "statement" or "type" grammar.
 *    AST: sexp (Sexp of sexp list paren, Atom).
 *
 * 2. Quote and quasiquote: ' (quote), ` (backquote), , (comma), and
 *    @ (splice) control evaluation, enabling code-as-data manipulation.
 *    AST: Special (Quote | BackQuote | Comma | At) wrapping an sexp.
 *
 * 3. Atoms as a universal leaf: numbers, strings, and identifiers are
 *    all atoms; the distinction is purely in their content.
 *    AST: atom (Number, String, Id).
 *
 * Example combining all three:
 *   (defmacro when (test &body body)
 *     `(if ,test (progn ,@body)))
 *   (when (> x 0)
 *     (print x)
 *     (decf x))
 *)

(*****************************************************************************)
(* The AST types *)
(*****************************************************************************)

(* ------------------------------------------------------------------------- *)
(* Token/info *)
(* ------------------------------------------------------------------------- *)

type tok = Tok.t
type 'a wrap = 'a * tok
type 'a paren = tok * 'a * tok

(* ------------------------------------------------------------------------- *)
(* Sexp *)
(* ------------------------------------------------------------------------- *)

type sexp =
  | Sexp of sexp list paren (* or backet actually *)
  | Atom of atom
  | Special of special wrap * sexp

and special = Quote | BackQuote | Comma | At
and atom = Number of string wrap | String of string wrap | Id of string wrap

(* with tarzan *)

type program = sexp list
