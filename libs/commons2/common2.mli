(* Yoann Padioleau
 *
 * Copyright (C) 1998-2009 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public License
 * version 2.1 as published by the Free Software Foundation, with the
 * special exception on linking described in file license.txt.
 *
 * This library is distributed in the hope that it will be useful, but
 * WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the file
 * license.txt for more details.
 *)

(*****************************************************************************)
(* Types *)
(*****************************************************************************)

type filename = string
type dirname = string
type path = string
type float_time = float

(** file position as line number (1-based) *)
type filepos = int

(*****************************************************************************)
(* Scoring / regression testing *)
(*****************************************************************************)

type score_result = Ok | Pb of string
type score = (string, score_result) Hashtbl.t
type score_list = (string * score_result) list

val empty_score : unit -> score
val regression_testing : score -> string (* best_score_file *) -> unit
val write_value : 'a -> string -> unit

(*****************************************************************************)
(* Modules *)
(*****************************************************************************)

module StringSet : sig
  include Set.S with type elt = string

  val of_list : string list -> t
  val to_list : t -> string list
end

(*****************************************************************************)
(* Debugging / logging *)
(*****************************************************************************)

val example : bool -> unit
val error_cant_have : 'a -> 'b
val warning : string -> 'a -> 'a
val mk_pr2_wrappers : bool ref -> (string -> unit) * (string -> unit)

(*****************************************************************************)
(* Regexp *)
(*****************************************************************************)

val ( ==~ ) : string -> Str.regexp -> bool

(*****************************************************************************)
(* String operations *)
(*****************************************************************************)

val string_of_char : char -> string
val string_of_chars : char list -> string
val string_of_list : ('a -> string) -> 'a list -> string
val string_of_option : ('a -> string) -> 'a option -> string
val list_of_string : string -> char list
val chop : string -> string
val strip : char -> string -> string
val n_space : int -> string
val string_match_substring : Str.regexp -> string -> bool

(*****************************************************************************)
(* Numeric *)
(*****************************************************************************)

val sum_int : int list -> int
val sum_float : float list -> float
val float_of_string_opt : string -> float option
val int64_of_string_opt : string -> int64 option
val int64_of_string_c_octal_opt : string -> int64 option

(*****************************************************************************)
(* Option *)
(*****************************************************************************)

val some : 'a option -> 'a
val optionise : (unit -> 'a) -> 'a option
val option_to_list : 'a option -> 'a list

(*****************************************************************************)
(* List operations *)
(*****************************************************************************)

val acc_map : ('a -> 'b) -> 'a list -> 'b list
val map_eff_rev : ('a -> 'b) -> 'a list -> 'b list
val map_flatten : ('a -> 'b list) -> 'a list -> 'b list
val unzip : ('a * 'b) list -> 'a list * 'b list
val unzip3 : ('a * 'b * 'c) list -> 'a list * 'b list * 'c list
val zip : 'a list -> 'b list -> ('a * 'b) list
val split_when : ('a -> bool) -> 'a list -> 'a list * 'a * 'a list
val split_gen_when : ('a list -> 'a list option) -> 'a list -> 'a list list
val span_tail_call : ('a -> bool) -> 'a list -> 'a list * 'a list
val head_middle_tail : 'a list -> 'a * 'a list * 'a
val list_last : 'a list -> 'a
val hd_opt : 'a list -> 'a option
val iter_with_previous_opt : ('a option -> 'a -> unit) -> 'a list -> unit
val uniq : 'a list -> 'a list
val repeat : 'a -> int -> 'a list
val foldl1 : ('a -> 'a -> 'a) -> 'a list -> 'a
val foldn : ('a -> int -> 'a) -> 'a -> int -> 'a
val fst3 : 'a * 'b * 'c -> 'a
val thd3 : 'a * 'b * 'c -> 'c
val maximum : 'a list -> 'a
val minimum : 'a list -> 'a
val and_list : bool list -> bool
val filter : ('a -> bool) -> 'a list -> 'a list
val minus_set : 'a list -> 'a list -> 'a list
val assoc : 'a -> ('a * 'b) list -> 'b
val assoc_opt : 'a -> ('a * 'b) list -> 'b option

(*****************************************************************************)
(* Hash *)
(*****************************************************************************)

val hkeys : ('a, 'b) Hashtbl.t -> 'a list
val hupdate_default :
  'a -> update:('b -> 'b) -> default:(unit -> 'b) -> ('a, 'b) Hashtbl.t -> unit
val diff_set_eff : 'a list -> 'a list -> 'a list * 'a list * 'a list

(*****************************************************************************)
(* File / system *)
(*****************************************************************************)

val glob : string -> string list
val dirs_and_base_of_file : string -> string list * string
val nblines_eff : string -> int
val unix_diff : string -> string -> string list

(*****************************************************************************)
(* Date *)
(*****************************************************************************)

val today : unit -> float
val month_before : float_time -> float_time

(*****************************************************************************)
(* Command line *)
(*****************************************************************************)

val cmdline_flags_devel : unit -> (string * Arg.spec * string) list
val cmdline_flags_other : unit -> (string * Arg.spec * string) list
