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
open Common

(*###########################################################################*)
(* Internal helpers (not exported) *)
(*###########################################################################*)

let rec (do_n : int -> (unit -> unit) -> unit) =
 fun i f ->
  if i =|= 0 then ()
  else (
    f ();
    do_n (i - 1) f)

let spf = Printf.sprintf

let _tab_level_print = ref 0
let _tab_indent = 5
let _prefix_pr = ref ""

let _chan_pr2 = ref (None : out_channel option)

let out_chan_pr2 ?(newline = true) s =
  match !_chan_pr2 with
  | None -> ()
  | Some chan ->
      output_string chan (s ^ if newline then "\n" else "");
      flush chan

let pr2 s =
  UStdlib.prerr_string !_prefix_pr;
  do_n !_tab_level_print (fun () -> UStdlib.prerr_string " ");
  UStdlib.prerr_string s;
  UStdlib.prerr_string "\n";
  flush UStdlib.stderr;
  out_chan_pr2 s;
  ()

let xxx_once f s =
  match () with
  | _ when !UCommon.disable_pr2_once ->
      (* nosemgrep: no-pr2 *)
      UCommon.pr2 s
  | _ when not (Hashtbl.mem UCommon._already_printed s) ->
      Hashtbl.add UCommon._already_printed s true;
      f ("(ONCE) " ^ s)
  | _else_ -> ()

let pr2_once s = xxx_once pr2 s

let (list_of_string : string -> char list) = function
  | "" -> []
  | s -> List_.enum 0 (String.length s - 1) |> List_.map (String.get s)

let split sep s = Str.split (Str.regexp sep) s
let join sep xs = String.concat sep xs

let hash_to_list h =
  Hashtbl.fold (fun k v acc -> (k, v) :: acc) h [] |> List.sort compare

let hash_of_list xs =
  let h = Hashtbl.create 101 in
  xs |> List.iter (fun (k, v) -> Hashtbl.replace h k v);
  h

let hashset_to_list h = hash_to_list h |> List_.map fst

let hashset_of_list xs =
  xs |> List_.map (fun x -> (x, true)) |> hash_of_list

let insert_set x xs =
  if List.mem x xs then xs
  else x :: xs

let union_set s1 s2 =
  s2
  |> List.fold_left
       (fun acc x -> if List.mem x s1 then acc else insert_set x acc)
       s1

let minus_set s1 s2 =
  s1 |> List.filter (fun x -> not (List.mem x s2))

let ( ==~ ) s re = Str.string_match re s 0

let chop = function
  | "" -> ""
  | s -> String.sub s 0 (String.length s - 1)

(*###########################################################################*)
(* Types *)
(*###########################################################################*)

type filename = string
type dirname = string
type path = string
type float_time = float
type filepos = int

(*###########################################################################*)
(* Scoring / regression testing *)
(*###########################################################################*)

type score_result = Ok | Pb of string
type score = (string, score_result) Hashtbl.t
type score_list = (string * score_result) list

let empty_score () : score = Hashtbl.create 101

let optionise f =
  try Some (f ()) with
  | Not_found -> None

let regression_testing_vs newscore bestscore =
  let newbestscore = empty_score () in
  let allres =
    hash_to_list newscore |> List_.map fst
    |> union_set (hash_to_list bestscore |> List_.map fst)
  in
  allres
  |> List.iter (fun res ->
         match
           ( optionise (fun () -> Hashtbl.find newscore res),
             optionise (fun () -> Hashtbl.find bestscore res) )
         with
         | None, None -> raise Common.Impossible
         | Some x, None ->
             UPrintf.printf "new test file appeared: %s\n" res;
             Hashtbl.add newbestscore res x
         | None, Some _x -> UPrintf.printf "old test file disappeared: %s\n" res
         | Some newone, Some bestone -> (
             match (newone, bestone) with
             | Ok, Ok -> Hashtbl.add newbestscore res Ok
             | Pb x, Ok ->
                 UPrintf.printf
                   "PBBBBBBBB: a test file does not work anymore!!! : %s\n" res;
                 UPrintf.printf "Error : %s\n" x;
                 Hashtbl.add newbestscore res Ok
             | Ok, Pb _x ->
                 UPrintf.printf "Great: a test file now works: %s\n" res;
                 Hashtbl.add newbestscore res Ok
             | Pb x, Pb y ->
                 Hashtbl.add newbestscore res (Pb x);
                 if not (x = y) then (
                   UPrintf.printf
                     "Semipb: still error but not same error : %s\n" res;
                   UPrintf.printf "%s\n" (chop ("Old error: " ^ y));
                   UPrintf.printf "New error: %s\n" x)));
  flush UStdlib.stdout;
  flush UStdlib.stderr;
  newbestscore

let get_value filename =
  let chan = UStdlib.open_in_bin filename in
  let x = UStdlib.input_value chan in
  close_in chan;
  x

let write_value valu filename =
  let chan = UStdlib.open_out_bin filename in
  UStdlib.output_value chan valu;
  close_out chan

let regression_testing newscore best_score_file =
  pr2 ("regression file: " ^ best_score_file);
  let (bestscore : score) =
    if not (USys.file_exists best_score_file) then
      write_value (empty_score ()) best_score_file;
    get_value best_score_file
  in
  let newbestscore = regression_testing_vs newscore bestscore in
  write_value newbestscore (best_score_file ^ ".old");
  write_value newbestscore best_score_file;
  ()

(*###########################################################################*)
(* Modules *)
(*###########################################################################*)

module StringSetOrig = Set.Make (struct
  type t = string
  let compare = String.compare
end)

module StringSet = struct
  include StringSetOrig

  let of_list xs =
    xs
    |> List.fold_left (fun acc e -> StringSetOrig.add e acc) StringSetOrig.empty

  let to_list t = StringSetOrig.elements t
end

(*###########################################################################*)
(* Debugging / logging *)
(*###########################################################################*)

let example b =
  if b then () else failwith ("ASSERT FAILURE: " ^ Printexc.get_backtrace ())

let internal_error s = failwith ("internal error: " ^ s)
let error_cant_have x = internal_error ("cant have this case" ^ Dumper.dump x)

let warning s v =
  pr2 ("Warning: " ^ s ^ "; value = " ^ Dumper.dump v);
  v

let mk_pr2_wrappers aref =
  let fpr2 s =
    if !aref then pr2 s else out_chan_pr2 s
  in
  let fpr2_once s = if !aref then pr2_once s else xxx_once out_chan_pr2 s in
  (fpr2, fpr2_once)

(*###########################################################################*)
(* String operations *)
(*###########################################################################*)

let string_of_char c = String.make 1 c
let string_of_chars cs = cs |> List_.map (String.make 1) |> String.concat ""

let string_of_list f xs =
  "[" ^ (xs |> List_.map f |> String.concat ";") ^ "]"

let string_of_option f = function
  | None -> "None "
  | Some x -> "Some " ^ f x

let strip c s =
  let rec remove_prefix s =
    match s with
    | [] -> []
    | c' :: cs -> if c =*= c' then remove_prefix cs else c' :: cs
  in
  list_of_string s |> remove_prefix |> List.rev |> remove_prefix |> List.rev
  |> string_of_chars

let repeat e n =
  let rec repeat_aux acc = function
    | 0 -> acc
    | n when n < 0 -> failwith "repeat"
    | n -> repeat_aux (e :: acc) (n - 1)
  in
  repeat_aux [] n

let n_space n = repeat " " n |> join ""

let string_match_substring re s =
  try
    let _i = Str.search_forward re s 0 in
    true
  with
  | Not_found -> false

(*###########################################################################*)
(* Numeric *)
(*###########################################################################*)

let sum_int = List.fold_left ( + ) 0
let sum_float = List.fold_left ( +. ) 0.0

let int64_of_string_opt s =
  try Some (Int64.of_string s) with
  | Failure _ -> None

let int64_of_string_c_octal_opt s =
  let open Common in
  if s =~ "^0\\([0-7]+\\)$" then
    let s = Common.matched1 s in
    int64_of_string_opt ("0o" ^ s)
  else int64_of_string_opt s

let float_of_string_opt s =
  match int64_of_string_c_octal_opt s with
  | Some i -> Some (Int64.to_float i)
  | None -> float_of_string_opt s

(*###########################################################################*)
(* Option *)
(*###########################################################################*)

let just = function
  | Some x -> x
  | _ -> failwith "just: pb"

let some = just

(* optionise defined above, near regression_testing *)

let option_to_list = function
  | None -> []
  | Some x -> [ x ]

(*###########################################################################*)
(* List operations *)
(*###########################################################################*)

let rec foldn f acc i =
  if i =|= 0 then acc else foldn f (f acc i) (i - 1)

let hd_opt = function
  | [] -> None
  | x :: _ -> Some x

let rec list_last = function
  | [] -> raise Not_found
  | [ x ] -> x
  | _x :: y :: xs -> list_last (y :: xs)

let foldl1 p xs =
  match xs with
  | x :: xs -> List.fold_left p x xs
  | [] -> failwith "foldl1: empty list"

let maximum l = foldl1 max l
let minimum l = foldl1 min l

let map_eff_rev f l =
  let rec map_eff_aux acc = function
    | [] -> acc
    | x :: xs -> map_eff_aux (f x :: acc) xs
  in
  map_eff_aux [] l

let acc_map f l =
  let rec loop acc = function
    | [] -> List.rev acc
    | x :: xs -> loop (f x :: acc) xs
  in
  loop [] l

let map_flatten f l =
  let rec map_flatten_aux accu = function
    | [] -> accu
    | e :: l -> map_flatten_aux (List.rev (f e) @ accu) l
  in
  List.rev (map_flatten_aux [] l)

let rec uniq = function
  | [] -> []
  | e :: l -> if List.mem e l then uniq l else e :: uniq l

let and_list = List.fold_left ( && ) true
let filter = List.filter

let rec zip xs ys =
  match (xs, ys) with
  | [], [] -> []
  | [], _ -> failwith "zip: not same length"
  | _, [] -> failwith "zip: not same length"
  | x :: xs, y :: ys -> (x, y) :: zip xs ys

let unzip zs =
  List_.fold_right (fun e (xs, ys) -> (fst e :: xs, snd e :: ys)) zs ([], [])

let unzip3 l =
  let rec unzip aa bb cc = function
    | (a, b, c) :: l -> unzip (a :: aa) (b :: bb) (c :: cc) l
    | [] -> (List.rev aa, List.rev bb, List.rev cc)
  in
  unzip [] [] [] l

let rec (split_when : ('a -> bool) -> 'a list -> 'a list * 'a * 'a list) =
 fun p -> function
  | [] -> raise Not_found
  | x :: xs ->
      if p x then ([], x, xs)
      else
        let l1, a, l2 = split_when p xs in
        (x :: l1, a, l2)

let rec split_gen_when_aux f acc xs =
  match xs with
  | [] -> if List_.null acc then [] else [ List.rev acc ]
  | x :: xs -> (
      match f (x :: xs) with
      | None -> split_gen_when_aux f (x :: acc) xs
      | Some rest ->
          let before = List.rev acc in
          if List_.null before then split_gen_when_aux f [] rest
          else before :: split_gen_when_aux f [] rest)

let split_gen_when f xs = split_gen_when_aux f [] xs

let (span_tail_call : ('a -> bool) -> 'a list -> 'a list * 'a list) =
 fun p xs ->
  let rec aux acc xs =
    match xs with
    | [] -> (List.rev acc, [])
    | x :: xs -> if p x then aux (x :: acc) xs else (List.rev acc, x :: xs)
  in
  aux [] xs

let head_middle_tail xs =
  match xs with
  | x :: y :: xs ->
      let head = x in
      let reversed = List.rev (y :: xs) in
      let tail = List_.hd_exn "unexpected empty list" reversed in
      let middle = List.rev (List_.tl_exn "unexpected empty list" reversed) in
      (head, middle, tail)
  | _ -> failwith "head_middle_tail, too small list"

let iter_with_previous_opt f = function
  | [] -> ()
  | e :: l ->
      f None e;
      let rec iter_with_previous_ previous = function
        | [] -> ()
        | e :: l ->
            f (Some previous) e;
            iter_with_previous_ e l
      in
      iter_with_previous_ e l

let fst3 (x, _, _) = x
let thd3 (_, _, z) = z

(* minus_set defined above, near internal helpers *)

let assoc = List.assoc
let assoc_opt k l = optionise (fun () -> List.assoc k l)

(*###########################################################################*)
(* Hash *)
(*###########################################################################*)

let hkeys h =
  let hkey = Hashtbl.create 101 in
  h |> Hashtbl.iter (fun k _v -> Hashtbl.replace hkey k true);
  hashset_to_list hkey

let hfind_default key value_if_not_found h =
  try Hashtbl.find h key with
  | Not_found ->
      Hashtbl.add h key (value_if_not_found ());
      Hashtbl.find h key

let hupdate_default key ~update:op ~default:value_if_not_found h =
  let old = hfind_default key value_if_not_found h in
  Hashtbl.replace h key (op old)

let diff_set_eff xs1 xs2 =
  let h1 = hashset_of_list xs1 in
  let h2 = hashset_of_list xs2 in

  let hcommon = Hashtbl.create 101 in
  let honly_in_h1 = Hashtbl.create 101 in
  let honly_in_h2 = Hashtbl.create 101 in

  h1
  |> Hashtbl.iter (fun k _ ->
         if Hashtbl.mem h2 k then Hashtbl.replace hcommon k true
         else Hashtbl.add honly_in_h1 k true);
  h2
  |> Hashtbl.iter (fun k _ ->
         if Hashtbl.mem h1 k then Hashtbl.replace hcommon k true
         else Hashtbl.add honly_in_h2 k true);
  ( hashset_to_list hcommon,
    hashset_to_list honly_in_h1,
    hashset_to_list honly_in_h2 )

(*###########################################################################*)
(* File / system *)
(*###########################################################################*)

let dir_regex = Str.regexp "^[^\\*]*"

let glob pattern =
  Str.search_forward dir_regex pattern 0 |> ignore;
  let dir = Str.matched_string pattern in
  let regex = pattern |> Re.Glob.glob ~anchored:true |> Re.compile in
  let caps = Cap.readdir_UNSAFE () in
  let files = UFile.Legacy.dir_contents caps dir in
  files |> List.filter (fun s -> Re.execp regex s)

let dirs_and_base_of_file file =
  let dir, base = Filename_.db_of_filename file in
  let dirs = split "/" dir in
  let dirs =
    match dirs with
    | [ "." ] -> []
    | _ -> dirs
  in
  (dirs, base)

(* Count the number of newlines in a buffer *)
let count_newlines s =
  let rec go n i =
    try
      let i' = Bytes.index_from s i '\n' in
      go (n + 1) (i' + 1)
    with
    | Not_found -> n
  in
  go 0 0

(* Fold over a file in chunks *)
let fold_file f x file_name =
  let buffer = Bytes.create 1024 in
  let file = UStdlib.open_in file_name in
  let rec go a =
    let length = input file buffer 0 (Bytes.length buffer) in
    let a' = f a (Bytes.sub buffer 0 length) in
    if length > 0 then go a' else a'
  in
  let r = go x in
  close_in file;
  r

let nblines_eff file = fold_file (fun x s -> x + count_newlines s) 0 file

let unix_diff file1 file2 =
  let cmd = (Cmd.Name "diff", [ "-u"; file1; file2 ]) in
  (* nosemgrep: forbid-exec *)
  match UCmd.lines_of_run ~trim:true cmd with
  | Ok (xs, _status) -> xs
  | Error (`Msg s) -> failwith (spf "unix_diff problem: %s" s)

(*###########################################################################*)
(* Date *)
(*###########################################################################*)

let day_secs : float = 86400.
let today : unit -> float = fun () -> UUnix.time ()
let month_before : float_time -> float_time = fun d -> d -. (30.0 *. day_secs)

(*###########################################################################*)
(* Command line *)
(*###########################################################################*)

let _check_stack = ref true
let _batch_mode = ref false

let cmdline_flags_devel () =
  [
    ( "-debugger",
      Arg.Set Common.debugger,
      " option to set if launched inside ocamldebug" );
  ]

let cmdline_flags_other () =
  [
    ("-nocheck_stack", Arg.Clear _check_stack, " ");
    ("-batch_mode", Arg.Set _batch_mode, " no interactivity");
  ]
