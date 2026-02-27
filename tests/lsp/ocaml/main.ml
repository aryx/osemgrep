(* Test file for --lsp with ocamllsp.
 *
 * The typed metavar pattern ($X : int) should match expressions
 * whose type is 'int' even when the type can only be determined
 * by looking at the signature in another file (lib.mli).
 *
 * Without --lsp, semgrep cannot know that Lib.add x 1 returns int
 * or that Lib.compute_ratio x 2 returns float.
 * With --lsp, ocamllsp resolves the types across modules.
 *)

let () =
  let x = 42 in
  (* Lib.add is defined in lib.ml, declared in lib.mli.
   * Only ocamllsp (via the .cmt) knows Lib.add x 1 has type int. *)
  let sum = Lib.add x 1 in
  (* Lib.compute_ratio returns float, so this should NOT match ($X : int). *)
  let ratio = Lib.compute_ratio x 2 in
  Printf.printf "sum=%d ratio=%f\n" sum ratio
