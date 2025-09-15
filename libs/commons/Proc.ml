(* just enough to be compatible with xix-libs *)

type pid = int

let apply_in_child_process = 
  CapProcess.apply_in_child_process
