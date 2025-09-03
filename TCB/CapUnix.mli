(* Capability-aware wrappers of the dangerous functions in Unix.ml *)

(* See also libs/commons/CapExec.ml *)
val execvp : < Cap.exec; .. > -> string -> string array -> 'a

(* You should use CapExec.ml instead *)
val system : < Cap.exec; .. > -> string -> Unix.process_status
val fork : < Cap.fork; .. > -> unit -> int
val alarm : <  Cap.time_limit; .. > -> int -> int

val setitimer :
  < Cap.time_limit; .. > ->
  Unix.interval_timer ->
  Unix.interval_timer_status ->
  Unix.interval_timer_status
