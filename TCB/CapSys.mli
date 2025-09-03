(* Capability-aware wrappers of the dangerous functions in Sys.ml *)

val argv : < Cap.argv; ..> -> string array
val set_signal : < Cap.signal; .. > -> int -> Sys.signal_behavior -> unit
val chdir : < Cap.chdir; .. > -> string -> unit
