(* Print a string, print a newline, and flush the stdout channel. *)
val print : < Cap.stdout; .. > -> string -> unit

(* Print a string and flush the stdout channel. *)
val print_no_nl : < Cap.stdout; .. > -> string -> unit

(* Print a string, print a newline, and flush the stderr channel. *)
val eprint : < Cap.stderr; .. > -> string -> unit

val ocolor_format_printf :
  < Cap.stdout; .. > -> ('b, Format.formatter, unit) format -> 'b
