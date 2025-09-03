(* print the text on stdout for just 'semgrep' *)
val print_help : < Cap.stdout; .. > -> unit

(* print the text on stdout for 'semgrep --help'*)
val print_semgrep_dashdash_help : < Cap.stdout; .. > -> unit
