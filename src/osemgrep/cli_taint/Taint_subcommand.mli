(*
   Parse AST+taint JSON for a list of files read from stdin.
*)

(* we need Cap.fork for parallel rule evaluation *)
type caps = < Cap.fork >

(*
   Parse an 'opengrep taint' command, execute it and return an exit code.

   Usage: main caps [| "opengrep-taint"; ... |]
*)
val main : < caps ; .. > -> string array -> Exit_code.t

(* internal *)
val run_conf : < caps ; .. > -> Taint_CLI.conf -> Exit_code.t
