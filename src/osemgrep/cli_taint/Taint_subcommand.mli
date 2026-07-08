(*
   Parse AST+taint JSON for a list of files read from stdin.
*)

(* Cap.fork for parallel rule evaluation; Cap.time_limit for taint per-rule timeouts. *)
type caps = < Cap.fork ; Cap.time_limit >

(*
   Parse an 'opengrep taint' command, execute it and return an exit code.

   Usage: main caps [| "opengrep-taint"; ... |]
*)
val main : < caps ; .. > -> string array -> Exit_code.t

(* internal *)
val run_conf : < caps ; .. > -> Taint_CLI.conf -> Exit_code.t
