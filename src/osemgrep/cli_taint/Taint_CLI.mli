(*
   'opengrep taint ...' command-line parsing.
*)

(* The result of parsing an 'opengrep taint ...' command *)
type conf = {
  rules_path : string option;
  rules_file : string option;
  format : [ `Json | `Binary ];
  jobs : int;
  with_diagnostics : bool;
  (* mix of --verbose/--debug; controls how much the pipeline logs to stderr *)
  logging_level : Logs.level option;
}

(* entry point *)
val parse_argv : string array -> conf
