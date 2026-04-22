(*
   'opengrep taint ...' command-line parsing.
*)

(* The result of parsing an 'opengrep taint ...' command *)
type conf = {
  rules_path : string option;
  rules_file : string option;
  format : [ `Json | `Binary ];
  jobs : int;
}

(* entry point *)
val parse_argv : string array -> conf
