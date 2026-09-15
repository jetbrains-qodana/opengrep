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
  timeout : float option;
  timeout_threshold : int option;
  logging_level : Logs.level option;
  (* Benchmark mode: path of the per-rule timing report to write. *)
  bench : string option;
  (* Zero or more SCIP (`index.scip`) protobuf indexes used to resolve
   * metavariable-type against types defined outside the scanned file. See
   * docs/superpowers/specs/2026-09-15-scip-metavariable-type-design.md. *)
  scip_index : string list;
}

(* entry point *)
val parse_argv : string array -> conf
