module Arg = Cmdliner.Arg
module Term = Cmdliner.Term
module Cmd = Cmdliner.Cmd

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(*
   'opengrep taint ...' command-line arguments processing.
*)

(*****************************************************************************)
(* Types *)
(*****************************************************************************)
type conf = {
  rules_path : string option;
  rules_file : string option;
  format : [ `Json | `Binary ];
  jobs : int;
  with_diagnostics : bool;
  timeout : float option;
  timeout_threshold : int option;
  logging_level : Logs.level option;
}

(*****************************************************************************)
(* Manpage Documentation *)
(*****************************************************************************)
let doc = "Parse files into AST+taint JSON, streamed to stdout"

let man : Cmdliner.Manpage.block list =
  [
    `S Cmdliner.Manpage.s_description;
    `P
      "Reads file paths from stdin (one per line), parses each file, and \
       streams newline delimited JSON results to stdout.";
  ]
  @ CLI_common.help_page_bottom

let cmdline_info : Cmd.info = Cmd.info "opengrep taint" ~doc ~man

(*****************************************************************************)
(* Flags *)
(*****************************************************************************)
let o_rules : string option Term.t =
  let info =
    Arg.info [ "rules"; "r" ] ~docv:"PATH"
      ~doc:
        "YAML file or directory with taint rules. If omitted, no taint \
         analysis is performed."
  in
  Arg.value (Arg.opt (Arg.some Arg.string) None info)

let o_format : [ `Json | `Binary ] Term.t =
  let format_enum = Arg.enum [ ("json", `Json); ("binary", `Binary) ] in
  let info =
    Arg.info [ "format"; "f" ] ~docv:"FORMAT"
      ~doc:"Output format. $(docv) must be $(b,json) or $(b,binary)."
  in
  Arg.value (Arg.opt format_enum `Json info)

let o_rules_file : string option Term.t =
  let info =
    Arg.info [ "rules-file" ] ~docv:"FILE"
      ~doc:
        "File containing rule paths, one per line. Each path may be a YAML \
         file or directory."
  in
  Arg.value (Arg.opt (Arg.some Arg.string) None info)

let o_jobs : int Term.t =
  let info =
    Arg.info [ "jobs"; "j" ] ~docv:"N"
      ~doc:"Number of parallel jobs (default: number of CPUs)."
  in
  Arg.value (Arg.opt Arg.int (Domainslib_.get_cpu_count ()) info)

let o_with_diagnostics : bool Term.t =
  let info =
    Arg.info [ "with-diagnostics" ]
      ~doc:
        "In addition to the AST and taint information, run the search/taint \
         rule engine on each file and emit LSP-style diagnostics under a \
         $(b,diagnostics) field of every line of output."
  in
  Arg.value (Arg.flag info)

let o_timeout : float option Term.t =
  let info =
    Arg.info [ "timeout" ] ~docv:"SECS"
      ~doc:
        (Printf.sprintf
           "Maximum time in seconds to spend matching a single rule on a \
            single file. If not set, no timeout is applied.")
  in
  Arg.value (Arg.opt (Arg.some Arg.float) None info)

let o_timeout_threshold : int option Term.t =
  let info =
    Arg.info [ "timeout-threshold" ] ~docv:"N"
      ~doc:
        (Printf.sprintf
           "Maximum number of rules that can time out on a file before the \
            whole file is skipped. If not set (or 0), the file is never \
            skipped for timeouts.")
  in
  Arg.value (Arg.opt (Arg.some Arg.int) None info)

(* Verbosity flags for the taint pipeline. By default only the pipeline's own
 * operational messages are shown at [Warning]; [--verbose]/[--debug] raise the
 * level and also unmute the taint engine sources (e.g. [semgrep.tainting]).
 * See [Ir_pipeline_logs.init_taint_subcommand_logging]. *)
let o_verbose : bool Term.t =
  let info =
    Arg.info [ "v"; "verbose" ]
      ~doc:"Show more details about the taint pipeline (Info level)."
  in
  Arg.value (Arg.flag info)

let o_debug : bool Term.t =
  let info =
    Arg.info [ "debug" ]
      ~doc:"All of --verbose, but with additional debugging information."
  in
  Arg.value (Arg.flag info)

(*************************************************************************)
(* Command-line parsing: turn argv into conf *)
(*************************************************************************)
let cmdline_term : conf Term.t =
  let combine format jobs rules_file rules_path with_diagnostics timeout
      timeout_threshold debug verbose =
    let logging_level =
      match (verbose, debug) with
      | _, true -> (* --debug *) Some Logs.Debug
      | true, false -> (* --verbose *) Some Logs.Info
      | false, false -> (* default *) Some Logs.Warning
    in
    {
      rules_path;
      rules_file;
      format;
      jobs;
      with_diagnostics;
      timeout;
      timeout_threshold;
      logging_level;
    }
  in
  Term.(
    const combine $ o_format $ o_jobs $ o_rules_file $ o_rules
    $ o_with_diagnostics $ o_timeout $ o_timeout_threshold $ o_debug $ o_verbose)

let parse_argv (argv : string array) : conf =
  let cmd : conf Cmd.t = Cmd.v cmdline_info cmdline_term in
  CLI_common.eval_value ~argv cmd
