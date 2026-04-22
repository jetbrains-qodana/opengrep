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

(*************************************************************************)
(* Command-line parsing: turn argv into conf *)
(*************************************************************************)
let cmdline_term : conf Term.t =
  let combine format jobs rules_file rules_path =
    { rules_path; rules_file; format; jobs }
  in
  Term.(const combine $ o_format $ o_jobs $ o_rules_file $ o_rules)

let parse_argv (argv : string array) : conf =
  let cmd : conf Cmd.t = Cmd.v cmdline_info cmdline_term in
  CLI_common.eval_value ~argv cmd
