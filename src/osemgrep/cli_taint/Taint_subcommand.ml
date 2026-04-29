(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(* Implementation of the 'opengrep taint' subcommand.
 *
 * Reads file paths from stdin (one per line), parses each file and its
 * taint specifications, and streams newline-delimited JSON (or base64
 * encoded binary) results to stdout.
 *)

module Y = Yojson.Safe
module Out = Semgrep_output_v1_t

(*****************************************************************************)
(* Types and constants *)
(*****************************************************************************)
(* we need Cap.fork for parallel rule evaluation *)
type caps = < Cap.fork >

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)

let read_file_list_from_stdin () : Fpath.t list =
  let files = ref [] in
  (try
     while true do
       let line = input_line Stdlib.stdin in
       let trimmed = String.trim line in
       if trimmed <> "" then
         if Sys.is_directory trimmed then
           UCommon.pr2
             (Printf.sprintf "[taint] Skipping directory: %s" trimmed)
         else files := Fpath.v trimmed :: !files
     done
   with End_of_file -> ());
  List.sort_uniq Fpath.compare !files

let load_rules_from_path (rules_path : Fpath.t) : Rule.t list =
  if try Sys.is_directory (Fpath.to_string rules_path) with _ -> false then (
    let rule_files =
      List_files.list rules_path |> List.filter Rule_file.is_valid_rule_filename
    in
    UCommon.pr2
      (Printf.sprintf "[taint] Found %d rule files in %s"
         (List.length rule_files)
         (Fpath.to_string rules_path));
    let all_rules = ref [] in
    let total_invalid = ref 0 in
    rule_files
    |> List.iter (fun file ->
           match Parse_rule.parse_and_filter_invalid_rules file with
           | Ok (valid_rules, invalid_rules) ->
               all_rules := !all_rules @ valid_rules;
               total_invalid := !total_invalid + List.length invalid_rules
           | Error err ->
               UCommon.pr2
                 (Printf.sprintf "[taint] Failed to parse %s: %s"
                    (Fpath.to_string file)
                    (Rule_error.string_of_error err)));
    UCommon.pr2
      (Printf.sprintf "[taint] Loaded %d valid rules, %d invalid from %s"
         (List.length !all_rules)
         !total_invalid
         (Fpath.to_string rules_path));
    !all_rules)
  else
    match Parse_rule.parse_and_filter_invalid_rules rules_path with
    | Ok (valid_rules, _) -> valid_rules
    | Error err ->
        UCommon.pr2
          (Printf.sprintf "[taint] Failed to parse rules: %s"
             (Rule_error.string_of_error err));
        []

let read_lines_from_file (path : string) : string list =
  let ic = Stdlib.open_in path in
  let lines = ref [] in
  (try
     while true do
       let line = input_line ic in
       let trimmed = String.trim line in
       if trimmed <> "" then lines := trimmed :: !lines
     done
   with End_of_file -> close_in ic);
  List.rev !lines

let load_rules (conf : Taint_CLI.conf) : Rule.t list =
  let paths =
    (match conf.rules_path with
    | Some p -> [ p ]
    | None -> [])
    @
    match conf.rules_file with
    | Some file -> read_lines_from_file file
    | None -> []
  in
  if paths = [] then (
    UCommon.pr2 "[taint] No rules path provided";
    [])
  else List.concat_map (fun p -> load_rules_from_path (Fpath.v p)) paths

(*****************************************************************************)
(* Diagnostics rendering (LSP-style JSON) *)
(*****************************************************************************)

(* Mirror of Convert_utils.convert_severity / DiagnosticSeverity in the LSP
 * spec. We avoid depending on the LSP library here so that this subcommand
 * stays small and stdin/stdout-only. *)
let lsp_severity_of_match_severity (s : Out.match_severity) : int =
  match s with
  | `Error
  | `Critical
  | `High ->
      1
  | `Warning
  | `Medium ->
      2
  | `Info
  | `Low
  | `Experiment
  | `Inventory ->
      3

let position_to_yojson (p : Out.position) : Y.t =
  (* LSP uses 0-based line/character; opengrep positions are 1-based. *)
  `Assoc
    [ ("line", `Int (p.line - 1));
      ("character", `Int (p.col - 1)) ]

let range_of_cli_match (m : Out.cli_match) : Y.t =
  `Assoc
    [ ("start", position_to_yojson m.start);
      ("end", position_to_yojson m.end_) ]

let lsp_diagnostic_of_cli_match (m : Out.cli_match) : Y.t =
  let check_id_str = Rule_ID.to_string m.check_id in
  let message =
    if String.equal m.extra.message "" then
      Printf.sprintf "Semgrep found: %s" check_id_str
    else m.extra.message
  in
  let metadata = (m.extra.metadata :> Y.t) in
  let shortlink =
    match metadata with
    | `Assoc _ ->
        metadata |> Y.Util.member "shortlink" |> Y.Util.to_string_option
    | _ -> None
  in
  let base =
    [ ("range", range_of_cli_match m);
      ("severity", `Int (lsp_severity_of_match_severity m.extra.severity));
      ("code", `String check_id_str);
      ("source", `String "Semgrep");
      ("message", `String message) ]
  in
  let fields =
    match shortlink with
    | None -> base
    | Some s ->
        base @ [ ("codeDescription", `Assoc [ ("href", `String s) ]) ]
  in
  `Assoc fields

(* Convert a parsed_file's Core_match list into a [cli_match list] following
 * the same conversion path that 'opengrep scan' uses (Core_runner.mk_result
 * + Output.preprocess_result), so message templating, severity overrides
 * and metavar interpolation match what scan would emit.
 *
 * [rules] is the full rule set passed to the engine (used for hrules
 * lookup during cli_match construction). *)
let cli_matches_of_parsed_file ~(rules : Rule.t list)
    (parsed : Taint_processor.parsed_file) : Out.cli_match list =
  let processed_matches =
    parsed.matches |> List_.map Core_result.mk_processed_match
  in
  let scanned = [ Target.mk_target parsed.xlang parsed.file ] in
  let core_result : Core_result.t =
    {
      processed_matches;
      errors = parsed.errors;
      skipped_targets = [];
      skipped_rules = [];
      valid_rules = rules;
      rules_with_targets = rules;
      scanned;
      profiling = None;
      explanations = None;
      rules_by_engine = [];
      interfile_languages_used = [];
    }
  in
  let runner_result = Core_runner.mk_result rules core_result in
  let cli_output = Output.preprocess_result ~fixed_lines:false runner_result in
  cli_output.results

let mk_render_diagnostics ~(rules : Rule.t list) :
    Taint_processor.parsed_file -> Y.t =
 fun parsed ->
  match cli_matches_of_parsed_file ~rules parsed with
  | exception exn ->
      UCommon.pr2
        (Printf.sprintf
           "[taint]   WARNING: failed to render diagnostics for %s: %s"
           (Fpath.to_string parsed.file)
           (Printexc.to_string exn));
      `List []
  | matches -> `List (List_.map lsp_diagnostic_of_cli_match matches)

(*****************************************************************************)
(* Main logic *)
(*****************************************************************************)

let run_conf (caps : < caps ; .. >) (conf : Taint_CLI.conf) : Exit_code.t =
  (* This subcommand is a streaming JSON pipeline: stdout carries the data
   * and stderr carries only our own operational status ([taint]/[ir-pipeline]
   * lines via UCommon.pr2). Drop the basic Logs reporter installed by
   * CLI.main so the engine's Logs.warn/err calls (naming, matching, parsing,
   * tree-sitter, ...) don't flood stderr. *)
  Logs.set_reporter Logs.nop_reporter;
  Logs.set_level None;

  Parsing_init.init ();

  let rules = load_rules conf in
  let files = read_file_list_from_stdin () in

  if files = [] then (
    UCommon.pr2 "[taint] No files provided on stdin";
    Exit_code.ok ~__LOC__)
  else (
    let render_diagnostics =
      if conf.with_diagnostics then Some (mk_render_diagnostics ~rules)
      else None
    in
    Taint_processor.parse_files_ast
      (caps :> < Cap.fork >)
      ~num_domains:conf.jobs ~format:conf.format
      ~with_diagnostics:conf.with_diagnostics ?render_diagnostics
      files "" rules;
    Exit_code.ok ~__LOC__)

(*****************************************************************************)
(* Entry point *)
(*****************************************************************************)
let main (caps : < caps ; .. >) (argv : string array) : Exit_code.t =
  let conf = Taint_CLI.parse_argv argv in
  run_conf caps conf
