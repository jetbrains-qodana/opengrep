open Taint_location
module Y = Yojson.Safe
module Out = Semgrep_output_v1_t

let yojson_of_taint_location (loc : taint_location) : Y.t =
  `Assoc
    [
      ("file_path", `String loc.file_path);
      ("line", `Int loc.line);
      ("col", `Int loc.col);
      ("offsetStart", `Int loc.offsetStart);
      ("offsetEnd", `Int loc.offsetEnd);
    ]

type taint_entry = {
  rule : string;
  loc : taint_location;
  pattern : string option;
}

type taint_propagator_entry = {
  rule : string;
  loc : taint_location;
  locFrom : taint_location;
  locTo : taint_location;
  pattern : string option;
}

let pattern_field = function
  | None -> []
  | Some pattern -> [ ("pattern", `String pattern) ]

let yojson_of_taint_entry ({ rule; loc; pattern } : taint_entry) : Y.t =
  let fields =
    [ ("rule", `String rule) ]
    @ pattern_field pattern
    @ [ ("loc", yojson_of_taint_location loc) ]
  in
  `Assoc fields

let yojson_of_propagator_entry
    ({ rule; loc; locFrom; locTo; pattern } : taint_propagator_entry) : Y.t =
  let fields =
    [ ("rule", `String rule) ]
    @ pattern_field pattern
    @ [
        ("loc", yojson_of_taint_location loc);
        ("locFrom", yojson_of_taint_location locFrom);
        ("locTo", yojson_of_taint_location locTo);
      ]
  in
  `Assoc fields

type taint_entries_block_t = taint_entry list
type taint_propagators_block_t = taint_propagator_entry list

type taint_entries_t =
  taint_entries_block_t
  * taint_entries_block_t
  * taint_entries_block_t
  * taint_propagators_block_t

let yojson_fields_of_taint_entries
    ((sources, sinks, sanitizers, propagators) : taint_entries_t) :
    (string * Y.t) list =
  [
    ("sources", `List (List.map yojson_of_taint_entry sources));
    ("sinks", `List (List.map yojson_of_taint_entry sinks));
    ("sanitizers", `List (List.map yojson_of_taint_entry sanitizers));
    ("propagators", `List (List.map yojson_of_propagator_entry propagators));
  ]

(*****************************************************************************)
(* LSP-style diagnostics rendering *)
(*****************************************************************************)
(* Convert engine results (Core_match.t list, Core_error.t list) into
 * LSP-style diagnostics suitable for streaming alongside the AST/taint
 * payload of the 'opengrep taint --with-diagnostics' subcommand.
 *
 * The shape mirrors LSP's [Diagnostic] type (range, severity, code, source,
 * message, codeDescription) so it matches the shape produced by
 * [Diagnostics.diagnostic_of_match] in the language server. We deliberately
 * avoid depending on the LSP library here so the rml lib stays small. *)

(* Mirror of Convert_utils.convert_severity / DiagnosticSeverity in LSP. *)
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

(* Convert engine output into [Out.cli_match list] using the same canonical
 * pipeline that [opengrep scan] uses (Core_runner.mk_result +
 * Output.preprocess_result), so message templating, metavar interpolation,
 * severity overrides and fingerprinting match what scan would emit. *)
let cli_matches_of_engine_results ~(rules : Rule.t list) ~(file : Fpath.t)
    ~(xlang : Xlang.t) ~(matches : Core_match.t list)
    ~(errors : Core_error.t list) : Out.cli_match list =
  let processed_matches = matches |> List_.map Core_result.mk_processed_match in
  let scanned = [ Target.mk_target xlang file ] in
  let core_result : Core_result.t =
    {
      processed_matches;
      errors;
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

(* Render engine results as a JSON array of LSP-style diagnostics.
 * Catches conversion failures so a single bad file doesn't take down a
 * batch run; emits a stderr warning and an empty array instead. *)
let render_lsp_diagnostics ~(rules : Rule.t list) ~(file : Fpath.t)
    ~(xlang : Xlang.t) ~(matches : Core_match.t list)
    ~(errors : Core_error.t list) : Y.t =
  match
    cli_matches_of_engine_results ~rules ~file ~xlang ~matches ~errors
  with
  | exception exn ->
      UCommon.pr2
        (Printf.sprintf
           "[taint]   WARNING: failed to render diagnostics for %s: %s"
           (Fpath.to_string file)
           (Printexc.to_string exn));
      `List []
  | cli_matches -> `List (List_.map lsp_diagnostic_of_cli_match cli_matches)
