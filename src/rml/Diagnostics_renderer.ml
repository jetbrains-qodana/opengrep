module Y = Yojson.Safe
module Out = Semgrep_output_v1_t

let position_to_yojson (p : Out.position) : Y.t =
  (* LSP uses 0-based line/character; opengrep positions are 1-based. *)
  `Assoc
    [ ("line", `Int (p.line - 1));
      ("character", `Int (p.col - 1));
      ("offset", `Int p.offset) ]

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
  let severity =
    Y.from_string (Semgrep_output_v1_j.string_of_match_severity m.extra.severity)
  in
  `Assoc
    [ ("range", range_of_cli_match m);
      ("severity", severity);
      ("code", `String check_id_str);
      ("message", `String message) ]

let cli_matches_of_engine_results ~(rules : Rule.t list) ~(file : Fpath.t)
    ~(xlang : Xlang.t) ~(matches : Core_match.t list)
    ~(errors : Core_error.t list) : Out.cli_match list =
  let core_result : Core_result.t =
    {
      processed_matches = List_.map Core_result.mk_processed_match matches;
      errors;
      skipped_targets = [];
      skipped_rules = [];
      valid_rules = rules;
      rules_with_targets = rules;
      scanned = [ Target.mk_target xlang file ];
      profiling = None;
      explanations = None;
      rules_by_engine = [];
      interfile_languages_used = [];
    }
  in
  let runner_result = Core_runner.mk_result rules core_result in
  let cli_output = Output.preprocess_result ~fixed_lines:false runner_result in
  cli_output.results

let render_diagnostics ~(rules : Rule.t list) ~(file : Fpath.t)
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
