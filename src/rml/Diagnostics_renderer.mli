(* LSP-style diagnostics rendering for the RML taint pipeline.
 *
 * The output shape mirrors LSP's [Diagnostic] type (range, severity, code,
 * message) so it matches the shape produced by [Diagnostics.diagnostic_of_match]
 * in the language server. We deliberately avoid depending on the LSP library
 * here so the rml lib stays small. *)

module Out = Semgrep_output_v1_t

(* Convert a single [Out.cli_match] into a JSON object shaped like an LSP
 * [Diagnostic]. *)
val lsp_diagnostic_of_cli_match : Out.cli_match -> Yojson.Safe.t

(* Convert engine output into [Out.cli_match list] using the same canonical
 * pipeline that [opengrep scan] uses ([Core_runner.mk_result] +
 * [Output.preprocess_result]), so message templating, metavar interpolation,
 * severity overrides and fingerprinting match what scan would emit. *)
val cli_matches_of_engine_results :
  rules:Rule.t list ->
  file:Fpath.t ->
  xlang:Xlang.t ->
  matches:Core_match.t list ->
  errors:Core_error.t list ->
  Out.cli_match list

(* Render engine results as a JSON array of LSP-style diagnostics.
 *
 * Catches conversion failures so a single bad file doesn't take down a
 * batch run; emits a stderr warning and an empty array instead. *)
val render_lsp_diagnostics :
  rules:Rule.t list ->
  file:Fpath.t ->
  xlang:Xlang.t ->
  matches:Core_match.t list ->
  errors:Core_error.t list ->
  Yojson.Safe.t
