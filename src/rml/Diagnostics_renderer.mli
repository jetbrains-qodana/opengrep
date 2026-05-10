(* LSP-style diagnostics rendering for the RML taint pipeline.
 *
 * The output shape mirrors LSP's [Diagnostic] type (range, severity, code,
 * message) so it matches the shape produced by [Diagnostics.diagnostic_of_match]
 * in the language server. We deliberately avoid depending on the LSP library
 * here so the rml lib stays small. *)

module Out = Semgrep_output_v1_t

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
