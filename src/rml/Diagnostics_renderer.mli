(* Diagnostics rendering for the RML taint pipeline.
 *
 * The output shape is similiar to LSP's [Diagnostic] type (range, severity, code,
 * message) but no the same. Severinity is serialized as string (not number like in LSP), 
 * positions contain offsets (in contrast to LSP). *)

module Out = Semgrep_output_v1_t

(* Render engine results as a JSON array.
 *
 * Catches conversion failures so a single bad file doesn't take down a
 * batch run; emits a stderr warning and an empty array instead. *)
val render_diagnostics :
  rules:Rule.t list ->
  file:Fpath.t ->
  xlang:Xlang.t ->
  matches:Core_match.t list ->
  errors:Core_error.t list ->
  Yojson.Safe.t
