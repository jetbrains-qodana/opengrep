(* Streaming NDJSON emitter for the CLI taint subcommand.
 *
 * Single concrete consumer used as [Taint_scan_config.on_parsed]. Writes
 * one JSON object per parsed file to [stdout], terminated by a newline,
 * with cross-domain access serialised through an internal mutex. *)

(** Emit one NDJSON line for [parsed] to [stdout].

    Output shape:
      [{"file": <string>, "data": <ast+taint>, "diagnostics": <lsp diags>}]

    [~ast_format] selects between the JSON or binary representation of the
    AST+taint payload. [~rules] is the full rule set the file was scanned
    against, used to look up rule metadata (severity, source, ...) when
    rendering the LSP-style diagnostic objects.

    Thread-safe: callers may invoke this concurrently from worker domains. *)
val write_result_to_stdout :
  ast_format:Ast_payload.ast_format ->
  rules:Rule.t list ->
  Taint_scan_config.parsed_file -> unit
