(* Batch and single-file orchestration of the taint scan engine.
 *
 * Calls [Taint_engine] for the actual analysis and [Ast_payload] for
 * serialization. *)

(** Run the taint engine on every file in [conf.files], dispatched across
    [conf.num_domains] worker domains, and invoke [conf.on_parsed] for each
    successfully parsed file.

    Files are filtered by language support and oversize threshold (currently
    a hard-coded 500 KB cap; oversized and unsupported files are silently
    dropped). Per-file [parse_file] errors are caught and logged via
    [Logs] under [semgrep.ir-pipeline]; they do not abort the batch.

    [conf.on_parsed] may be invoked concurrently from multiple worker
    domains, so the callback must be thread-safe. *)
val parse_files_ast :
  < Cap.fork ; Cap.time_limit > -> Taint_scan_config.t -> unit

(** Single-file entry point used by the LSP server. Parses [infile], runs
    the taint engine on [rules], and serialises the result as JSON or binary.

    [~after_file] is invoked after serialisation with the same [infile] path
    (default [Fun.const ()]). The LSP uses this for per-file cache cleanup and
    memory hygiene; keep it cheap and thread-safe if you override it. *)
val parse_and_serialize_file :
  < Cap.time_limit > ->
  ?format:Ast_payload.ast_format ->
  ?after_file:(Fpath.t -> unit) ->
  ?timeout:float option ->
  ?timeout_threshold:int option ->
  Fpath.t -> Rule.t list -> string
