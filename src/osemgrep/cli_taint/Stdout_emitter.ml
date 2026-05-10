module Y = Yojson.Safe

(* Single mutex protects all concurrent writes to [stdout] from the parmap
 * worker domains. Without it interleaved bytes from different lines would
 * corrupt the NDJSON stream. *)
let stdout_mutex = Mutex.create ()

let write_result_to_stdout ~(ast_format : Ast_payload.ast_format)
    ~(rules : Rule.t list) (parsed : Taint_scan_config.parsed_file) : unit =
  let data =
    match ast_format with
    | `Json ->
        Ast_payload.serialize_ast_with_taint_to_string
          parsed.ast parsed.taint_entries
    | `Binary ->
        Ast_payload.serialize_ast_with_taint_to_binary_string
          parsed.ast parsed.taint_entries
  in
  let diag =
    Diagnostics_renderer.render_lsp_diagnostics ~rules ~file:parsed.file
      ~xlang:parsed.xlang ~matches:parsed.matches ~errors:parsed.errors
  in
  let fields =
    [ ("file", `String (Fpath.to_string parsed.file));
      ("data", data);
      ("diagnostics", diag) ]
  in
  let line = Y.to_string (`Assoc fields) in
  Mutex.protect stdout_mutex (fun () -> Printf.fprintf stdout "%s\n%!" line)
