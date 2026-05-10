(* Files larger than this are silently dropped from the batch (no event
 * emitted). The taint engine and the JSON serialiser are both quadratic
 * enough on huge inputs that letting them run on the long tail dominates
 * the wall clock for a whole batch. *)
let skip_taint_large_file_bytes = 500_000

let parse_files_ast (caps : < Cap.fork >) (conf : Taint_scan_config.t) : unit =
  let n_files = List.length conf.files in
  Logs.app ~src:Ir_pipeline_logs.src (fun m ->
    m "Processing %d files" n_files);

  let glob_start_time = Unix.gettimeofday () in

  let applicable_files_sorted =
    conf.files
    |> List.filter (fun file_path ->
           match Lang.langs_of_filename file_path with
           | [] -> false  (* Not supported language *)
           | _ :: _ -> true)
    |> List.filter (fun path ->
           (Unix.stat @@ Fpath.to_string path).Unix.st_size
           < skip_taint_large_file_bytes)
    |> List_.sort_by_key UFile.filesize (Fun.flip Int.compare)
  in

  (* Precompute the analyzer-specific rule classification once per distinct
   * language seen in the batch. Without this, every file re-walks the full
   * [conf.rules] list to compute its own search/taint subsets, which on
   * large rulesets dominates the per-file overhead. *)
  let rules_by_lang : (Lang.t, Taint_engine.analyzer_rules) Hashtbl.t =
    Hashtbl.create 16
  in
  applicable_files_sorted
  |> List.iter (fun f ->
         let lang = Lang.lang_of_filename_exn f in
         if not (Hashtbl.mem rules_by_lang lang) then
           Hashtbl.add rules_by_lang lang
             (Taint_engine.classify_rules_for_analyzer
                ~analyzer:(Xlang.of_lang lang) conf.rules));

  (* Count outcomes via atomics rather than the parmap result list, so the
   * per-file [parse_file] results (which previously survived in the result
   * list until the whole batch finished) can be released as soon as the
   * [on_parsed] callback returns. *)
  let success_count = Atomic.make 0 in
  let error_count = Atomic.make 0 in

  let process_file (file : Fpath.t) =
    let lang = Lang.lang_of_filename_exn file in
    let ar = Hashtbl.find rules_by_lang lang in
    conf.on_parsed (Taint_engine.parse_file ~mode:conf.mode file ar);
    Atomic.incr success_count
  in

  let exception_handler (file : Fpath.t) (e : Exception.t) =
    Atomic.incr error_count;
    Logs.err ~src:Ir_pipeline_logs.src (fun m ->
      m "ERROR: %s - %s" (Fpath.to_string file) (Exception.to_string e))
  in

  if conf.num_domains <= 1 then
    applicable_files_sorted
    |> List.iter (fun f ->
         ignore (Domainslib_.wrap_result process_file ~exception_handler f
                 : (unit, unit) result))
  else
    ignore (Domainslib_.parmap caps ~num_domains:conf.num_domains ~chunksize:1
              ~exception_handler process_file applicable_files_sorted
            : (unit, unit) result list);

  let success_count = Atomic.get success_count in
  let error_count = Atomic.get error_count in

  Logs.app ~src:Ir_pipeline_logs.src (fun m ->
    m "Successfully processed %d/%d files (%d errors)" success_count n_files
      error_count);
  let glob_end_time = Unix.gettimeofday () in
  let glob_elapsed_ms = (glob_end_time -. glob_start_time) *. 1000.0 in
  let avg_ms_str =
    if n_files = 0 then "n/a"
    else Printf.sprintf "%.2f ms" (glob_elapsed_ms /. float_of_int n_files)
  in
  Logs.app ~src:Ir_pipeline_logs.src (fun m ->
    m "Total time - %.2f ms; average time - %s" glob_elapsed_ms avg_ms_str)

let parse_and_serialize_file (_caps : < Cap.fork >) ~(num_domains : int)
    ?(format = `Json) ?(after_file = Fun.const ()) (infile : Fpath.t)
    (rules : Rule.t list) : string =
  let _ = num_domains in
  (* Single-file path: no batch to amortize across, so classify on every
   * call. [Lang.lang_of_filename_exn] is also called inside [parse_file];
   * the cost is sub-microsecond, not worth the API churn to deduplicate. *)
  let lang = Lang.lang_of_filename_exn infile in
  let ar =
    Taint_engine.classify_rules_for_analyzer
      ~analyzer:(Xlang.of_lang lang) rules
  in
  let parsed = Taint_engine.parse_file infile ar in
  let result =
    match format with
    | `Json ->
        Ast_payload.serialize_ast_with_taint_to_string
          parsed.ast parsed.taint_entries
        |> Yojson.Safe.pretty_to_string
    | `Binary ->
        Ast_payload.serialize_ast_with_taint_to_binary_string
          parsed.ast parsed.taint_entries
        |> Yojson.Safe.pretty_to_string
  in
  after_file infile;
  result
