(* Files larger than this are silently dropped from the batch (no event
 * emitted). *)
let skip_taint_large_file_bytes = 500_000

let parse_files_ast (caps : < Cap.fork ; Cap.time_limit >)
    (conf : Taint_scan_config.t) : unit =
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
   * [conf.rules] list to compute its own search/taint subsets. *)
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

  let success_count = Atomic.make 0 in
  let error_count = Atomic.make 0 in
  let timeout_count = Atomic.make 0 in

  let process_file (file : Fpath.t) =
    let lang = Lang.lang_of_filename_exn file in
    let ar = Hashtbl.find rules_by_lang lang in
    match
      Taint_engine.parse_file (caps :> < Cap.time_limit >) ~mode:conf.mode
        ~timeout:conf.timeout ~timeout_threshold:conf.timeout_threshold file ar
    with
    | parsed ->
        conf.on_parsed parsed;
        Atomic.incr success_count
    | exception Match_rules.File_timeout rule_ids ->
        Atomic.incr timeout_count;
        Logs.warn ~src:Ir_pipeline_logs.src (fun m ->
          m "Too many rule timeouts on %s (rules: %s), skipping file"
            (Fpath.to_string file)
            (rule_ids |> List_.map Rule_ID.to_string |> String.concat ","))
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
    ignore (Domainslib_.parmap (caps :> < Cap.fork >)
              ~num_domains:conf.num_domains ~chunksize:1
              ~exception_handler process_file applicable_files_sorted
            : (unit, unit) result list);

  let success_count = Atomic.get success_count in
  let error_count = Atomic.get error_count in
  let timeout_count = Atomic.get timeout_count in

  Logs.app ~src:Ir_pipeline_logs.src (fun m ->
    m "Successfully processed %d/%d files (%d unsupported lang or too big, %d errors, %d timeouts)"
      success_count n_files (n_files - List.length applicable_files_sorted) error_count timeout_count);
  let glob_end_time = Unix.gettimeofday () in
  let glob_elapsed_ms = (glob_end_time -. glob_start_time) *. 1000.0 in
  let avg_ms_str =
    if n_files = 0 then "n/a"
    else Printf.sprintf "%.2f ms" (glob_elapsed_ms /. float_of_int n_files)
  in
  Logs.app ~src:Ir_pipeline_logs.src (fun m ->
    m "Total time - %.2f ms; average time - %s" glob_elapsed_ms avg_ms_str)

let parse_and_serialize_file (caps : < Cap.time_limit >) ?(format = `Json)
    ?(after_file = Fun.const ()) (infile : Fpath.t) (rules : Rule.t list) :
    string =
  let lang = Lang.lang_of_filename_exn infile in
  let ar =
    Taint_engine.classify_rules_for_analyzer
      ~analyzer:(Xlang.of_lang lang) rules
  in
  let parsed = Taint_engine.parse_file caps infile ar
  in
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
