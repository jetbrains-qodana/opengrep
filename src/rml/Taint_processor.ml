module G = AST_generic
module Y = Yojson.Safe

type ast_format = [ `Json | `Binary ]

let base64_table = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/"

let base64_encode (data : string) : string =
  let len = String.length data in
  let buf = Buffer.create (((len + 2) / 3) * 4) in
  let get i = if i < len then Char.code data.[i] else 0 in
  let rec loop i =
    if i >= len then ()
    else
      let b1 = get i in
      let b2 = get (i + 1) in
      let b3 = get (i + 2) in
      let triple = (b1 lsl 16) lor (b2 lsl 8) lor b3 in
      Buffer.add_char buf base64_table.[(triple lsr 18) land 0x3F];
      Buffer.add_char buf base64_table.[(triple lsr 12) land 0x3F];
      if i + 1 < len then
        Buffer.add_char buf base64_table.[(triple lsr 6) land 0x3F]
      else
        Buffer.add_char buf '=';
      if i + 2 < len then
        Buffer.add_char buf base64_table.[triple land 0x3F]
      else
        Buffer.add_char buf '=';
      loop (i + 3)
  in
  loop 0;
  Buffer.contents buf

let serialize_ast_to_json_string (ast : AST_generic.program) : string =
  let v1_ast = AST_generic_to_v1.program ast in
  Ast_generic_v1_j.string_of_program v1_ast

let ast_to_yojson (ast : AST_generic.program) : Y.t =
  ast |> serialize_ast_to_json_string |> Y.from_string

let serialize_ast_with_taint_to_string (ast : AST_generic.program)
    (taint_entries : Taint_serializer.taint_entries_t) : Y.t =
  let ast_json = ast_to_yojson ast in
  let taint_value = `Assoc (Taint_serializer.yojson_fields_of_taint_entries taint_entries) in
  `Assoc [ ("ast", ast_json); ("taint", taint_value) ]

let serialize_ast_with_taint_to_binary_string (ast : AST_generic.program)
    (taint_entries : Taint_serializer.taint_entries_t) : Y.t =
  let v1_ast = AST_generic_to_v1.program ast in
  let pool_builder = Ast_binary_serializer.create_string_pool_builder () in
  Ast_binary_serializer.collect_program v1_ast pool_builder;
  Ast_binary_serializer.collect_taint_entries taint_entries pool_builder;
  let pool = Ast_binary_serializer.build_string_pool pool_builder in
  let ast_binary = Ast_binary_serializer.serialize_program v1_ast pool in
  let taint_binary = Ast_binary_serializer.serialize_taint_entries taint_entries pool in
  `Assoc
    [ ("stringPool", Ast_binary_serializer.string_pool_to_yojson pool);
      ("astBinary", `String (base64_encode ast_binary));
      ("taintBinary", `String (base64_encode taint_binary)) ]

let empty_taint_entries : Taint_serializer.taint_entries_t = ([], [], [], [])

let serialize_empty_ast_with_taint_to_string () =
  serialize_ast_with_taint_to_string [] empty_taint_entries |> Y.pretty_to_string

let skip_taint_large_file_bytes = 500_000

let stdout_mutex = Mutex.create ()

let write_result_to_stdout ~(ast_format : ast_format) ~(rules : Rule.t list) (parsed : Taint_scan_config.parsed_file) : unit =
  let data =
    match ast_format with
    | `Json ->
        serialize_ast_with_taint_to_string parsed.ast parsed.taint_entries
    | `Binary ->
        serialize_ast_with_taint_to_binary_string parsed.ast parsed.taint_entries
  in
  let diag =
        Diagnostics_renderer.render_lsp_diagnostics ~rules ~file:parsed.file
          ~xlang:parsed.xlang ~matches:parsed.matches ~errors:parsed.errors
      in
  let fields =
    [ ("file", `String (Fpath.to_string parsed.file)); ("data", data); ("diagnostics", diag) ]
  in
  let line = Y.to_string (`Assoc fields) in
  Mutex.protect stdout_mutex (fun () -> Printf.fprintf stdout "%s\n%!" line)


(* Extract deduplication key for taint entries (sources/sinks/sanitizers) *)
let taint_entry_key (rule_name, loc : string * Taint_location.taint_location) =
  Printf.sprintf "%s:%s" rule_name (Taint_location.loc_string loc)

(* Extract deduplication key for propagators *)
let propagator_key (rule_name, loc, locFrom, locTo : string * Taint_location.taint_location * Taint_location.taint_location * Taint_location.taint_location) =
  Printf.sprintf "%s:%s:%d:%d:%d:%d:%d:%d:%d:%d:%d:%d"
    rule_name
    loc.file_path loc.line loc.col loc.offsetStart loc.offsetEnd
    locFrom.line locFrom.col locFrom.offsetStart locFrom.offsetEnd
    locTo.line locTo.col

(* Per-domain regexp prefilter cache shared across all files processed by
 * that domain. This mirrors what Core_scan.scan_exn does: without it, every
 * rule in the rule set is evaluated against every file, which is the main
 * reason 'opengrep scan' is dramatically faster than a naive per-file
 * Match_rules.check call. The Hashtbl behind the DLS key memoizes the
 * compiled per-rule prefilter regex, keyed by Rule_ID. *)
let prefilter_cache_dls : Analyze_rule.prefilter_cache =
  Domain.DLS.new_key (fun () -> Hashtbl.create 1024)

let xconfig_with_prefilter_cache : Match_env.xconfig =
  {
    Match_env.config = Rule_options.default;
    equivs = [];
    nested_formula = false;
    matching_conf = Match_patterns.default_matching_conf;
    matching_explanations = false;
    filter_irrelevant_rules =
      Match_env.PrefilterWithCache prefilter_cache_dls;
  }

(* Pre-classified rule set for a single target analyzer.
 *
 * - [search_rules] are all rules whose [target_analyzer] is compatible with
 *   the file's analyzer, deduplicated by Rule_ID. This is what
 *   [Match_rules.check] (search-engine) wants.
 * - [taint_rules] is the subset of [search_rules] that have a [`Taint] mode,
 *   also deduplicated by Rule_ID. Kept as [Rule.t list] (rather than
 *   [Rule.taint_rule list]) so it can flow through [summarize_prefilter_rules]
 *   without a round-trip cast; the refinement to [Rule.taint_rule] happens
 *   right before [collect_taint_entries] is called.
 *
 * Computing this once per (analyzer, rules) instead of three times per file
 * (the previous shape) turns O(files * rules) work into O(distinct_langs *
 * rules + files), which matters for large rulesets and many files. *)
type analyzer_rules = {
  search_rules : Rule.t list;
  taint_rules : Rule.t list;
}

let classify_rules_for_analyzer ~(analyzer : Xlang.t) (rules : Rule.t list) :
    analyzer_rules =
  let seen = Hashtbl.create 64 in
  let search_acc = ref [] in
  let taint_acc = ref [] in
  rules |> List.iter (fun (r : Rule.t) ->
    if Xlang.is_compatible ~require:analyzer ~provide:r.target_analyzer then begin
      let id = Rule_ID.to_string (fst r.Rule.id) in
      if not (Hashtbl.mem seen id) then begin
        Hashtbl.add seen id ();
        search_acc := r :: !search_acc;
        match r.Rule.mode with
        | `Taint _ -> taint_acc := r :: !taint_acc
        | _ -> ()
      end
    end);
  { search_rules = List.rev !search_acc;
    taint_rules = List.rev !taint_acc }

let classify_rule_for_ast_prefilter ~(content : string) (rule : Rule.t) :
    [ `Pass | `Reject | `Unknown ] =
  let formulas = Rule.formulas_of_mode rule.Rule.mode in
  let rec loop saw_extractable = function
    | [] -> if saw_extractable then `Reject else `Unknown
    | formula :: rest -> (
        match
          Analyze_rule.regexp_prefilter_of_formula ~xlang:rule.target_analyzer
            formula
        with
        | None -> `Unknown
        | Some (_prefilter_formula, prefilter) ->
            if prefilter content then `Pass else loop true rest)
  in
  loop false formulas

let summarize_prefilter_rules ~(content : string) (rules : Rule.t list) : Rule.t list =
  List.fold_left
    (fun keep_rules (rule : Rule.t) ->
      match classify_rule_for_ast_prefilter ~content rule with
      | `Pass
      | `Unknown ->
          rule :: keep_rules
      | `Reject -> keep_rules)
    [] rules
  |> List.rev

let xtarget_for_ast (infile : Fpath.t) (analyzer : Xlang.t)
    (lazy_ast_and_errors :
      (AST_generic.program * Tok.location list) Lazy.t) : Xtarget.t =
  {
    Xtarget.path = { origin = Origin.File infile; internal_path_to_content = infile };
    xlang = analyzer;
    lazy_content = lazy (UFile.read_file infile);
    lazy_ast_and_errors;
  }

let collect_taint_entries
    ~(infile_s : string)
    ~(ast : AST_generic.program) (taint_rules : Rule.taint_rule list) :
    Taint_serializer.taint_entries_t =
  if taint_rules = [] then empty_taint_entries
  else
    let formula_cache = Formula_cache.mk_specialized_formula_cache taint_rules in
    let taint_configs_and_matches =
      List.filter_map
        (fun (rule : Rule.taint_rule) ->
          let spec_matches, _expls =
            Match_taint_spec.spec_matches_of_taint_rule
              ~per_file_formula_cache:formula_cache xconfig_with_prefilter_cache
              infile_s (ast, []) rule
          in
          match spec_matches with
          | { Match_taint_spec.sources = []; sinks = [];
              sanitizers = []; propagators = [] } ->
              None
          | _ ->
              Some (fst rule.Rule.id, spec_matches))
        taint_rules
    in
    let make_taint_entry rule_id rwm =
      let range = rwm.Range_with_metavars.r in
      let tok1, _tok2 = rwm.Range_with_metavars.origin.Core_match.range_loc in
      let rule_name = Rule_ID.to_string rule_id in
      let loc = Taint_location.mk_loc_from_tok ~file_path:infile_s tok1 range in
      (rule_name, loc)
    in
    (* Shared shape for sources/sinks/sanitizers: per-rule list of (rwm, _),
     * mapped to a [(rule_name, loc)] entry and deduplicated. *)
    let collect_simple proj =
      taint_configs_and_matches
      |> List.concat_map (fun (rule_id, spec_matches) ->
             proj spec_matches
             |> List.map (fun (rwm, _spec) -> make_taint_entry rule_id rwm))
      |> List_.deduplicate_gen ~get_key:taint_entry_key
    in
    let taint_sources    = collect_simple (fun sm -> sm.Match_taint_spec.sources)    in
    let taint_sinks      = collect_simple (fun sm -> sm.Match_taint_spec.sinks)      in
    let taint_sanitizers = collect_simple (fun sm -> sm.Match_taint_spec.sanitizers) in
    let taint_propagators =
      taint_configs_and_matches
      |> List.concat_map (fun (rule_id, spec_matches) ->
             spec_matches.Match_taint_spec.propagators
             |> List.map (fun (prop_match : Match_taint_spec.propagator_match) ->
                    let rule_name, loc =
                      make_taint_entry rule_id prop_match.rwm
                    in
                    let locFrom =
                      Taint_location.mk_loc_from_range ~file_path:infile_s prop_match.from
                    in
                    let locTo =
                      Taint_location.mk_loc_from_range ~file_path:infile_s prop_match.to_
                    in
                    (rule_name, loc, locFrom, locTo)))
      |> List_.deduplicate_gen ~get_key:propagator_key
    in
    (taint_sources, taint_sinks, taint_sanitizers, taint_propagators)

(* Run the search/taint rule engine on [xtarget] for the precomputed
 * [search_rules] (already filtered for analyzer compatibility and deduplicated
 * by [classify_rules_for_analyzer]). Returns the matches and errors so callers
 * can convert them into diagnostics.
 *
 * We deliberately do *not* call [summarize_prefilter_rules] on these rules:
 * [Match_rules.check] already does per-rule regex prefiltering with a
 * shared cache (see [xconfig_with_prefilter_cache]). The engine's filter
 * is strictly more powerful than [summarize_prefilter_rules] (it combines
 * all subformulas of a rule into a single prefilter formula and caches
 * the compiled regex once per rule across the whole run), so doing both
 * just doubles the work. *)
let run_rules_engine_for_diagnostics (xtarget : Xtarget.t)
    (search_rules : Rule.t list) : Core_match.t list * Core_error.t list =
  if search_rules = [] then ([], [])
  else
    try
      let res =
        Match_rules.check
          ~match_hook:(fun _ -> ())
          ~timeout:None
          xconfig_with_prefilter_cache
          search_rules
          xtarget
      in
      (res.matches, Core_error.ErrorSet.elements res.errors)
    with
    | Match_rules.File_timeout rule_ids ->
        UCommon.pr2
          (Printf.sprintf
             "[ir-pipeline]   WARNING: file timeout while computing \
              diagnostics, rules: %s"
             (rule_ids
              |> List_.map Rule_ID.to_string
              |> String.concat ","));
        ([], [])

(* When [~with_search_diagnostics:true], the search-engine ([Match_rules.check])
 * is also run on the file, so [parsed_file.matches] and [parsed_file.errors]
 * are populated; otherwise both are empty and only the taint engine runs.
 *
 * [ar] is the precomputed analyzer-specific rule set produced by
 * [classify_rules_for_analyzer]. The caller is responsible for passing rules
 * matched to the file's language; passing rules for the wrong analyzer will
 * silently produce no taint entries (the engine's prefilter will reject
 * everything) but is otherwise safe. *)
let parse_file ?(with_search_diagnostics = false)
    (infile : Fpath.t) (ar : analyzer_rules) : Taint_scan_config.parsed_file =
  Parsing_init.init ();
  let lang = Lang.lang_of_filename_exn infile in
  let parse_result = Parse_target.just_parse_with_lang lang infile in
  let ast = parse_result.ast in
  let analyzer = Xlang.of_lang lang in
  Naming_AST.resolve lang ast;
  Implicit_return.mark_implicit_return lang ast;
  let xtarget =
    xtarget_for_ast infile analyzer (lazy (ast, parse_result.skipped_tokens))
  in
  let mk_parsed ?(taint_entries = empty_taint_entries) ?(matches = [])
      ?(errors = []) () : Taint_scan_config.parsed_file =
    { ast; lang; xlang = analyzer; file = infile; taint_entries; matches;
      errors }
  in
  let matches, errors =
    if with_search_diagnostics then
      run_rules_engine_for_diagnostics xtarget ar.search_rules
    else ([], [])
  in
  match ar.taint_rules with
  | [] -> mk_parsed ~matches ~errors ()
  | taint_rules ->
      let taint_rules =
        summarize_prefilter_rules
          ~content:(Lazy.force xtarget.Xtarget.lazy_content)
          taint_rules
      in
      (* Refine [Rule.t] back to [Rule.taint_rule] for the engine API.
       * Safe: [classify_rules_for_analyzer] guarantees every entry has a
       * [`Taint] mode, so [filter_map] never drops anything here. *)
      let taint_rules =
        taint_rules
        |> List.filter_map (fun r ->
               match r.Rule.mode with
               | `Taint _ as mode -> Some { r with mode }
               | _ -> None)
      in
      let taint_entries =
        collect_taint_entries
          ~infile_s:(Fpath.to_string infile) ~ast taint_rules
      in
      mk_parsed ~taint_entries ~matches ~errors ()

(* [conf.on_parsed parsed] is invoked for every successfully parsed file
 * (one per file that passes the language and size filters). It may be
 * called concurrently from multiple worker domains when [num_domains > 1],
 * so the callback must be thread-safe. *)
let parse_files_ast (caps : < Cap.fork >) (conf : Taint_scan_config.t) : unit =
  let n_files = List.length conf.files in
  UCommon.pr2 (Printf.sprintf "[ir-pipeline] Processing %d files" n_files);

  let glob_start_time = Unix.gettimeofday () in

  let applicable_files_sorted =
    conf.files
    |> List.filter (fun file_path ->
           match Lang.langs_of_filename file_path with
           | [] -> false  (* Not supported language *)
           | _ :: _ -> true)
    |> List.filter (fun path -> (Unix.stat @@ Fpath.to_string path).Unix.st_size < skip_taint_large_file_bytes)
    |> List_.sort_by_key UFile.filesize (Fun.flip Int.compare)
  in

  (* Precompute the analyzer-specific rule classification once per distinct
   * language seen in the batch. Without this, every file re-walks the full
   * [conf.rules] list to compute its own search/taint subsets, which on
   * large rulesets dominates the per-file overhead. *)
  let rules_by_lang : (Lang.t, analyzer_rules) Hashtbl.t = Hashtbl.create 16 in
  applicable_files_sorted
  |> List.iter (fun f ->
         let lang = Lang.lang_of_filename_exn f in
         if not (Hashtbl.mem rules_by_lang lang) then
           Hashtbl.add rules_by_lang lang
             (classify_rules_for_analyzer
                ~analyzer:(Xlang.of_lang lang) conf.rules));

  (* Count outcomes via atomics rather than the parmap result list, so the
   * per-file [parse_file] results (which previously survived in [results]
   * until the whole batch finished) can be released as soon as the
   * [on_parsed] callback returns. *)
  let success_count = Atomic.make 0 in
  let error_count = Atomic.make 0 in

  let process_file (file : Fpath.t) =
    let lang = Lang.lang_of_filename_exn file in
    let ar = Hashtbl.find rules_by_lang lang in
    conf.on_parsed (parse_file ~with_search_diagnostics:true file ar);
    Atomic.incr success_count
  in

  let exception_handler (file : Fpath.t) (e : Exception.t) =
    Atomic.incr error_count;
    UCommon.pr2 (Printf.sprintf "[ir-pipeline]   ERROR: %s - %s" (Fpath.to_string file) (Exception.to_string e))
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

  UCommon.pr2 (Printf.sprintf "[ir-pipeline] Successfully processed %d/%d files (%d errors)"
    success_count n_files error_count);
  let glob_end_time = Unix.gettimeofday () in
  let glob_elapsed_ms = (glob_end_time -. glob_start_time) *. 1000.0 in
  let avg_ms_str =
    if n_files = 0 then "n/a"
    else Printf.sprintf "%.2f ms" (glob_elapsed_ms /. float_of_int n_files)
  in
  UCommon.pr2 (Printf.sprintf "[ir-pipeline]   Total time - %.2f ms; average time - %s"
    glob_elapsed_ms avg_ms_str)

(* Counter for periodic GC compaction *)
let parse_counter = Atomic.make 0

(* [caps] and [~num_domains] are accepted for API stability with the LSP
 * caller but are no longer used internally (kept here to avoid touching
 * Scan_helpers.ml in this pass). *)
let parse_and_serialize_file (_caps : < Cap.fork >) ~(num_domains : int)
    ?(format = `Json) (infile : Fpath.t) (rules: Rule.t list) : string =
  let _ = num_domains in
  (* Single-file path: no batch to amortize across, so classify on every call.
   * [Lang.lang_of_filename_exn] is also called inside [parse_file]; the cost
   * is sub-microsecond, not worth the API churn to deduplicate. *)
  let lang = Lang.lang_of_filename_exn infile in
  let ar =
    classify_rules_for_analyzer ~analyzer:(Xlang.of_lang lang) rules
  in
  let parsed = parse_file infile ar
  in
  let result =
    match format with
    | `Json ->
        serialize_ast_with_taint_to_string parsed.ast parsed.taint_entries |> Y.pretty_to_string
    | `Binary ->
        serialize_ast_with_taint_to_binary_string parsed.ast parsed.taint_entries |> Y.pretty_to_string
  in
  (* Clean up per-file caches that would otherwise accumulate indefinitely
     in the LSP server. These caches store file contents and line/column
     converters keyed by file path. In CLI mode they are cleaned up via
     Globals.reset() or temp-file hooks, but in the LSP server neither
     mechanism fires for real (non-temp) file paths. *)
  Kcas_data.Hashtbl.remove Range.hmemo infile;
  Kcas_data.Hashtbl.remove Xpattern_matcher.hmemo infile;
  (* Ask the C allocator to return freed large blocks to the OS.
     Tree-sitter parsing allocates large C blocks that, once freed,
     remain as dirty MALLOC_LARGE (empty) pages on macOS.
     Calling this after each file keeps RSS from growing unboundedly. *)
  Memory_release.release ();
  let count = Atomic.fetch_and_add parse_counter 1 + 1 in
  if count mod 200 = 0 then Gc.compact ();
  result
