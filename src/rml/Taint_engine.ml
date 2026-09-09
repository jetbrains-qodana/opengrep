(* Per-domain regexp prefilter cache shared across all files processed by
 * that domain. This mirrors what Core_scan.scan_exn does: without it, every
 * rule in the rule set is evaluated against every file. The Hashtbl behind the DLS key memoizes the
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
    defer_metavariable_hooks = true;
  }

type analyzer_rules = {
  search_rules : Rule.t list;
  taint_rules : Rule.taint_rule list;
}

let classify_rules_for_analyzer ~(analyzer : Xlang.t) (rules : Rule.t list) :
    analyzer_rules =
  let seen = Hashtbl.create 64 in
  let search_acc = ref [] in
  let taint_acc : Rule.taint_rule list ref = ref [] in
  rules |> List.iter (fun (r : Rule.t) ->
    if Xlang.is_compatible ~require:analyzer ~provide:r.target_analyzer then begin
      let id = Rule_ID.to_string (fst r.Rule.id) in
      if not (Hashtbl.mem seen id) then begin
        Hashtbl.add seen id ();
        search_acc := r :: !search_acc;
        match r.Rule.mode with
        | `Taint _ as mode ->
            taint_acc := { r with mode } :: !taint_acc
        | _ -> ()
      end
    end);
  { search_rules = List.rev !search_acc;
    taint_rules = List.rev !taint_acc }

(* Per-domain cache for the union taint prefilter. Kept separate from
 * [prefilter_cache_dls] because the two compute different formulas for the
 * same Rule_ID (AND-combined sources∧sinks vs. OR over all taint formulas),
 * so they must not share keyspace. *)
let taint_union_prefilter_cache_dls
    : (Rule_ID.t, (string -> bool) option) Hashtbl.t Domain.DLS.key =
  Domain.DLS.new_key (fun () -> Hashtbl.create 256)

(* Drop files without taint entries from analysis, because such files 
 * has no effect on analysis result. *)
let union_prefilter_of_taint_rule (r : Rule.taint_rule)
    : (string -> bool) option =
  let rule_id, rule_tok = r.Rule.id in
  match Rule.formulas_of_mode (r.Rule.mode :> Rule.mode) with
  | [] -> None
  | formulas -> (
      let f = Rule.f (Rule.Or (rule_tok, formulas)) in
      try
        Analyze_rule.regexp_prefilter_of_formula
          ~xlang:r.Rule.target_analyzer f
        |> Option.map snd
      with
      | Analyze_rule.CNF_exploded ->
          Logs.warn ~src:Ir_pipeline_logs.src (fun m ->
            m "CNF size exploded on rule id %s" (Rule_ID.to_string rule_id));
          None
      | Stack_overflow ->
          Logs.warn ~src:Ir_pipeline_logs.src (fun m ->
            m "Stack overflow on rule id %s" (Rule_ID.to_string rule_id));
          None)

(* Drop taint rules whose union regexp prefilter rejects [content]. Keeps
 * order.
 *
 * Builds a single [Or] over the rule's sources, sinks, sanitizers and
 * propagators (via [Rule.formulas_of_mode]) and feeds it to
 * [Analyze_rule.regexp_prefilter_of_formula]. This is intentionally looser
 * than [Analyze_rule.regexp_prefilter_of_rule] for taint rules, which
 * AND-combines sources and sinks and so would drop files containing only
 * one half of the flow. Union semantics keeps such files, which matters
 * for any cross-file taint flow: a file with no taint-related token cannot 
 * produce a finding from this rule. *)
let prefilter_taint_rules ?on_prefilter ~(content : string)
    (rules : Rule.taint_rule list) : Rule.taint_rule list =
  let cache = Domain.DLS.get taint_union_prefilter_cache_dls in
  rules
  |> List.filter (fun (r : Rule.taint_rule) ->
         let rule_id = fst r.Rule.id in
         let decide () =
           let pred_opt =
             match Hashtbl.find_opt cache rule_id with
             | Some v -> v
             | None ->
                 let v = union_prefilter_of_taint_rule r in
                 Hashtbl.add cache rule_id v;
                 v
           in
           match pred_opt with
           | None -> true
           | Some pred -> pred content
         in
         (* Unmeasured unless someone asked for the measurement. *)
         match on_prefilter with
         | None -> decide ()
         | Some f ->
             let survived, seconds = Common.with_time decide in
             f rule_id seconds;
             survived)

let xtarget_for_ast (infile : Fpath.t) (analyzer : Xlang.t)
    (lazy_ast_and_errors :
      (AST_generic.program * Tok.location list) Lazy.t) : Xtarget.t =
  {
    Xtarget.path = { origin = Origin.File infile; internal_path_to_content = infile };
    xlang = analyzer;
    lazy_content = lazy (UFile.read_file infile);
    lazy_ast_and_errors;
  }

(* Dedup key for taint entries (sources/sinks/sanitizers). *)
let taint_metadata_key metavars hooks =
  match hooks with
  | [] -> ""
  | hooks ->
      let metavars_key =
        metavars
        |> List.sort (fun (left, _) (right, _) -> String.compare left right)
        |> List.map (fun (name, loc) ->
               name ^ "=" ^ Taint_location.loc_string loc)
        |> String.concat ","
      in
      let hooks_key =
        hooks |> List.map Rule.show_taint_stmt_hook_call |> String.concat ","
      in
      metavars_key ^ ":" ^ hooks_key

let taint_entry_key
    ({ rule; loc; metavars; hooks; _ } : Taint_serializer.taint_entry) =
  Printf.sprintf "%s:%s:%s" rule (Taint_location.loc_string loc)
    (taint_metadata_key metavars hooks)

(* Dedup key for propagators. *)
let propagator_key
    ({ rule; loc; locFrom; locTo; metavars; hooks; _ } :
      Taint_serializer.taint_propagator_entry)
    =
  Printf.sprintf "%s:%s:%d:%d:%d:%d:%d:%d:%d:%d:%d:%d:%s"
    rule
    loc.file_path loc.line loc.col loc.offsetStart loc.offsetEnd
    locFrom.line locFrom.col locFrom.offsetStart locFrom.offsetEnd
    locTo.line locTo.col (taint_metadata_key metavars hooks)

let string_of_taint_formula (formula : Rule.formula) =
  let rec go formula =
    match formula.Rule.f with
    | Rule.P p -> fst p.Xpattern.pstr
    | Rule.Anywhere (_, formula)
    | Rule.Inside (_, formula)
    | Rule.Not (_, formula) ->
        go formula
    | Rule.Or (_, formulas)
    | Rule.And (_, formulas) ->
        formulas |> List_.map go |> String.concat "\n"
  in
  let pattern = go formula in
  if pattern = "" then None else Some pattern

let matched_pattern_of_range (rwm : Range_with_metavars.t) =
  match rwm.Range_with_metavars.origin.Core_match.rule_id.pattern_string with
  | "" -> None
  | pattern -> Some pattern

let collect_taint_entries (caps : < Cap.time_limit >)
    ?on_spec_time ~(timeout : float option)
    ~(timeout_threshold : int option) ~(infile_s : string)
    ~(ast : AST_generic.program) (taint_rules : Rule.taint_rule list) :
    Taint_serializer.taint_entries_t =
  if taint_rules = [] then Ast_payload.empty_taint_entries
  else
    let formula_cache = Formula_cache.mk_specialized_formula_cache taint_rules in
    let timed_out = ref [] in
    (* This pass is gated by the *union* prefilter, which is looser than the
     * one guarding [Match_rules.check]. A rule rejected for matching can
     * still pay full cost here, so it needs its own measurement or the
     * report shows it as free. A rule killed by [~timeout] is charged what
     * it burned before being killed. *)
    let run_spec (rule : Rule.taint_rule) =
      let f () =
        Match_taint_spec.spec_matches_of_taint_rule
          ~per_file_formula_cache:formula_cache xconfig_with_prefilter_cache
          infile_s (ast, []) rule
      in
      let run () =
        match timeout with
        | None -> Some (f ())
        | Some t ->
            Time_limit.set_timeout caps
              ~name:"Taint_engine.collect_taint_entries" t f
      in
      match on_spec_time with
      | None -> run ()
      | Some g ->
          let res, seconds = Common.with_time run in
          g (fst rule.Rule.id) seconds;
          res
    in
    let taint_configs_and_matches =
      List.filter_map
        (fun (rule : Rule.taint_rule) ->
          match run_spec rule with
          | None ->
              let rule_id = fst rule.Rule.id in
              timed_out := rule_id :: !timed_out;
              Logs.warn ~src:Ir_pipeline_logs.src (fun m ->
                m "Timeout on taint rule %s in %s" (Rule_ID.to_string rule_id)
                  infile_s);
              (match timeout_threshold with
              | Some n when n > 0 && List.length !timed_out >= n ->
                  raise (Match_rules.File_timeout !timed_out)
              | _ -> ());
              None
          | Some (spec_matches, _expls) -> (
              match spec_matches with
              | { Match_taint_spec.sources = []; sinks = [];
                  sanitizers = []; propagators = [] } ->
                  None
              | _ ->
                  Some (fst rule.Rule.id, spec_matches)))
        taint_rules
    in
    let make_taint_entry rule_id fallback_pattern rwm =
      let range = rwm.Range_with_metavars.r in
      let tok1, _tok2 = rwm.Range_with_metavars.origin.Core_match.range_loc in
      let rule_name = Rule_ID.to_string rule_id in
      let loc = Taint_location.mk_loc_from_tok ~file_path:infile_s tok1 range in
      let pattern =
        match matched_pattern_of_range rwm with
        | Some _ as pattern -> pattern
        | None -> fallback_pattern
      in
      let metavars =
        rwm.Range_with_metavars.mvars
        |> List_.filter_map (fun (name, value) ->
               match Metavariable.range_of_mvalue value with
               | None -> None
               | Some (file, range) ->
                   let file_path = Fpath.to_string file in
                   Some
                     ( name,
                       Taint_location.mk_loc_from_range ~file_path range ))
      in
      let hooks = List.rev rwm.Range_with_metavars.hooks in
      { Taint_serializer.rule = rule_name; loc; pattern; metavars; hooks }
    in
    let collect_simple proj formula =
      taint_configs_and_matches
      |> List.concat_map (fun (rule_id, spec_matches) ->
             proj spec_matches
             |> List.map (fun (rwm, spec) ->
                    make_taint_entry rule_id (string_of_taint_formula @@ formula spec) rwm
                ))
      |> List_.deduplicate_gen ~get_key:taint_entry_key
    in
    let taint_sources    = collect_simple (fun sm -> sm.Match_taint_spec.sources) (fun (t) -> t.Rule.source_formula)    in
    let taint_sinks      = collect_simple (fun sm -> sm.Match_taint_spec.sinks) (fun (t) -> t.Rule.sink_formula)      in
    let taint_sanitizers = collect_simple (fun sm -> sm.Match_taint_spec.sanitizers) (fun (t) -> t.Rule.sanitizer_formula) in
    let taint_propagators =
      taint_configs_and_matches
      |> List.concat_map (fun (rule_id, spec_matches) ->
             spec_matches.Match_taint_spec.propagators
             |> List.map (fun (prop_match : Match_taint_spec.propagator_match) ->
                    let entry =
                      make_taint_entry rule_id
                        (string_of_taint_formula
                           prop_match.spec.Rule.propagator_formula)
                        prop_match.rwm
                    in
                    let locFrom =
                      Taint_location.mk_loc_from_range ~file_path:infile_s prop_match.from
                    in
                    let locTo =
                      Taint_location.mk_loc_from_range ~file_path:infile_s prop_match.to_
                    in
                    {
                      Taint_serializer.rule = entry.rule;
                      loc = entry.loc;
                      locFrom;
                      locTo;
                      pattern = entry.pattern;
                      metavars = entry.metavars;
                      hooks = entry.hooks;
                    }))
      |> List_.deduplicate_gen ~get_key:propagator_key
    in
    (taint_sources, taint_sinks, taint_sanitizers, taint_propagators)

let no_timing : Taint_timing.file_timing =
  { candidates = []; prefilter_times = []; match_times = []; spec_times = [];
    timed_out = []; truncated = false }

(* The rules [Match_rules.check] will actually consider for a file. It skips
 * [`SCA] rules for reasons unrelated to prefiltering and raises on
 * [`Steps], so excluding both here keeps the report's [not_run] column
 * attributable to the prefilter alone. *)
let benchmark_candidates (search_rules : Rule.t list) : string list =
  search_rules
  |> List.filter_map (fun (r : Rule.t) ->
         match r.Rule.mode with
         | `Taint _
         | `Search _
         | `Extract _ ->
             Some (Rule_ID.to_string (fst r.Rule.id))
         | `SCA _
         | `Steps _ ->
             None)

(* Rules that exceeded [--timeout] surface as [Timeout] errors carrying their
 * rule id. They also carry a [rule_match_time] of 0.0 (see
 * [Core_profiling.empty_rule_profiling]), so without pulling them out here a
 * rule that always times out would be reported as the cheapest one. *)
let timed_out_rules_of_errors (errors : Core_error.t list) : string list =
  errors
  |> List.filter_map (fun (e : Core_error.t) ->
         match (e.Core_error.typ, e.Core_error.rule_id) with
         | Semgrep_output_v1_t.Timeout, Some rule_id ->
             Some (Rule_ID.to_string rule_id)
         | _ -> None)

let harvest_match_times (res : Core_result.matches_single_file) :
    (string * float) list =
  match res.Core_result.profiling with
  | None -> []
  | Some p ->
      (* [rule_match_time] is in seconds; the report is in milliseconds. *)
      p.Core_profiling.p_rule_times
      |> List.map (fun (rp : Core_profiling.rule_profiling) ->
             ( Rule_ID.to_string rp.Core_profiling.rule_id,
               rp.Core_profiling.rule_match_time *. 1000.0 ))

(* Run the search-engine on [xtarget] for the precomputed [search_rules]
 * (already filtered for analyzer compatibility and deduplicated by
 * [classify_rules_for_analyzer]). Returns the matches and errors so callers
 * can convert them into diagnostics, plus the engine's per-rule timings for
 * the benchmark report. *)
let run_rules_engine_for_diagnostics (caps : < Cap.time_limit >)
    ?(collect_timing = false) ~(timeout : float option)
    ~(timeout_threshold : int option) (xtarget : Xtarget.t)
    (search_rules : Rule.t list) :
    Core_match.t list * Core_error.t list * Taint_timing.file_timing =
  if search_rules = [] then ([], [], no_timing)
  else
    let timeout_config =
      match timeout with
      | None -> None
      | Some t ->
          Some
            Match_rules.
              {
                timeout = t;
                allow_rule_timeout_control = false;
                dynamic_timeout = false;
                dynamic_timeout_max_multiplier = -1;
                dynamic_timeout_unit_kb = -1;
                threshold =
                  (match timeout_threshold with Some n -> n | None -> 0);
                caps;
              }
    in
    (* Prefiltering is decided inside [Match_rules.check] before any per-rule
     * match timing begins, so its cost lands in no [rule_profiling]. The
     * callback is the only place it can be attributed per rule. Left off
     * outside benchmark mode, which is also what keeps [check] from timing
     * anything. *)
    let prefilter_acc : (string * float) list ref = ref [] in
    let on_prefilter =
      if not collect_timing then None
      else
        Some
          (fun rule_id _survived (seconds : float) ->
            prefilter_acc :=
              (Rule_ID.to_string rule_id, seconds *. 1000.0) :: !prefilter_acc)
    in
    try
      let res =
        Match_rules.check
          ?on_prefilter
          ~match_hook:(fun _ -> ())
          ~timeout:timeout_config
          xconfig_with_prefilter_cache
          search_rules
          xtarget
      in
      let errors = Core_error.ErrorSet.elements res.errors in
      (* [benchmark_candidates] walks every rule for the file, so it is not
       * something to build and throw away on the normal path. *)
      let timing : Taint_timing.file_timing =
        if not collect_timing then no_timing
        else
          {
            candidates = benchmark_candidates search_rules;
            prefilter_times = !prefilter_acc;
            match_times = harvest_match_times res;
            spec_times = [];
            timed_out = timed_out_rules_of_errors errors;
            truncated = false;
          }
      in
      (res.matches, errors, timing)
    with
    | Match_rules.File_timeout rule_ids ->
        Logs.warn ~src:Ir_pipeline_logs.src (fun m ->
          m
            "File timeout while computing diagnostics, rules: %s"
            (rule_ids |> List_.map Rule_ID.to_string |> String.concat ","));
        (* [--timeout-threshold] aborted the file, so the engine threw away
         * the times of the rules that had already finished on it. All we can
         * still recover is which rules timed out. *)
        let timing : Taint_timing.file_timing =
          if not collect_timing then no_timing
          else
            {
              candidates = benchmark_candidates search_rules;
              (* Whatever screening was paid for before the abort still
               * happened, so keep it. *)
              prefilter_times = !prefilter_acc;
              match_times = [];
              spec_times = [];
              timed_out = rule_ids |> List_.map Rule_ID.to_string;
              truncated = true;
            }
        in
        ([], [], timing)

(* Per-file pipeline: parse + naming + (optional) search engine + (optional)
 * taint engine. See [parse_file]'s doc in the .mli for the public contract.
 *
 * Two short-circuit paths:
 *   - [mode = `Taint] skips [run_rules_engine_for_diagnostics] entirely.
 *   - [ar.taint_rules = []] skips the prefilter + taint engine entirely. *)
let parse_file (caps : < Cap.time_limit >)
    ?(mode : Taint_scan_config.mode = `Taint) ?(timeout : float option = Some 5.0)
    ?(timeout_threshold : int option = Some 3)
    ?(on_timing : (Taint_timing.file_timing -> unit) option)
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
  let mk_parsed ?(taint_entries = Ast_payload.empty_taint_entries)
      ?(matches = []) ?(errors = []) () : Taint_scan_config.parsed_file =
    { ast; lang; xlang = analyzer; file = infile; taint_entries; matches;
      errors }
  in
  let matches, errors, timing =
    match mode with
    | `All ->
        run_rules_engine_for_diagnostics caps
          ~collect_timing:(Option.is_some on_timing) ~timeout
          ~timeout_threshold xtarget ar.search_rules
    | `Taint -> ([], [], no_timing)
  in
  (* The union taint prefilter below is a second, independent screening pass
   * (see [prefilter_taint_rules]); it is charged to the same
   * [prefilter_times] bucket, so [on_timing] has to wait for it. *)
  let union_prefilter_acc : (string * float) list ref = ref [] in
  let on_union_prefilter =
    match on_timing with
    | None -> None
    | Some _ ->
        Some
          (fun rule_id (seconds : float) ->
            union_prefilter_acc :=
              (Rule_ID.to_string rule_id, seconds *. 1000.0)
              :: !union_prefilter_acc)
  in
  let spec_acc : (string * float) list ref = ref [] in
  let on_spec_time =
    match on_timing with
    | None -> None
    | Some _ ->
        Some
          (fun rule_id (seconds : float) ->
            spec_acc :=
              (Rule_ID.to_string rule_id, seconds *. 1000.0) :: !spec_acc)
  in
  (* [~truncated_by] carries the rule ids that made [--timeout-threshold]
   * abandon the file inside the taint pass, so that file still gets a row
   * instead of silently vanishing from the report. *)
  let report_timing ?(truncated_by = []) () =
    match on_timing with
    | None -> ()
    | Some f ->
        f
          {
            timing with
            Taint_timing.prefilter_times =
              !union_prefilter_acc @ timing.Taint_timing.prefilter_times;
            spec_times = !spec_acc;
            timed_out = truncated_by @ timing.Taint_timing.timed_out;
            truncated = truncated_by <> [] || timing.Taint_timing.truncated;
          }
  in
  match ar.taint_rules with
  | [] ->
      report_timing ();
      mk_parsed ~matches ~errors ()
  | taint_rules ->
      let taint_rules =
        prefilter_taint_rules ?on_prefilter:on_union_prefilter
          ~content:(Lazy.force xtarget.Xtarget.lazy_content)
          taint_rules
      in
      let taint_entries =
        try
          collect_taint_entries caps ?on_spec_time ~timeout ~timeout_threshold
            ~infile_s:(Fpath.to_string infile) ~ast taint_rules
        with
        | Match_rules.File_timeout rule_ids ->
            report_timing
              ~truncated_by:(rule_ids |> List_.map Rule_ID.to_string) ();
            raise (Match_rules.File_timeout rule_ids)
      in
      report_timing ();
      mk_parsed ~taint_entries ~matches ~errors ()
