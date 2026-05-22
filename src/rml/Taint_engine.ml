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

(* Pre-classified rule set for a single target analyzer. Computing this once
 * per (analyzer, rules) instead of three times per file (the previous
 * shape) turns O(files * rules) work into O(distinct_langs * rules + files),
 * which matters for large rulesets and many files. *)
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

let union_prefilter_of_taint_rule (r : Rule.taint_rule)
    : (string -> bool) option =
  let _rule_id, rule_tok = r.Rule.id in
  match Rule.formulas_of_mode (r.Rule.mode :> Rule.mode) with
  | [] -> None
  | formulas ->
      let f = Rule.f (Rule.Or (rule_tok, formulas)) in
      Analyze_rule.regexp_prefilter_of_formula
        ~xlang:r.Rule.target_analyzer f
      |> Option.map snd

(* Drop taint rules whose union regexp prefilter rejects [content]. Keeps
 * order.
 *
 * Builds a single [Or] over the rule's sources, sinks, sanitizers and
 * propagators (via [Rule.formulas_of_mode]) and feeds it to
 * [Analyze_rule.regexp_prefilter_of_formula]. This is intentionally looser
 * than [Analyze_rule.regexp_prefilter_of_rule] for taint rules, which
 * AND-combines sources and sinks and so would drop files containing only
 * one half of the flow. Union semantics keeps such files, which matters
 * for any future cross-file taint flow and is still sound for the current
 * intra-file engine: a file with no taint-related token cannot produce a
 * finding from this rule.
 *
 * A rule whose formulas yield no extractable prefilter ([None]) is
 * conservatively kept. The compiled per-rule predicate is memoised in
 * [taint_union_prefilter_cache_dls] so each rule's regex is compiled at
 * most once per worker domain. *)
let prefilter_taint_rules ~(content : string)
    (rules : Rule.taint_rule list) : Rule.taint_rule list =
  let cache = Domain.DLS.get taint_union_prefilter_cache_dls in
  rules
  |> List.filter (fun (r : Rule.taint_rule) ->
         let rule_id = fst r.Rule.id in
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
         | Some pred -> pred content)

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
let taint_entry_key (rule_name, loc : string * Taint_location.taint_location) =
  Printf.sprintf "%s:%s" rule_name (Taint_location.loc_string loc)

(* Dedup key for propagators. *)
let propagator_key (rule_name, loc, locFrom, locTo
    : string * Taint_location.taint_location
      * Taint_location.taint_location * Taint_location.taint_location) =
  Printf.sprintf "%s:%s:%d:%d:%d:%d:%d:%d:%d:%d:%d:%d"
    rule_name
    loc.file_path loc.line loc.col loc.offsetStart loc.offsetEnd
    locFrom.line locFrom.col locFrom.offsetStart locFrom.offsetEnd
    locTo.line locTo.col

let collect_taint_entries
    ~(infile_s : string)
    ~(ast : AST_generic.program) (taint_rules : Rule.taint_rule list) :
    Taint_serializer.taint_entries_t =
  if taint_rules = [] then Ast_payload.empty_taint_entries
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

(* Run the search-engine on [xtarget] for the precomputed [search_rules]
 * (already filtered for analyzer compatibility and deduplicated by
 * [classify_rules_for_analyzer]). Returns the matches and errors so callers
 * can convert them into diagnostics.
 *
 * We deliberately do *not* run a separate prefilter on these rules:
 * [Match_rules.check] already does per-rule regex prefiltering with a shared
 * cache (see [xconfig_with_prefilter_cache]). The taint path can't piggy-back
 * on it because it bypasses [Match_rules.check] and calls
 * [Match_taint_spec.spec_matches_of_taint_rule] directly; that's why
 * [prefilter_taint_rules] above exists as a separate step. *)
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
        Logs.warn ~src:Ir_pipeline_logs.src (fun m ->
          m
            "File timeout while computing diagnostics, rules: %s"
            (rule_ids |> List_.map Rule_ID.to_string |> String.concat ","));
        ([], [])

(* Per-file pipeline: parse + naming + (optional) search engine + (optional)
 * taint engine. See [parse_file]'s doc in the .mli for the public contract.
 *
 * Two short-circuit paths:
 *   - [mode = `Taint] skips [run_rules_engine_for_diagnostics] entirely.
 *   - [ar.taint_rules = []] skips the prefilter + taint engine entirely. *)
let parse_file ?(mode: Taint_scan_config.mode = `Taint)
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
  let matches, errors =
    match mode with
    | `All -> run_rules_engine_for_diagnostics xtarget ar.search_rules
    | `Taint -> ([], [])
  in
  match ar.taint_rules with
  | [] -> mk_parsed ~matches ~errors ()
  | taint_rules ->
      let taint_rules =
        prefilter_taint_rules
          ~content:(Lazy.force xtarget.Xtarget.lazy_content)
          taint_rules
      in
      let taint_entries =
        collect_taint_entries
          ~infile_s:(Fpath.to_string infile) ~ast taint_rules
      in
      mk_parsed ~taint_entries ~matches ~errors ()
