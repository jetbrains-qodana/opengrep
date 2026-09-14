(* Per-file analysis: parsing + naming + the search-engine and taint-engine
 * pipelines, returning a [Taint_scan_config.parsed_file].
  *)

(** Pre-classified rule set for a single target analyzer.
    - [search_rules] are all rules whose [target_analyzer] is compatible with
      the file's analyzer, deduplicated by [Rule_ID]. This is what
      [Match_rules.check] (search-engine) wants.
    - [taint_rules] is the [`Taint]-mode subset, refined to [Rule.taint_rule]
      so the taint engine API can be called directly without a round-trip
      cast. *)
type analyzer_rules = {
  search_rules : Rule.t list;
  taint_rules : Rule.taint_rule list;
}

(** Single-pass classification of [rules] for a given [analyzer]. Callers in
    a batch context typically memoize one [analyzer_rules] per distinct
    language seen in the batch and reuse it for every matching file. *)
val classify_rules_for_analyzer :
  analyzer:Xlang.t -> Rule.t list -> analyzer_rules

(** Parse [infile] and run the taint (and optionally the search) engine on
    it, using the precomputed [analyzer_rules].

    [~mode] (default [`Taint]) selects which engines run:
    - [`Taint]: only the taint engine runs. [parsed_file.matches] and
      [parsed_file.errors] are empty.
    - [`All]: the search engine ([Match_rules.check]) runs in addition to
      the taint engine, and populates [parsed_file.matches] and
      [parsed_file.errors] so the caller can render diagnostics from them.

    [~timeout] bounds each rule's run on this file and, past
    [~timeout_threshold] rules, abandons the file. [None] means unbounded.

    [~on_timing] is invoked exactly once per call with the engine's per-rule
    profiling for this file, for [opengrep taint --bench]. Under
    [~mode:`Taint] there is no [Match_rules.check] pass, so the harvest is
    empty. Rules dropped by the prefilter never run and are absent from it.

    Raises [Match_rules.File_timeout] when [~timeout_threshold] rules have
    timed out on this file, from either pass, and the file is abandoned - it
    is the caller's job to decide what that means. [~on_timing] is called
    with whatever had been measured before the abort, so a benchmark run
    still gets a row for the file.

    The caller is responsible for matching [analyzer_rules] to the file's
    language; passing rules for a wrong analyzer will silently produce no
    taint entries (the engine's prefilter will reject everything) *)
val parse_file :
  < Cap.time_limit > ->
  ?mode:Taint_scan_config.mode ->
  ?timeout:float option ->
  ?timeout_threshold:int option ->
  ?on_timing:(Taint_timing.file_timing -> unit) ->
  Fpath.t -> analyzer_rules -> Taint_scan_config.parsed_file
