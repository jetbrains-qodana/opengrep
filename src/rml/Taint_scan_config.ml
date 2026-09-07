type ast_format = [ `Json | `Binary ]
type mode = [ `Taint | `All ]

type parsed_file = {
  ast : AST_generic.program;
  lang : Lang.t;
  xlang : Xlang.t;
  file : Fpath.t;
  taint_entries : Taint_serializer.taint_entries_t;
  (* Search-engine matches. Empty unless [parse_file] was called with
   * [~mode:`All] (or the field was populated by some other code path). *)
  matches : Core_match.t list;
  (* Errors raised by the search engine. Empty unless [parse_file] was called
   * with [~mode:`All]. *)
  errors : Core_error.t list;
}

type t = {
  num_domains : int;
  files : Fpath.t list;
  rules : Rule.t list;
  mode : mode;
  timeout : float option;
  timeout_threshold : int option;
  on_parsed : parsed_file -> unit;
  (* Benchmark mode ([opengrep taint --logs]). When set, the batch folds the
   * engine's per-rule profiling for every processed file into this
   * aggregate. Only meaningful with [mode = `All] (the profiling comes from
   * the [Match_rules.check] pass) and [num_domains = 1]. *)
  timing_sink : Taint_timing.t option;
}
[@@deriving show]