(* Per-rule timing aggregate for the taint subcommand's benchmark mode
 * ([opengrep taint --logs <csv>]).
 *
 * The numbers come from the engine itself, not from a wrapper clock: every
 * mode dispatched by [Match_rules.check] (taint, search, extract) records a
 * [Core_profiling.rule_profiling], and [Taint_engine] hands the per-file
 * harvest here. These are the same [rule_match_time] values that
 * [opengrep scan --time] reports. *)

type t

(** One file's worth of harvested engine profiling. *)
type file_timing = {
  candidates : string list;
      (* Every rule handed to [Match_rules.check] for this file, i.e. already
       * filtered for analyzer compatibility and deduplicated. A candidate
       * that produced neither a timing nor a timeout was dropped by the
       * engine's regexp prefilter. *)
  rule_times : (string * float) list;
      (* (rule_id, milliseconds) for every rule that ran to completion. *)
  timed_out : string list;
      (* Rule ids that hit [--timeout]. These report a [rule_match_time] of
       * 0.0 in the engine's profiling, so they must be tracked separately
       * or the slowest rules would look like the cheapest. *)
  truncated : bool;
      (* [--timeout-threshold] aborted this file mid-way, so the timings of
       * the rules that had already finished on it were discarded by the
       * engine and are missing from [rule_times]. *)
}

(** Build an aggregate over [rules], recording each rule's mode for the
    report. Non-taint rules are included: search and extract rules are
    dispatched by [Match_rules.check] too and are equally worth triaging.

    Side effect: sets [Core_profiling.profiling], without which
    [Core_profiling.profiling_opt] returns [None] and the engine reports no
    per-rule times at all. That ref is documented as "should be set exactly
    once after the CLI arguments are read" and it increases memory use, so
    only construct a sink when benchmarking. *)
val make_sink : Rule.t list -> t

(** Fold one file's harvest into the aggregate. Safe to call concurrently
    from worker domains. *)
val record_file : t -> file_s:string -> file_timing -> unit

(** Number of files aborted by [--timeout-threshold]. Non-zero means the
    [total_ms] column undercounts. *)
val truncated_files : t -> int

(** Write the per-rule report to [out_csv]. Every rule that was a candidate
    on at least one file gets a row, including rules the prefilter always
    rejected - those show [files_run = 0] and are the point of the
    [files_candidate] and [not_run] columns.

    Rules that timed out come first,
    then descending [total_ms]. The timing columns ([total_ms], [mean_ms],
    [max_ms], [files_run]) cover completed runs only: a timed-out rule is
    reported by the engine with a synthetic 0.0, so including it would make
    the most expensive rules look like the cheapest. Read the [timeouts]
    column alongside them. *)
val write_csv : t -> out_csv:Fpath.t -> unit
