(* Per-rule cost report for the taint subcommand's benchmark mode
 * ([opengrep taint --logs <csv>]).
 *
 * A rule can cost time in three distinct places, and a report showing only
 * one of them is actively misleading - a rule can look free while burning
 * seconds:
 *
 *   - screening: deciding whether to run the rule at all. Two independent
 *     prefilters do this ([Match_rules.group_rules]'s per-rule prefilter and
 *     [Taint_engine.prefilter_taint_rules]'s union prefilter). Charged even
 *     when the rule is rejected, because rejection is what it cost.
 *   - matching: [Match_rules.check] running the rule for diagnostics. This
 *     is the number [opengrep scan --time] reports.
 *   - spec matching: [Match_taint_spec.spec_matches_of_taint_rule] finding
 *     sources/sinks/sanitizers/propagators for the taint payload. Taint
 *     rules only, and gated by a *looser* prefilter than the matching pass,
 *     so a rule rejected for matching can still pay this in full. *)

type t

(** One file's worth of measurements. *)
type file_timing = {
  candidates : string list;
      (* Every rule handed to [Match_rules.check] for this file, already
       * filtered for analyzer compatibility and deduplicated. *)
  prefilter_times : (string * float) list;
      (* (rule_id, ms) screening cost, from either prefilter. *)
  match_times : (string * float) list;
      (* (rule_id, ms) for rules that ran to completion under
       * [Match_rules.check]. *)
  spec_times : (string * float) list;
      (* (rule_id, ms) taint spec matching, for the taint payload. *)
  timed_out : string list;
      (* Rule ids that hit [--timeout]. The engine reports these with a
       * synthetic 0.0 match time, so they are tracked separately or the
       * slowest rules would rank as the cheapest. *)
  truncated : bool;
      (* [--timeout-threshold] abandoned this file, so the engine discarded
       * the match times of rules that had already finished on it. *)
}

(** Build an aggregate over [rules], recording each rule's mode.

    Side effect: sets [Core_profiling.profiling], without which
    [Core_profiling.profiling_opt] returns [None] and the engine reports no
    per-rule match times. That ref is documented as "should be set exactly
    once after the CLI arguments are read" and it increases memory use, so
    only construct a sink when benchmarking. *)
val make_sink : Rule.t list -> t

(** Fold one file's measurements into the aggregate. Safe to call
    concurrently from worker domains. *)
val record_file : t -> file_s:string -> file_timing -> unit

(** Number of files abandoned by [--timeout-threshold]. Non-zero means
    [match_ms] undercounts. *)
val truncated_files : t -> int

(** Write the report to [out_csv]. Every rule that was a candidate on at
    least one file gets a row, including rules both prefilters always
    rejected - those still show their screening cost.

    Timed-out rules sort first, then descending [total_cost_ms]
    ([prefilter_ms] + [match_ms] + [spec_ms]). [max_cost_ms] and
    [worst_file] track the worst single file by that same total, and
    [mean_cost_ms] is the total amortised over every file the rule was
    offered - the fairest way to compare rules with very different
    [files_candidate]. *)
val write_csv : t -> out_csv:Fpath.t -> unit
