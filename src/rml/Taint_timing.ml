type file_timing = {
  candidates : string list;
  prefilter_times : (string * float) list;
  prefilter_rejected : string list;
  match_times : (string * float) list;
  spec_times : (string * float) list;
  match_timed_out : string list;
  spec_timed_out : string list;
  truncated : bool;
}

(* Running totals for one rule across the whole batch. *)
type rule_acc = {
  (* Files on which the engine was asked to consider this rule at all. The
   * gap between this and [files_matched] + [timeouts] is what the matching
   * pass's prefilter rejected. *)
  mutable files_candidate : int;
  mutable files_matched : int;
  mutable files_spec : int;
  mutable prefilter_ms : float;
  mutable match_ms : float;
  mutable spec_ms : float;
  (* Worst single file by combined cost, and which file that was. *)
  mutable max_cost_ms : float;
  mutable worst_file : string;
  (* Files where either pass killed the rule; this is the reported count. *)
  mutable timeouts : int;
  (* Files where the matching pass's prefilter screened the rule out. Counted
   * from the engine's own decision rather than inferred from the absence of
   * a match time: on a file [--timeout-threshold] abandoned, rules that had
   * already finished lose their times too, and rules past the abort were
   * never reached at all. Neither was screened out. *)
  mutable files_not_run : int;
}

type t = {
  (* rule_id -> mode label, so the column is populated even for a rule that
   * never ran anywhere. *)
  modes : (string, string) Hashtbl.t;
  accs : (string, rule_acc) Hashtbl.t;
  mutable truncated : int;
  (* [record_file] may be called from parmap worker domains. Benchmark mode
   * pins the batch to one domain, but the lock keeps the aggregate correct
   * if a caller ever wires it into a parallel run. *)
  mutex : Mutex.t;
}

let mode_label (r : Rule.t) : string =
  match r.Rule.mode with
  | `Taint _ -> "taint"
  | `Search _ -> "search"
  | `Extract _ -> "extract"
  | `SCA _ -> "sca"
  | `Steps _ -> "steps"

let make_sink (rules : Rule.t list) : t =
  (* Without this the engine's [profiling_opt] discards every
   * [rule_profiling] and the match times come out empty. *)
  Core_profiling.profiling := true;
  let modes = Hashtbl.create 256 in
  rules
  |> List.iter (fun (r : Rule.t) ->
         let id = Rule_ID.to_string (fst r.Rule.id) in
         if not (Hashtbl.mem modes id) then
           Hashtbl.add modes id (mode_label r));
  { modes; accs = Hashtbl.create 256; truncated = 0; mutex = Mutex.create () }

let acc_for (sink : t) (rule_id : string) : rule_acc =
  match Hashtbl.find_opt sink.accs rule_id with
  | Some a -> a
  | None ->
      let a =
        { files_candidate = 0; files_matched = 0; files_spec = 0;
          prefilter_ms = 0.0; match_ms = 0.0; spec_ms = 0.0;
          max_cost_ms = 0.0; worst_file = ""; timeouts = 0;
          files_not_run = 0 }
      in
      Hashtbl.add sink.accs rule_id a;
      a

(* Sum duplicate entries for the same rule id. Screening can be charged more
 * than once per file, since the matching pass and the taint payload pass run
 * separate prefilters. *)
let tally (entries : (string * float) list) : (string, float) Hashtbl.t =
  let t = Hashtbl.create (List.length entries + 1) in
  entries
  |> List.iter (fun (id, ms) ->
         let prev = Option.value ~default:0.0 (Hashtbl.find_opt t id) in
         Hashtbl.replace t id (prev +. ms));
  t

let set_of (ids : string list) : (string, unit) Hashtbl.t =
  let t = Hashtbl.create (List.length ids + 1) in
  ids |> List.iter (fun id -> Hashtbl.replace t id ());
  t

let record_file (sink : t) ~(file_s : string) (ft : file_timing) : unit =
  let prefilter = tally ft.prefilter_times in
  let spec = tally ft.spec_times in
  (* A rule the matching pass killed is still reported by the engine, but
   * with a synthetic 0.0 from [Core_profiling.empty_rule_profiling].
   * Counting that as a measurement would drag the averages down and rank the
   * worst rules as the cheapest, so the match columns cover completed runs
   * only. Keyed off [match_timed_out] rather than off [ms = 0.0], since a
   * genuinely fast rule can measure 0.0 too.
   *
   * Only the matching pass's timeouts discard anything. A rule the spec pass
   * killed may have completed the matching pass, and that measurement is
   * real; so is the spec time itself, which is what the rule burned before
   * being killed rather than a synthetic zero. *)
  let match_timed_out = set_of ft.match_timed_out in
  let matched =
    tally
      (ft.match_times
      |> List.filter (fun (id, _) -> not (Hashtbl.mem match_timed_out id)))
  in
  (* The reported [timeouts] column does not care which pass did the killing,
   * and a rule killed in both passes on one file is still one file. *)
  let timed_out = set_of (ft.match_timed_out @ ft.spec_timed_out) in
  let rejected = set_of ft.prefilter_rejected in
  let touched = set_of ft.candidates in
  Mutex.protect sink.mutex (fun () ->
      if ft.truncated then sink.truncated <- sink.truncated + 1;
      touched
      |> Hashtbl.iter (fun rule_id () ->
             let a = acc_for sink rule_id in
             let get tbl =
               Option.value ~default:0.0 (Hashtbl.find_opt tbl rule_id)
             in
             let pf = get prefilter and mt = get matched and sp = get spec in
             a.files_candidate <- a.files_candidate + 1;
             a.prefilter_ms <- a.prefilter_ms +. pf;
             a.match_ms <- a.match_ms +. mt;
             a.spec_ms <- a.spec_ms +. sp;
             if Hashtbl.mem matched rule_id then
               a.files_matched <- a.files_matched + 1;
             if Hashtbl.mem spec rule_id then
               a.files_spec <- a.files_spec + 1;
             if Hashtbl.mem timed_out rule_id then
               a.timeouts <- a.timeouts + 1;
             if Hashtbl.mem rejected rule_id then
               a.files_not_run <- a.files_not_run + 1;
             let cost = pf +. mt +. sp in
             if a.files_candidate = 1 || cost > a.max_cost_ms then begin
               a.max_cost_ms <- cost;
               a.worst_file <- file_s
             end))

let truncated_files (sink : t) : int =
  Mutex.protect sink.mutex (fun () -> sink.truncated)

let csv_escape (s : string) : string =
  let needs_quoting =
    String.contains s ',' || String.contains s '"' || String.contains s '\n'
    || String.contains s '\r'
  in
  if not needs_quoting then s
  else
    let escaped = String.concat "\"\"" (String.split_on_char '"' s) in
    "\"" ^ escaped ^ "\""

let total_cost (a : rule_acc) : float =
  a.prefilter_ms +. a.match_ms +. a.spec_ms

let write_csv (sink : t) ~(out_csv : Fpath.t) : unit =
  let rows =
    Mutex.protect sink.mutex (fun () ->
        Hashtbl.fold (fun rule_id a rows -> (rule_id, a) :: rows) sink.accs [])
  in
  (* Worst first: the point of the report is to read the top few lines and
   * stop. Rules that timed out sort above everything else - a rule that
   * always blows the limit has a near-zero match time precisely because it
   * never finished, and it is the most important thing in the report. *)
  let rows =
    List.sort
      (fun (_, a) (_, b) ->
        match Int.compare b.timeouts a.timeouts with
        | 0 -> Float.compare (total_cost b) (total_cost a)
        | c -> c)
      rows
  in
  let buf = Buffer.create 4096 in
  Buffer.add_string buf
    ("rule_id,mode,files_candidate,files_matched,files_spec,not_run,"
   ^ "total_cost_ms,prefilter_ms,match_ms,spec_ms,mean_cost_ms,max_cost_ms,"
   ^ "worst_file,timeouts\n");
  rows
  |> List.iter (fun (rule_id, a) ->
         let mode =
           match Hashtbl.find_opt sink.modes rule_id with
           | Some m -> m
           | None -> ""
         in
         (* [files_matched + timeouts + not_run] adds up to
          * [files_candidate] on any file the engine saw through to the end.
          * On a truncated one it falls short, and the gap - rules that
          * finished but lost their times, and rules the abort never reached
          * - is exactly what cannot be attributed. *)
         let not_run = a.files_not_run in
         let cost = total_cost a in
         let mean_cost =
           if a.files_candidate = 0 then 0.0
           else cost /. float_of_int a.files_candidate
         in
         Buffer.add_string buf
           (Printf.sprintf
              "%s,%s,%d,%d,%d,%d,%.3f,%.3f,%.3f,%.3f,%.3f,%.3f,%s,%d\n"
              (csv_escape rule_id) (csv_escape mode) a.files_candidate
              a.files_matched a.files_spec not_run cost a.prefilter_ms
              a.match_ms a.spec_ms mean_cost a.max_cost_ms
              (csv_escape a.worst_file)
              a.timeouts));
  UFile.write_file ~file:out_csv (Buffer.contents buf)
