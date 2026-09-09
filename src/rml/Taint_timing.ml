type file_timing = {
  candidates : string list;
  rule_times : (string * float) list;
  timed_out : string list;
  truncated : bool;
}

(* Running totals for one rule across the whole batch. *)
type rule_acc = {
  (* Files on which the engine was asked to consider this rule at all. The
   * gap between this and [files_run] + [timeouts] is what the prefilter
   * rejected. *)
  mutable files_candidate : int;
  mutable files_run : int;
  mutable total_ms : float;
  mutable max_ms : float;
  mutable worst_file : string;
  mutable timeouts : int;
}

type t = {
  (* rule_id -> mode label, for the [mode] column. Rules that never run
   * still resolve here, so the column is never blank for a known rule. *)
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
   * [rule_profiling] and the report comes out empty. *)
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
        { files_candidate = 0; files_run = 0; total_ms = 0.0; max_ms = 0.0;
          worst_file = ""; timeouts = 0 }
      in
      Hashtbl.add sink.accs rule_id a;
      a

let record_file (sink : t) ~(file_s : string) (ft : file_timing) : unit =
  (* A rule that timed out is still reported in the engine's per-rule times,
   * but with a synthetic 0.0 from [Core_profiling.empty_rule_profiling].
   * Counting that as a measurement would drag [mean_ms] down and make the
   * worst rules look free, so timing columns cover completed runs only and
   * timeouts are counted on their own. Keyed off [timed_out] rather than
   * off [ms = 0.0], since a genuinely fast rule can measure 0.0 too. *)
  let timed_out = Hashtbl.create (List.length ft.timed_out) in
  ft.timed_out |> List.iter (fun id -> Hashtbl.replace timed_out id ());
  Mutex.protect sink.mutex (fun () ->
      if ft.truncated then sink.truncated <- sink.truncated + 1;
      (* Touch every candidate so a rule the prefilter always rejects still
       * gets a row, rather than vanishing from the report entirely. *)
      ft.candidates
      |> List.iter (fun rule_id ->
             let a = acc_for sink rule_id in
             a.files_candidate <- a.files_candidate + 1);
      ft.rule_times
      |> List.iter (fun (rule_id, ms) ->
             if not (Hashtbl.mem timed_out rule_id) then begin
               let a = acc_for sink rule_id in
               a.files_run <- a.files_run + 1;
               a.total_ms <- a.total_ms +. ms;
               if ms > a.max_ms then begin
                 a.max_ms <- ms;
                 a.worst_file <- file_s
               end
             end);
      ft.timed_out
      |> List.iter (fun rule_id ->
             let a = acc_for sink rule_id in
             a.timeouts <- a.timeouts + 1))

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

let write_csv (sink : t) ~(out_csv : Fpath.t) : unit =
  let rows =
    Mutex.protect sink.mutex (fun () ->
        Hashtbl.fold (fun rule_id a rows -> (rule_id, a) :: rows) sink.accs [])
  in
  (* Worst first: the point of the report is to read the top few lines and
   * stop. Rules that timed out sort above everything else regardless of
   * their measured time - a rule that always blows the limit has a
   * [total_ms] near zero precisely because it never finished, and it is the
   * single most important thing in the report. *)
  let rows =
    List.sort
      (fun (_, a) (_, b) ->
        match Int.compare b.timeouts a.timeouts with
        | 0 -> Float.compare b.total_ms a.total_ms
        | c -> c)
      rows
  in
  let buf = Buffer.create 4096 in
  Buffer.add_string buf
    ("rule_id,mode,files_candidate,files_run,not_run,total_ms,mean_ms,"
   ^ "max_ms,worst_file,timeouts\n");
  rows
  |> List.iter (fun (rule_id, a) ->
         let mode =
           match Hashtbl.find_opt sink.modes rule_id with
           | Some m -> m
           | None -> ""
         in
         let mean_ms =
           if a.files_run = 0 then 0.0
           else a.total_ms /. float_of_int a.files_run
         in
         (* A candidate that neither finished nor timed out never made it
          * past the engine's regexp prefilter. *)
         let not_run = a.files_candidate - a.files_run - a.timeouts in
         Buffer.add_string buf
           (Printf.sprintf "%s,%s,%d,%d,%d,%.3f,%.3f,%.3f,%s,%d\n"
              (csv_escape rule_id) (csv_escape mode) a.files_candidate
              a.files_run not_run a.total_ms mean_ms a.max_ms
              (csv_escape a.worst_file)
              a.timeouts));
  UFile.write_file ~file:out_csv (Buffer.contents buf)
