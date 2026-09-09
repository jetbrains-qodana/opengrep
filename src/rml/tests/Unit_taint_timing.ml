(* Unit tests for [Taint_timing], the per-rule cost report behind
 * [opengrep taint --bench].
 *
 * Everything the report does after the engine hands it a [file_timing] is
 * pure arithmetic, so none of this needs an engine run - which is the point:
 * the parts most likely to be wrong (what counts as "not run", what a
 * timeout does to the averages) are exactly the parts that are awkward to
 * observe from an end-to-end benchmark.
 *)

let t = Testo.create

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)

(* [make_sink] sets [Core_profiling.profiling], which the rest of the test
 * suite shares, so put it back. *)
let with_sink (rules : Rule.t list) (f : Taint_timing.t -> 'a) : 'a =
  let saved = !Core_profiling.profiling in
  Fun.protect
    ~finally:(fun () -> Core_profiling.profiling := saved)
    (fun () -> f (Taint_timing.make_sink rules))

let file_timing ?(candidates = []) ?(prefilter = []) ?(rejected = [])
    ?(matched = []) ?(spec = []) ?(match_timed_out = [])
    ?(spec_timed_out = []) ?(truncated = false) () : Taint_timing.file_timing =
  {
    candidates;
    prefilter_times = prefilter;
    prefilter_rejected = rejected;
    match_times = matched;
    spec_times = spec;
    match_timed_out;
    spec_timed_out;
    truncated;
  }

(* Write the report and read it back as (header fields, rows in file order,
 * each row already split into fields). Quoting is only exercised by the
 * escaping test, which looks at the raw line instead, so a plain split on
 * ',' is enough here. *)
let report_of (sink : Taint_timing.t) : string list * string list list =
  let file = Filename.temp_file "unit_taint_timing" ".csv" in
  Fun.protect
    ~finally:(fun () -> Sys.remove file)
    (fun () ->
      Taint_timing.write_csv sink ~out_csv:(Fpath.v file);
      let lines =
        UFile.read_file (Fpath.v file)
        |> String.split_on_char '\n'
        |> List.filter (fun s -> s <> "")
      in
      match lines with
      | [] -> failwith "write_csv produced an empty file"
      | header :: rows ->
          ( String.split_on_char ',' header,
            rows |> List_.map (String.split_on_char ',') ))

(* The value of [column] in [row], looked up by name so the tests do not
 * have to be renumbered every time a column is added. *)
let field (header : string list) (row : string list) (column : string) : string
    =
  match List.assoc_opt column (List_.index_list_0 header) with
  | None -> failwith (Common.spf "no %s column in the report" column)
  | Some i -> (
      match List.nth_opt row i with
      | Some v -> v
      | None -> failwith (Common.spf "row is too short for %s" column))

let row_for (header : string list) (rows : string list list) (rule_id : string)
    : string list =
  match
    rows |> List.find_opt (fun row -> field header row "rule_id" = rule_id)
  with
  | Some row -> row
  | None -> failwith (Common.spf "no row for %s" rule_id)

let check_field header rows rule_id column expected =
  Alcotest.(check string)
    (Common.spf "%s.%s" rule_id column)
    expected
    (field header (row_for header rows rule_id) column)

(*****************************************************************************)
(* Tests *)
(*****************************************************************************)

(* The three cost centres add up, and the per-file maximum tracks their sum
 * rather than any one of them. *)
let test_aggregates_across_files () =
  with_sink [] (fun sink ->
      Taint_timing.record_file sink ~file_s:"a.py"
        (file_timing ~candidates:[ "r" ] ~prefilter:[ ("r", 1.0) ]
           ~matched:[ ("r", 2.0) ] ~spec:[ ("r", 3.0) ] ());
      Taint_timing.record_file sink ~file_s:"b.py"
        (file_timing ~candidates:[ "r" ] ~prefilter:[ ("r", 0.5) ]
           ~matched:[ ("r", 0.5) ] ());
      let header, rows = report_of sink in
      check_field header rows "r" "files_candidate" "2";
      check_field header rows "r" "files_matched" "2";
      check_field header rows "r" "files_spec" "1";
      check_field header rows "r" "prefilter_ms" "1.500";
      check_field header rows "r" "match_ms" "2.500";
      check_field header rows "r" "spec_ms" "3.000";
      check_field header rows "r" "total_cost_ms" "7.000";
      (* Amortised over the files the rule was offered, not the ones it ran
       * on: 7.0 / 2. *)
      check_field header rows "r" "mean_cost_ms" "3.500";
      (* a.py cost 6.0 and b.py 1.0. *)
      check_field header rows "r" "max_cost_ms" "6.000";
      check_field header rows "r" "worst_file" "a.py")

(* A rule the prefilter always rejected is the whole reason the report has a
 * [files_candidate] column: it has to appear, and it has to be visibly
 * unmeasured rather than absent. *)
let test_prefiltered_rule_still_gets_a_row () =
  with_sink [] (fun sink ->
      List.iter
        (fun file_s ->
          let ran_here = file_s = "a.py" in
          Taint_timing.record_file sink ~file_s
            (file_timing ~candidates:[ "screened"; "ran" ]
               ~rejected:
                 (if ran_here then [ "screened" ] else [ "screened"; "ran" ])
               ~matched:(if ran_here then [ ("ran", 4.0) ] else [])
               ()))
        [ "a.py"; "b.py"; "c.py" ];
      let header, rows = report_of sink in
      check_field header rows "screened" "files_candidate" "3";
      check_field header rows "screened" "files_matched" "0";
      check_field header rows "screened" "not_run" "3";
      check_field header rows "screened" "total_cost_ms" "0.000";
      check_field header rows "ran" "files_matched" "1";
      check_field header rows "ran" "not_run" "2")

(* The engine reports a rule killed by --timeout with a synthetic 0.0 match
 * time. Counted as a measurement it would make the most expensive rule in
 * the run look like the cheapest, so it must be dropped from the timing
 * columns and the rule must still sort to the top. *)
let test_timeouts_beat_measured_cost () =
  with_sink [] (fun sink ->
      Taint_timing.record_file sink ~file_s:"a.py"
        (file_timing
           ~candidates:[ "times-out"; "merely-slow" ]
             (* the synthetic 0.0 the engine emits for a killed rule *)
           ~matched:[ ("times-out", 0.0); ("merely-slow", 900.0) ]
           ~match_timed_out:[ "times-out" ] ());
      let header, rows = report_of sink in
      Alcotest.(check string)
        "timed-out rule sorts first" "times-out"
        (field header (List.hd rows) "rule_id");
      check_field header rows "times-out" "timeouts" "1";
      check_field header rows "times-out" "match_ms" "0.000";
      check_field header rows "times-out" "files_matched" "0";
      (* Neither run nor screened out: it is accounted for by [timeouts]. *)
      check_field header rows "times-out" "not_run" "0")

(* A rule can clear the matching pass and then be killed in the taint spec
 * pass. The two timeouts mean opposite things for the measurements: the
 * matching pass's is a synthetic zero to be thrown away, the spec pass's is
 * a real partial measurement, and the completed match time is real either
 * way. Pooling them would silently delete a good number. *)
let test_spec_timeout_keeps_match_time () =
  with_sink [] (fun sink ->
      Taint_timing.record_file sink ~file_s:"a.py"
        (file_timing ~candidates:[ "r" ]
           ~matched:[ ("r", 12.0) ]
             (* what it burned before the kill *)
           ~spec:[ ("r", 30.0) ] ~spec_timed_out:[ "r" ] ());
      let header, rows = report_of sink in
      check_field header rows "r" "match_ms" "12.000";
      check_field header rows "r" "spec_ms" "30.000";
      check_field header rows "r" "total_cost_ms" "42.000";
      check_field header rows "r" "files_matched" "1";
      check_field header rows "r" "timeouts" "1";
      (* It ran; it is not a rule the prefilter screened out. *)
      check_field header rows "r" "not_run" "0")

(* A rule killed in both passes on one file is one file, not two. *)
let test_timeouts_are_counted_per_file () =
  with_sink [] (fun sink ->
      Taint_timing.record_file sink ~file_s:"a.py"
        (file_timing ~candidates:[ "r" ] ~matched:[ ("r", 0.0) ]
           ~match_timed_out:[ "r" ] ~spec_timed_out:[ "r" ] ());
      let header, rows = report_of sink in
      check_field header rows "r" "timeouts" "1";
      check_field header rows "r" "files_candidate" "1";
      check_field header rows "r" "match_ms" "0.000";
      check_field header rows "r" "not_run" "0")

(* [Match_rules] screens every rule, including the `SCA and `Steps ones it
 * then refuses to run, so a screening cost can arrive for a rule that is
 * deliberately not a candidate. Such a rule must not gain a row: it would be
 * counted in files_candidate and reported as prefiltered away. *)
let test_only_candidates_get_rows () =
  with_sink [] (fun sink ->
      Taint_timing.record_file sink ~file_s:"a.py"
        (file_timing ~candidates:[ "real" ]
           ~prefilter:[ ("real", 1.0); ("unsupported-mode", 0.5) ]
           ());
      let header, rows = report_of sink in
      Alcotest.(check int) "exactly one row" 1 (List.length rows);
      check_field header rows "real" "files_candidate" "1";
      check_field header rows "real" "prefilter_ms" "1.000")

(* [--timeout-threshold] makes the engine discard the whole file's match
 * times, so on such a file "no match time" says nothing about whether the
 * rule ran. Only the rule the prefilter actually rejected may be counted as
 * not run; the one that finished before the abort and the one the abort
 * never reached are simply unaccounted for. *)
let test_truncation_does_not_invent_rejections () =
  with_sink [] (fun sink ->
      Taint_timing.record_file sink ~file_s:"a.py"
        (file_timing
           ~candidates:[ "finished"; "killed"; "unreached"; "screened" ]
           ~rejected:[ "screened" ]
             (* the engine threw the completed times away with the file *)
           ~matched:[] ~match_timed_out:[ "killed" ] ~truncated:true ());
      let header, rows = report_of sink in
      check_field header rows "screened" "not_run" "1";
      check_field header rows "finished" "not_run" "0";
      check_field header rows "unreached" "not_run" "0";
      check_field header rows "killed" "not_run" "0";
      check_field header rows "killed" "timeouts" "1";
      Alcotest.(check int) "truncated file counted" 1
        (Taint_timing.truncated_files sink))

(* Both prefilters can screen the same rule on the same file, and both
 * charges are real. *)
let test_screening_is_charged_per_pass () =
  with_sink [] (fun sink ->
      Taint_timing.record_file sink ~file_s:"a.py"
        (file_timing ~candidates:[ "r" ]
           ~prefilter:[ ("r", 1.25); ("r", 0.75) ]
           ());
      let header, rows = report_of sink in
      check_field header rows "r" "prefilter_ms" "2.000";
      check_field header rows "r" "files_candidate" "1")

(* --timeout-threshold makes the engine discard a file's match times, so the
 * caller has to be told the totals are short. *)
let test_truncated_files_are_counted () =
  with_sink [] (fun sink ->
      Taint_timing.record_file sink ~file_s:"a.py"
        (file_timing ~candidates:[ "r" ] ~truncated:true ());
      Taint_timing.record_file sink ~file_s:"b.py"
        (file_timing ~candidates:[ "r" ] ~matched:[ ("r", 1.0) ] ());
      Alcotest.(check int) "truncated files" 1 (Taint_timing.truncated_files sink))

(* [Common.with_time] is a [gettimeofday] delta, so a prefilter decision
 * finishing inside a microsecond measures exactly 0.0 - and a rule that is a
 * candidate on one cheap file can therefore total 0.0. The column is
 * documented to name the worst file, so it has to name one anyway. The same
 * test pins the tie-breaking: equal costs keep the earliest file, and a
 * dearer file still takes over. *)
let test_zero_cost_candidate_still_names_a_file () =
  with_sink [] (fun sink ->
      Taint_timing.record_file sink ~file_s:"a.py"
        (file_timing ~candidates:[ "r" ] ());
      let header, rows = report_of sink in
      check_field header rows "r" "max_cost_ms" "0.000";
      check_field header rows "r" "worst_file" "a.py";
      Taint_timing.record_file sink ~file_s:"b.py"
        (file_timing ~candidates:[ "r" ] ());
      let header, rows = report_of sink in
      check_field header rows "r" "worst_file" "a.py";
      Taint_timing.record_file sink ~file_s:"c.py"
        (file_timing ~candidates:[ "r" ] ~matched:[ ("r", 1.0) ] ());
      let header, rows = report_of sink in
      check_field header rows "r" "worst_file" "c.py")

(* The two passes screen independently and the taint payload pass uses the
 * looser union prefilter, so one candidate file can be rejected for matching
 * and still be run - and killed - in spec matching. Both charges are real,
 * so [not_run] and [timeouts] both count it and their sum overshoots
 * [files_candidate]. Not a partition, by design; see [write_csv]'s doc. *)
let test_not_run_and_timeouts_can_overlap () =
  with_sink [] (fun sink ->
      Taint_timing.record_file sink ~file_s:"a.py"
        (file_timing ~candidates:[ "r" ] ~rejected:[ "r" ]
           ~spec:[ ("r", 5.0) ] ~spec_timed_out:[ "r" ] ());
      let header, rows = report_of sink in
      check_field header rows "r" "files_candidate" "1";
      check_field header rows "r" "files_matched" "0";
      check_field header rows "r" "not_run" "1";
      check_field header rows "r" "timeouts" "1";
      check_field header rows "r" "files_spec" "1";
      check_field header rows "r" "spec_ms" "5.000")

(* Rule ids are author-controlled and end up in a CSV. *)
let test_rule_id_is_csv_escaped () =
  with_sink [] (fun sink ->
      Taint_timing.record_file sink ~file_s:"a.py"
        (file_timing ~candidates:[ {|weird,"id|} ] ());
      let file = Filename.temp_file "unit_taint_timing" ".csv" in
      Fun.protect
        ~finally:(fun () -> Sys.remove file)
        (fun () ->
          Taint_timing.write_csv sink ~out_csv:(Fpath.v file);
          let body = UFile.read_file (Fpath.v file) in
          Alcotest.(check bool)
            "comma and quote are escaped" true
            (String.starts_with ~prefix:{|"weird,""id"|}
               (List.nth (String.split_on_char '\n' body) 1))))

(* The report is meant to be opened in a spreadsheet, where a cell opening
 * with [=], [+], [-] or [@] is a formula rather than a label. Rule ids come
 * from the loaded rule pack and paths from the scanned repo, so neither can
 * be trusted to be inert, and CSV quoting alone does not make them so. *)
let test_formula_leading_fields_are_neutralized () =
  with_sink [] (fun sink ->
      Taint_timing.record_file sink ~file_s:{|@SUM(A1)|}
        (file_timing ~candidates:[ {|=1+1|} ] ());
      let header, rows = report_of sink in
      check_field header rows {|'=1+1|} "worst_file" {|'@SUM(A1)|};
      (* A field that is both a formula and needs quoting gets both, in that
       * order, and still round-trips as a single cell. *)
      Taint_timing.record_file sink ~file_s:"b.py"
        (file_timing ~candidates:[ {|-a,b|} ] ());
      let file = Filename.temp_file "unit_taint_timing" ".csv" in
      Fun.protect
        ~finally:(fun () -> Sys.remove file)
        (fun () ->
          Taint_timing.write_csv sink ~out_csv:(Fpath.v file);
          let body = UFile.read_file (Fpath.v file) in
          Alcotest.(check bool)
            "neutralised inside the quotes" true
            (body
            |> String.split_on_char '\n'
            |> List.exists (String.starts_with ~prefix:{|"'-a,b"|}))))

(*****************************************************************************)
(* Entry point *)
(*****************************************************************************)

let tests =
  Testo.categorize "Taint_timing"
    [
      t "aggregates cost across files" test_aggregates_across_files;
      t "gives prefiltered rules a row" test_prefiltered_rule_still_gets_a_row;
      t "sorts timeouts above measured cost" test_timeouts_beat_measured_cost;
      t "keeps the match time of a spec-pass timeout"
        test_spec_timeout_keeps_match_time;
      t "counts a doubly timed-out rule once per file"
        test_timeouts_are_counted_per_file;
      t "gives rows to candidates only" test_only_candidates_get_rows;
      t "does not invent rejections on a truncated file"
        test_truncation_does_not_invent_rejections;
      t "charges screening per prefilter pass" test_screening_is_charged_per_pass;
      t "counts truncated files" test_truncated_files_are_counted;
      t "names a worst file even at zero cost"
        test_zero_cost_candidate_still_names_a_file;
      t "lets not_run and timeouts overlap"
        test_not_run_and_timeouts_can_overlap;
      t "escapes rule ids in the csv" test_rule_id_is_csv_escaped;
      t "neutralizes spreadsheet formulas"
        test_formula_leading_fields_are_neutralized;
    ]
