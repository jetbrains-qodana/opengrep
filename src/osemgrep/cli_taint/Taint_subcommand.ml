(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(* Implementation of the 'opengrep taint' subcommand.
 *
 * Reads file paths from stdin (one per line), parses each file and its
 * taint specifications, and streams newline-delimited JSON (or base64
 * encoded binary) results to stdout.
 *)

(*****************************************************************************)
(* Types and constants *)
(*****************************************************************************)
(* we need Cap.fork for parallel rule evaluation *)
type caps = < Cap.fork >

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)

let read_file_list_from_stdin () : Fpath.t list =
  let files = ref [] in
  (try
     while true do
       let line = input_line Stdlib.stdin in
       let trimmed = String.trim line in
       if trimmed <> "" then
         if Sys.is_directory trimmed then
           Logs.warn ~src:Ir_pipeline_logs.src (fun m ->
             m "Skipping directory: %s" trimmed)
         else files := Fpath.v trimmed :: !files
     done
   with End_of_file -> ());
  List.sort_uniq Fpath.compare !files

let load_rules_from_path (rules_path : Fpath.t) : Rule.t list =
  if try Sys.is_directory (Fpath.to_string rules_path) with _ -> false then (
    let rule_files =
      List_files.list rules_path |> List.filter Rule_file.is_valid_rule_filename
    in
    Logs.app ~src:Ir_pipeline_logs.src (fun m ->
      m "Found %d rule files in %s" (List.length rule_files)
        (Fpath.to_string rules_path));
    let all_rules = ref [] in
    let total_invalid = ref 0 in
    rule_files
    |> List.iter (fun file ->
           match Parse_rule.parse_and_filter_invalid_rules file with
           | Ok (valid_rules, invalid_rules) ->
               all_rules := !all_rules @ valid_rules;
               total_invalid := !total_invalid + List.length invalid_rules
           | Error err ->
               Logs.err ~src:Ir_pipeline_logs.src (fun m ->
                 m "Failed to parse %s: %s" (Fpath.to_string file)
                   (Rule_error.string_of_error err)));
    Logs.app ~src:Ir_pipeline_logs.src (fun m ->
      m "Loaded %d valid rules, %d invalid from %s" (List.length !all_rules)
        !total_invalid
        (Fpath.to_string rules_path));
    !all_rules)
  else
    match Parse_rule.parse_and_filter_invalid_rules rules_path with
    | Ok (valid_rules, _) -> valid_rules
    | Error err ->
        Logs.err ~src:Ir_pipeline_logs.src (fun m ->
          m "Failed to parse rules: %s" (Rule_error.string_of_error err));
        []

let read_lines_from_file (path : string) : string list =
  let ic = Stdlib.open_in path in
  let lines = ref [] in
  (try
     while true do
       let line = input_line ic in
       let trimmed = String.trim line in
       if trimmed <> "" then lines := trimmed :: !lines
     done
   with End_of_file -> close_in ic);
  List.rev !lines

let load_rules (conf : Taint_CLI.conf) : Rule.t list =
  let paths =
    (match conf.rules_path with
    | Some p -> [ p ]
    | None -> [])
    @
    match conf.rules_file with
    | Some file -> read_lines_from_file file
    | None -> []
  in
  if paths = [] then (
    Logs.warn ~src:Ir_pipeline_logs.src (fun m ->
      m "No rules path provided");
    [])
  else List.concat_map (fun p -> load_rules_from_path (Fpath.v p)) paths

(*****************************************************************************)
(* Main logic *)
(*****************************************************************************)

let run_conf (caps : < caps ; .. >) (conf : Taint_CLI.conf) : Exit_code.t =
  (* This subcommand is a streaming JSON pipeline: stdout carries the data
   * and stderr carries operational status via [Logs] under
   * [semgrep.ir-pipeline] only. [init_taint_subcommand_logging] replaces the
   * default reporter from [CLI.main] so other sources' [Logs] traffic does
   * not flood stderr. *)
  Ir_pipeline_logs.init_taint_subcommand_logging ~level:conf.logging_level ();

  Parsing_init.init ();

  let rules = load_rules conf in
  let files = read_file_list_from_stdin () in

  if files = [] then (
    Logs.warn ~src:Ir_pipeline_logs.src (fun m ->
      m "No files provided on stdin");
    Exit_code.ok ~__LOC__)
  else (
    Taint_pipeline.parse_files_ast
      (caps :> < Cap.fork >)
      {
        num_domains = conf.jobs;
        mode = if conf.with_diagnostics then `All else `Taint;
        on_parsed =
          Stdout_emitter.write_result_to_stdout
            ~ast_format:conf.format ~rules;
        files;
        rules;
      };
    Exit_code.ok ~__LOC__)

(*****************************************************************************)
(* Entry point *)
(*****************************************************************************)
let main (caps : < caps ; .. >) (argv : string array) : Exit_code.t =
  let conf = Taint_CLI.parse_argv argv in
  run_conf caps conf
