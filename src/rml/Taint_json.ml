module Arg = Cmdliner.Arg
module Term = Cmdliner.Term
module Cmd = Cmdliner.Cmd

type conf = {
  rules_path : string option;
  rules_file : string option;
  format : [ `Json | `Binary ];
  jobs : int;
}

let o_rules : string option Term.t =
  let info =
    Arg.info [ "rules"; "r" ] ~docv:"PATH"
      ~doc:"YAML file or directory with taint rules. If omitted, no taint analysis is performed."
  in
  Arg.value (Arg.opt (Arg.some Arg.string) None info)

let o_format : [ `Json | `Binary ] Term.t =
  let format_enum = Arg.enum [ ("json", `Json); ("binary", `Binary) ] in
  let info =
    Arg.info [ "format"; "f" ] ~docv:"FORMAT"
      ~doc:"Output format. $(docv) must be $(b,json) or $(b,binary)."
  in
  Arg.value (Arg.opt format_enum `Json info)

let o_rules_file : string option Term.t =
  let info =
    Arg.info [ "rules-file" ] ~docv:"FILE"
      ~doc:"File containing rule paths, one per line. Each path may be a YAML file or directory."
  in
  Arg.value (Arg.opt (Arg.some Arg.string) None info)

let o_jobs : int Term.t =
  let info =
    Arg.info [ "jobs"; "j" ] ~docv:"N"
      ~doc:"Number of parallel jobs (default: number of CPUs)."
  in
  Arg.value (Arg.opt Arg.int (Domainslib_.get_cpu_count ()) info)

let cmdline_term : conf Term.t =
  let combine format jobs rules_file rules_path =
    { rules_path; rules_file; format; jobs }
  in
  Term.(const combine $ o_format $ o_jobs $ o_rules_file $ o_rules)

let doc = "Parse files into AST+taint JSON, streamed to stdout"

let man : Cmdliner.Manpage.block list =
  [
    `S Cmdliner.Manpage.s_description;
    `P "Reads file paths from stdin (one per line), parses each file, \
        and streams newline delimited JSON results to stdout.";
  ]

let cmdline_info : Cmd.info = Cmd.info "taint-json" ~doc ~man

let parse_argv (argv : string array) : conf =
  let cmd : conf Cmd.t = Cmd.v cmdline_info cmdline_term in
  match Cmd.eval_value ~argv cmd with
  | Ok (`Ok conf) -> conf
  | Ok `Version | Ok `Help -> UStdlib.exit 0
  | Error _ -> UStdlib.exit 2

let read_file_list_from_stdin (_caps : Cap.stdin) : Fpath.t list =
  let files = ref [] in
  (try
     while true do
       let line = input_line UStdlib.stdin in
       let trimmed = String.trim line in
       if trimmed <> "" then
         if USys.is_directory trimmed then
           UCommon.pr2
             (Printf.sprintf "[taint-json] Skipping directory: %s" trimmed)
         else
           files := Fpath.v trimmed :: !files
     done
   with End_of_file -> ());
  List.sort_uniq Fpath.compare !files

let load_rules_from_path (rules_path : Fpath.t) : Rule.t list =
  if try USys.is_directory (Fpath.to_string rules_path) with _ -> false then (
    let rule_files =
      List_files.list rules_path
      |> List.filter Rule_file.is_valid_rule_filename
    in
    UCommon.pr2 (Printf.sprintf "[taint-json] Found %d rule files in %s"
      (List.length rule_files) (Fpath.to_string rules_path));
    let all_rules = ref [] in
    let total_invalid = ref 0 in
    rule_files |> List.iter (fun file ->
      match Parse_rule.parse_and_filter_invalid_rules file with
      | Ok (valid_rules, invalid_rules) ->
          all_rules := !all_rules @ valid_rules;
          total_invalid := !total_invalid + List.length invalid_rules;
      | Error err ->
          UCommon.pr2 (Printf.sprintf "[taint-json] Failed to parse %s: %s"
            (Fpath.to_string file)
            (Rule_error.string_of_error err)));
    UCommon.pr2 (Printf.sprintf "[taint-json] Loaded %d valid rules, %d invalid from %s"
      (List.length !all_rules) !total_invalid (Fpath.to_string rules_path));
    !all_rules
  ) else (
    match Parse_rule.parse_and_filter_invalid_rules rules_path with
    | Ok (valid_rules, _) ->
        valid_rules
    | Error err ->
        UCommon.pr2 (Printf.sprintf "[taint-json] Failed to parse rules: %s"
          (Rule_error.string_of_error err));
        [])

let read_lines_from_file (path : string) : string list =
  let ic = UStdlib.open_in path in
  let lines = ref [] in
  (try
     while true do
       let line = input_line ic in
       let trimmed = String.trim line in
       if trimmed <> "" then lines := trimmed :: !lines
     done
   with End_of_file -> close_in ic);
  List.rev !lines

let load_rules (conf : conf) : Rule.t list =
  let paths =
    (match conf.rules_path with
     | Some p -> [ p ]
     | None -> [])
    @
    (match conf.rules_file with
     | Some file -> read_lines_from_file file
     | None -> [])
  in
  if paths = [] then (
    UCommon.pr2 "[taint-json] No rules path provided";
    []
  ) else
    List.concat_map (fun p -> load_rules_from_path (Fpath.v p)) paths

let main (caps : Cap.all_caps) : unit =
  let argv = CapSys.argv caps#argv in
  let conf = parse_argv argv in

  Parsing_init.init ();

  let rules = load_rules conf in
  let files = read_file_list_from_stdin (caps :> < Cap.stdin >) in

  if files = [] then (
    UCommon.pr2 "[taint-json] No files provided on stdin";
    CapStdlib.exit caps#exit 0
  );

  Taint_processor.parse_files_ast (caps :> < Cap.fork >) ~num_domains:conf.jobs
    ~format:conf.format files "" rules

let () = Cap.main main
