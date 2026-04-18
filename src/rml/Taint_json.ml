let read_file_list_from_stdin () : Fpath.t list =
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

let main (caps : Cap.all_caps) : unit =
  let argv = CapSys.argv caps#argv in
  let usage () =
    UCommon.pr2
      "Usage: taint-json [<rules-path>] [format=json|binary] [jobs=N]";
    UCommon.pr2 "  File list is read from stdin, one path per line.";
    UCommon.pr2 "  Results are streamed to stdout as length-prefixed JSON messages.";
    UCommon.pr2 "  <rules-path>: Optional YAML file or directory with taint rules. If omitted, no taint analysis is performed.";
    UCommon.pr2 "  format=json|binary: Optional output format (default json).";
    UCommon.pr2 "  jobs=N:       Number of parallel jobs (default: number of CPUs)."
  in

  let arg_index = ref 1 in

  let remaining = Array.length argv - !arg_index in
  if remaining > 3 then (
    usage ();
    CapStdlib.exit caps#exit 2
  );

  (* Initialize parsing before parsing rules *)
  Parsing_init.init ();

  let rules_path_opt = ref None in
  let format = ref `Json in
  let num_domains = ref (Domainslib_.get_cpu_count ()) in
  let parse_optional_arg value =
    if String.starts_with ~prefix:"format=" value then (
      let raw = String.sub value 7 (String.length value - 7) in
      (match raw with
      | "json" -> format := `Json
      | "binary" -> format := `Binary
      | _ ->
          UCommon.pr2
            (Printf.sprintf
               "[taint-json] Unknown format '%s', expected json|binary; using %s"
               raw
               (match !format with
               | `Json -> "json"
               | `Binary -> "binary")));
      true)
    else if String.starts_with ~prefix:"jobs=" value then (
      let raw = String.sub value 5 (String.length value - 5) in
      (match int_of_string_opt raw with
      | Some n when n >= 1 -> num_domains := n
      | _ ->
          UCommon.pr2
            (Printf.sprintf
               "[taint-json] Invalid jobs value '%s', expected positive integer"
               raw);
          usage ();
          CapStdlib.exit caps#exit 2);
      true)
    else false
  in
  for i = 0 to remaining - 1 do
    let arg = argv.(!arg_index + i) in
    if not (parse_optional_arg arg) then
      if Option.is_none !rules_path_opt then rules_path_opt := Some arg
      else (
        usage ();
        CapStdlib.exit caps#exit 2)
  done;

  let rules =
    match !rules_path_opt with
    | None ->
        UCommon.pr2 "[taint-json] No rules path provided";
        []
    | Some rules_path_str ->
        let rules_path = Fpath.v rules_path_str in
        let is_dir =
          try
            let files = List_files.list rules_path in
            match files with
            | [] -> true
            | [ single ] -> not (Fpath.equal single rules_path)
            | _ :: _ :: _ -> true
          with _ -> false
        in
        if is_dir then (
          let rule_files =
            List_files.list rules_path
            |> List.filter Rule_file.is_valid_rule_filename
          in
          UCommon.pr2 (Printf.sprintf "[taint-json] Found %d rule files" (List.length rule_files));
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
          UCommon.pr2 (Printf.sprintf "[taint-json] Loaded %d valid rules, %d invalid from directory"
            (List.length !all_rules) !total_invalid);
          !all_rules
        ) else (
          match Parse_rule.parse_and_filter_invalid_rules rules_path with
          | Ok (valid_rules, _) ->
              valid_rules
          | Error err ->
              UCommon.pr2 (Printf.sprintf "[taint-json] Failed to parse rules: %s"
                (Rule_error.string_of_error err));
              [])
  in

  let files = read_file_list_from_stdin () in

  if files = [] then (
    UCommon.pr2 "[taint-json] No files provided on stdin";
    CapStdlib.exit caps#exit 0
  );

  let fork_caps = (caps :> < Cap.fork >) in
  let num_domains = !num_domains in

  Taint_processor.parse_files_ast fork_caps ~num_domains files "" rules

let () = Cap.main main
