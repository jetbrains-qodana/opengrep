let main (caps : Cap.all_caps) : unit =
  let argv = CapSys.argv caps#argv in
  let usage () =
    UCommon.pr2
      "Usage: taint-json <input-path> <output-file> [<rules-path>] [format=json|binary]";
    UCommon.pr2 "  <input-path>: Source file, directory, or *.extension pattern (e.g., *.cs) to process.";
    UCommon.pr2 "                Directories and extension patterns are processed recursively.";
    UCommon.pr2 "  <rules-path>: Optional YAML file or directory with taint rules. If omitted, no taint analysis is performed.";
    UCommon.pr2 "  format=json|binary: Optional output format (default json)."
  in

  let arg_index = ref 1 in

  let remaining = Array.length argv - !arg_index in
  if remaining < 2 || remaining > 4 then (
    usage ();
    CapStdlib.exit caps#exit 2
  );

  (* Initialize parsing before parsing rules *)
  Parsing_init.init ();

  let infile_s = argv.(!arg_index) in
  let outfile_s = argv.(!arg_index + 1) in
  let rules_path_opt = ref None in
  let format = ref `Json in
  let parse_format_arg value =
    if String.starts_with ~prefix:"format=" value then
      let raw = String.sub value 7 (String.length value - 7) in
      match raw with
      | "json" -> Some `Json
      | "binary" -> Some `Binary
      | _ ->
          UCommon.pr2
            (Printf.sprintf
               "[taint-json] Unknown format '%s', expected json|binary; using %s"
               raw
               (match !format with
               | `Json -> "json"
               | `Binary -> "binary"));
          Some !format
    else
      None
  in
  if remaining >= 3 then (
    let arg3 = argv.(!arg_index + 2) in
    match parse_format_arg arg3 with
    | Some v -> format := v
    | None -> rules_path_opt := Some arg3
  );
  if remaining = 4 then (
    let arg4 = argv.(!arg_index + 3) in
    match parse_format_arg arg4 with
    | Some v -> format := v
    | None ->
        if Option.is_none !rules_path_opt then
          rules_path_opt := Some arg4
        else (
          usage ();
          CapStdlib.exit caps#exit 2
        )
  );
  let infile = Fpath.v infile_s in
  let outfile = Fpath.v outfile_s in

  (* Load rules if provided *)
  let rules =
    match !rules_path_opt with
    | None ->
        (* No rules path provided - run without taint analysis *)
        UCommon.pr2 "[taint-json] No rules path provided";
        []
    | Some rules_path_str ->
        let rules_path = Fpath.v rules_path_str in
        (* Determine if it's a directory by checking if list returns multiple files or the path itself *)
        let is_dir =
          try
            let files = List_files.list rules_path in
            (* If we get multiple files, or the single file is not the path itself, it's a directory *)
            match files with
            | [] -> true (* Empty directory *)
            | [ single ] -> not (Fpath.equal single rules_path) (* Directory if file ≠ path *)
            | _ :: _ :: _ -> true (* Multiple files means directory *)
          with _ -> false
        in
        if is_dir then (
          (* Directory: load all rule files *)
          let rule_files =
            List_files.list rules_path
            |> List.filter Rule_file.is_valid_rule_filename
          in
          UCommon.pr2 (Printf.sprintf "[taint-json] Found %d rule files" (List.length rule_files));

          (* Load each file and collect all rules *)
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
          (* Single file *)
          match Parse_rule.parse_and_filter_invalid_rules rules_path with
          | Ok (valid_rules, _) ->
              valid_rules
          | Error err ->
              UCommon.pr2 (Printf.sprintf "[taint-json] Failed to parse rules: %s"
                (Rule_error.string_of_error err));
              [])
  in

  (* Detect if input is "*.extension" pattern, directory, or file *)
  let is_extension_pattern = Taint_processor.is_simple_extension_pattern infile_s in
  let is_directory =
    if is_extension_pattern then false
    else
      try
        let files = List_files.list infile in
        (* If we get multiple files, or the single file is not the path itself, it's a directory *)
        match files with
        | [] -> true (* Empty directory *)
        | [ single ] -> not (Fpath.equal single infile) (* Directory if file ≠ path *)
        | _ :: _ :: _ -> true (* Multiple files means directory *)
      with _ -> false
  in

  if !format = `Binary && (is_extension_pattern || is_directory) then (
    UCommon.pr2 "[taint-json] Binary format supports single file input only";
    CapStdlib.exit caps#exit 2
  );

  let fork_caps = (caps :> < Cap.fork >) in
  let num_domains = Domainslib_.get_cpu_count () in

  (* Generate IR with taint analysis *)
  let s =
    if !format = `Binary then (
      Taint_processor.parse_and_serialize_file fork_caps ~num_domains ~format:`Binary infile infile_s rules
    ) else if is_extension_pattern then (
      UCommon.pr2 (Printf.sprintf "[taint-json] Input is extension pattern, expanding: %s" infile_s);
      Taint_processor.parse_and_serialize_extension_pattern fork_caps ~num_domains infile_s rules
    ) else if is_directory then (
      UCommon.pr2 "[taint-json] Input is a directory, processing recursively";
      Taint_processor.parse_and_serialize_folder fork_caps ~num_domains infile infile_s rules
    ) else (
      UCommon.pr2 "[taint-json] Input is a file";
      Taint_processor.parse_and_serialize_file fork_caps ~num_domains infile infile_s rules
    )
  in

  UFile.write_file ~file:outfile s

let () = Cap.main main
