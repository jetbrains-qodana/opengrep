module G = AST_generic
module Y = Yojson.Safe

type ast_format = [ `Json | `Binary ]

let base64_table = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/"

let base64_encode (data : string) : string =
  let len = String.length data in
  let buf = Buffer.create (((len + 2) / 3) * 4) in
  let get i = if i < len then Char.code data.[i] else 0 in
  let rec loop i =
    if i >= len then ()
    else
      let b1 = get i in
      let b2 = get (i + 1) in
      let b3 = get (i + 2) in
      let triple = (b1 lsl 16) lor (b2 lsl 8) lor b3 in
      Buffer.add_char buf base64_table.[(triple lsr 18) land 0x3F];
      Buffer.add_char buf base64_table.[(triple lsr 12) land 0x3F];
      if i + 1 < len then
        Buffer.add_char buf base64_table.[(triple lsr 6) land 0x3F]
      else
        Buffer.add_char buf '=';
      if i + 2 < len then
        Buffer.add_char buf base64_table.[triple land 0x3F]
      else
        Buffer.add_char buf '=';
      loop (i + 3)
  in
  loop 0;
  Buffer.contents buf

type parsed_file = {
  ast : AST_generic.program;
  lang : Lang.t;
  taint_entries : Taint_serializer.taint_entries_t;
}

let serialize_ast_to_json_string (ast : AST_generic.program) : string =
  let v1_ast = AST_generic_to_v1.program ast in
  Ast_generic_v1_j.string_of_program v1_ast

let ast_to_yojson (ast : AST_generic.program) : Y.t =
  ast |> serialize_ast_to_json_string |> Y.from_string

let serialize_ast_with_taint_to_string (asts : Y.t list)
    (taint_entries : Taint_serializer.taint_entries_t) : string =
  let ast_value =
    match asts with
    | [ single ] -> single
    | _ -> `List asts
  in
  let taint_value = `Assoc (Taint_serializer.yojson_fields_of_taint_entries taint_entries) in
  `Assoc [ ("ast", ast_value); ("taint", taint_value) ]
  |> Y.pretty_to_string

let serialize_ast_with_taint_to_binary_string (ast : AST_generic.program)
    (taint_entries : Taint_serializer.taint_entries_t) : string =
  let v1_ast = AST_generic_to_v1.program ast in
  let pool_builder = Ast_binary_serializer.create_string_pool_builder () in
  Ast_binary_serializer.collect_program v1_ast pool_builder;
  Ast_binary_serializer.collect_taint_entries taint_entries pool_builder;
  let pool = Ast_binary_serializer.build_string_pool pool_builder in
  let ast_binary = Ast_binary_serializer.serialize_program v1_ast pool in
  let taint_binary = Ast_binary_serializer.serialize_taint_entries taint_entries pool in
  `Assoc
    [ ("stringPool", Ast_binary_serializer.string_pool_to_yojson pool);
      ("astBinary", `String (base64_encode ast_binary));
      ("taintBinary", `String (base64_encode taint_binary)) ]
  |> Y.pretty_to_string


(* Extract deduplication key for taint entries (sources/sinks/sanitizers) *)
let taint_entry_key (rule_name, loc : string * Taint_location.taint_location) =
  Printf.sprintf "%s:%s" rule_name (Taint_location.loc_string loc)

(* Extract deduplication key for propagators *)
let propagator_key (rule_name, loc, locFrom, locTo : string * Taint_location.taint_location * Taint_location.taint_location * Taint_location.taint_location) =
  Printf.sprintf "%s:%s:%d:%d:%d:%d:%d:%d:%d:%d:%d:%d"
    rule_name
    loc.file_path loc.line loc.col loc.offsetStart loc.offsetEnd
    locFrom.line locFrom.col locFrom.offsetStart locFrom.offsetEnd
    locTo.line locTo.col

(* Split list into at most n chunks of roughly equal size *)
let split_into_chunks n lst =
  let a = Array.of_list lst in
  let total = Array.length a in
  let n = min n total in
  if n <= 1 then [ lst ]
  else
    List.init n (fun i ->
        let lo = i * total / n in
        let hi = (i + 1) * total / n in
        Array.to_list (Array.sub a lo (hi - lo)))
    |> List.filter (fun c -> c <> [])

(* Get all supported files from a directory recursively *)
let get_supported_files (path : Fpath.t) : Fpath.t list =
  List_files.list_regular_files path
  |> List.filter (fun file_path ->
       match Lang.langs_of_filename file_path with
       | [] -> false  (* No supported language *)
       | _ :: _ -> true)

(* Check if pattern is simple "*.extension" format *)
let is_simple_extension_pattern (path_s : string) : bool =
  String.starts_with ~prefix:"*." path_s && not (String.contains (String.sub path_s 2 (String.length path_s - 2)) '*')

(* Expand "*.extension" pattern to matching files in current directory recursively *)
let expand_extension_pattern (pattern_s : string) : Fpath.t list =
  (* Extract extension from "*.ext" *)
  let extension = String.sub pattern_s 2 (String.length pattern_s - 2) in
  let current_dir = Fpath.v "." in

  (* List all files recursively in current directory *)
  try
    List_files.list_regular_files current_dir
    |> List.filter (fun file_path ->
         Filename.check_suffix (Fpath.to_string file_path) extension)
    |> List.filter (fun file_path ->
         match Lang.langs_of_filename file_path with
         | [] -> false
         | _ :: _ -> true)
  with _ -> []

(* Merge and deduplicate taint entries from multiple files *)
let merge_taint_entries (entries_list : Taint_serializer.taint_entries_t list)
    : Taint_serializer.taint_entries_t =
  let all_sources = entries_list |> List.concat_map (fun (s, _, _, _) -> s) in
  let all_sinks = entries_list |> List.concat_map (fun (_, si, _, _) -> si) in
  let all_sanitizers = entries_list |> List.concat_map (fun (_, _, sa, _) -> sa) in
  let all_propagators = entries_list |> List.concat_map (fun (_, _, _, p) -> p) in

  (* Deduplicate using the helper functions defined above *)
  let deduped_sources = List_.deduplicate_gen ~get_key:taint_entry_key all_sources in
  let deduped_sinks = List_.deduplicate_gen ~get_key:taint_entry_key all_sinks in
  let deduped_sanitizers = List_.deduplicate_gen ~get_key:taint_entry_key all_sanitizers in
  let deduped_propagators = List_.deduplicate_gen ~get_key:propagator_key all_propagators in

  (deduped_sources, deduped_sinks, deduped_sanitizers, deduped_propagators)

let parse_file (caps : < Cap.fork >) ~(num_domains : int) (infile : Fpath.t) (infile_s : string) (rules: Rule.t list) : parsed_file =
  Parsing_init.init ();
  let ast = Parse_target.parse_program infile in
  let lang = Lang.lang_of_filename_exn infile in

  (* 1. Filter the rules by the language (as in Core_scan.rules_for_analyzer) *)
  let analyzer = Xlang.of_lang lang in

  (* Resolve names and mark implicit returns BEFORE pattern matching *)
  Naming_AST.resolve lang ast;
  Implicit_return.mark_implicit_return lang ast;
  Taint_location.set_current_file_path infile_s;
  let filtered_rules =
    rules
    |> List.filter (fun (r : Rule.t) ->
         (* Don't run a Python rule on a JavaScript target *)
         let compatible = Xlang.is_compatible ~require:analyzer ~provide:r.target_analyzer in
         compatible)
  in

  (* Deduplicate rules by ID after language filtering to avoid running
     the same rule multiple times for this specific target language *)
  let filtered_rules =
    List_.deduplicate_gen
      ~get_key:(fun r -> Rule_ID.to_string (fst r.Rule.id))
      filtered_rules
  in

  (* 2. Group and filter the rules (as in Match_rules.group_rules) *)
  (* Create a minimal xconfig for filtering *)
  let xconf =
    {
      Match_env.config = Rule_options.default;
      equivs = [];
      nested_formula = false;
      matching_conf = Match_patterns.default_matching_conf;
      matching_explanations = false;
      filter_irrelevant_rules = Match_env.NoPrefiltering;
      skip_taint = false;
    }
  in

  (* Create an xtarget for filtering *)
  let lazy_ast_and_errors = lazy (ast, []) in
  let xtarget =
    {
      Xtarget.path = {
        origin = Origin.File infile;
        internal_path_to_content = infile;
      };
      xlang = analyzer;
      lazy_content = lazy (UFile.read_file infile);
      lazy_ast_and_errors;
    }
  in

  (* Group rules: separate taint rules from others and filter by relevance *)
  let taint_rules, _nontaint_rules, _skipped_rules =
    filtered_rules
    |> Either_.partition_either3 (fun r ->
         let relevant_rule =
           Match_rules.is_relevant_rule_for_xtarget r xconf xtarget
         in
         match r.Rule.mode with
         | _ when not relevant_rule -> Either_.Right3 r
         | `Taint _ as mode -> Either_.Left3 { r with mode }
         | (`Extract _ | `Search _) as mode -> Either_.Middle3 { r with mode }
         | `SCA _ -> Either_.Right3 r
         | `Steps _ -> Either_.Right3 r)
  in

  (* 3. Match the applicable taint rules with the file and its AST.
     Each chunk of rules gets its own formula cache so chunks can run in parallel
     without sharing mutable state. Formula deduplication is preserved within
     each chunk; rules sharing formulas across chunks may redo some pattern
     matching, but parallelism more than compensates for large rule sets. *)
  let process_chunk (chunk : Rule.taint_rule list) =
    let chunk_cache = Formula_cache.mk_specialized_formula_cache chunk in
    List.filter_map (fun (rule : Rule.taint_rule) ->
        match
          Match_taint_spec.taint_config_of_rule
            ~per_file_formula_cache:chunk_cache
            xconf
            lang
            infile
            (ast, [])
            rule
        with
        | Some (_taint_config, spec_matches, _expls) ->
            Some (fst rule.Rule.id, spec_matches)
        | None -> None)
      chunk
  in

  let taint_configs_and_matches =
    let chunks = split_into_chunks num_domains taint_rules in
    match chunks with
    | [ single_chunk ] -> process_chunk single_chunk
    | _ ->
        let exception_handler (_chunk : Rule.taint_rule list) (e : Exception.t) =
          UCommon.pr2
            (Printf.sprintf "[ir-pipeline]   WARNING: taint rule chunk failed: %s"
               (Exception.to_string e));
          []
        in
        Domainslib_.parmap caps ~num_domains ~exception_handler process_chunk chunks
        |> List.concat_map (function Ok r -> r | Error r -> r)
  in

  let make_taint_entry rule_id rwm =
    let range = rwm.Range_with_metavars.r in
    let tok1, _tok2 = rwm.Range_with_metavars.origin.Core_match.range_loc in
    let rule_name = Rule_ID.to_string rule_id in
    let loc = Taint_location.mk_loc_from_tok tok1 range in
    (rule_name, loc)
  in

  let taint_sources =
    taint_configs_and_matches
    |> List.concat_map (fun (rule_id, spec_matches) ->
         spec_matches.Match_taint_spec.sources
         |> List.map (fun (rwm, _spec) -> make_taint_entry rule_id rwm))
    |> List_.deduplicate_gen ~get_key:taint_entry_key
  in

  let taint_sinks =
    taint_configs_and_matches
    |> List.concat_map (fun (rule_id, spec_matches) ->
         spec_matches.Match_taint_spec.sinks
         |> List.map (fun (rwm, _spec) -> make_taint_entry rule_id rwm))
    |> List_.deduplicate_gen ~get_key:taint_entry_key
  in

  let taint_sanitizers =
    taint_configs_and_matches
    |> List.concat_map (fun (rule_id, spec_matches) ->
         spec_matches.Match_taint_spec.sanitizers
         |> List.map (fun (rwm, _spec) -> make_taint_entry rule_id rwm))
    |> List_.deduplicate_gen ~get_key:taint_entry_key
  in

  let taint_propagators =
    taint_configs_and_matches
    |> List.concat_map (fun (rule_id, spec_matches) ->
         spec_matches.Match_taint_spec.propagators
         |> List.map (fun (prop_match : Match_taint_spec.propagator_match) ->
            let (rule_name, loc) = make_taint_entry rule_id prop_match.rwm in
            let locFrom = Taint_location.mk_loc_from_range prop_match.from in
            let locTo = Taint_location.mk_loc_from_range prop_match.to_ in
            (rule_name, loc, locFrom, locTo)
         ))
    |> List_.deduplicate_gen ~get_key:propagator_key
  in

  {
    ast;
    lang;
    taint_entries = (taint_sources, taint_sinks, taint_sanitizers, taint_propagators);
  }

let parse_files_ast (caps : < Cap.fork >) ~(num_domains : int) (files : Fpath.t list)
    (description : string) (rules : Rule.t list)
    : Y.t list * Taint_serializer.taint_entries_t =
  UCommon.pr2 (Printf.sprintf "[ir-pipeline] Processing %d files %s"
    (List.length files) description);

  let asts = ref [] in
  let taint_entries_list = ref [] in
  let error_count = ref 0 in
  let glob_start_time = Unix.gettimeofday () in

  List.iter (fun file ->
    try
      let file_s = Fpath.to_string file in
      UCommon.pr2 (Printf.sprintf "[ir-pipeline]   Processing: %s" file_s);
      let start_time = Unix.gettimeofday () in
      let parsed = parse_file caps ~num_domains file file_s rules in
      let ast_json = ast_to_yojson parsed.ast in
      let end_time = Unix.gettimeofday () in
      let elapsed_ms = (end_time -. start_time) *. 1000.0 in
      UCommon.pr2 (Printf.sprintf "[ir-pipeline]   Completed in %.2f ms" elapsed_ms);
      asts := ast_json :: !asts;
      taint_entries_list := parsed.taint_entries :: !taint_entries_list
    with
    | exn ->
        let error_msg = Printexc.to_string exn in
        UCommon.pr2 (Printf.sprintf "[ir-pipeline]   ERROR: %s - %s"
          (Fpath.to_string file) error_msg);
        incr error_count
  ) files;

  UCommon.pr2 (Printf.sprintf "[ir-pipeline] Successfully processed %d/%d files (%d errors)"
    (List.length !asts) (List.length files) !error_count);
  let glob_end_time = Unix.gettimeofday () in
  let glob_elapsed_ms = (glob_end_time -. glob_start_time) *. 1000.0 in
  UCommon.pr2 (Printf.sprintf "[ir-pipeline]   Total time - %.2f ms; average time - %.2f ms" glob_elapsed_ms (glob_elapsed_ms /. (float_of_int (List.length files))));

  let merged_taint_entries = merge_taint_entries !taint_entries_list in
  (List.rev !asts, merged_taint_entries)

let parse_folder_ast (caps : < Cap.fork >) ~(num_domains : int) (folder_path : Fpath.t)
    (folder_path_s : string) (rules : Rule.t list)
    : Y.t list * Taint_serializer.taint_entries_t =
  let files = get_supported_files folder_path in
  parse_files_ast caps ~num_domains files (Printf.sprintf "from folder: %s" folder_path_s) rules

let parse_extension_pattern_ast (caps : < Cap.fork >) ~(num_domains : int) (pattern_s : string)
    (rules : Rule.t list)
    : Y.t list * Taint_serializer.taint_entries_t =
  let files = expand_extension_pattern pattern_s in
  parse_files_ast caps ~num_domains files (Printf.sprintf "matching pattern: %s" pattern_s) rules

(* Serialize folder processing results to JSON string *)
let parse_and_serialize_folder (caps : < Cap.fork >) ~(num_domains : int)
    (folder_path : Fpath.t) (folder_path_s : string)
    (rules : Rule.t list) : string =
  let asts, taint_entries =
    parse_folder_ast caps ~num_domains folder_path folder_path_s rules
  in
  serialize_ast_with_taint_to_string asts taint_entries

(* Serialize extension pattern processing results to JSON string *)
let parse_and_serialize_extension_pattern (caps : < Cap.fork >) ~(num_domains : int)
    (pattern_s : string) (rules : Rule.t list) : string =
  let asts, taint_entries =
    parse_extension_pattern_ast caps ~num_domains pattern_s rules
  in
  serialize_ast_with_taint_to_string asts taint_entries

let parse_and_serialize_file (caps : < Cap.fork >) ~(num_domains : int)
    ?(format = `Json) (infile : Fpath.t) (infile_s : string)
    (rules: Rule.t list) : string =
  let parsed = parse_file caps ~num_domains infile infile_s rules in
  match format with
  | `Json ->
      let ast_json = ast_to_yojson parsed.ast in
      serialize_ast_with_taint_to_string [ast_json] parsed.taint_entries
  | `Binary ->
      serialize_ast_with_taint_to_binary_string parsed.ast parsed.taint_entries

let serialize_to_json_file ~(file : Fpath.t) (ast : AST_generic.program) : unit =
  let json_string = serialize_ast_to_json_string ast in
  UFile.write_file ~file json_string
