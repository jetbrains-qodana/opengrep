(* See Scip_index.mli. *)

type symbol_info = {
  display_name : string;
  raw_signature : string option;
  type_symbol : string option;
  relationships : (string * bool) list;
}

type t = {
  (* (relative_path, start_line0, start_char0_utf16) -> symbol_info *)
  positions : (string * int * int, symbol_info) Hashtbl.t;
  (* symbol string -> symbol_info, for global/external symbols only (never
   * "local N" symbols - see is_local_symbol below). This is what lets a
   * caller start from a [type_symbol] found at some position and walk its
   * [relationships] to other (global/external) symbols. *)
  by_symbol : (string, symbol_info) Hashtbl.t;
  (* display name -> symbol string, for the same global/external symbols as
   * [by_symbol] (multiple symbols can share a name, e.g. overloads or
   * same-named types in different namespaces - see [find_by_display_name]).
   * This is what lets a caller start from a bare type NAME parsed out of
   * raw hover text (no symbol string available) and still walk
   * relationships, as long as that name resolves to a type defined *within*
   * the indexed project (see symbol_info_of_scip's [raw_signature] comment
   * for why display_name itself is frequently empty). *)
  by_display_name : (string, string) Hashtbl.t;
}

(* Per the SCIP grammar (scip.proto, around "<symbol> ::= ... | 'local '
 * <local-id>"), a symbol string starting with "local " is only meaningful
 * within the Document that declared it; the same string can and does mean
 * different things in different documents, so local symbols must be
 * resolved against a per-document table, never the global one. *)
let is_local_symbol (sym : string) : bool = String.starts_with ~prefix:"local " sym

(* Some indexers (e.g. scip-dotnet as of 0.2.14) never populate
 * [signature_documentation], and instead render the hover/signature text as
 * a markdown code block inside the deprecated [documentation] field -
 * scip.proto's own comment on that field calls this out explicitly: "Due to
 * historical reasons, indexers may include signature documentation in this
 * field by rendering markdown code blocks." Strip the fence (```<lang> ...
 * ```) to recover the bare signature text underneath, e.g.
 * "```cs\nSQLiteDataAdapter da\n```" -> "SQLiteDataAdapter da". Returns [s]
 * unchanged if it isn't fenced. *)
let strip_markdown_code_fence (s : string) : string =
  match String.split_on_char '\n' (String.trim s) with
  | first :: (_ :: _ as rest) when String.starts_with ~prefix:"```" first -> (
      match List.rev rest with
      | last :: body_rev when String.trim last = "```" ->
          String.concat "\n" (List.rev body_rev)
      | _ -> String.concat "\n" rest)
  | _ -> s

let symbol_info_of_scip (si : Scip.symbol_information) : symbol_info =
  let raw_signature =
    match si.signature_documentation with
    | Some { Scip.text; _ } when text <> "" -> Some text
    | _ -> (
        si.documentation
        |> List.find_map (fun doc ->
               match strip_markdown_code_fence doc with
               | "" -> None
               | s -> Some s))
  in
  (* Best-effort: the first symbol referenced inside the hover signature is
   * heuristically the type being hovered over (e.g. "SQLiteDataAdapter" in
   * the C# local-variable hover "SQLiteDataAdapter adapter") - the
   * variable's own name is the symbol's *definition*, not a reference, so
   * indexers don't emit a signature occurrence for it. *)
  let type_symbol =
    match si.signature_documentation with
    | Some { Scip.occurrences = occ :: _; _ } when occ.Scip.symbol <> "" ->
        Some occ.Scip.symbol
    | _ -> None
  in
  let relationships =
    si.relationships
    |> List.filter_map (fun (r : Scip.relationship) ->
           if r.Scip.symbol = "" then None
           else Some (r.Scip.symbol, r.Scip.is_implementation))
  in
  { display_name = si.display_name; raw_signature; type_symbol; relationships }

(* Start (line0, char0) of an occurrence, preferring the structured
 * typed_range and falling back to the deprecated packed `range` field. *)
let start_of_occurrence (occ : Scip.occurrence) : (int * int) option =
  match occ.typed_range with
  | Some (Scip.Single_line_range r) ->
      Some (Int32.to_int r.line, Int32.to_int r.start_character)
  | Some (Scip.Multi_line_range r) ->
      Some (Int32.to_int r.start_line, Int32.to_int r.start_character)
  | None -> (
      match occ.range with
      | [ line; start_char; _end_char ] ->
          Some (Int32.to_int line, Int32.to_int start_char)
      | [ start_line; start_char; _end_line; _end_char ] ->
          Some (Int32.to_int start_line, Int32.to_int start_char)
      | _else_ -> None)

(* The motivating indexers (scip-dotnet, scip-typescript) both always use
 * UTF-16 code unit offsets. Documents declaring UTF-8 or UTF-32 would need
 * per-document byte<->codeunit conversion using the document's own source
 * text, which isn't worth building until an indexer that emits it shows up;
 * skip such documents rather than risk silently wrong positions. *)
let has_supported_encoding (doc : Scip.document) : bool =
  match doc.position_encoding with
  | Scip.Unspecified_position_encoding
  | Scip.Utf16_code_unit_offset_from_line_start ->
      true
  | Scip.Utf8_code_unit_offset_from_line_start
  | Scip.Utf32_code_unit_offset_from_line_start ->
      false

let register_symbol table (si : Scip.symbol_information) : unit =
  if si.symbol <> "" && not (is_local_symbol si.symbol) then
    Hashtbl.replace table si.symbol (symbol_info_of_scip si)

(* Per scip.proto's grammar for a global <symbol>: "<scheme> ' ' <package>
 * ' ' <descriptor>+", where <package> is itself "<manager> ' '
 * <package-name> ' ' <version>" - i.e. exactly 4 space-separated fields
 * before the descriptor path (e.g. "scip-dotnet nuget . . DB/Foo#" or
 * "scip-dotnet nuget System.Data 2.0.0.0 Common/DbDataAdapter#Fill()."). The
 * descriptor path itself has no unescaped spaces in practice, so it's
 * exactly everything after those first 4 fields. This lets us derive a
 * symbol's own simple name straight from its symbol string when the indexer
 * doesn't populate [display_name] (see symbol_info_of_scip) - this is
 * spec-mandated, not a scip-dotnet-specific hack: "the symbol
 * `com/example/MyClass#myMethod(+1).` should have the display name
 * `myMethod`". *)
let simple_name_of_symbol (sym : string) : string option =
  match String.split_on_char ' ' sym with
  | _scheme :: _manager :: _pkg_name :: _pkg_version :: (_ :: _ as rest) -> (
      let s = String.concat " " rest in
      (* Strip a trailing term/method terminator ('.'), then a method's
       * "(...)" disambiguator if present, then a trailing type terminator
       * ('#'). What's left ends exactly at the descriptor's own name. *)
      let strip_trailing_char c s =
        let len = String.length s in
        if len > 0 && s.[len - 1] = c then String.sub s 0 (len - 1) else s
      in
      let s = strip_trailing_char '.' s in
      let s =
        let len = String.length s in
        if len > 0 && s.[len - 1] = ')' then
          match String.rindex_opt s '(' with
          | Some i -> String.sub s 0 i
          | None -> s
        else s
      in
      let s = strip_trailing_char '#' s in
      let last_descriptor_sep =
        [ '/'; '#'; '.' ]
        |> List.filter_map (String.rindex_opt s)
        |> function
        | [] -> None
        | firsts -> Some (List.fold_left max (List.hd firsts) firsts)
      in
      match last_descriptor_sep with
      | Some i ->
          let name = String.sub s (i + 1) (String.length s - i - 1) in
          if name = "" then None else Some name
      | None -> if s = "" then None else Some s)
  | _ -> None

(* Prefer the indexer-declared [display_name] when present; otherwise derive
 * it from the symbol string itself (see simple_name_of_symbol). *)
let display_name_of_symbol (symbol : string) (info : symbol_info) :
    string option =
  if info.display_name <> "" then Some info.display_name
  else simple_name_of_symbol symbol

let index_document positions global_table (doc : Scip.document) : unit =
  if has_supported_encoding doc then (
    let local_table : (string, symbol_info) Hashtbl.t = Hashtbl.create 16 in
    doc.symbols
    |> List.iter (fun (si : Scip.symbol_information) ->
           if si.symbol <> "" && is_local_symbol si.symbol then
             Hashtbl.replace local_table si.symbol (symbol_info_of_scip si));
    doc.occurrences
    |> List.iter (fun (occ : Scip.occurrence) ->
           match start_of_occurrence occ with
           | None -> ()
           | Some (line0, char0) ->
               let table =
                 if is_local_symbol occ.symbol then local_table
                 else global_table
               in
               Hashtbl.find_opt table occ.symbol
               |> Option.iter (fun symbol_info ->
                      Hashtbl.replace positions
                        (doc.relative_path, line0, char0)
                        symbol_info)))

let load (paths : Fpath.t list) : t =
  let indices =
    paths
    |> List.map (fun path ->
           let content = UFile.read_file path in
           Scip.decode_pb_index (Pbrt.Decoder.of_string content))
  in
  let global_table : (string, symbol_info) Hashtbl.t = Hashtbl.create 4096 in
  indices
  |> List.iter (fun (idx : Scip.index) ->
         idx.external_symbols |> List.iter (register_symbol global_table);
         idx.documents
         |> List.iter (fun (doc : Scip.document) ->
                doc.symbols |> List.iter (register_symbol global_table)));
  let positions : (string * int * int, symbol_info) Hashtbl.t =
    Hashtbl.create 4096
  in
  indices
  |> List.iter (fun (idx : Scip.index) ->
         idx.documents |> List.iter (index_document positions global_table));
  let by_display_name : (string, string) Hashtbl.t = Hashtbl.create 4096 in
  global_table
  |> Hashtbl.iter (fun symbol info ->
         display_name_of_symbol symbol info
         |> Option.iter (fun name -> Hashtbl.add by_display_name name symbol));
  { positions; by_symbol = global_table; by_display_name }

let lookup (t : t) ~(rel_path : string) ~(line0 : int) ~(char0_utf16 : int) :
    symbol_info option =
  Hashtbl.find_opt t.positions (rel_path, line0, char0_utf16)

let find (t : t) (symbol : string) : symbol_info option =
  Hashtbl.find_opt t.by_symbol symbol

let find_by_display_name (t : t) (name : string) : string list =
  Hashtbl.find_all t.by_display_name name

let effective_display_name (t : t) (symbol : string) : string option =
  match find t symbol with
  | Some info when info.display_name <> "" -> Some info.display_name
  | Some _ | None ->
      (* Either [symbol] has no SymbolInformation of its own at all (e.g. a
       * relationship target that was only ever mentioned, never separately
       * registered), or it has one but with an empty display_name (see
       * symbol_info_of_scip). Either way, [simple_name_of_symbol] needs no
       * table lookup - it's a pure parse of the symbol string itself, valid
       * for any well-formed global <symbol>. *)
      simple_name_of_symbol symbol
