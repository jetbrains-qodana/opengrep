(* See Scip_index.mli. *)

type symbol_info = { display_name : string; raw_signature : string option }

type t = {
  (* (relative_path, start_line0, start_char0_utf16) -> symbol_info *)
  positions : (string * int * int, symbol_info) Hashtbl.t;
}

(* Per the SCIP grammar (scip.proto, around "<symbol> ::= ... | 'local '
 * <local-id>"), a symbol string starting with "local " is only meaningful
 * within the Document that declared it; the same string can and does mean
 * different things in different documents, so local symbols must be
 * resolved against a per-document table, never the global one. *)
let is_local_symbol (sym : string) : bool = String.starts_with ~prefix:"local " sym

let symbol_info_of_scip (si : Scip.symbol_information) : symbol_info =
  let raw_signature =
    match si.signature_documentation with
    | Some { Scip.text = ""; _ }
    | None ->
        None
    | Some { Scip.text; _ } -> Some text
  in
  { display_name = si.display_name; raw_signature }

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
  { positions }

let lookup (t : t) ~(rel_path : string) ~(line0 : int) ~(char0_utf16 : int) :
    symbol_info option =
  Hashtbl.find_opt t.positions (rel_path, line0, char0_utf16)
