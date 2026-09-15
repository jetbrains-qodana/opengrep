(*
   Unit tests for Scip_index: decoding SCIP protobuf indexes into a
   position -> symbol lookup table.

   Fixtures are built in-memory with the ocaml-protoc-generated Scip.make_*
   smart constructors, encoded, and written to real temp files, since
   Scip_index.load only takes Fpath.t paths (see Scip_index.mli).
*)

let t = Testo.create ~category:[ "Scip_index" ]

(*****************************************************************************)
(* Fixture helpers *)
(*****************************************************************************)

let encode_index (index : Scip.index) : string =
  let encoder = Pbrt.Encoder.create () in
  Scip.encode_pb_index index encoder;
  Pbrt.Encoder.to_string encoder

(* Writes each index to its own temp file and calls [f] with a table loaded
   from all of them combined (mirrors --scip-index being repeatable). *)
let with_table (indexes : Scip.index list) (f : Scip_index.t -> unit) : unit =
  let rec go acc = function
    | [] -> f (Scip_index.load (List.rev acc))
    | index :: rest ->
        UTmp.with_temp_file ~contents:(encode_index index) ~suffix:".scip"
          (fun path -> go (path :: acc) rest)
  in
  go [] indexes

let occurrence ~symbol ~line ~start_character ~end_character () : Scip.occurrence
    =
  Scip.make_occurrence ~symbol
    ~typed_range:
      (Scip.Single_line_range
         (Scip.make_single_line_range ~line:(Int32.of_int line)
            ~start_character:(Int32.of_int start_character)
            ~end_character:(Int32.of_int end_character) ()))
    ()

let symbol_info ~symbol ~display_name ?signature_text () :
    Scip.symbol_information =
  let signature_documentation =
    Option.map
      (fun text -> Scip.make_signature ~language:"typescript" ~text ())
      signature_text
  in
  Scip.make_symbol_information ~symbol ~display_name
    ?signature_documentation ()

let document ?(position_encoding = Scip.Utf16_code_unit_offset_from_line_start)
    ~relative_path ~occurrences ~symbols () : Scip.document =
  Scip.make_document ~relative_path ~language:"typescript" ~occurrences
    ~symbols ~position_encoding ()

let index ?(external_symbols = []) (documents : Scip.document list) :
    Scip.index =
  Scip.make_index ~documents ~external_symbols ()

(* Compares by structural equality: Scip_index.symbol_info is a plain record
   of string/string option, no functional values, so (=) is safe here. *)
let show_result = function
  | None -> "None"
  | Some { Scip_index.display_name; raw_signature } ->
      Printf.sprintf "Some (display_name = %S; raw_signature = %s)"
        display_name
        (match raw_signature with
        | None -> "None"
        | Some s -> Printf.sprintf "Some %S" s)

let check_lookup ~msg table ~rel_path ~line0 ~char0_utf16 expected =
  let got = Scip_index.lookup table ~rel_path ~line0 ~char0_utf16 in
  if got <> expected then
    Alcotest.failf "%s: expected %s, got %s" msg (show_result expected)
      (show_result got)

(*****************************************************************************)
(* Tests *)
(*****************************************************************************)

let tests =
  [
    t "single document: resolves an occurrence to its symbol" (fun () ->
        let x_symbol = "scip-typescript npm . . `index.ts`/x." in
        let idx =
          index
            [
              document ~relative_path:"index.ts"
                ~occurrences:
                  [
                    occurrence ~symbol:x_symbol ~line:1 ~start_character:2
                      ~end_character:3 ();
                  ]
                ~symbols:
                  [
                    symbol_info ~symbol:x_symbol ~display_name:"x"
                      ~signature_text:"let x: Foo" ();
                  ]
                ();
            ]
        in
        with_table [ idx ] (fun table ->
            check_lookup ~msg:"hit" table ~rel_path:"index.ts" ~line0:1
              ~char0_utf16:2
              (Some { Scip_index.display_name = "x"; raw_signature = Some "let x: Foo" });
            check_lookup ~msg:"wrong column" table ~rel_path:"index.ts"
              ~line0:1 ~char0_utf16:3 None;
            check_lookup ~msg:"wrong line" table ~rel_path:"index.ts" ~line0:0
              ~char0_utf16:2 None;
            check_lookup ~msg:"wrong file" table ~rel_path:"other.ts" ~line0:1
              ~char0_utf16:2 None));
    t "no coverage: an index with no matching occurrence returns None"
      (fun () ->
        let idx =
          index
            [ document ~relative_path:"empty.ts" ~occurrences:[] ~symbols:[] () ]
        in
        with_table [ idx ] (fun table ->
            check_lookup ~msg:"no occurrences at all" table
              ~rel_path:"empty.ts" ~line0:0 ~char0_utf16:0 None;
            check_lookup ~msg:"unknown file entirely" table
              ~rel_path:"unknown.ts" ~line0:0 ~char0_utf16:0 None));
    t "multiple documents: lookups do not cross-contaminate" (fun () ->
        let a_symbol = "scip-typescript npm . . `a.ts`/a." in
        let b_symbol = "scip-typescript npm . . `b.ts`/b." in
        let idx =
          index
            [
              document ~relative_path:"a.ts"
                ~occurrences:
                  [ occurrence ~symbol:a_symbol ~line:0 ~start_character:0 ~end_character:1 () ]
                ~symbols:[ symbol_info ~symbol:a_symbol ~display_name:"A" () ]
                ();
              document ~relative_path:"b.ts"
                ~occurrences:
                  [ occurrence ~symbol:b_symbol ~line:0 ~start_character:0 ~end_character:1 () ]
                ~symbols:[ symbol_info ~symbol:b_symbol ~display_name:"B" () ]
                ();
            ]
        in
        with_table [ idx ] (fun table ->
            check_lookup ~msg:"a.ts resolves to A" table ~rel_path:"a.ts"
              ~line0:0 ~char0_utf16:0
              (Some { Scip_index.display_name = "A"; raw_signature = None });
            check_lookup ~msg:"b.ts resolves to B" table ~rel_path:"b.ts"
              ~line0:0 ~char0_utf16:0
              (Some { Scip_index.display_name = "B"; raw_signature = None });
            check_lookup ~msg:"same coordinates in an unindexed file" table
              ~rel_path:"c.ts" ~line0:0 ~char0_utf16:0 None));
    t "UTF-16 surrogate pairs: the column is passed through verbatim"
      (fun () ->
        (* "let " (4 code units) + an astral emoji (2 UTF-16 code units as a
           surrogate pair) + "y" at code unit 6. A byte-oriented (UTF-8)
           column for the same source would land elsewhere, since the emoji
           takes 4 bytes in UTF-8 but only 2 UTF-16 code units. Scip_index
           must not recompute this: it just stores/looks up whatever
           char0_utf16 the SCIP occurrence carries. *)
        let y_symbol = "scip-typescript npm . . `index.ts`/y." in
        let idx =
          index
            [
              document ~relative_path:"index.ts"
                ~occurrences:
                  [
                    occurrence ~symbol:y_symbol ~line:0 ~start_character:6
                      ~end_character:7 ();
                  ]
                ~symbols:
                  [ symbol_info ~symbol:y_symbol ~display_name:"y" () ]
                ();
            ]
        in
        with_table [ idx ] (fun table ->
            check_lookup ~msg:"hit at the UTF-16 column" table
              ~rel_path:"index.ts" ~line0:0 ~char0_utf16:6
              (Some { Scip_index.display_name = "y"; raw_signature = None });
            (* The naive UTF-8 byte offset of the same character would be 8
               (4 bytes for the emoji instead of 2 code units); make sure
               that offset is *not* what resolves the symbol. *)
            check_lookup ~msg:"byte offset is not a valid key" table
              ~rel_path:"index.ts" ~line0:0 ~char0_utf16:8 None));
    t "unsupported position encoding: the document is not indexed" (fun () ->
        let x_symbol = "scip-typescript npm . . `index.ts`/x." in
        let idx =
          index
            [
              document ~relative_path:"index.ts"
                ~position_encoding:Scip.Utf8_code_unit_offset_from_line_start
                ~occurrences:
                  [
                    occurrence ~symbol:x_symbol ~line:1 ~start_character:2
                      ~end_character:3 ();
                  ]
                ~symbols:[ symbol_info ~symbol:x_symbol ~display_name:"x" () ]
                ();
            ]
        in
        with_table [ idx ] (fun table ->
            check_lookup ~msg:"UTF-8-encoded document is skipped" table
              ~rel_path:"index.ts" ~line0:1 ~char0_utf16:2 None));
    t "local symbols are scoped per document" (fun () ->
        let local_symbol = "local 0" in
        let idx =
          index
            [
              document ~relative_path:"a.ts"
                ~occurrences:
                  [ occurrence ~symbol:local_symbol ~line:0 ~start_character:0 ~end_character:1 () ]
                ~symbols:
                  [
                    symbol_info ~symbol:local_symbol ~display_name:"localA" ();
                  ]
                ();
              document ~relative_path:"b.ts"
                ~occurrences:
                  [ occurrence ~symbol:local_symbol ~line:0 ~start_character:0 ~end_character:1 () ]
                ~symbols:
                  [
                    symbol_info ~symbol:local_symbol ~display_name:"localB" ();
                  ]
                ();
            ]
        in
        with_table [ idx ] (fun table ->
            check_lookup ~msg:"a.ts's local 0 resolves to localA" table
              ~rel_path:"a.ts" ~line0:0 ~char0_utf16:0
              (Some { Scip_index.display_name = "localA"; raw_signature = None });
            check_lookup ~msg:"b.ts's local 0 resolves to localB" table
              ~rel_path:"b.ts" ~line0:0 ~char0_utf16:0
              (Some { Scip_index.display_name = "localB"; raw_signature = None })));
    t "an occurrence resolves through an external (cross-file) symbol"
      (fun () ->
        let foo_symbol = "scip-typescript npm . . `foo.d.ts`/Foo#" in
        let idx =
          index
            ~external_symbols:
              [ symbol_info ~symbol:foo_symbol ~display_name:"Foo" () ]
            [
              document ~relative_path:"index.ts"
                ~occurrences:
                  [
                    occurrence ~symbol:foo_symbol ~line:2 ~start_character:9
                      ~end_character:12 ();
                  ]
                ~symbols:[] ();
            ]
        in
        with_table [ idx ] (fun table ->
            check_lookup ~msg:"resolves via external_symbols" table
              ~rel_path:"index.ts" ~line0:2 ~char0_utf16:9
              (Some { Scip_index.display_name = "Foo"; raw_signature = None })));
  ]
