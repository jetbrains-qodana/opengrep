open Taint_location
module Y = Yojson.Safe

let yojson_of_taint_location (loc : taint_location) : Y.t =
  `Assoc
    [
      ("file_path", `String loc.file_path);
      ("line", `Int loc.line);
      ("col", `Int loc.col);
      ("offsetStart", `Int loc.offsetStart);
      ("offsetEnd", `Int loc.offsetEnd);
    ]

type taint_entry = {
  rule : string;
  loc : taint_location;
  pattern : string option;
}

type taint_propagator_entry = {
  rule : string;
  loc : taint_location;
  locFrom : taint_location;
  locTo : taint_location;
  pattern : string option;
}

let pattern_field = function
  | None -> []
  | Some pattern -> [ ("pattern", `String pattern) ]

let yojson_of_taint_entry ({ rule; loc; pattern } : taint_entry) : Y.t =
  let fields =
    [ ("rule", `String rule) ]
    @ pattern_field pattern
    @ [ ("loc", yojson_of_taint_location loc) ]
  in
  `Assoc fields

let yojson_of_propagator_entry
    ({ rule; loc; locFrom; locTo; pattern } : taint_propagator_entry) : Y.t =
  let fields =
    [ ("rule", `String rule) ]
    @ pattern_field pattern
    @ [
        ("loc", yojson_of_taint_location loc);
        ("locFrom", yojson_of_taint_location locFrom);
        ("locTo", yojson_of_taint_location locTo);
      ]
  in
  `Assoc fields

type taint_entries_block_t = taint_entry list
type taint_propagators_block_t = taint_propagator_entry list

type taint_entries_t =
  taint_entries_block_t
  * taint_entries_block_t
  * taint_entries_block_t
  * taint_propagators_block_t

let yojson_fields_of_taint_entries
    ((sources, sinks, sanitizers, propagators) : taint_entries_t) :
    (string * Y.t) list =
  [
    ("sources", `List (List.map yojson_of_taint_entry sources));
    ("sinks", `List (List.map yojson_of_taint_entry sinks));
    ("sanitizers", `List (List.map yojson_of_taint_entry sanitizers));
    ("propagators", `List (List.map yojson_of_propagator_entry propagators));
  ]
