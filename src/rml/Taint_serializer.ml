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

let yojson_of_taint_entry ((rule_name, loc) : string * taint_location) : Y.t =
  `Assoc
    [
      ("rule", `String rule_name);
      ("loc", yojson_of_taint_location loc);
    ]

let yojson_of_propagator_entry ((rule_name, loc, locFrom, locTo) : string * taint_location * taint_location * taint_location) : Y.t =
  `Assoc
    [
      ("rule", `String rule_name);
      ("loc", yojson_of_taint_location loc);
      ("locFrom", yojson_of_taint_location locFrom);
      ("locTo", yojson_of_taint_location locTo);
    ]

type taint_entries_block_t = (string * taint_location) list
type taint_propagators_block_t = (string * taint_location * taint_location * taint_location) list
type taint_entries_t = taint_entries_block_t * taint_entries_block_t * taint_entries_block_t * taint_propagators_block_t

let yojson_fields_of_taint_entries ((sources, sinks, sanitizers, propagators) : taint_entries_t)
    : (string * Y.t) list =
  [
    ( "sources",
      `List (List.map yojson_of_taint_entry sources) );
    ( "sinks",
      `List (List.map yojson_of_taint_entry sinks) );
    ( "sanitizers",
      `List (List.map yojson_of_taint_entry sanitizers) );
    ( "propagators",
      `List (List.map yojson_of_propagator_entry propagators) );
  ]
