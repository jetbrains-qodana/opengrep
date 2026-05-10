type ast_format = [ `Json | `Binary ]
type mode = [ `Taint | `All ]

type parsed_file = {
  ast : AST_generic.program;
  lang : Lang.t;
  xlang : Xlang.t;
  file : Fpath.t;
  taint_entries : Taint_serializer.taint_entries_t;
  (* Empty unless [parse_file] was called with [~with_diagnostics:true]. *)
  matches : Core_match.t list;
  (* Errors raised by the search/taint engine. Empty unless [with_diagnostics]
   * was true. *)
  errors : Core_error.t list;
}

type t = {
  num_domains : int;
  files : Fpath.t list;
  rules : Rule.t list;
  mode : mode;
  on_parsed : parsed_file -> unit;
}
[@@deriving show]