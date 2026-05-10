type ast_format = [ `Json | `Binary ]
type mode = [ `Taint | `All ]

type t = {
  num_domains : int;
  format : ast_format;
  files : Fpath.t list;
  rules : Rule.t list;
  mode : mode;
}
[@@deriving show]