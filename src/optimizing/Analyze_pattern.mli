module MvarSet = Common2.StringSet

type mvars = MvarSet.t

val extract_specific_strings : ?lang:Lang.t -> Pattern.t -> string list

(*
   Extract strings and metavariables that occur in the source code.
*)
val extract_strings_and_mvars :
  ?lang:Lang.t -> Pattern.t -> string list * Metavariable.mvar list

(*
   Extract metavariables that occur in an "id position" so that, if we
   encounter a `metavariable-regex` operator on any of those metavariables,
   we can use the corresponding `regex` for pre-filtering.
*)
val extract_mvars_in_id_position : ?lang:Lang.t -> Pattern.t -> mvars

(*
   Extract metavariables that appear as the entire content of a string-literal
   pattern (e.g. the pattern '"$ARG0"').  When we also have a
   `metavariable-regex` on such a metavariable, the regex value must be present
   verbatim in the source file, so we can use it for pre-filtering.
*)
val extract_mvars_in_string_position : ?lang:Lang.t -> Pattern.t -> mvars
