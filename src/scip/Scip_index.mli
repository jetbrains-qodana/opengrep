(* Loads one or more SCIP (`index.scip`) protobuf indexes and exposes a
 * position -> symbol lookup table, used to resolve `metavariable-type`
 * against types defined outside the scanned file (see the design doc
 * docs/superpowers/specs/2026-09-15-scip-metavariable-type-design.md). *)

type symbol_info = { display_name : string; raw_signature : string option }
type t

(* [load paths] decodes every SCIP index at [paths] and merges them into a
 * single lookup table. Raises if a path cannot be read or does not contain
 * a valid SCIP index (fail fast: the user passed --scip-index explicitly). *)
val load : Fpath.t list -> t

(* [lookup t ~rel_path ~line0 ~char0_utf16] returns the symbol occurring at
 * that (0-based line, 0-based UTF-16 code unit column) in the document at
 * [rel_path], if any SCIP index covers that position. [rel_path] must match
 * the `relative_path` as recorded in the SCIP index (project-root-relative). *)
val lookup :
  t -> rel_path:string -> line0:int -> char0_utf16:int -> symbol_info option
