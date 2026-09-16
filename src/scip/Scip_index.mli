(* Loads one or more SCIP (`index.scip`) protobuf indexes and exposes a
 * position -> symbol lookup table, used to resolve `metavariable-type`
 * against types defined outside the scanned file (see the design doc
 * docs/superpowers/specs/2026-09-15-scip-metavariable-type-design.md). *)

type symbol_info = {
  display_name : string;
      (* The symbol's OWN name, e.g. "adapter" for a local variable or
       * "DbDataAdapter" for a class - per scip.proto: "the symbol
       * `com/example/MyClass#myMethod(+1).` should have the display name
       * `myMethod`". For a variable/parameter occurrence this is NOT its
       * type; use [type_symbol]/[raw_signature] for that. *)
  raw_signature : string option;
  type_symbol : string option;
      (* Best-effort: the SCIP symbol referenced by the first occurrence
       * inside [raw_signature]'s hover text (e.g. the "SQLiteDataAdapter"
       * reference inside the hover "SQLiteDataAdapter adapter" for a local
       * variable). None if the indexer didn't emit signature occurrences.
       * This is what actually identifies the expression's *type* as a SCIP
       * symbol, so its own relationships/display_name can be looked up via
       * [find]. *)
  relationships : (string * bool) list;
      (* (related_symbol, is_implementation) pairs from this symbol's own
       * SCIP SymbolInformation.relationships - populated for type symbols
       * (a class/struct/interface) that extend/implement another type, per
       * scip.proto's Relationship message. Empty for symbols with no
       * recorded relationships (including most local variables). *)
}

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

(* [find t symbol] looks up a symbol by its SCIP symbol string (as found in
 * e.g. [type_symbol] or the `related_symbol` half of [relationships]).
 * Local symbols ("local N") are intentionally not resolvable this way -
 * they're only meaningful within the document that declared them (see
 * Scip_index.ml's is_local_symbol) - but [type_symbol] never points at one
 * in practice, since a type worth walking relationships on is always a
 * global or external symbol. *)
val find : t -> string -> symbol_info option
