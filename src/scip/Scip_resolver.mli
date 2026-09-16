(* Wires a loaded SCIP index into the engine's type-resolution hooks (see
 * design doc docs/superpowers/specs/2026-09-15-scip-metavariable-type-design.md,
 * §5.3): Typing.pro_hook_type_of_expr (primary path: reconstruct a
 * best-effort Type.t from the SCIP symbol's raw_signature) and
 * Match_search_mode.hook_scip_metavariable_type_matches (fallback path,
 * consulted by the CondType branch when the primary Type.t-based comparison
 * isn't conclusive: resolve the expression's *type* symbol - not its own
 * display_name, which for a variable/parameter is just its identifier, see
 * Scip_index.symbol_info - and compare that type's name against the raw
 * type:/types: text, walking `is_implementation` relationships so a pattern
 * naming a base type/interface also matches a subtype). All SCIP-specific
 * logic - including this fallback's string matching and relationship walk -
 * lives in this module, exactly like a real Pro engine module would only
 * expose an install-style entry point and keep its internals out of the
 * core engine files. *)

(* [install index] sets both hooks. Call once per process, after
 * Scip_index.load, before any file is matched. *)
val install : Scip_index.t -> unit
