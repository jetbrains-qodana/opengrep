(* Pure serialization of an [AST_generic.program] (and optional taint
 * analysis output) to either a JSON or a binary-with-string-pool payload.
 *
 * No I/O, no mutability, no engine dependencies. Functions in this module
 * are safe to call from any worker domain. *)

type ast_format = [ `Json | `Binary ]

(** Serialize an AST to its v1 JSON string. *)
val serialize_ast_to_json_string : AST_generic.program -> string

(** Serialize an AST to a Yojson value (parsed back from the JSON string). *)
val ast_to_yojson : AST_generic.program -> Yojson.Safe.t

(** Build the JSON payload for a parsed file: [{"ast": ..., "taint": ...}]. *)
val serialize_ast_with_taint_to_string :
  AST_generic.program -> Taint_serializer.taint_entries_t -> Yojson.Safe.t

(** Build the binary payload for a parsed file: a string-pool plus base64
    encoded AST and taint binary blobs:
    [{"stringPool": ..., "astBinary": "...", "taintBinary": "..."}]. *)
val serialize_ast_with_taint_to_binary_string :
  AST_generic.program -> Taint_serializer.taint_entries_t -> Yojson.Safe.t

(** Empty taint-entries record (no sources, sinks, sanitizers, or
    propagators). The default for a parsed file with no taint analysis. *)
val empty_taint_entries : Taint_serializer.taint_entries_t

(** Pretty-printed JSON for an empty AST + empty taint payload. Used by the
    LSP server as a fallback when no real result is available. *)
val serialize_empty_ast_with_taint_to_string : unit -> string
