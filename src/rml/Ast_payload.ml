module Y = Yojson.Safe

type ast_format = [ `Json | `Binary ]

(* Standard MIME base64 alphabet plus '=' padding. Used to wrap the binary
 * AST/taint blobs as JSON-safe strings. *)
let base64_table =
  "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/"

let base64_encode (data : string) : string =
  let len = String.length data in
  let buf = Buffer.create (((len + 2) / 3) * 4) in
  let get i = if i < len then Char.code data.[i] else 0 in
  let rec loop i =
    if i >= len then ()
    else
      let b1 = get i in
      let b2 = get (i + 1) in
      let b3 = get (i + 2) in
      let triple = (b1 lsl 16) lor (b2 lsl 8) lor b3 in
      Buffer.add_char buf base64_table.[(triple lsr 18) land 0x3F];
      Buffer.add_char buf base64_table.[(triple lsr 12) land 0x3F];
      if i + 1 < len then
        Buffer.add_char buf base64_table.[(triple lsr 6) land 0x3F]
      else
        Buffer.add_char buf '=';
      if i + 2 < len then
        Buffer.add_char buf base64_table.[triple land 0x3F]
      else
        Buffer.add_char buf '=';
      loop (i + 3)
  in
  loop 0;
  Buffer.contents buf

let serialize_ast_to_json_string (ast : AST_generic.program) : string =
  let v1_ast = AST_generic_to_v1.program ast in
  Ast_generic_v1_j.string_of_program v1_ast

let ast_to_yojson (ast : AST_generic.program) : Y.t =
  ast |> serialize_ast_to_json_string |> Y.from_string

let serialize_ast_with_taint_to_string (ast : AST_generic.program)
    (taint_entries : Taint_serializer.taint_entries_t) : Y.t =
  let ast_json = ast_to_yojson ast in
  let taint_value =
    `Assoc (Taint_serializer.yojson_fields_of_taint_entries taint_entries)
  in
  `Assoc [ ("ast", ast_json); ("taint", taint_value) ]

let serialize_ast_with_taint_to_binary_string (ast : AST_generic.program)
    (taint_entries : Taint_serializer.taint_entries_t) : Y.t =
  let v1_ast = AST_generic_to_v1.program ast in
  let pool_builder = Ast_binary_serializer.create_string_pool_builder () in
  Ast_binary_serializer.collect_program v1_ast pool_builder;
  Ast_binary_serializer.collect_taint_entries taint_entries pool_builder;
  let pool = Ast_binary_serializer.build_string_pool pool_builder in
  let ast_binary = Ast_binary_serializer.serialize_program v1_ast pool in
  let taint_binary =
    Ast_binary_serializer.serialize_taint_entries taint_entries pool
  in
  `Assoc
    [ ("stringPool", Ast_binary_serializer.string_pool_to_yojson pool);
      ("astBinary", `String (base64_encode ast_binary));
      ("taintBinary", `String (base64_encode taint_binary)) ]

let empty_taint_entries : Taint_serializer.taint_entries_t = ([], [], [], [])

let serialize_empty_ast_with_taint_to_string () =
  serialize_ast_with_taint_to_string [] empty_taint_entries
  |> Y.pretty_to_string
