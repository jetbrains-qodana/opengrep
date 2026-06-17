module Y = Yojson.Safe

type ast_format = [ `Json | `Binary ]

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
      ("astBinary", `String (Base64.encode_string ast_binary));
      ("taintBinary", `String (Base64.encode_string taint_binary)) ]

let empty_taint_entries : Taint_serializer.taint_entries_t = ([], [], [], [])

let serialize_empty_ast_with_taint_to_string () =
  serialize_ast_with_taint_to_string [] empty_taint_entries
  |> Y.pretty_to_string
