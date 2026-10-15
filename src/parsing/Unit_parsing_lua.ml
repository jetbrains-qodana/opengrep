module G = AST_generic

let t = Testo.create

(*****************************************************************************)
(* Purpose *)
(*****************************************************************************)
(* Checks on the generic AST built for Lua programs and patterns *)

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)

let parse_program code =
  UTmp.with_temp_file ~contents:code ~suffix:".lua" (fun file ->
      Parse_target.parse_and_resolve_name_strict Lang.Lua file)

let parse_pattern code =
  match Parse_pattern.parse_pattern Lang.Lua code with
  | Ok pattern -> pattern
  | Error msg -> Alcotest.failf "the pattern %S does not parse: %s" code msg

let parameters_of_first_function (any : G.any) =
  let params = ref None in
  let visitor =
    object
      inherit [_] G.iter as super

      method! visit_function_definition env def =
        if Option.is_none !params then
          params := Some (Tok.unbracket def.fparams);
        super#visit_function_definition env def
    end
  in
  visitor#visit_any () any;
  match !params with
  | Some params -> params
  | None -> Alcotest.fail "no function definition"

let if_conditions (any : G.any) =
  let conditions = ref [] in
  let visitor =
    object
      inherit [_] G.iter as super

      method! visit_condition env cond =
        (match cond with
        | G.Cond { e = G.N (G.Id ((name, _), _)); _ } ->
            conditions := name :: !conditions
        | _ -> ());
        super#visit_condition env cond
    end
  in
  visitor#visit_any () any;
  List.rev !conditions

let unary_operators (any : G.any) =
  let operators = ref [] in
  let visitor =
    object
      inherit [_] G.iter as super

      method! visit_expr env e =
        (match e.e with
        | G.Call ({ e = G.IdSpecial (G.Op op, _); _ }, (_, [ _ ], _)) ->
            operators := op :: !operators
        | _ -> ());
        super#visit_expr env e
    end
  in
  visitor#visit_any () any;
  List.rev !operators

let string_values (any : G.any) =
  let values = ref [] in
  let visitor =
    object
      inherit [_] G.iter as super

      method! visit_literal env lit =
        (match lit with
        | G.String (_, (value, _), _) -> values := value :: !values
        | _ -> ());
        super#visit_literal env lit
    end
  in
  visitor#visit_any () any;
  List.rev !values

(* "t.x" for t.x, "(t.a).b" for t.a.b, "?" for any other shape *)
let rec field_access (e : G.expr) =
  match e.e with
  | G.N (G.Id ((name, _), _)) -> name
  | G.DotAccess (obj, _, G.FN (G.Id ((field, _), _))) -> (
      match obj.e with
      | G.DotAccess _ -> Printf.sprintf "(%s).%s" (field_access obj) field
      | _ -> Printf.sprintf "%s.%s" (field_access obj) field)
  | _ -> "?"

let first_statement code =
  match parse_program code with
  | stmt :: _ -> stmt
  | [] -> Alcotest.fail "no statement"

(*****************************************************************************)
(* Tests *)
(*****************************************************************************)

let tests =
  Testo.categorize "parsing_lua"
    [
      t "a vararg parameter is a rest parameter" (fun () ->
          let ast = parse_program "local function f(a, ...) return ... end" in
          (* the conversion fails on a pattern construct *)
          AST_generic_to_v1.program ast |> ignore;
          match parameters_of_first_function (G.Pr ast) with
          | [ G.Param _; G.ParamRest _ ] -> ()
          | _ -> Alcotest.fail "expected a parameter and a rest parameter");
      t "a vararg parameter in a pattern is an ellipsis" (fun () ->
          match
            parameters_of_first_function (parse_pattern "function $F(...) end")
          with
          | [ G.ParamEllipsis _ ] -> ()
          | _ -> Alcotest.fail "expected an ellipsis parameter");
      t "elseif branches keep their order" (fun () ->
          let ast =
            parse_program
              "if a then x() elseif b then x() elseif c then x() elseif d then \
               x() end"
          in
          Alcotest.(check (list string))
            "conditions" [ "a"; "b"; "c"; "d" ] (if_conditions (G.Pr ast)));
      t "not is a logical negation" (fun () ->
          match unary_operators (G.Pr (parse_program "local b = not a")) with
          | [ G.Not ] -> ()
          | ops ->
              Alcotest.failf "expected Not, got %s"
                (String.concat ", " (List.map G.show_operator ops)));
      t "strings do not keep their delimiters" (fun () ->
          let ast =
            parse_program
              "local a = \"x\"\n\
               local b = 'x'\n\
               local c = [[x]]\n\
               local d = [==[x]==]\n\
               local e = [[\n\
               x]]\n"
          in
          Alcotest.(check (list string))
            "values"
            [ "x"; "x"; "x"; "x"; "x" ]
            (string_values (G.Pr ast)));
      t "a field read is a field access" (fun () ->
          match (first_statement "local a = t.a.b").s with
          | G.DefStmt (_, G.VarDef { vinit = Some e; _ }) ->
              Alcotest.(check string) "read" "(t.a).b" (field_access e)
          | _ -> Alcotest.fail "expected a local definition");
      t "a field store is a field access" (fun () ->
          match (first_statement "t.a.b = 1").s with
          | G.ExprStmt ({ e = G.Assign (target, _, _); _ }, _) ->
              Alcotest.(check string) "store" "(t.a).b" (field_access target)
          | _ -> Alcotest.fail "expected an assignment");
    ]
