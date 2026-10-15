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
    ]
