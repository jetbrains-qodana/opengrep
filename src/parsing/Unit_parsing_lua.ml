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

let binary_operators (any : G.any) =
  let operators = ref [] in
  let visitor =
    object
      inherit [_] G.iter as super

      method! visit_expr env e =
        (match e.e with
        | G.Call ({ e = G.IdSpecial (G.Op op, _); _ }, (_, [ _; _ ], _)) ->
            operators := op :: !operators
        | _ -> ());
        super#visit_expr env e
    end
  in
  visitor#visit_any () any;
  List.rev !operators

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

(* "T.m" for the name T.m, "?" for a name that is not made of identifiers *)
let name_text (name : G.name) =
  match name with
  | G.Id ((s, _), _) -> s
  | G.IdQualified
      { name_last = last, _; name_middle = Some (G.QDots qualifiers); _ } ->
      String.concat "."
        (List.map (fun ((s, _), _) -> s) qualifiers @ [ fst last ])
  | G.IdQualified { name_last = last, _; name_middle = None; _ } -> fst last
  | G.IdQualified _ -> "?"

(* Parsing an invalid pattern must fail with a parse error, never with
 * another exception. tree-sitter recovers from syntax errors by inserting
 * empty tokens, which crashed the translation of strings and the checks
 * of metavariables. *)
let check_pattern_does_not_crash code =
  match Parse_pattern.parse_pattern Lang.Lua code with
  | Ok any ->
      (* nor accept the pattern with an empty name inserted by tree-sitter *)
      let visitor =
        object
          inherit [_] G.iter_no_id_info as super

          method! visit_ident env ((s, _) as id) =
            if s = "" then
              Alcotest.failf "the pattern %S has an empty name" code;
            super#visit_ident env id
        end
      in
      visitor#visit_any () any
  | Error _ -> ()
  | exception Parsing.Parse_error -> ()
  | exception Failure "no pattern found" -> ()
  | exception exn ->
      Alcotest.failf "the pattern %S raised %s" code (Printexc.to_string exn)

(* the prefixes of a pattern that end at a delimiter *)
let prefixes code =
  let delimiters = " \n\t(){}[],.:\"'=" in
  String.to_seq code
  |> Seq.mapi (fun i c -> (i, c))
  |> Seq.filter (fun (_, c) -> String.contains delimiters c)
  |> Seq.map (fun (i, _) -> String.sub code 0 (i + 1))
  |> List.of_seq

(*****************************************************************************)
(* Tests *)
(*****************************************************************************)

let tests =
  Testo.categorize "parsing_lua"
    [
      t "an invalid pattern is a parse error, not a crash" (fun () ->
          [
            (* a missing string argument: o:m"" *)
            "{..., $KEY: $VALUE, ...}";
            "$X:$Y";
            "f(";
            "x = \"";
            "local a <";
            "function $F(";
            "{ [";
          ]
          |> List.iter check_pattern_does_not_crash);
      t "the prefixes of the Lua pattern tests do not crash" (fun () ->
          Common2.glob "tests/patterns/lua/*.sgrep"
          @ Common2.glob "tests/patterns/POLYGLOT/*.sgrep"
          |> List.iter (fun file ->
                 let code = UFile.read_file (Fpath.v file) in
                 code :: prefixes code
                 |> List.iter check_pattern_does_not_crash));
      t "a pattern starting with # is a length expression, not a shebang"
        (fun () ->
          match parse_pattern "#$T > 0" with
          | G.E
              {
                e =
                  G.Call
                    ( { e = G.IdSpecial (G.Op G.Gt, _); _ },
                      ( _,
                        G.Arg
                          {
                            e =
                              G.Call
                                ({ e = G.IdSpecial (G.Op G.Length, _); _ }, _);
                            _;
                          }
                        :: _,
                        _ ) );
                _;
              } ->
              ()
          | _ -> Alcotest.fail "expected the expression #$T > 0");
      t "the Lua pattern tests are not empty patterns" (fun () ->
          (* e.g. '#$T' was parsed as a shebang line followed by nothing *)
          Common2.glob "tests/patterns/lua/*.sgrep"
          |> List.iter (fun file ->
                 match parse_pattern (UFile.read_file (Fpath.v file)) with
                 | G.Ss []
                 | G.Pr [] ->
                     Alcotest.failf "%s is an empty pattern" file
                 | _ -> ()));
      t "a metavariable alone is a statement in a sequence" (fun () ->
          match parse_pattern "foo()\n$S\nbar()" with
          | G.Ss
              [
                { s = G.ExprStmt ({ e = G.Call _; _ }, _); _ };
                { s = G.ExprStmt ({ e = G.N (G.Id (("$S", _), _)); _ }, _); _ };
                { s = G.ExprStmt ({ e = G.Call _; _ }, _); _ };
              ] ->
              ()
          | _ -> Alcotest.fail "expected three statements");
      t "a keyword alone is not a metavariable statement" (fun () ->
          (match parse_pattern "not $X" with
          | G.E { e = G.Call ({ e = G.IdSpecial (G.Op G.Not, _); _ }, _); _ }
            ->
              ()
          | _ -> Alcotest.fail "expected the expression not $X");
          (match parse_pattern "true" with
          | G.E { e = G.L (G.Bool (true, _)); _ } -> ()
          | _ -> Alcotest.fail "expected true");
          match parse_pattern "nil" with
          | G.E { e = G.L (G.Null _); _ } -> ()
          | _ -> Alcotest.fail "expected nil");
      t "an anonymous function is not a statement" (fun () ->
          (* tree-sitter inserts the missing name of a function declaration:
           * it was DefStmt with the name "" *)
          [
            "foo()\nfunction(...)\n  ...\nend";
            "function()\n  ...\nend\nbar()";
            "local f = 1\nfunction($X) return $X end";
          ]
          |> List.iter (fun code ->
                 match Parse_pattern.parse_pattern Lang.Lua code with
                 | exception Parsing.Parse_error -> ()
                 | Error _ -> ()
                 | Ok _ -> Alcotest.failf "the pattern %S parses" code);
          (* alone, it is an expression *)
          match parse_pattern "function(...)\n  ...\nend" with
          | G.E { e = G.Lambda _; _ } -> ()
          | _ -> Alcotest.fail "expected an anonymous function");
      t "an integer that doesn't fit in 64 bits is read as Lua does" (fun () ->
          let literal code =
            match parse_pattern code with
            | G.E { e = G.L (G.Int (Some i, _)); _ } -> Int64.to_string i
            | G.E { e = G.L (G.Float (Some f, _)); _ } -> Printf.sprintf "%g" f
            | _ -> "?"
          in
          (* a decimal integer is a float, a hex one keeps its low 64 bits *)
          Alcotest.(check (list string))
            "literals"
            [ "1e+22"; "578437695752307201"; "-1"; "9223372036854775807" ]
            (List.map literal
               [
                 "10000000000000000000000";
                 "0x13121110090807060504030201";
                 "0xffffffffffffffff";
                 "9223372036854775807";
               ]));
      t "an expression pattern can end with a semicolon" (fun () ->
          match parse_pattern "$X + 1;" with
          | G.E { e = G.Call ({ e = G.IdSpecial (G.Op G.Plus, _); _ }, _); _ }
            ->
              ()
          | _ -> Alcotest.fail "expected the expression $X + 1");
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
      t "line breaks in long strings are newlines" (fun () ->
          let ast =
            parse_program
              "local a = [[\r\nx\r\ny]]\n\
               local b = [[\rx\ry]]\n\
               local c = [[\n\rx\n\ry]]\n"
          in
          Alcotest.(check (list string))
            "values" [ "x\ny"; "x\ny"; "x\ny" ] (string_values (G.Pr ast)));
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
      t "a method definition keeps the method name" (fun () ->
          match (first_statement "function T:m(x) return x end").s with
          | G.DefStmt ({ name = G.EN name; _ }, G.FuncDef _) ->
              Alcotest.(check string) "name" "T.m" (name_text name)
          | _ -> Alcotest.fail "expected a function definition");
      t "a method definition takes self as its first parameter" (fun () ->
          match
            parameters_of_first_function
              (G.Pr (parse_program "function T:m(x) return x end"))
          with
          | [
           G.Param { pname = Some ("self", _); _ };
           G.Param { pname = Some ("x", _); _ };
          ] ->
              ()
          | _ -> Alcotest.fail "expected the parameters self and x");
      t "self refers to the self parameter" (fun () ->
          let resolved = ref [] in
          let visitor =
            object
              inherit [_] G.iter as super

              method! visit_expr env e =
                (match e.e with
                | G.N (G.Id (("self", _), info)) ->
                    resolved := Option.is_some !(info.id_resolved) :: !resolved
                | _ -> ());
                super#visit_expr env e
            end
          in
          visitor#visit_program ()
            (parse_program "function T:m() return self end");
          Alcotest.(check (list bool)) "self" [ true ] !resolved);
      t "self outside a method is a plain name" (fun () ->
          (* self! for a self resolved by Naming_AST *)
          let selves code =
            let selves = ref [] in
            let visitor =
              object
                inherit [_] G.iter as super

                method! visit_expr env e =
                  (match e.e with
                  | G.N (G.Id (("self", _), info)) ->
                      selves :=
                        (if Option.is_some !(info.id_resolved) then "self!"
                         else "self")
                        :: !selves
                  | G.IdSpecial (G.Self, _) -> selves := "special" :: !selves
                  | _ -> ());
                  super#visit_expr env e
              end
            in
            visitor#visit_program () (parse_program code);
            List.rev !selves
          in
          Alcotest.(check (list string))
            "function" [ "self" ]
            (selves "function f() return self end");
          Alcotest.(check (list string))
            "local" [ "self!" ]
            (selves "local self = {}\nreturn self"));
      t "repeat until loops while the condition is false" (fun () ->
          match (first_statement "repeat x() until c").s with
          | G.DoWhile
              ( _,
                _,
                {
                  e =
                    G.Call
                      ( { e = G.IdSpecial (G.Op G.Not, _); _ },
                        (_, [ G.Arg { e = G.N (G.Id (("c", _), _)); _ } ], _) );
                  _;
                } ) ->
              ()
          | _ -> Alcotest.fail "expected a do-while on not c");
      t "a numeric for iterates over a range" (fun () ->
          match (first_statement "for i = a, b, c do end").s with
          | G.For
              ( _,
                G.ForEach
                  ( G.PatId (("i", _), _),
                    _,
                    {
                      e =
                        G.Call
                          ( { e = G.IdSpecial (G.Op G.Range, _); _ },
                            (_, [ G.Arg _; G.Arg _; G.Arg _ ], _) );
                      _;
                    } ),
                _ ) ->
              ()
          | _ -> Alcotest.fail "expected i over Range(a, b, c)");
      t "a multiple assignment assigns tuples" (fun () ->
          let shape code =
            match (first_statement code).s with
            | G.ExprStmt ({ e = G.Assign (lhs, _, rhs); _ }, _) -> (
                let names (e : G.expr) =
                  match e.e with
                  | G.Container (G.Tuple, (_, xs, _)) ->
                      "(" ^ String.concat "," (List.map field_access xs) ^ ")"
                  | _ -> field_access e
                in
                match rhs.e with
                | G.Call _ -> names lhs ^ " = call"
                | _ -> names lhs ^ " = " ^ names rhs)
            | _ -> "?"
          in
          Alcotest.(check (list string))
            "assignments"
            [ "(x,y) = (y,x)"; "(x,y) = call"; "x = y"; "(a) = (a,b,c)" ]
            (List.map shape
               [ "x, y = y, x"; "x, y = f()"; "x = y"; "a = a, b, c" ]));
      t "missing values of a multiple assignment are nil" (fun () ->
          let value (e : G.expr) =
            match e.e with
            | G.L (G.Null _) -> "nil"
            | G.Call _ -> "call"
            | _ -> field_access e
          in
          let shape code =
            match (first_statement code).s with
            | G.ExprStmt
                ( {
                    e = G.Assign ({ e = G.Container (G.Tuple, _); _ }, _, rhs);
                    _;
                  },
                  _ ) -> (
                match rhs.e with
                | G.Container (G.Tuple, (_, xs, _)) ->
                    "(" ^ String.concat "," (List.map value xs) ^ ")"
                | _ -> value rhs)
            | _ -> "?"
          in
          Alcotest.(check (list string))
            "values"
            [ "(t,nil)"; "(x,y,nil)"; "call"; "(x,f.g,nil)"; "..." ]
            (List.map shape
               [
                 "a, b = t";
                 "a, b, c = x, y";
                 "a, b = f()";
                 "a, b, c = x, f.g";
                 (* the main chunk of Lua takes varargs *)
                 "a, b = ...";
               ]));
      t "the body of a function goes from its parameters to its end"
        (fun () ->
          let bodies = ref [] in
          let visitor =
            object
              inherit [_] G.iter as super

              method! visit_function_definition env def =
                (match def.fbody with
                | G.FBStmt { s = G.Block (l, _, r); _ } ->
                    bodies :=
                      (Tok.content_of_tok l ^ " " ^ Tok.content_of_tok r)
                      :: !bodies
                | _ -> ());
                super#visit_function_definition env def
            end
          in
          visitor#visit_program ()
            (parse_program
               "function f(a) end\n\
                local function g() end\n\
                h = function(...) end");
          Alcotest.(check (list string))
            "bodies" [ ") end"; ") end"; ") end" ] (List.rev !bodies));
      t "a table constructor is a dictionary of key-value pairs" (fun () ->
          let fields (any : G.any) =
            let fields = ref [] in
            let visitor =
              object
                inherit [_] G.iter as super

                method! visit_expr env e =
                  (match e.e with
                  | G.Container (G.Dict, (_, xs, _)) -> fields := xs
                  | _ -> ());
                  super#visit_expr env e
              end
            in
            visitor#visit_any () any;
            let field (e : G.expr) =
              match e.e with
              | G.Container
                  ( G.Tuple,
                    (_, [ { e = G.L (G.String (_, (k, _), _)); _ }; _ ], _) ) ->
                  k ^ " = value"
              | G.Container (G.Tuple, (_, [ { e = G.L (G.Int i); _ }; _ ], _))
                ->
                  (match Parsed_int.to_int_opt i with
                  | Some i -> string_of_int i
                  | None -> "?")
                  ^ " = value"
              | G.Ellipsis _ -> "..."
              | _ -> "?"
            in
            List.map field !fields
          in
          (* a positional field has the next integer key, from 1 *)
          Alcotest.(check (list string))
            "program"
            [ "x = value"; "1 = value"; "y = value"; "2 = value" ]
            (fields (G.Pr (parse_program "t = { x = 1, 2, [\"y\"] = 3, 4 }")));
          Alcotest.(check (list string))
            "pattern" [ "..."; "1 = value" ]
            (fields (parse_pattern "{ ..., $X }")));
      t "~ is a bitwise xor and ^ a power" (fun () ->
          match
            binary_operators
              (G.Pr (parse_program "local a = x ~ y\nlocal b = x ^ y"))
          with
          | [ G.BitXor; G.Pow ] -> ()
          | ops ->
              Alcotest.failf "expected BitXor, Pow, got %s"
                (String.concat ", " (List.map G.show_operator ops)));
      t "a vararg expression is the name of the vararg parameter" (fun () ->
          match
            (first_statement
               "local function f(...) return select(\"#\", ...) end")
              .s
          with
          | G.DefStmt (_, G.FuncDef { fbody = G.FBStmt body; _ }) -> (
              let args = ref [] in
              let visitor =
                object
                  inherit [_] G.iter as super

                  method! visit_expr env e =
                    (match e.e with
                    | G.Call (_, (_, xs, _)) -> args := xs
                    | _ -> ());
                    super#visit_expr env e
                end
              in
              visitor#visit_stmt () body;
              match !args with
              (* the name is not resolved: Naming_AST declares no rest
                 parameter in any language *)
              | [ _; G.Arg { e = G.N (G.Id (("...", _), _)); _ } ] -> ()
              | _ -> Alcotest.fail "expected the name ...")
          | _ -> Alcotest.fail "expected a local function");
    ]
