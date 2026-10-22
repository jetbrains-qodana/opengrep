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
      t "a local statement of several names is a multiple assignment"
        (fun () ->
          (* a! for a name resolved by Naming_AST, [a,b] for a list *)
          let rec value (e : G.expr) =
            match e.e with
            | G.L (G.Null _) -> "nil"
            | G.Call _ -> "call"
            | G.N (G.Id ((name, _), { id_resolved; _ })) ->
                if Option.is_some !id_resolved then name ^ "!" else name
            | G.Container (G.Tuple, (_, xs, _)) -> "(" ^ values xs ^ ")"
            | G.Container (G.List, (_, xs, _)) -> "[" ^ values xs ^ "]"
            | _ -> "?"
          and values xs = String.concat "," (List.map value xs) in
          let shape (stmt : G.stmt) =
            match stmt.s with
            | G.DefStmt
                ( { name = G.EN (G.Id ((name, _), _)); _ },
                  G.VarDef { vinit; _ } ) -> (
                match vinit with
                | Some e -> "local " ^ name ^ " = " ^ value e
                | None -> "local " ^ name)
            | G.ExprStmt ({ e = G.Assign (lhs, _, rhs); _ }, _) ->
                value lhs ^ " = " ^ value rhs
            | _ -> "?"
          in
          let shapes stmts = List.map shape stmts in
          let check code expected =
            Alcotest.(check (list string))
              code expected
              (shapes (parse_program code))
          in
          check "local a, b = f()" [ "local a"; "local b"; "[a!,b!] = call" ];
          check "local a, b = x" [ "local a"; "local b"; "[a!,b!] = (x,nil)" ];
          check "local a = f()" [ "local a = call" ];
          check "local a, b" [ "local a"; "local b" ];
          (* the names of a local statement are a list, the targets of an
           * assignment a tuple *)
          check "a, b = f()" [ "(a,b) = call" ];
          match parse_pattern "local $A, $B = $F()" with
          | G.Ss stmts ->
              Alcotest.(check (list string))
                "pattern"
                [ "local $A"; "local $B"; "[$A,$B] = call" ]
                (shapes stmts)
          | _ -> Alcotest.fail "expected statements");
      t "an integer numeral is an integer" (fun () ->
          let literals code =
            let literals = ref [] in
            let visitor =
              object
                inherit [_] G.iter as super

                method! visit_literal env lit =
                  (match lit with
                  | G.Int (Some i, _) ->
                      literals := Int64.to_string i :: !literals
                  | G.Float _ -> literals := "float" :: !literals
                  | _ -> ());
                  super#visit_literal env lit
              end
            in
            visitor#visit_program () (parse_program code);
            List.rev !literals
          in
          Alcotest.(check (list string))
            "numerals"
            [ "10"; "31"; "float"; "float"; "float"; "float" ]
            (literals "f(10, 0x1F, 1.5, 1e3, 0x1p4, 99999999999999999999)");
          (* / divides floats, so 0 / 0 is nan and not a division by zero *)
          Alcotest.(check (list string))
            "operands of /" [ "float"; "float" ]
            (literals "nan = 0 / 0"));
      t "the blocks of if statements and labels have tokens" (fun () ->
          let fake = ref 0 in
          let visitor =
            object
              inherit [_] G.iter as super

              method! visit_stmt env stmt =
                (match stmt.s with
                | G.Block (l, _, r) when Tok.is_fake l || Tok.is_fake r ->
                    incr fake
                | _ -> ());
                super#visit_stmt env stmt
            end
          in
          visitor#visit_program ()
            (parse_program
               "if a then elseif b then x() else end\nif c then end\n::l::");
          Alcotest.(check int) "blocks with a fake token" 0 !fake);
      t "the nil of a missing value has a location" (fun () ->
          match (first_statement "a, b = a").s with
          | G.ExprStmt
              ( {
                  e =
                    G.Assign
                      ( _,
                        _,
                        { e = G.Container (G.Tuple, (_, [ _; nil ], _)); _ } );
                  _;
                },
                _ ) ->
              Alcotest.(check bool)
                "location" true
                (Option.is_some
                   (AST_generic_helpers.range_of_any_opt (G.E nil)))
          | _ -> Alcotest.fail "expected (a, b) = (a, nil)");
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
