(* Ruin0x11
 *
 * Copyright (c) 2020 Semgrep Inc.
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public License
 * version 2.1 as published by the Free Software Foundation, with the
 * special exception on linking described in file LICENSE.
 *
 * This library is distributed in the hope that it will be useful, but
 * WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the file
 * LICENSE for more details.
 *)
open Common
open Fpath_.Operators
module CST = Tree_sitter_lua_new.CST
module H = Parse_tree_sitter_helpers
module G = AST_generic
module H2 = AST_generic_helpers
module Log = Log_parser_lua.Log

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(* Lua parser using tree-sitter-lang/semgrep-lua-new, based on
 * tree-sitter-grammars/tree-sitter-lua, and converting directly to
 * AST_generic.
 *
 * See languages/lua/migration/grammar-differences.md for the differences
 * with the previous grammar and how they are translated.
 *)

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)
type mode = Pattern | Target
type env = mode H.env

let token = H.token
let str = H.str
let fb = Tok.unsafe_fake_bracket

(* TODO: remove once every construct is translated *)
let todo (_env : env) _ = failwith "Parse_lua_tree_sitter: not implemented yet"

(* x, y, z *)
let sep_list (x, xs) = x :: List_.map snd xs
let identifier (env : env) (tok : CST.identifier) : G.ident = str env tok

(* local a, b = x gives a = x and b without a value *)
let mk_vars xs ys =
  let rec aux xs ys =
    match (xs, ys) with
    | [], [] -> []
    | x :: xs, [] -> (x, G.VarDef G.empty_var) :: aux xs ys
    | x :: xs, y :: ys ->
        (x, G.VarDef { G.vinit = Some y; vtype = None; vtok = G.no_sc })
        :: aux xs ys
    | [], _y :: _ys -> []
  in
  aux xs ys

(* several targets or values are assigned as tuples, as in Python:
 * x, y = y, x is (x, y) = (y, x) and x, y = f() is (x, y) = f().
 * x = a, b keeps the targets a tuple, (x) = (a, b), since x gets the
 * first value and not a tuple *)
let assign (targets : G.expr list) (equal : Tok.t) (values : G.expr list) :
    G.stmt =
  let tuple xs = G.Container (G.Tuple, Tok.unsafe_fake_bracket xs) |> G.e in
  (* a call or ... gives all its values, as do $X and ... in a pattern;
   * any other expression gives one value *)
  let multiple_values (e : G.expr) =
    match e.e with
    | G.Call ({ e = G.IdSpecial (G.Op _, _); _ }, _) -> false
    | G.Call _
    | G.Ellipsis _
    | G.N (G.Id (("...", _), _)) ->
        true
    | G.N (G.Id ((s, _), _)) -> G.is_metavar_name s
    | _ -> false
  in
  (* targets left without a value get nil: a, b = x is (a, b) = (x, nil) *)
  let values =
    let missing = List.length targets - List.length values in
    let last_gives_all =
      match List.rev values with
      | last :: _ -> multiple_values last
      | [] -> false
    in
    if missing > 0 && not last_gives_all then
      values @ List.init missing (fun _ -> G.L (G.Null (G.fake "nil")) |> G.e)
    else values
  in
  let lhs, rhs =
    match (targets, values) with
    | [ x ], [ v ] -> (x, v)
    | _, [ v ] -> (tuple targets, v)
    | _, _ -> (tuple targets, tuple values)
  in
  G.ExprStmt (G.Assign (lhs, equal, rhs) |> G.e, G.sc) |> G.s

(*****************************************************************************)
(* Expressions *)
(*****************************************************************************)

(* "..." is a reference to the vararg parameter in a program but an ellipsis in
 * a pattern *)
let vararg_expression (env : env) tok =
  match env.extra with
  | Pattern -> G.Ellipsis tok
  | Target -> G.N (G.Id (("...", tok), G.empty_id_info ()))

(* The content of a long string such as [[x]] or [==[x]==]: a newline right
 * after the opening bracket is not part of the string, and each line break,
 * \r\n, \n\r, \n or \r, is a \n. *)
let long_string_content s =
  let s = Str.global_replace (Str.regexp "\r\n\\|\n\r\\|\r") "\n" s in
  if String.starts_with ~prefix:"\n" s then Str.string_after s 1 else s

(* The value of a short string is its source text between the quotes:
 * escape sequences are kept as they are, as with the previous grammar. *)
let map_string (env : env) (x : CST.string_) : G.expr =
  let quoted l (parts : (string * Tok.t) list) r =
    let l = token env l and r = token env r in
    let content =
      match parts with
      | [] -> ("", Tok.empty_tok_after l)
      | (s, t) :: xs ->
          (String.concat "" (s :: List_.map fst xs), Tok.combine_toks t (List_.map snd xs))
    in
    G.L (G.String (l, content, r)) |> G.e
  in
  match x with
  | `Quote_str (`DQUOT_opt_doub_str_content_DQUOT (v1, v2, v3)) ->
      let parts =
        Option.value ~default:[] v2
        |> List_.map (function
             | `Imm_tok_prec_p1_pat_3a2a380 tok
             | `Esc_seq tok ->
                 str env tok)
      in
      quoted v1 parts v3
  | `Quote_str (`SQUOT_opt_sing_str_content_SQUOT (v1, v2, v3)) ->
      let parts =
        Option.value ~default:[] v2
        |> List_.map (function
             | `Imm_tok_prec_p1_pat_dc28280 tok
             | `Esc_seq tok ->
                 str env tok)
      in
      quoted v1 parts v3
  | `Blk_str (v1, v2, v3) ->
      let v1 = token env v1 (* "[[" *) in
      let s, t = str env v2 in
      let v3 = token env v3 (* "]]" *) in
      G.L (G.String (v1, (long_string_content s, t), v3)) |> G.e

(* Lua 5.4 attributes of local variables, 'local x <const> = 1'. The only
 * attributes defined by Lua are 'const' and 'close'. *)
let map_attrib (env : env) ((v1, v2, v3) : CST.attrib) : G.attribute =
  let _v1 = token env v1 (* "<" *) in
  let s, t = identifier env v2 in
  let _v3 = token env v3 (* ">" *) in
  match s with
  | "const" -> G.KeywordAttr (G.Const, t)
  | _ -> G.OtherAttribute ((s, t), [])

let global_attr (env : env) tok : G.attribute =
  G.OtherAttribute (("global", token env tok), [])

(* "..." is a vararg parameter in a program but an ellipsis in a pattern *)
let vararg_parameter (env : env) tok =
  match env.extra with
  | Pattern -> G.ParamEllipsis tok
  | Target -> G.ParamRest (tok, G.param_of_id ("...", tok))

let map_parameter (env : env) (x : CST.parameter) : G.parameter =
  match x with
  | `Id tok -> G.Param (G.param_of_id (identifier env tok))
  | `Vararg_param (v1, None) -> vararg_parameter env (token env v1) (* "..." *)
  (* Lua 5.5 named vararg, 'function f(...args)': args is a table of the
   * extra arguments, like a rest parameter in JS *)
  | `Vararg_param (v1, Some v2) ->
      G.ParamRest (token env v1, G.param_of_id (identifier env v2))

let map_parameters (env : env) ((v1, v2, v3) : CST.parameters) : G.parameters =
  let lp = token env v1 (* "(" *) in
  let params =
    match v2 with
    | Some x -> List_.map (map_parameter env) (sep_list x)
    | None -> []
  in
  let rp = token env v3 (* ")" *) in
  (lp, params, rp)

let binary_operator (x : CST.binary_expression) =
  match x with
  | `Exp_or_exp (v1, v2, v3) -> (v1, G.Or, v2, v3)
  | `Exp_and_exp (v1, v2, v3) -> (v1, G.And, v2, v3)
  | `Exp_LT_exp (v1, v2, v3) -> (v1, G.Lt, v2, v3)
  | `Exp_LTEQ_exp (v1, v2, v3) -> (v1, G.LtE, v2, v3)
  | `Exp_EQEQ_exp (v1, v2, v3) -> (v1, G.Eq, v2, v3)
  | `Exp_TILDEEQ_exp (v1, v2, v3) -> (v1, G.NotEq, v2, v3)
  | `Exp_GTEQ_exp (v1, v2, v3) -> (v1, G.GtE, v2, v3)
  | `Exp_GT_exp (v1, v2, v3) -> (v1, G.Gt, v2, v3)
  | `Exp_BAR_exp (v1, v2, v3) -> (v1, G.BitOr, v2, v3)
  | `Exp_TILDE_exp (v1, v2, v3) -> (v1, G.BitXor, v2, v3)
  | `Exp_AMP_exp (v1, v2, v3) -> (v1, G.BitAnd, v2, v3)
  | `Exp_LTLT_exp (v1, v2, v3) -> (v1, G.LSL, v2, v3)
  | `Exp_GTGT_exp (v1, v2, v3) -> (v1, G.LSR, v2, v3)
  | `Exp_PLUS_exp (v1, v2, v3) -> (v1, G.Plus, v2, v3)
  | `Exp_DASH_exp (v1, v2, v3) -> (v1, G.Minus, v2, v3)
  | `Exp_STAR_exp (v1, v2, v3) -> (v1, G.Mult, v2, v3)
  | `Exp_SLASH_exp (v1, v2, v3) -> (v1, G.Div, v2, v3)
  | `Exp_SLASHSLASH_exp (v1, v2, v3) -> (v1, G.FloorDiv, v2, v3)
  | `Exp_PERC_exp (v1, v2, v3) -> (v1, G.Mod, v2, v3)
  | `Exp_DOTDOT_exp (v1, v2, v3) -> (v1, G.Concat, v2, v3)
  | `Exp_HAT_exp (v1, v2, v3) -> (v1, G.Pow, v2, v3)

let rec map_expression (env : env) (x : CST.expression) : G.expr =
  match x with
  | `Choice_nil x -> (
      match x with
      | `Nil tok -> G.L (G.Null (token env tok)) |> G.e (* "nil" *)
      | `False tok -> G.L (G.Bool (false, token env tok)) |> G.e
      | `True tok -> G.L (G.Bool (true, token env tok)) |> G.e
      | `Num tok -> todo env tok
      | `Str x -> map_string env x
      | `Vararg_exp tok -> vararg_expression env (token env tok) |> G.e
      | `Func_defi (v1, v2) ->
          let v1 = token env v1 (* "function" *) in
          G.Lambda (map_function_body env v2 v1) |> G.e
      | `Var x -> map_variable env x
      | `Func_call x -> map_function_call env x
      | `Paren_exp x -> map_parenthesized_expression env x
      | `Table_cons x -> map_table_constructor env x
      | `Bin_exp x ->
          let v1, op, v2, v3 = binary_operator x in
          let v1 = map_expression env v1 in
          let v2 = token env v2 in
          let v3 = map_expression env v3 in
          G.Call (G.IdSpecial (G.Op op, v2) |> G.e, fb [ G.Arg v1; G.Arg v3 ])
          |> G.e
      | `Un_exp (v1, v2) ->
          let op, tok =
            match v1 with
            | `Not tok -> (G.Not, tok) (* "not" *)
            | `HASH tok -> (G.Length, tok) (* "#" *)
            | `DASH tok -> (G.Minus, tok) (* "-" *)
            | `TILDE tok -> (G.BitNot, tok) (* "~" *)
          in
          let v2 = map_expression env v2 in
          G.Call (G.IdSpecial (G.Op op, token env tok) |> G.e, fb [ G.Arg v2 ])
          |> G.e)
  | `Deep_ellips (v1, v2, v3) ->
      let v1 = token env v1 (* "<..." *) in
      let v2 = map_expression env v2 in
      let v3 = token env v3 (* "...>" *) in
      G.DeepEllipsis (v1, v2, v3) |> G.e

(* the parentheses are dropped, as with the previous grammar *)
and map_parenthesized_expression (env : env)
    ((v1, v2, v3) : CST.parenthesized_expression) : G.expr =
  let _v1 = token env v1 (* "(" *) in
  let v2 = map_expression env v2 in
  let _v3 = token env v3 (* ")" *) in
  v2

and map_prefix_expression (env : env) (x : CST.prefix_expression) : G.expr =
  match x with
  | `Var x -> map_variable env x
  | `Func_call x -> map_function_call env x
  | `Paren_exp x -> map_parenthesized_expression env x

and map_variable (env : env) (x : CST.variable) : G.expr =
  match x with
  (* 'global' is a keyword only at the start of a declaration (Lua 5.5),
   * elsewhere it is a name *)
  | `Cont_kw tok -> G.N (G.Id (identifier env tok, G.empty_id_info ())) |> G.e
  | `Id tok -> G.N (G.Id (identifier env tok, G.empty_id_info ())) |> G.e
  | `Brac_index_exp (v1, v2, v3, v4) ->
      let v1 = map_prefix_expression env v1 in
      let v2 = token env v2 (* "[" *) in
      let v3 = map_expression env v3 in
      let v4 = token env v4 (* "]" *) in
      G.ArrayAccess (v1, (v2, v3, v4)) |> G.e
  | `Dot_index_exp (v1, v2, v3) ->
      let v1 = map_prefix_expression env v1 in
      let v2 = token env v2 (* "." *) in
      let v3 = identifier env v3 in
      G.DotAccess (v1, v2, G.FN (G.Id (v3, G.empty_id_info ()))) |> G.e

and map_function_call (env : env) ((v1, v2) : CST.function_call) : G.expr =
  match v1 with
  | `Prefix_exp x ->
      let fn = map_prefix_expression env x in
      let args = map_arguments env v2 in
      G.Call (fn, args) |> G.e
  | `Meth_index_exp (v1, v3, v4) ->
      (* o:m(x) *)
      let prefix = map_prefix_expression env v1 in
      let colon = token env v3 (* ":" *) in
      let fn_name = identifier env v4 in
      let qualified_info =
        {
          G.name_last = (fn_name, None);
          G.name_middle = Some (G.QExpr (prefix, colon));
          G.name_top = None;
          G.name_info = G.empty_id_info ();
        }
      in
      let args = map_arguments env v2 in
      G.Call (G.N (G.IdQualified qualified_info) |> G.e, args) |> G.e

and map_arguments (env : env) (x : CST.arguments) : G.arguments =
  match x with
  | `LPAR_opt_exp_rep_COMMA_exp_RPAR (v1, v2, v3) ->
      let v1 = token env v1 (* "(" *) in
      let v2 =
        match v2 with
        | Some x -> List_.map (fun e -> G.Arg e) (map_expression_list env x)
        | None -> []
      in
      let v3 = token env v3 (* ")" *) in
      (v1, v2, v3)
  | `Table_cons x -> fb [ G.Arg (map_table_constructor env x) ]
  | `Str x -> fb [ G.Arg (map_string env x) ]

and map_expression_list (env : env) (x : CST.expression_list) : G.expr list =
  List_.map (map_expression env) (sep_list x)

(* a table constructor is a dictionary, as in Python: a field is a key-value
 * pair *)
and map_table_constructor (env : env) ((v1, v2, v3) : CST.table_constructor) :
    G.expr =
  let v1 = token env v1 (* "{" *) in
  let fields =
    match v2 with
    | Some (v1, v2, _v3) ->
        (* the positional fields get the next integer key, from 1 *)
        let positions = ref 0 in
        let first = map_field env positions v1 in
        first :: List_.map (fun (_sep, x) -> map_field env positions x) v2
    | None -> []
  in
  let v3 = token env v3 (* "}" *) in
  G.Container (G.Dict, (v1, fields, v3)) |> G.e

and map_field (env : env) (positions : int ref) (x : CST.field) : G.expr =
  match x with
  | `LBRACK_exp_RBRACK_EQ_exp (v1, v2, v3, v4, v5) ->
      let _v1 = token env v1 (* "[" *) in
      let v2 = map_expression env v2 in
      let _v3 = token env v3 (* "]" *) in
      let v4 = token env v4 (* "=" *) in
      let v5 = map_expression env v5 in
      G.keyval v2 v4 v5
  | `Choice_cont_kw_EQ_exp (v1, v2, v3) ->
      let v1 =
        match v1 with
        | `Cont_kw tok (* "global" *)
        | `Id tok ->
            identifier env tok
      in
      let v2 = token env v2 (* "=" *) in
      let v3 = map_expression env v3 in
      (* x = v is ["x"] = v *)
      G.keyval (G.L (G.String (fb v1)) |> G.e) v2 v3
  | `Exp x -> (
      let v = map_expression env x in
      match v.e with
      (* an ellipsis in a pattern stands for fields, not for a value *)
      | G.Ellipsis _ -> v
      | _ ->
          (* { a, b } is { [1] = a, [2] = b } *)
          incr positions;
          let key = G.L (G.Int (Parsed_int.of_int !positions)) |> G.e in
          G.keyval key (G.fake "=") v)

(* the body is a block delimited by the 'function' and 'end' tokens *)
and map_function_body (env : env) ((v1, v2, v3) : CST.function_body)
    (tfunction : Tok.t) : G.function_definition =
  let fparams = map_parameters env v1 in
  let body = G.Block (tfunction, map_block env v2, token env v3) |> G.s in
  {
    G.fparams;
    frettype = None;
    fkind = (G.Function, tfunction);
    fbody = G.FBStmt body;
  }

(*****************************************************************************)
(* Statements *)
(*****************************************************************************)
(* Statements are mutually recursive with expressions because of the bodies
 * of anonymous functions. *)

and map_block (env : env) (x : CST.block option) : G.stmt list =
  match x with
  | None -> []
  | Some (`Rep1_stmt_opt_ret_stmt (stmts, ret)) ->
      map_statements env stmts @ Option.to_list (Option.map (map_return env) ret)
  | Some (`Rep_stmt_ret_stmt (stmts, ret)) ->
      map_statements env stmts @ [ map_return env ret ]

and map_statements (env : env) (xs : CST.statement list) : G.stmt list =
  List.concat_map (map_statement env) xs

and map_return (env : env) ((v1, v2, v3) : CST.return_statement) : G.stmt =
  let v1 = token env v1 (* "return" *) in
  let v2 =
    v2
    |> Option.map (fun x ->
           G.Container (G.Tuple, fb (map_expression_list env x))
           |> G.e)
  in
  let v3 =
    match v3 with
    | Some tok -> token env tok (* ";" *)
    | None -> G.sc
  in
  G.Return (v1, v2, v3) |> G.s

and map_statement (env : env) (x : CST.statement) : G.stmt list =
  match x with
  | `Choice_empty_stmt x -> (
      match x with
      | `Empty_stmt _tok -> [] (* ";" *)
      | `Assign_stmt (v1, v2, v3) ->
          let targets = List_.map (map_variable env) (sep_list v1) in
          let equal = token env v2 (* "=" *) in
          let values = map_expression_list env v3 in
          [ assign targets equal values ]
      | `Func_call x -> [ G.ExprStmt (map_function_call env x, G.sc) |> G.s ]
      | `Label_stmt (v1, v2, v3) ->
          let _v1 = token env v1 (* "::" *) in
          let v2 = identifier env v2 in
          let _v3 = token env v3 (* "::" *) in
          [ G.Label (v2, G.Block (fb []) |> G.s) |> G.s ]
      | `Brk_stmt tok -> [ G.Break (token env tok, LNone, G.sc) |> G.s ]
      | `Goto_stmt (v1, v2) ->
          let v1 = token env v1 (* "goto" *) in
          let v2 = identifier env v2 in
          [ G.Goto (v1, v2, G.sc) |> G.s ]
      | `Do_stmt (v1, v2, v3) ->
          [ G.Block (token env v1, map_block env v2, token env v3) |> G.s ]
      | `While_stmt (v1, v2, v3, v4, v5) ->
          let v1 = token env v1 (* "while" *) in
          let v2 = map_expression env v2 in
          let block = G.Block (token env v3, map_block env v4, token env v5) in
          [ G.While (v1, G.Cond v2, block |> G.s) |> G.s ]
      | `Repeat_stmt (v1, v2, v3, v4) ->
          let t = token env v1 (* "repeat" *) in
          let until = token env v3 (* "until" *) in
          let block = G.Block (t, map_block env v2, until) |> G.s in
          let v4 = map_expression env v4 in
          (* the loop goes on while the until condition is false *)
          let not_ = G.IdSpecial (G.Op G.Not, until) |> G.e in
          let cond = G.Call (not_, fb [ G.Arg v4 ]) |> G.e in
          [ G.DoWhile (t, block, cond) |> G.s ]
      | `If_stmt (v1, v2, v3, v4, v5, v6, v7) ->
          let v1 = token env v1 (* "if" *) in
          let v2 = map_expression env v2 in
          let _v3 = token env v3 (* "then" *) in
          let then_ = G.Block (fb (map_block env v4)) |> G.s in
          let else_ =
            v6
            |> Option.map (fun (v1, v2) ->
                   let _v1 = token env v1 (* "else" *) in
                   G.Block (fb (map_block env v2)) |> G.s)
          in
          (* each elseif is the else branch of the one before it, the else is
           * the else branch of the last one *)
          let elseifs =
            List.fold_right
              (fun ((v1, v2, v3, v4) : CST.elseif_statement)
                   (acc : G.stmt option) ->
                let v1 = token env v1 (* "elseif" *) in
                let v2 = map_expression env v2 in
                let _v3 = token env v3 (* "then" *) in
                let then_ = G.Block (fb (map_block env v4)) |> G.s in
                Some (G.If (v1, G.Cond v2, then_, acc) |> G.s))
              v5 else_
          in
          let _v7 = token env v7 (* "end" *) in
          [ G.If (v1, G.Cond v2, then_, elseifs) |> G.s ]
      | `For_stmt (v1, v2, v3, v4, v5) ->
          let v1 = token env v1 (* "for" *) in
          let header =
            match v2 with
            | `For_gene_clause x -> map_for_generic_clause env x
            | `For_nume_clause x -> map_for_numeric_clause env x
          in
          let block = G.Block (token env v3, map_block env v4, token env v5) in
          [ G.For (v1, header, block |> G.s) |> G.s ]
      | `Decl x -> map_declaration env x)
  | `Semg_ellips_stmt tok ->
      [ G.ExprStmt (G.Ellipsis (token env tok) |> G.e, G.sc) |> G.s ]

(* for k, v in e1, e2, e3 *)
and map_for_generic_clause (env : env) ((v1, v2, v3) : CST.for_generic_clause)
    : G.for_header =
  let pat =
    match List_.map (identifier env) (sep_list v1) with
    | [ id ] -> G.PatId (id, G.empty_id_info ())
    | ids ->
        G.PatTuple (fb (List_.map (fun id -> G.PatId (id, G.empty_id_info ())) ids))
  in
  let v2 = token env v2 (* "in" *) in
  (* Usually a single iterator call, e.g. pairs(t), but the expressions
   * can also be the iterator function, the state and the initial value,
   * e.g. 'next, t, nil'. They are kept as a tuple; the previous
   * translation dropped all but the first one. *)
  let iter =
    match map_expression_list env v3 with
    | [ e ] -> e
    | es -> G.Container (G.Tuple, fb es) |> G.e
  in
  G.ForEach (pat, v2, iter)

(* for i = start, limit, step *)
and map_for_numeric_clause (env : env)
    ((v1, v2, v3, v4, v5, v6) : CST.for_numeric_clause) : G.for_header =
  let v1 = identifier env v1 in
  let v2 = token env v2 (* "=" *) in
  let v3 = map_expression env v3 in
  let v4 = token env v4 (* "," *) in
  let v5 = map_expression env v5 in
  let v6 =
    match v6 with
    | Some (_comma, e) -> [ map_expression env e ]
    | None -> []
  in
  (* for i = start, limit, step iterates i over the range from start to
   * limit, as the range loops of Rust *)
  let range =
    G.Call
      ( G.IdSpecial (G.Op G.Range, v4) |> G.e,
        fb (List_.map G.arg (v3 :: v5 :: v6)) )
    |> G.e
  in
  G.ForEach (G.PatId (v1, G.empty_id_info ()), v2, range)

and map_declaration (env : env) (x : CST.declaration) : G.stmt list =
  match x with
  | `Func_decl (v1, v2, v3) ->
      let tfunction = token env v1 (* "function" *) in
      let name =
        match v2 with
        | `Func_name_prefix_exp (`Id tok) -> H2.name_of_id (identifier env tok)
        | x -> todo env x
      in
      let def = map_function_body env v3 tfunction in
      let ent = { G.name = G.EN name; G.attrs = []; G.tparams = None } in
      [ G.DefStmt (ent, G.FuncDef def) |> G.s ]
  | `Local_func_decl (v1, v2, v3, v4) ->
      let local = token env v1 (* "local" *) in
      let tfunction = token env v2 (* "function" *) in
      let def = map_function_body env v4 tfunction in
      let ent =
        G.basic_entity (identifier env v3)
          ~attrs:[ G.KeywordAttr (G.Static, local) ]
      in
      [ G.DefStmt (ent, G.FuncDef def) |> G.s ]
  | `Var_decl (v1, v2) ->
      let local = token env v1 (* "local" *) in
      map_variable_declaration env (G.KeywordAttr (G.Static, local)) v2
  (* Lua 5.5 global declarations. AST_generic has no 'global' modifier, so
   * it is an OtherAttribute. *)
  | `Global_func_decl (v1, v2, v3, v4) ->
      let global = global_attr env v1 in
      let tfunction = token env v2 (* "function" *) in
      let def = map_function_body env v4 tfunction in
      let ent = G.basic_entity (identifier env v3) ~attrs:[ global ] in
      [ G.DefStmt (ent, G.FuncDef def) |> G.s ]
  | `Global_var_decl (v1, `Att_name_list x) ->
      (* 'global x' only declares that x is global, as Python's 'global x' *)
      let tglobal = token env v1 in
      map_att_name_list env (global_attr env v1) x
      |> List_.map (fun ent -> G.DefStmt (ent, G.UseOuterDecl tglobal) |> G.s)
  | `Global_var_decl (v1, (`Var_assign _ as x)) ->
      map_variable_declaration env (global_attr env v1) x
  | `Global_impl_var_decl (v1, v2, v3) ->
      (* 'global *' and 'global <const> *': all the free names are globals *)
      let v1 = token env v1 (* "global" *) in
      let attrs = Option.to_list (Option.map (map_attrib env) v2) in
      let v3 = token env v3 (* "*" *) in
      let args = List_.map (fun x -> G.At x) attrs @ [ G.Tk v3 ] in
      [ G.DirectiveStmt (G.OtherDirective (("global", v1), args) |> G.d) |> G.s ]

(* local a, b [= x, y] *)
and map_variable_declaration (env : env) (kind : G.attribute)
    (x : CST.anon_choice_att_name_list_26e8983) : G.stmt list =
  let names, values =
    match x with
    | `Att_name_list x -> (x, [])
    | `Var_assign (v1, v2, v3) ->
        let _v2 = token env v2 (* "=" *) in
        (v1, map_expression_list env v3)
  in
  let entities = map_att_name_list env kind names in
  mk_vars entities values |> List_.map (fun x -> G.DefStmt x |> G.s)

(* attnamelist ::= [attrib] Name [attrib] {',' Name [attrib]} *)
and map_att_name_list (env : env) (kind : G.attribute)
    ((v1, v2, v3, v4) : CST.att_name_list) : G.entity list =
  let attrib x = Option.to_list (Option.map (map_attrib env) x) in
  (* Lua 5.5: an attribute before the first name applies to all the names,
   * 'local <const> a, b' *)
  let prefix = attrib v1 in
  (v2, v3) :: List_.map (fun (_comma, id, attr) -> (id, attr)) v4
  |> List_.map (fun (id, attr) ->
         G.basic_entity (identifier env id)
           ~attrs:((kind :: prefix) @ attrib attr))

(*****************************************************************************)
(* Entry point *)
(*****************************************************************************)

let map_chunk (env : env) (x : CST.chunk) : G.any =
  match x with
  | `Opt_hash_bang_line_rep_stmt_opt_ret_stmt (shebang, stmts, ret) ->
      let () =
        match shebang with
        | Some tok -> todo env tok
        | None -> ()
      in
      G.Ss
        (map_statements env stmts
        @ Option.to_list (Option.map (map_return env) ret))
  | `Semg_exp (_v1, v2) -> G.E (map_expression env v2)

let parse file =
  H.wrap_parser
    (fun () -> Tree_sitter_lua_new.Parse.file !!file)
    (fun cst _extras ->
      let env = { H.file; conv = H.line_col_to_pos file; extra = Target } in
      match map_chunk env cst with
      | G.Ss xs -> xs
      | _ -> failwith "not a program")

(* A pattern is parsed as statements, or as an expression if that fails:
 * unlike in the previous grammar, an expression such as '$X + 1' is not a
 * statement. *)
let parse_statements_or_expression str =
  let res = Tree_sitter_lua_new.Parse.string str in
  match res.errors with
  | [] -> res
  | _ ->
      let expr_res =
        Tree_sitter_lua_new.Parse.string ("__SEMGREP_EXPRESSION " ^ str)
      in
      if expr_res.errors =*= [] then expr_res else res

let parse_pattern str =
  H.wrap_parser
    (fun () -> parse_statements_or_expression str)
    (fun cst _extras ->
      let file = Fpath.v "<pattern>" in
      let env =
        { H.file; conv = H.line_col_to_pos_pattern str; extra = Pattern }
      in
      map_chunk env cst)
