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

(*****************************************************************************)
(* Expressions *)
(*****************************************************************************)

(* "..." is a reference to the vararg parameter in a program but an ellipsis in
 * a pattern *)
let vararg_expression (env : env) tok =
  match env.extra with
  | Pattern -> G.Ellipsis tok
  | Target -> G.N (G.Id (("...", tok), G.empty_id_info ()))

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
      | `Str x -> todo env x
      | `Vararg_exp tok -> vararg_expression env (token env tok) |> G.e
      | `Func_defi x -> todo env x
      | `Var x -> map_variable env x
      | `Func_call x -> map_function_call env x
      | `Paren_exp x -> map_parenthesized_expression env x
      | `Table_cons x -> todo env x
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
  | `Deep_ellips x -> todo env x

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
  | `Cont_kw tok -> todo env tok
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
  | `Table_cons x -> todo env x
  | `Str x -> todo env x

and map_expression_list (env : env) (x : CST.expression_list) : G.expr list =
  List_.map (map_expression env) (sep_list x)

(*****************************************************************************)
(* Statements *)
(*****************************************************************************)

let rec map_block (env : env) (x : CST.block option) : G.stmt list =
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
      | `Assign_stmt x -> todo env x
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
      | `While_stmt x -> todo env x
      | `Repeat_stmt x -> todo env x
      | `If_stmt x -> todo env x
      | `For_stmt x -> todo env x
      | `Decl x -> todo env x)
  | `Semg_ellips_stmt x -> todo env x

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
  | `Semg_exp x -> todo env x

let parse file =
  H.wrap_parser
    (fun () -> Tree_sitter_lua_new.Parse.file !!file)
    (fun cst _extras ->
      let env = { H.file; conv = H.line_col_to_pos file; extra = Target } in
      match map_chunk env cst with
      | G.Ss xs -> xs
      | _ -> failwith "not a program")

let parse_pattern str =
  H.wrap_parser
    (fun () -> Tree_sitter_lua_new.Parse.string str)
    (fun cst _extras ->
      let file = Fpath.v "<pattern>" in
      let env =
        { H.file; conv = H.line_col_to_pos_pattern str; extra = Pattern }
      in
      map_chunk env cst)
