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

let map_expression (env : env) (x : CST.expression) : G.expr = todo env x

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
           G.Container (G.Tuple, fb (List_.map (map_expression env) (sep_list x)))
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
      | `Func_call x -> todo env x
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
