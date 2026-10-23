# Structural invariants of the Lua AST (agent `structure`)

## 1. Method

The oracle does not need a reference parser: it checks properties that *any* correct
translation must satisfy, on the AST JSON of the new parser
(`opengrep-core -lang lua -full_token_info -dump_ast -json`), against an independent
small Lua lexer (`lualex.py`, bytes, 5.1-5.5 + LuaJIT numbers, long brackets, `\z`).
Every AST `OriginTok` is mapped to the source token that contains its byte position.

Invariants (all in `check.py`):

1. **Token presence (lost code).** Every source token must be the origin of at least one AST
   token, except tokens dropped by design, which the checker recognises from context:
   `( ) , ;`, `then`, `else`, `::`, the `end` of an `if` (block matcher), `<`/`>` of
   attributes, `.`/`:` inside `function a.b:m` names, `=` of local/global declarations and of
   table fields, `[`/`]` around table keys.
2. **Contiguity (garbled / misplaced code).** For every AST node (variant constructor:
   statements, expressions, `Arg`, `Block`, `FuncDef` from its `(`, ...) the non-droppable
   source tokens between the first and last token of the node must all be in the node's
   subtree. A token of the source that sits inside a node but belongs elsewhere (or nowhere)
   is a violation. For the file node (`Pr`) the range is the whole file, so anything outside
   all top-level statements is reported. `DefStmt`s produced by one `local a, b = ...` are
   checked as a group (they share `local` by design).
3. **Order / non-overlap.** The direct children of every node, in JSON field order, must have
   increasing, non-overlapping source ranges (statement lists, call args, table entries,
   `If`/`While`/`For` parts, `ForEach (pat, in, iter)`, `Assign`, `DotAccess`, ...). Known
   reorderings are handled: binary `Call(IdSpecial op, [l; r])` is checked as `l < op < r`,
   numeric-for `Range` likewise, `IdQualified` (`name_last` first), `DefStmt`
   (`local < name < init`, `function < name < (`), the inserted `self` parameter (token `:`).
   Shared boundary tokens allowed by design: `)` (params / body block), `until` (block close /
   `not`), `repeat`, `...` (ParamRest), long-string pieces.
4. **Block structure.** A lexer block matcher pairs `function/if/while/for/repeat/do` with
   their `end`/`until` and builds the construct tree. The AST must contain exactly one
   construct per source opener (FuncDef/Lambda by `function` token, If by `if`, While, For,
   DoWhile, standalone `Block` by `do`), with the **same closing token** (body Block close =
   that `end`/`until`, loop body opens at its `do`), the **same parent construct** (nesting),
   and each function body Block must start at the `)` of the parameters.
5. **Shapes that must never occur:** empty `Id`, `Id` with fake token or text != token,
   OriginTok with empty text, tokens that do not map to a source token ("ghost"),
   `Ellipsis`/`DeepEllipsis`/other pattern nodes in a target, `Other*`/`Raw*` nodes (except
   the documented `OtherAttribute global/close`, `OtherDirective global`), `Call` with a literal
   callee, assignment targets other than Id/DotAccess/ArrayAccess, `Dict` entries that are not
   pairs, empty `IdQualified` parts, `DefStmt` without name.
6. **Duplicate origin tokens:** the same source token used in two places of the AST; expected
   duplicates (`)`, `until`, `repeat`, `local`/`global` shared by several DefStmts, `...`,
   prefix `<const>` of Lua 5.5, long-string pieces) are tallied, anything else is a violation.

Why it detects lost/garbled constructs: a lost expression shows up as absent tokens and as a
contiguity hole in every enclosing node; an expression attached to the wrong parent or
reordered breaks contiguity or order; a wrong block end / nesting breaks invariant 4. The
oracle was validated on the OLD parser, where it finds the known defects (function bodies
that start at `function`, 45 939 functions; `OtherExpr`; empty tokens), see section 5.3.

## 2. How to run

```
cd /tmp/tc/structure
python3 run.py new          # -> results_new.jsonl (resumable, 2 workers, docker exec ogb, timeout 20 s/file)
python3 run.py old          # -> results_old.jsonl
python3 aggregate.py new    # -> agg_new.json (+ table on stdout); same for old
python3 check.py FILE.lua FILE.json   # single file, prints violations + examples
```
`runall.sh` runs both detached. `old_vs_new.json`: comparison on the 5 002 files that both
parsers parse without error.

## 3. Coverage

All 5 352 files of `/tmp/tc/files.txt` were processed with both parsers (about 430 records
broken by container restarts (docker exit 128/2) were removed and rerun).
New parser: 5 346 files without parse error checked, 6 files with parse errors
(3 with a partial AST, 3 without AST), 0 timeouts, 0 checker failures. Old parser: 348 files
with parse errors. 3 375 131 non-droppable source tokens checked in the 5 346 files.

Parse-error files (new), all (d), the source is not valid Lua:
`Kong_kong/spec/fixtures/invalid-module.lua` (intentional syntax error),
`lunarmodules_luacheck/spec/samples/compound_operators.lua` (`i += 10`),
`lunarmodules_luacheck/spec/samples/utf8_error.lua`, `lunarmodules_busted/spec/cl_compile_fail.lua`
(intentional), `nvim-treesitter/tests/indent/lua/cond.lua` and `func.lua` (truncated snippets:
`if x > 2 then` / `function hi()` at the end of the file; the old parser accepted them).

Per project (violations = sum of all invariant violations in clean files; old = old parser on
its own clean files):

| project | files | clean | parse err (new) | no AST | src tokens checked | absent tokens | invariant violations (new) | parse err (old) | violations (old) |
|---|---|---|---|---|---|---|---|---|---|
| Kong_kong | 400 | 399 | 1 | 0 | 338296 | 0 | 0 | 17 | 19452 |
| L3MON4D3_LuaSnip | 124 | 124 | 0 | 0 | 69807 | 0 | 3 | 4 | 6948 |
| LuaLS_lua-language-server | 400 | 400 | 0 | 0 | 245226 | 0 | 0 | 46 | 9571 |
| apache_apisix | 400 | 400 | 0 | 0 | 259964 | 0 | 0 | 12 | 13220 |
| awesomeWM_awesome | 400 | 400 | 0 | 0 | 133635 | 0 | 0 | 15 | 7962 |
| folke_lazy.nvim | 65 | 65 | 0 | 0 | 49336 | 0 | 0 | 3 | 3502 |
| folke_snacks.nvim | 141 | 141 | 0 | 0 | 150696 | 0 | 0 | 10 | 9327 |
| hrsh7th_nvim-cmp | 57 | 57 | 0 | 0 | 37026 | 0 | 0 | 1 | 2893 |
| kikito_inspect.lua | 4 | 4 | 0 | 0 | 3153 | 0 | 0 | 0 | 431 |
| kikito_middleclass | 11 | 11 | 0 | 0 | 4655 | 0 | 0 | 1 | 946 |
| koreader_koreader | 400 | 400 | 0 | 0 | 525295 | 0 | 0 | 32 | 28744 |
| lite-xl_lite-xl | 89 | 89 | 0 | 0 | 78183 | 0 | 0 | 2 | 6214 |
| lua_lua | 34 | 34 | 0 | 0 | 76663 | 2 | 9 | 28 | 335 |
| luarocks_luarocks | 162 | 162 | 0 | 0 | 118348 | 0 | 0 | 16 | 7022 |
| lunarmodules_Penlight | 115 | 115 | 0 | 0 | 69152 | 0 | 0 | 8 | 5378 |
| lunarmodules_busted | 98 | 97 | 0 | 1 | 27781 | 0 | 0 | 3 | 3365 |
| lunarmodules_luacheck | 135 | 133 | 2 | 0 | 63163 | 0 | 0 | 15 | 3978 |
| luvit_luvit | 199 | 199 | 0 | 0 | 65641 | 4 | 30 | 1 | 8066 |
| mpeterv_argparse | 15 | 15 | 0 | 0 | 12371 | 0 | 0 | 2 | 901 |
| neovim_nvim-lspconfig | 400 | 400 | 0 | 0 | 23300 | 0 | 0 | 2 | 1603 |
| nvim-lualine_lualine.nvim | 119 | 119 | 0 | 0 | 41511 | 0 | 0 | 8 | 1512 |
| nvim-telescope_telescope.nvim | 77 | 77 | 0 | 0 | 68717 | 0 | 0 | 5 | 5922 |
| nvim-treesitter_nvim-treesitter | 79 | 77 | 0 | 2 | 24889 | 0 | 0 | 6 | 971 |
| openresty_lua-resty-core | 38 | 38 | 0 | 0 | 20882 | 0 | 0 | 3 | 1102 |
| pkulchenko_ZeroBraneStudio | 324 | 324 | 0 | 0 | 294511 | 0 | 0 | 30 | 10211 |
| rxi_json.lua | 6 | 6 | 0 | 0 | 2447 | 0 | 0 | 1 | 122 |
| rxi_lume | 3 | 3 | 0 | 0 | 7525 | 0 | 0 | 0 | 879 |
| tarantool_tarantool | 400 | 400 | 0 | 0 | 288771 | 0 | 0 | 59 | 19814 |
| teal-language_tl | 257 | 257 | 0 | 0 | 161813 | 13 | 34 | 6 | 6776 |
| xmake-io_xmake | 400 | 400 | 0 | 0 | 112374 | 0 | 0 | 10 | 8937 |
| **total** | 5352 | 5346 | 3 | 3 | 3375131 | 19 | 76 | 346 | 196104 |

## 4. Statistics (new parser, 5 346 clean files)

Constructs (lexer block matcher vs AST; a construct counts as agreeing only if it is present
once, with the same closing token and the same parent construct) and source tokens by kind
(expected = non-droppable source tokens, in AST = tokens that are origin of an AST token):

| category | expected (oracle) | in AST | missing | extra | agreement |
|---|---|---|---|---|---|
| construct function | 55195 | 55195 | 0 | 0 | 100.0000% |
| construct if | 56107 | 56107 | 0 | 0 | 100.0000% |
| construct while | 1198 | 1198 | 0 | 0 | 100.0000% |
| construct for | 12303 | 12303 | 0 | 0 | 100.0000% |
| construct repeat | 183 | 183 | 0 | 0 | 100.0000% |
| construct do | 735 | 735 | 0 | 0 | 100.0000% |
| token identifiers | 1589177 | 1589169 | 8 | 0 | 99.9995% |
| token punct:. | 317153 | 317151 | 2 | 0 | 99.9994% |
| token strings | 296465 | 296463 | 2 | 0 | 99.9993% |
| token operators | 173761 | 173757 | 4 | 0 | 99.9977% |
| token numbers | 122209 | 122208 | 1 | 0 | 99.9992% |
| token kw:local | 111344 | 111344 | 0 | 0 | 100.0000% |
| token punct:{ | 105163 | 105162 | 1 | 0 | 99.9990% |
| token punct:} | 105163 | 105162 | 1 | 0 | 99.9990% |
| token punct:: | 75869 | 75869 | 0 | 0 | 100.0000% |
| token punct:= | 73669 | 73669 | 0 | 0 | 100.0000% |
| token kw:end | 69431 | 69431 | 0 | 0 | 100.0000% |
| token kw:if | 56107 | 56107 | 0 | 0 | 100.0000% |
| token kw:function | 55195 | 55195 | 0 | 0 | 100.0000% |
| token nil/true/false | 52739 | 52739 | 0 | 0 | 100.0000% |
| token kw:return | 45702 | 45702 | 0 | 0 | 100.0000% |
| token punct:[ | 38879 | 38879 | 0 | 0 | 100.0000% |
| token punct:] | 38879 | 38879 | 0 | 0 | 100.0000% |
| token kw:do | 14236 | 14236 | 0 | 0 | 100.0000% |
| token kw:for | 12303 | 12303 | 0 | 0 | 100.0000% |
| token kw:in | 9533 | 9533 | 0 | 0 | 100.0000% |
| token kw:elseif | 5644 | 5644 | 0 | 0 | 100.0000% |
| token vararg | 3031 | 3031 | 0 | 0 | 100.0000% |
| token kw:break | 1481 | 1481 | 0 | 0 | 100.0000% |
| token kw:while | 1198 | 1198 | 0 | 0 | 100.0000% |
| token kw:goto | 431 | 431 | 0 | 0 | 100.0000% |
| token kw:repeat | 183 | 183 | 0 | 0 | 100.0000% |
| token kw:until | 183 | 183 | 0 | 0 | 100.0000% |

Structural invariants (new parser, clean files):

| invariant | checked | violations | files |
|---|---|---|---|
| function bodies: Block from `)` to its `end` | 55 195 functions (25 713 FuncDef + 29 482 Lambda) | 0 | 0 |
| block close token = lexer `end`/`until`; each `end` owned by exactly one construct | 69 431 `end` + 183 `until` | 0 | 0 |
| construct nesting (parent construct) = lexer nesting | 125 721 constructs | 0 | 0 |
| loop body Block opens at its `do` | 13 501 loops | 0 | 0 |
| order / non-overlap of children (all nodes, incl. statement lists) | ~7.8 M nodes | 0 | 0 |
| contiguity of node token ranges | ~7.8 M nodes | 46 (nested nodes around 4 places) | 4 |
| token presence | 3 375 131 tokens | 19 (same 4 places) | 3 |
| shapes (empty/fake Id, Other*, pattern nodes, literal callee, bad assign target, Dict entry not pair, empty IdQualified part, DefStmt w/o name) | | 0 | 0 |
| ghost tokens (AST position not in a source token) | | 0 | 0 |
| OriginTok with empty text | | 10 | 6 |
| unexpected duplicate origin tokens | | 0 | 0 |

Informative tallies: callees N.Id 120 360, DotAccess 100 857, N.IdQualified (method calls)
75 869 (= number of `:` tokens), IdSpecial (operators) 176 693, Call 2 303, ArrayAccess 261,
Lambda 52. `then` dropped 61 751 = If nodes 61 751 (= 56 107 `if` + 5 644 `elseif`); `end`
dropped 56 107 = number of `if`; shared `)` 55 195 = number of functions; `until`/`repeat`
shared 183 = number of repeat loops: all exactly as designed.

## 5. Discrepancies

### 5.1 Extra values of a `local` declaration are dropped — (a), 4 occurrences, pre-existing

All 46 contiguity violations and all 19 absent tokens of the clean files come from this one
cause. `mk_vars` (`Parse_lua_tree_sitter.ml`, case `[], _y :: _ys -> []`) pairs names with
values and silently discards values beyond the number of names. Lua evaluates them (calls run),
so code disappears from the AST:

- `lua_lua/testes/gc.lua:661` `local a = 'xuxu'..(10+3)..'joao', {}` — the table `{}` is absent.
- `luvit_luvit/tests/test-tls-client-abort.lua:22-26` `local options = { port = 32333, host = '127.0.0.1' }, conn` — `conn` absent.
- `luvit_luvit/tests/test-tls-client-abort2.lua:26` `local options = {...}, conn, onConnect, onError` — 3 names absent.
- `teal-language_tl/teal/debug.lua:49` `local name = info.name or "<anon>", info.currentline > 0 and "@" .. info.currentline or ""` —
  the whole second expression (13 tokens: field access, comparison, `and`/`or`, concat, 2 strings, a number) is absent.

The old parser drops the same code in the same files (it is in the old results too), so this
is not a regression of the migration, but it is a real loss (a rule `$F(...)`/`g()` cannot match
`local a = f(), g()`). Same code path for `global x = a, b` (Lua 5.5).

### 5.2 OriginTok with empty text — (b), 10 occurrences in 6 files

`[[]]` (empty long string), e.g. `L3MON4D3_LuaSnip/tests/unit/environ_spec.lua:91`
`check_undefined(test_name .. " without initialization", [[]], var_name)` and
`teal-language_tl/spec/cli/run_spec.lua:8` `util.write_tmp_file(finally, [[]])`.
The AST is `L String ("[[", ("", tok ""), "]]")`: the content token is empty because the content
is empty (documented in `map_string`), not a tree-sitter MISSING token. No impact.

### 5.3 Old parser (comparison on the 5 002 files that both parse without error)

New: 66 violations; old: 196 089. Main old-only findings (they validate that the oracle
detects structural defects):

- function body Block starts at the `function` keyword (45 939 functions): breaks
  `function_body_not_from_rparen`, duplicates the `function` token (45 942 dup tokens), makes
  the name and parameters lie inside the body range (contiguity of Block/FBStmt 28 945) and the
  body overlap the parameters (order Lambda 25 363, FuncDef 20 576). Fixed in the new parser.
- `OtherExpr` nodes (74 in 41 files), Ids whose text differs from their token / empty tokens
  (29 + 37), ghost tokens (8), `Call` with a literal callee (19), statements misplaced in
  `for` (24) and `if` (26), lost `nil`, `[ ]`, `.` tokens.
- 344 files with parse errors in the old parser parse cleanly with the new one.

Full table (new vs old, same files):

| invariant | new: violations | new: files | old: violations | old: files |
|---|---|---|---|---|
| dup_token | 0 | 0 | 45942 | 3808 |
| block:function_body_not_from_rparen | 0 | 0 | 45939 | 3808 |
| contiguity:Block | 8 | 3 | 28945 | 3216 |
| contiguity:FBStmt | 5 | 3 | 28927 | 3216 |
| order_overlap:Lambda | 0 | 0 | 25363 | 2785 |
| order_overlap:FuncDef | 0 | 0 | 20576 | 2509 |
| shape:Other_node:OtherExpr | 0 | 0 | 74 | 41 |
| absent_token:name | 8 | 3 | 55 | 20 |
| missing_token_inserted | 9 | 5 | 37 | 19 |
| contiguity:If | 4 | 1 | 26 | 7 |
| shape:Id_text_differs_from_token | 0 | 0 | 29 | 13 |
| contiguity:For | 0 | 0 | 24 | 16 |
| contiguity:Pr(file) | 3 | 3 | 20 | 20 |
| shape:Call_with_literal_callee | 0 | 0 | 19 | 15 |
| contiguity:FuncDef | 0 | 0 | 17 | 13 |
| contiguity:DefStmt | 0 | 0 | 17 | 13 |
| absent_token:. | 2 | 1 | 12 | 4 |
| contiguity:Lambda | 5 | 3 | 8 | 5 |
| contiguity:ExprStmt | 5 | 3 | 7 | 5 |
| contiguity:Arg | 5 | 3 | 4 | 4 |
| contiguity:Call | 5 | 3 | 4 | 4 |
| ghost_token | 0 | 0 | 8 | 5 |
| order_overlap:Call(op) | 0 | 0 | 6 | 2 |
| absent_token:[ | 0 | 0 | 4 | 1 |
| absent_token:] | 0 | 0 | 4 | 1 |
| absent_token:nil | 0 | 0 | 4 | 2 |
| absent_token:str | 2 | 1 | 2 | 1 |
| absent_token:.. | 1 | 1 | 1 | 1 |
| contiguity:Assign | 0 | 0 | 2 | 2 |
| absent_token:and | 1 | 1 | 1 | 1 |
| absent_token:num | 1 | 1 | 1 | 1 |
| absent_token:or | 1 | 1 | 1 | 1 |
| shape:empty_Id | 0 | 0 | 2 | 2 |
| absent_token:> | 1 | 1 | 1 | 1 |
| order_overlap:ArrayAccess | 0 | 0 | 1 | 1 |
| contiguity:While | 0 | 0 | 1 | 1 |
| absent_token:= | 0 | 0 | 1 | 1 |
| contiguity:Return | 0 | 0 | 1 | 1 |
| order_overlap:stmt_list | 0 | 0 | 1 | 1 |
| contiguity:Container | 0 | 0 | 1 | 1 |
| order_overlap:DotAccess | 0 | 0 | 1 | 1 |

### 5.4 Classification summary

| cause | count (new, clean files) | class |
|---|---|---|
| extra values of local/global declaration dropped | 4 places (46 node + 19 token violations) | (a), pre-existing |
| empty `[[]]` content token | 10 | (b) |
| parse errors (invalid/truncated source) | 6 files | (d) |
| everything else | 0 | - |

## 6. Verdict

Real translation bugs found (new parser): **one**, inherited from the old translation:

- **Extra values of `local` (and `global`) declarations are dropped.**
  ```lua
  local a = f(), g()
  ```
  AST: `DefStmt(a, VarDef {vinit = Call(f)})` — the call `g()` does not exist anywhere in the
  AST. Also `local t = {...}, conn`. 4 occurrences in 5 346 files. Fix suggestion: keep the
  extra values (e.g. as `ExprStmt`s after the DefStmts, or translate `local a = x, y` as
  `DefStmt a` + the whole RHS as a Tuple like `assign` does).

No other structural defect was found: on 5 346 files / 3.38 M tokens / 125 721 block
constructs, every function, if, while, for, repeat and do block is present once, with the
right `end`/`until`, the right nesting, function bodies spanning `)`..`end`; every node's
children are in source order without overlap; no token is lost or attached to the wrong node
apart from 5.1; no forbidden shape appears.

Residual risk / limitations of the method:
- It checks *where* tokens are, not *what* the nodes mean: a wrong operator kind (e.g. `Lt` for
  `<=`), a swapped literal value, wrong `Int`/`Float`, wrong string normalisation or a wrong
  constructor with the right tokens (e.g. DotAccess vs ArrayAccess) is not detected (other
  agents' lexical/differential checks cover those).
- Tokens dropped by design (`( ) , ; then else`, the `end` of `if`, `=` of declarations/fields)
  are not checked for presence; a lost `if` end cannot be seen, but If ranges and nesting are.
- Synthetic content (fake `nil`, positional keys, `self`) is invisible to a token oracle.
- If branches use fake brackets, so their exact extents are only checked through contiguity.
- The lexer treats bytes >= 0x80 as identifier characters (consistent with the corpus; no
  disagreement was observed).
Estimated residual risk of an undetected *structural* loss: very low (0 violations in ~7.8 M
nodes besides 5.1); semantic mislabelling remains outside this oracle's scope.
