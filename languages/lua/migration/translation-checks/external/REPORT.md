# External reference parser check of the Lua -> AST_generic translation (agent `external`)

## 1. Method

Oracle: **luaparse 0.3.1** (npm, independent hand-written JS parser), run with `luaVersion: "5.3"`
(fallback `LuaJIT`, then `5.1`), `ranges: true`, `encodingMode: pseudo-latin1`, `extendedIdentifiers: true`.
Files are read as latin1, so luaparse offsets are byte offsets, the same unit as opengrep's `bytepos`.
A shebang line is blanked (offsets kept).

Both ASTs are reduced to a multiset of **facts** `(category, byte position, descriptor)`:

| category | position | descriptor |
|---|---|---|
| func_global / func_local / func_method | last name of the function name | qualified name `a.b:m` + `/nparams` (inserted `self` excluded) |
| func_anon | `function` keyword | nparams |
| param | parameter token | name or `...` |
| call / method_call | **end** of the call (`)`, string or `}`) | nargs / `method/nargs` |
| table | `{` | number of fields |
| field_pos / field_named / field_bracket | value start / key name / key start | key name |
| local_var | name | name, `=` if it gets a value |
| assign | first target | `ntargets=nvalues` (fake `nil`s not counted) |
| return | `return` | number of values |
| if / elseif / while / repeat / do / for_num / for_gen / for_var / break / goto / label | keyword or name | `#elseif+else`, numeric 2/3 exprs, `nvars in nexprs`, label name |
| binop / unop | operator token | Lua operator (`..`, `~=`, `//`, `not`, ...) |
| string | first token | value (short: text between quotes; long: normalized as in the brief) |
| number | token | raw text; `number_kind`: expected Int/Float/Imag |
| bool / nil / vararg | token | |
| dot_access / index_access | field name / index expr start | field name |
| ident | every name token (variables, fields, params, function-name parts, labels, `x =` keys) | name |

Structure checks (detect garbled trees, not only lost nodes):
* `binop_shape` / `unop_shape`: start position of each operand (catches wrong precedence/associativity, swapped operands);
* `call_shape`: start of callee (receiver for method calls) and of every argument;
* `scope:<cat>`: the `function` keyword position of the innermost enclosing function for every
  function, call, method call, table, local, assignment, return, if, loop, binop and string
  (catches statements moved to the wrong body / lost scopes).

Matching: for each category and position, equal descriptors are *matched*; leftover pairs at the
same position are *desc-mismatch*; leftovers only in the oracle are *missing*, only in the AST are
*extra*. Because every node is identified by its exact source position, a lost or duplicated node,
a wrong literal value, a dropped argument, a wrong operator or a mis-nested subtree shows up
individually with its location.

By-design differences are neutralised in the extraction (and counted as notes):
inserted `self` parameter (token `:`), fake `nil` values in assignments, synthetic positional keys,
dropped parentheses (oracle positions skip leading `(`), `not` of `repeat ... until` (token `until`),
one `DefStmt` per local name, `Tuple` wrapping of multiple targets/values/returns, `Range` call of numeric for.

## 2. How to run

```
cd /tmp/tc/external
npm install luaparse                                   # once
node extract_ref.js /tmp/tc/files.txt ref.jsonl         # oracle facts, ~10 s
NPROC=1 python3 run.py new                              # -> results_new.jsonl (resumable)
NPROC=1 python3 run.py old                              # -> results_old.jsonl (old parser)
python3 -c 'import analyze; analyze.report_tables()'    # -> report_tables.md
python3 rt.py FILE.lua [BINARY]                         # one file, prints all discrepancies
```
Files: `extract_ref.js` (oracle), `extract_ast.py` (AST_generic JSON -> facts), `compare.py`,
`run.py` (docker exec per file, `timeout 20`), `analyze.py`, `repro/` (reproducers).

## 3. Coverage

All 5352 files of the sample were processed with both parsers. 51 files are skipped because
luaparse rejects them (Lua 5.4 `<const>`/`<close>` attributes, mostly `lua_lua/testes`, LuaLS meta,
luacheck tests) - an oracle limitation, not opengrep's. No timeouts.
**The new parser parses all 5301 remaining files without error; the old parser fails on 301 of them**
(old files with parse errors are excluded from the old statistics; 1 old file produced no AST).

### Coverage per project (new parser; old in parentheses)

| project | files | luaparse failed (skipped) | opengrep parse errors | timeouts/no AST | compared |
|---|---:|---:|---:|---:|---:|
| Kong_kong | 400 | 1 | 0 (16) | 0 (0) | 399 (383) |
| L3MON4D3_LuaSnip | 124 | 0 | 0 (4) | 0 (0) | 124 (120) |
| LuaLS_lua-language-server | 400 | 18 | 0 (28) | 0 (0) | 382 (354) |
| apache_apisix | 400 | 0 | 0 (12) | 0 (0) | 400 (388) |
| awesomeWM_awesome | 400 | 0 | 0 (15) | 0 (0) | 400 (385) |
| folke_lazy.nvim | 65 | 0 | 0 (3) | 0 (0) | 65 (62) |
| folke_snacks.nvim | 141 | 0 | 0 (10) | 0 (0) | 141 (131) |
| hrsh7th_nvim-cmp | 57 | 0 | 0 (1) | 0 (0) | 57 (56) |
| kikito_inspect.lua | 4 | 0 | 0 (0) | 0 (0) | 4 (4) |
| kikito_middleclass | 11 | 0 | 0 (1) | 0 (0) | 11 (10) |
| koreader_koreader | 400 | 0 | 0 (32) | 0 (0) | 400 (368) |
| lite-xl_lite-xl | 89 | 0 | 0 (2) | 0 (0) | 89 (87) |
| lua_lua | 34 | 23 | 0 (7) | 0 (0) | 11 (4) |
| luarocks_luarocks | 162 | 0 | 0 (16) | 0 (0) | 162 (146) |
| lunarmodules_Penlight | 115 | 0 | 0 (8) | 0 (0) | 115 (107) |
| lunarmodules_busted | 98 | 1 | 0 (3) | 0 (0) | 97 (94) |
| lunarmodules_luacheck | 135 | 3 | 0 (13) | 0 (0) | 132 (119) |
| luvit_luvit | 199 | 0 | 0 (1) | 0 (0) | 199 (198) |
| mpeterv_argparse | 15 | 0 | 0 (2) | 0 (1) | 15 (12) |
| neovim_nvim-lspconfig | 400 | 0 | 0 (2) | 0 (0) | 400 (398) |
| nvim-lualine_lualine.nvim | 119 | 0 | 0 (8) | 0 (0) | 119 (111) |
| nvim-telescope_telescope.nvim | 77 | 0 | 0 (5) | 0 (0) | 77 (72) |
| nvim-treesitter_nvim-treesitter | 79 | 2 | 0 (6) | 0 (0) | 77 (71) |
| openresty_lua-resty-core | 38 | 0 | 0 (3) | 0 (0) | 38 (35) |
| pkulchenko_ZeroBraneStudio | 324 | 0 | 0 (30) | 0 (0) | 324 (294) |
| rxi_json.lua | 6 | 0 | 0 (1) | 0 (0) | 6 (5) |
| rxi_lume | 3 | 0 | 0 (0) | 0 (0) | 3 (3) |
| tarantool_tarantool | 400 | 0 | 0 (59) | 0 (0) | 400 (341) |
| teal-language_tl | 257 | 0 | 0 (6) | 0 (0) | 257 (251) |
| xmake-io_xmake | 400 | 3 | 0 (7) | 0 (0) | 397 (390) |
| **total** | 5352 | 51 | 0 (301) | 0 (1) | 5301 (4999) |


## 4. Statistics

Agreement % = matched / max(expected, actual). `expected` = oracle (luaparse), `actual` = AST_generic.

### Per-category statistics, new parser (files without parse errors)

| category | expected | actual | matched | missing | extra | desc-mismatch | agreement % |
|---|---:|---:|---:|---:|---:|---:|---:|
| func_global | 8154 | 8154 | 8154 | 0 | 0 | 0 | 100.000 |
| func_local | 8565 | 8565 | 8565 | 0 | 0 | 0 | 100.000 |
| func_method | 8429 | 8429 | 8429 | 0 | 0 | 0 | 100.000 |
| func_anon | 28792 | 28792 | 28792 | 0 | 0 | 0 | 100.000 |
| param | 50985 | 50985 | 50985 | 0 | 0 | 0 | 100.000 |
| call | 214096 | 214096 | 214096 | 0 | 0 | 0 | 100.000 |
| method_call | 75426 | 75426 | 75426 | 0 | 0 | 0 | 100.000 |
| table | 103545 | 103544 | 103544 | 1 | 0 | 0 | 99.999 |
| field_pos | 81007 | 81007 | 81007 | 0 | 0 | 0 | 100.000 |
| field_named | 165675 | 165675 | 165675 | 0 | 0 | 0 | 100.000 |
| field_bracket | 25647 | 25647 | 25647 | 0 | 0 | 0 | 100.000 |
| local_var | 113103 | 113103 | 113103 | 0 | 0 | 0 | 100.000 |
| assign | 73113 | 73113 | 73113 | 0 | 0 | 0 | 100.000 |
| return | 44557 | 44557 | 44557 | 0 | 0 | 0 | 100.000 |
| if | 55137 | 55137 | 55137 | 0 | 0 | 0 | 100.000 |
| elseif | 5562 | 5562 | 5562 | 0 | 0 | 0 | 100.000 |
| while | 1149 | 1149 | 1149 | 0 | 0 | 0 | 100.000 |
| repeat | 166 | 166 | 166 | 0 | 0 | 0 | 100.000 |
| do | 461 | 461 | 461 | 0 | 0 | 0 | 100.000 |
| for_num | 2544 | 2544 | 2544 | 0 | 0 | 0 | 100.000 |
| for_gen | 9240 | 9240 | 9240 | 0 | 0 | 0 | 100.000 |
| for_var | 20329 | 20329 | 20329 | 0 | 0 | 0 | 100.000 |
| break | 1442 | 1442 | 1442 | 0 | 0 | 0 | 100.000 |
| goto | 342 | 342 | 342 | 0 | 0 | 0 | 100.000 |
| label | 172 | 172 | 172 | 0 | 0 | 0 | 100.000 |
| binop | 134791 | 134787 | 134787 | 4 | 0 | 0 | 99.997 |
| unop | 30205 | 30205 | 30205 | 0 | 0 | 0 | 100.000 |
| string | 290122 | 290120 | 290120 | 2 | 0 | 0 | 99.999 |
| number | 115706 | 115705 | 115705 | 1 | 0 | 0 | 99.999 |
| bool | 32615 | 32615 | 32615 | 0 | 0 | 0 | 100.000 |
| nil | 18901 | 18901 | 18901 | 0 | 0 | 0 | 100.000 |
| vararg | 2890 | 2890 | 2890 | 0 | 0 | 0 | 100.000 |
| dot_access | 311650 | 311648 | 311648 | 2 | 0 | 0 | 99.999 |
| index_access | 37592 | 37592 | 37592 | 0 | 0 | 0 | 100.000 |
| ident | 1552336 | 1552328 | 1552328 | 8 | 0 | 0 | 99.999 |
| number_kind | 115706 | 115705 | 115705 | 1 | 0 | 0 | 99.999 |
| **all (w/o number_kind)** | 3624446 | 3624428 | 3624428 | | | | 99.9995 |

Structure checks (tree shape and enclosing function):

| category | expected | actual | matched | missing | extra | desc-mismatch | agreement % |
|---|---:|---:|---:|---:|---:|---:|---:|
| binop_shape | 134791 | 134787 | 134787 | 4 | 0 | 0 | 99.997 |
| unop_shape | 30205 | 30205 | 30205 | 0 | 0 | 0 | 100.000 |
| call_shape | 289522 | 289522 | 289522 | 0 | 0 | 0 | 100.000 |
| scope:func_global | 8154 | 8154 | 8154 | 0 | 0 | 0 | 100.000 |
| scope:func_local | 8565 | 8565 | 8565 | 0 | 0 | 0 | 100.000 |
| scope:func_method | 8429 | 8429 | 8429 | 0 | 0 | 0 | 100.000 |
| scope:func_anon | 28792 | 28792 | 28792 | 0 | 0 | 0 | 100.000 |
| scope:call | 214096 | 214096 | 214096 | 0 | 0 | 0 | 100.000 |
| scope:method_call | 75426 | 75426 | 75426 | 0 | 0 | 0 | 100.000 |
| scope:table | 103545 | 103544 | 103544 | 1 | 0 | 0 | 99.999 |
| scope:local_var | 113103 | 113103 | 113103 | 0 | 0 | 0 | 100.000 |
| scope:assign | 73113 | 73113 | 73113 | 0 | 0 | 0 | 100.000 |
| scope:return | 44557 | 44557 | 44557 | 0 | 0 | 0 | 100.000 |
| scope:if | 55137 | 55137 | 55137 | 0 | 0 | 0 | 100.000 |
| scope:while | 1149 | 1149 | 1149 | 0 | 0 | 0 | 100.000 |
| scope:repeat | 166 | 166 | 166 | 0 | 0 | 0 | 100.000 |
| scope:for_num | 2544 | 2544 | 2544 | 0 | 0 | 0 | 100.000 |
| scope:for_gen | 9240 | 9240 | 9240 | 0 | 0 | 0 | 100.000 |
| scope:binop | 134791 | 134787 | 134787 | 4 | 0 | 0 | 99.997 |
| scope:string | 290122 | 290120 | 290120 | 2 | 0 | 0 | 99.999 |
| **all (w/o number_kind)** | 1625447 | 1625436 | 1625436 | | | | 99.9993 |

By-design notes counted in the AST (new): {'fake_nil': 15, 'repeat_not': 166, 'self_inserted': 8429, 'number_value_mismatch': 20}

Discrepancy groups (new): ident/missing: 8, binop/missing: 4, scope:binop/missing: 4, binop_shape/missing: 4, scope:string/missing: 2, string/missing: 2, dot_access/missing: 2, scope:table/missing: 1, table/missing: 1, number/missing: 1, number_kind/missing: 1


### Per project, new parser: agreement over all base categories

| project | expected facts | matched | missing | extra | desc-mismatch | agreement % |
|---|---:|---:|---:|---:|---:|---:|
| koreader_koreader | 575961 | 575961 | 0 | 0 | 0 | 100.0000 |
| Kong_kong | 378410 | 378410 | 0 | 0 | 0 | 100.0000 |
| pkulchenko_ZeroBraneStudio | 330923 | 330923 | 0 | 0 | 0 | 100.0000 |
| tarantool_tarantool | 317389 | 317389 | 0 | 0 | 0 | 100.0000 |
| apache_apisix | 281356 | 281356 | 0 | 0 | 0 | 100.0000 |
| LuaLS_lua-language-server | 232809 | 232809 | 0 | 0 | 0 | 100.0000 |
| teal-language_tl | 183342 | 183329 | 13 | 0 | 0 | 99.9929 |
| folke_snacks.nvim | 159752 | 159752 | 0 | 0 | 0 | 100.0000 |
| awesomeWM_awesome | 146132 | 146132 | 0 | 0 | 0 | 100.0000 |
| luarocks_luarocks | 129237 | 129237 | 0 | 0 | 0 | 100.0000 |
| xmake-io_xmake | 123415 | 123415 | 0 | 0 | 0 | 100.0000 |
| lite-xl_lite-xl | 83423 | 83423 | 0 | 0 | 0 | 100.0000 |
| lunarmodules_Penlight | 78321 | 78321 | 0 | 0 | 0 | 100.0000 |
| L3MON4D3_LuaSnip | 75523 | 75523 | 0 | 0 | 0 | 100.0000 |
| nvim-telescope_telescope.nvim | 74795 | 74795 | 0 | 0 | 0 | 100.0000 |
| lunarmodules_luacheck | 72800 | 72800 | 0 | 0 | 0 | 100.0000 |
| luvit_luvit | 72522 | 72518 | 4 | 0 | 0 | 99.9945 |
| folke_lazy.nvim | 54378 | 54378 | 0 | 0 | 0 | 100.0000 |
| nvim-lualine_lualine.nvim | 46536 | 46536 | 0 | 0 | 0 | 100.0000 |
| hrsh7th_nvim-cmp | 39588 | 39588 | 0 | 0 | 0 | 100.0000 |
| lunarmodules_busted | 30671 | 30671 | 0 | 0 | 0 | 100.0000 |
| nvim-treesitter_nvim-treesitter | 28353 | 28353 | 0 | 0 | 0 | 100.0000 |
| neovim_nvim-lspconfig | 27120 | 27120 | 0 | 0 | 0 | 100.0000 |
| lua_lua | 26114 | 26113 | 1 | 0 | 0 | 99.9962 |
| openresty_lua-resty-core | 22386 | 22386 | 0 | 0 | 0 | 100.0000 |
| mpeterv_argparse | 13352 | 13352 | 0 | 0 | 0 | 100.0000 |
| rxi_lume | 8554 | 8554 | 0 | 0 | 0 | 100.0000 |
| kikito_middleclass | 5067 | 5067 | 0 | 0 | 0 | 100.0000 |
| kikito_inspect.lua | 3481 | 3481 | 0 | 0 | 0 | 100.0000 |
| rxi_json.lua | 2736 | 2736 | 0 | 0 | 0 | 100.0000 |

### Per-category statistics, old parser (files without parse errors)

| category | expected | actual | matched | missing | extra | desc-mismatch | agreement % |
|---|---:|---:|---:|---:|---:|---:|---:|
| func_global | 7177 | 7178 | 7177 | 0 | 1 | 0 | 99.986 |
| func_local | 6911 | 6910 | 6910 | 1 | 0 | 0 | 99.986 |
| func_method | 6482 | 6482 | 6482 | 0 | 0 | 0 | 100.000 |
| func_anon | 25357 | 25357 | 25357 | 0 | 0 | 0 | 100.000 |
| param | 41747 | 41747 | 41747 | 0 | 0 | 0 | 100.000 |
| call | 182952 | 181549 | 181549 | 1403 | 0 | 0 | 99.233 |
| method_call | 62069 | 62069 | 62069 | 0 | 0 | 0 | 100.000 |
| table | 87426 | 87426 | 87426 | 0 | 0 | 0 | 100.000 |
| field_pos | 73158 | 73158 | 73158 | 0 | 0 | 0 | 100.000 |
| field_named | 136067 | 136067 | 136067 | 0 | 0 | 0 | 100.000 |
| field_bracket | 16416 | 16416 | 16416 | 0 | 0 | 0 | 100.000 |
| local_var | 96262 | 96262 | 96262 | 0 | 0 | 0 | 100.000 |
| assign | 59266 | 59265 | 59261 | 4 | 3 | 1 | 99.992 |
| return | 36946 | 36946 | 36946 | 0 | 0 | 0 | 100.000 |
| if | 44293 | 44293 | 44293 | 0 | 0 | 0 | 100.000 |
| elseif | 4488 | 4488 | 4488 | 0 | 0 | 0 | 100.000 |
| while | 905 | 905 | 905 | 0 | 0 | 0 | 100.000 |
| repeat | 128 | 128 | 128 | 0 | 0 | 0 | 100.000 |
| do | 327 | 327 | 327 | 0 | 0 | 0 | 100.000 |
| for_num | 2090 | 2090 | 2090 | 0 | 0 | 0 | 100.000 |
| for_gen | 7013 | 7013 | 6991 | 0 | 0 | 22 | 99.686 |
| for_var | 15667 | 15667 | 15667 | 0 | 0 | 0 | 100.000 |
| break | 1141 | 1141 | 1141 | 0 | 0 | 0 | 100.000 |
| goto | 287 | 287 | 287 | 0 | 0 | 0 | 100.000 |
| label | 137 | 137 | 137 | 0 | 0 | 0 | 100.000 |
| binop | 108885 | 108881 | 108881 | 4 | 0 | 0 | 99.996 |
| unop | 24766 | 24766 | 24766 | 0 | 0 | 0 | 100.000 |
| string | 251597 | 251595 | 251595 | 2 | 0 | 0 | 99.999 |
| number | 93450 | 93449 | 93448 | 1 | 0 | 1 | 99.998 |
| bool | 26121 | 26121 | 26121 | 0 | 0 | 0 | 100.000 |
| nil | 15206 | 15202 | 15202 | 4 | 0 | 0 | 99.974 |
| vararg | 2285 | 2285 | 2285 | 0 | 0 | 0 | 100.000 |
| dot_access | 263668 | 263657 | 263656 | 12 | 1 | 0 | 99.995 |
| index_access | 28992 | 28988 | 28988 | 4 | 0 | 0 | 99.986 |
| ident | 1281098 | 1281054 | 1281043 | 55 | 11 | 0 | 99.996 |
| number_kind | 93450 | 93449 | 2249 | 1 | 0 | 91200 | 2.407 |
| **all (w/o number_kind)** | 3010780 | 3009306 | 3009266 | | | | 99.9497 |

Structure checks (tree shape and enclosing function):

| category | expected | actual | matched | missing | extra | desc-mismatch | agreement % |
|---|---:|---:|---:|---:|---:|---:|---:|
| binop_shape | 108885 | 108881 | 108881 | 4 | 0 | 0 | 99.996 |
| unop_shape | 24766 | 24766 | 24766 | 0 | 0 | 0 | 100.000 |
| call_shape | 245021 | 243618 | 243569 | 1403 | 0 | 49 | 99.407 |
| scope:func_global | 7177 | 7178 | 7177 | 0 | 1 | 0 | 99.986 |
| scope:func_local | 6911 | 6910 | 6910 | 1 | 0 | 0 | 99.986 |
| scope:func_method | 6482 | 6482 | 6482 | 0 | 0 | 0 | 100.000 |
| scope:func_anon | 25357 | 25357 | 25357 | 0 | 0 | 0 | 100.000 |
| scope:call | 182952 | 181549 | 181549 | 1403 | 0 | 0 | 99.233 |
| scope:method_call | 62069 | 62069 | 62069 | 0 | 0 | 0 | 100.000 |
| scope:table | 87426 | 87426 | 87426 | 0 | 0 | 0 | 100.000 |
| scope:local_var | 96262 | 96262 | 96262 | 0 | 0 | 0 | 100.000 |
| scope:assign | 59266 | 59265 | 59262 | 4 | 3 | 0 | 99.993 |
| scope:return | 36946 | 36946 | 36946 | 0 | 0 | 0 | 100.000 |
| scope:if | 44293 | 44293 | 44293 | 0 | 0 | 0 | 100.000 |
| scope:while | 905 | 905 | 905 | 0 | 0 | 0 | 100.000 |
| scope:repeat | 128 | 128 | 128 | 0 | 0 | 0 | 100.000 |
| scope:for_num | 2090 | 2090 | 2090 | 0 | 0 | 0 | 100.000 |
| scope:for_gen | 7013 | 7013 | 7013 | 0 | 0 | 0 | 100.000 |
| scope:binop | 108885 | 108881 | 108881 | 4 | 0 | 0 | 99.996 |
| scope:string | 251597 | 251595 | 251595 | 2 | 0 | 0 | 99.999 |
| **all (w/o number_kind)** | 1364431 | 1361614 | 1361561 | | | | 99.7897 |

By-design notes counted in the AST (old): {'fake_nil': 5, 'self_inserted': 6482, 'other_expr': 74, 'repeat_not': 128, 'number_value_mismatch': 13}

Discrepancy groups (old): number_kind/desc: 82308, call_shape/missing: 1172, scope:call/missing: 834, call/missing: 649, ident/missing: 55, call_shape/desc: 49, for_gen/desc: 22, dot_access/missing: 12, ident/extra: 11, assign/missing: 4, scope:assign/missing: 4, nil/missing: 4, index_access/missing: 4, binop_shape/missing: 4, binop/missing: 4, scope:binop/missing: 4, scope:assign/extra: 3, assign/extra: 3, scope:string/missing: 2, string/missing: 2, scope:func_global/extra: 1, assign/desc: 1, dot_access/extra: 1, scope:func_local/missing: 1, func_global/extra: 1, func_local/missing: 1, number/desc: 1, number_kind/missing: 1, number/missing: 1


### Per project, old parser: agreement over all base categories

| project | expected facts | matched | missing | extra | desc-mismatch | agreement % |
|---|---:|---:|---:|---:|---:|---:|
| koreader_koreader | 461313 | 461298 | 12 | 0 | 3 | 99.9967 |
| Kong_kong | 335612 | 335603 | 9 | 0 | 0 | 99.9973 |
| apache_apisix | 260168 | 260165 | 3 | 0 | 0 | 99.9988 |
| tarantool_tarantool | 259919 | 259905 | 13 | 1 | 1 | 99.9946 |
| pkulchenko_ZeroBraneStudio | 236684 | 236675 | 7 | 0 | 2 | 99.9962 |
| LuaLS_lua-language-server | 179741 | 178625 | 1114 | 1 | 2 | 99.3791 |
| folke_snacks.nvim | 136299 | 136293 | 6 | 0 | 0 | 99.9956 |
| awesomeWM_awesome | 132243 | 132199 | 40 | 0 | 4 | 99.9667 |
| teal-language_tl | 126881 | 126863 | 18 | 0 | 0 | 99.9858 |
| xmake-io_xmake | 115722 | 115716 | 6 | 0 | 0 | 99.9948 |
| luarocks_luarocks | 100675 | 100671 | 2 | 0 | 2 | 99.9960 |
| lite-xl_lite-xl | 79741 | 79741 | 0 | 0 | 0 | 100.0000 |
| luvit_luvit | 72168 | 72068 | 100 | 0 | 0 | 99.8614 |
| lunarmodules_Penlight | 68900 | 68892 | 7 | 6 | 1 | 99.9884 |
| L3MON4D3_LuaSnip | 67609 | 67602 | 5 | 0 | 2 | 99.9896 |
| nvim-telescope_telescope.nvim | 63961 | 63953 | 8 | 2 | 0 | 99.9875 |
| lunarmodules_luacheck | 55508 | 55492 | 13 | 0 | 3 | 99.9712 |
| folke_lazy.nvim | 53197 | 53194 | 3 | 2 | 0 | 99.9944 |
| hrsh7th_nvim-cmp | 38708 | 38705 | 3 | 0 | 0 | 99.9922 |
| nvim-lualine_lualine.nvim | 34184 | 34184 | 0 | 0 | 0 | 100.0000 |
| lunarmodules_busted | 30242 | 30220 | 18 | 0 | 4 | 99.9273 |
| nvim-treesitter_nvim-treesitter | 25710 | 25707 | 3 | 0 | 0 | 99.9883 |
| neovim_nvim-lspconfig | 25106 | 25106 | 0 | 0 | 0 | 100.0000 |
| openresty_lua-resty-core | 19376 | 19376 | 0 | 0 | 0 | 100.0000 |
| rxi_lume | 8554 | 8554 | 0 | 0 | 0 | 100.0000 |
| mpeterv_argparse | 7224 | 7126 | 98 | 4 | 0 | 98.6434 |
| lua_lua | 6038 | 6038 | 0 | 0 | 0 | 100.0000 |
| kikito_middleclass | 4369 | 4369 | 0 | 0 | 0 | 100.0000 |
| kikito_inspect.lua | 3481 | 3481 | 0 | 0 | 0 | 100.0000 |
| rxi_json.lua | 1447 | 1445 | 2 | 0 | 0 | 99.8618 |

## 5. Discrepancies, grouped by cause

### New parser: 11 discrepant facts in total, all from one cause

**N1 - extra values of `local` dropped (a): 4 occurrences (4 files, 1 + 2 + 1 + 1 objects affected).**
`mk_vars` in `Parse_lua_tree_sitter.ml` (`| [], _y :: _ys -> []`) discards the values that have no
name. The oracle reports the nodes inside them as missing:
* `lua_lua/testes/gc.lua:661` `local a = 'xuxu'..(10+3)..'joao', {}` - table `{}` missing (table, scope:table).
* `teal-language_tl/teal/debug.lua:49` `local name = info.name or "<anon>", info.currentline > 0 and "@" .. info.currentline or ""`
  - 4 binops, 2 strings, number `0`, 2 dot accesses, 4 identifiers of the second expression missing.
* `luvit_luvit/tests/test-tls-client-abort.lua:26` `local options = {...}, conn` - ident `conn` missing;
  `test-tls-client-abort2.lua:26` `..., conn, onConnect, onError` - 3 idents missing.

Verified by hand on the AST dump and with a rule (`repro/extra_local.lua`, `repro/r.yaml`):
pattern `os.execute(...)` matches `local a = os.execute(cmd)` but **not** `local a = 1, os.execute(cmd)`
(0 results with both the new and the old binary). Pre-existing (identical in the old parser), but a
real loss of code: a call with side effects hidden in an extra value is invisible to rules.

**Not counted as bugs (c, oracle/dumper limitation):** 20 `number_value_mismatch` notes: floats of
magnitude >= ~1e17 (`1e20`, `1e+42`, `1e70`, `1e100`, `0x1.fffffep+127`) appear as `{"some":0}` in the
**JSON** dump, but the text dump (`-dump_ast`) shows the correct value (`Some(100000000000000000000.000000)`),
so it is a JSON-serialisation artefact, not a translation error. Same with the old binary.

By-design differences seen and neutralised (b): 8429 inserted `self`, 166 `repeat` negations,
15 fake `nil` values (`a, b = x`), synthetic positional keys for 81007 positional fields.

### Old parser (for comparison): discrepancies the new parser fixed

* **O1 call chains split** (649 calls missing, 1172 call_shape): `lm:rule "x" { ... }` / `f "a" "b"` -
  the old grammar ends the call after the string argument and makes `{...}` / `"b"` a separate
  expression statement (D09). Ex.: `LuaLS make.lua` (many `lm:rule "..." {`), repro `repro/chain.lua`.
* **O2 `-- [[` comment swallows code** (D02): `Kong kong/tools/queue.lua` `-- [[ For testing purposes only`
  hides `Queue._CAPACITY_WARNING_THRESHOLD = ...` (assign missing); `Penlight luabalanced.lua`: `-- [=[...]=], etc.`
  comment swallows the `local` of `local function match_bracketed` (func_local becomes func_global, idents `local`, `etc` extra).
* **O3 generic `for` iterator list truncated** (22 for_gen desc, D22): `for k, v in next, encode_escape_map do`
  (LuaLS jsonc.lua, json-edit.lua), `for k,_ in next, _G, nil do` (busted context.lua: `_G`, `nil` missing).
* **O4 numbers**: all literals are `Float` (91200 number_kind mismatches, by design of the old translation);
  `1ULL` -> number `1` + identifier `ULL` (tarantool gh_6119_select_big_numbers_test.lua, D06).
* **O5 `self` / `next`**: `self` as `IdSpecial Self` (counted as ident), `next` as `OtherExpr` (74 notes);
  a few ident extras with empty name around `self =` targets (Penlight test-app.lua, telescope layout.lua).
* plus the 301 files the old parser cannot parse at all, and the same N1 bug.

## 6. Verdict

**One real translation bug found, pre-existing (also in the old parser), rare (4 occurrences in 5301 files):**

```lua
local a = 1, os.execute(cmd)
```
AST: `DefStmt(a, VarDef {vinit = Some (L (Int 1))})` only - the expression `os.execute(cmd)` is
not in the AST at all, so `pattern: os.execute(...)` finds nothing. Same for
`local b, c = f(), g(), h()` (`h()` lost). Fix: keep the extra expressions, e.g. as an
`ExprStmt` after the definitions or as a tuple value, as `assign` already does for `x = a, b`.

Apart from that, on 5301 files / 3.62M base facts (+1.63M structure facts) the new translation
agrees with the oracle on **every** function (global/local/method/anonymous: 53,940), parameter,
call (214,096) and method call (75,426), table and field, local, assignment, return, control-flow
statement, operator, string, number (value text and Int/Float kind), identifier, field/index access,
the tree shape of every operator and call, and the enclosing function of every statement.
Agreement: base 99.9995 %, structure 99.9993 %, all residual differences explained by N1.
No parse errors on oracle-valid files (old: 301).

Residual risk: low for constructs that luaparse understands. **Limitations:**
* Lua 5.4/5.5 syntax (attributes, `global`, named varargs) and LuaJIT
  `LL/ULL/i` literals are not covered by the oracle (51 files skipped; luaparse's LuaJIT mode
  does not parse `ULL`); these features are only covered by the migration's own rule tests.
* String values are compared as raw source text (escapes not interpreted, as in the translation).
* Not checked: attributes (`Static`/`Const`), id_info/naming, statement order inside a block,
  which value goes to which name in multi-assignments (only counts), patterns (semgrep extensions).
* Files with parse errors would be excluded (none for the new parser).
