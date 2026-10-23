# Lexical oracle for the Lua -> AST_generic translation (id: `lexical`)

## 1. Method

**What is compared.** An independent Lua lexer written in Python (`lexcheck.py`, no tree-sitter, no opengrep code) tokenizes each
source file: the shebang line, `--` / `--[[ ]]` / `--[==[ ]==]` comments, short strings with escapes (`\"`, `\`+newline, `\z`),
long strings `[==[ ]==]`, numbers (decimal, hex, hex floats, `0b`, LuaJIT `LL`/`ULL`/`i`), names (incl. UTF-8 bytes), keywords and all
operators. A light token-level analysis (bracket matching, `function/if/do/repeat ... end/until` matching, a small forward/backward
expression-list scanner) derives **constructs keyed by the byte position of a characteristic source token**, with a value:

| category | key token | value checked |
|---|---|---|
| name | every identifier | the name |
| string / number / nil / true / false / vararg | the literal | string value (raw text between quotes; long strings normalized), number text |
| number_value | number literal | Int/Float/Imag kind and the numeric value Lua would read |
| function | `function` | kind (statement / method `a:m` / local / anonymous / global) and number of params (inserted `self` excluded) |
| local, local_name, local_values | `local`, each declared name | number of names; (names, number of values that end up in the AST) |
| call | `(` / string / `{` of the arguments | number of arguments |
| method_call | `:` | method name, number of arguments |
| dot, index | `.`, `[` | field name |
| table | `{` | (positional, `name =`, `[k] =`) field counts |
| op | the operator token | operator and arity (unary/binary told apart, `<const>` attrs excluded) |
| assign | `=` | (number of targets, number of values) |
| return | `return` | number of returned values |
| for / for_iter | `for` | numeric (number of range exprs) / generic (number of vars); number of iterator exprs |
| while, repeat, if, elseif, goto, label, break | keyword / label name | for `if`/`elseif`: whether an `else` follows; goto target |
| else (count only) | `else` | per-file count |

The AST side (`AstExtractor` in `lexcheck.py`) walks the `-full_token_info -dump_ast -json` output and extracts the same constructs from
the AST nodes (`Call`, `IdSpecial Op`, `IdQualified/QExpr`, `DotAccess`, `ArrayAccess`, `Container Dict`, `DefStmt FuncDef/VarDef`,
`Lambda`, `Return`, `For/ForEach`, `While`, `DoWhile`, `If`, `Goto`, `Label`, `Break`, `Assign`, `L ...`), keyed by the `bytepos` of
the corresponding `OriginTok`. Names are all `OriginTok`s whose text is a name (excluding long-string content tokens).

Per file and category: **matched** (same key, same value), **missing** (in source, not in AST = lost construct), **extra** (in AST,
not in source = invented/shifted construct), **mismatch** (same key, other value = garbled: wrong arity, lost argument/field/value,
wrong operator, wrong literal). Because keys are exact byte positions, a lost sub-expression shows up as missing names/calls/ops at
precise lines, and a lost element of a list as an arity mismatch.

**By-design differences that are neutralized:** fake tokens (no `OriginTok`) are ignored: synthetic positional keys `1, 2, ...`
(counted: 82,278), fake `nil`s added to assignments (31), the `Op Range` of numeric `for` (token `,`, 2,770) and the `Not` of
`repeat ... until` (token `until`, 183) are skipped; the inserted `self` parameter is recognised by its `:` token and removed from the
parameter count; `x = v` table keys (fake-bracket strings, 167,325) are counted as named fields, not strings; dropped parentheses,
comments, `then`/`do`/`,`/`;` are not compared. The JSON dumper prints floats >= ~1e15 as `0` (also for Python, the text dump
is right): this is a dumper artefact and is not counted as a mismatch.

**Validation of the oracle itself:** it was developed on the Lua 5.5 test suite (`lua_lua/testes`) and a 200-file sample until
it had zero false positives there; run on the OLD parser it detects every known old-grammar defect listed in grammar-differences.md
(see section 5), which shows the method is sensitive.

## 2. How to run

```sh
cd /tmp/tc/lexical
# one parser, resumable (skips files already done), optional start/step for 2 workers:
python3 run.py new results_new.jsonl 0 2 &  python3 run.py new results_new1.jsonl 1 2
python3 run.py old results_old.jsonl 0 2 &  python3 run.py old results_old1.jsonl 1 2
python3 merge.py merged_new.jsonl results_new.jsonl results_new1.jsonl     # last good record per file
python3 merge.py merged_old.jsonl results_old*.jsonl
python3 aggregate.py merged_new.jsonl merged_old.jsonl > tables.md          # also writes stats.json
python3 gen_summary.py                                                       # summary.json
python3 lexcheck.py FILE.lua FILE.json    # debug one file (JSON from opengrep-core -full_token_info -dump_ast -json)
N=20 python3 peek.py merged_new.jsonl     # discrepancy examples (clean files)
```
`run.py` calls `docker exec ogb timeout 20 <bin> -lang lua -full_token_info -dump_ast -json FILE` per file and retries
infrastructure failures (container restarts, `vfork` EAGAIN). Raw per-file results: `merged_new.jsonl`, `merged_old.jsonl`.

## 3. Coverage

All 5352 files of `/tmp/tc/files.txt` were processed with both parsers (whole sample, no subset). New parser: 5349 files with an
AST, of which 3 have parse errors; 3 files produce no AST at all; 0 timeouts. All 6 are genuinely invalid Lua (checked by hand):
`Kong/spec/fixtures/invalid-module.lua` (`local a = "hello",`), `busted/spec/cl_compile_fail.lua` (prose), `luacheck/spec/samples/compound_operators.lua`
(`i += 10`), `luacheck/spec/samples/utf8_error.lua` (invalid UTF-8 name), `nvim-treesitter/tests/indent/lua/{cond,func}.lua` (truncated
on purpose, unclosed `if`/`function`) -> class (d). Old parser: 346 files with parse errors, 2 without AST.

### Coverage per project (new parser)

| project | files | processed (AST) | with parse errors | no AST | timeouts | other |
|---|---:|---:|---:|---:|---:|---:|
| Kong_kong | 400 | 400 | 1 | 0 | 0 | 0 |
| L3MON4D3_LuaSnip | 124 | 124 | 0 | 0 | 0 | 0 |
| LuaLS_lua-language-server | 400 | 400 | 0 | 0 | 0 | 0 |
| apache_apisix | 400 | 400 | 0 | 0 | 0 | 0 |
| awesomeWM_awesome | 400 | 400 | 0 | 0 | 0 | 0 |
| folke_lazy.nvim | 65 | 65 | 0 | 0 | 0 | 0 |
| folke_snacks.nvim | 141 | 141 | 0 | 0 | 0 | 0 |
| hrsh7th_nvim-cmp | 57 | 57 | 0 | 0 | 0 | 0 |
| kikito_inspect.lua | 4 | 4 | 0 | 0 | 0 | 0 |
| kikito_middleclass | 11 | 11 | 0 | 0 | 0 | 0 |
| koreader_koreader | 400 | 400 | 0 | 0 | 0 | 0 |
| lite-xl_lite-xl | 89 | 89 | 0 | 0 | 0 | 0 |
| lua_lua | 34 | 34 | 0 | 0 | 0 | 0 |
| luarocks_luarocks | 162 | 162 | 0 | 0 | 0 | 0 |
| lunarmodules_Penlight | 115 | 115 | 0 | 0 | 0 | 0 |
| lunarmodules_busted | 98 | 97 | 0 | 1 | 0 | 0 |
| lunarmodules_luacheck | 135 | 135 | 2 | 0 | 0 | 0 |
| luvit_luvit | 199 | 199 | 0 | 0 | 0 | 0 |
| mpeterv_argparse | 15 | 15 | 0 | 0 | 0 | 0 |
| neovim_nvim-lspconfig | 400 | 400 | 0 | 0 | 0 | 0 |
| nvim-lualine_lualine.nvim | 119 | 119 | 0 | 0 | 0 | 0 |
| nvim-telescope_telescope.nvim | 77 | 77 | 0 | 0 | 0 | 0 |
| nvim-treesitter_nvim-treesitter | 79 | 77 | 0 | 2 | 0 | 0 |
| openresty_lua-resty-core | 38 | 38 | 0 | 0 | 0 | 0 |
| pkulchenko_ZeroBraneStudio | 324 | 324 | 0 | 0 | 0 | 0 |
| rxi_json.lua | 6 | 6 | 0 | 0 | 0 | 0 |
| rxi_lume | 3 | 3 | 0 | 0 | 0 | 0 |
| tarantool_tarantool | 400 | 400 | 0 | 0 | 0 | 0 |
| teal-language_tl | 257 | 257 | 0 | 0 | 0 | 0 |
| xmake-io_xmake | 400 | 400 | 0 | 0 | 0 | 0 |
| **total** | 5352 | 5349 | 3 | 3 | 0 | 0 |

## 4. Statistics

Agreement % = matched / max(expected, AST). "all" excludes `number_value`.

### new parser, files without parse errors

| category | expected | AST | matched | missing | extra | value mismatch | agreement % |
|---|---:|---:|---:|---:|---:|---:|---:|
| name | 1589177 | 1589169 | 1589169 | 8 | 0 | 0 | 99.9995 |
| string | 296465 | 296463 | 296463 | 2 | 0 | 0 | 99.9993 |
| number | 122209 | 122208 | 122208 | 1 | 0 | 0 | 99.9992 |
| number_value | 122208 | 122208 | 122208 | 0 | 0 | 0 | 100.0000 |
| nil | 19557 | 19557 | 19557 | 0 | 0 | 0 | 100.0000 |
| true | 22046 | 22046 | 22046 | 0 | 0 | 0 | 100.0000 |
| false | 11136 | 11136 | 11136 | 0 | 0 | 0 | 100.0000 |
| vararg | 3031 | 3031 | 3031 | 0 | 0 | 0 | 100.0000 |
| function | 55195 | 55195 | 55195 | 0 | 0 | 0 | 100.0000 |
| local | 102344 | 102344 | 102344 | 0 | 0 | 0 | 100.0000 |
| local_name | 115973 | 115973 | 115973 | 0 | 0 | 0 | 100.0000 |
| local_values | 102344 | 102344 | 102340 | 0 | 0 | 4 | 99.9961 |
| call | 223833 | 223833 | 223833 | 0 | 0 | 0 | 100.0000 |
| method_call | 75869 | 75869 | 75869 | 0 | 0 | 0 | 100.0000 |
| dot | 317153 | 317151 | 317151 | 2 | 0 | 0 | 99.9994 |
| index | 38879 | 38879 | 38879 | 0 | 0 | 0 | 100.0000 |
| table | 105163 | 105162 | 105162 | 1 | 0 | 0 | 99.9990 |
| op | 173744 | 173740 | 173740 | 4 | 0 | 0 | 99.9977 |
| assign | 74798 | 74798 | 74798 | 0 | 0 | 0 | 100.0000 |
| return | 45702 | 45702 | 45702 | 0 | 0 | 0 | 100.0000 |
| for | 12303 | 12303 | 12303 | 0 | 0 | 0 | 100.0000 |
| for_iter | 9533 | 9533 | 9533 | 0 | 0 | 0 | 100.0000 |
| while | 1198 | 1198 | 1198 | 0 | 0 | 0 | 100.0000 |
| repeat | 183 | 183 | 183 | 0 | 0 | 0 | 100.0000 |
| if | 56107 | 56107 | 56107 | 0 | 0 | 0 | 100.0000 |
| elseif | 5644 | 5644 | 5644 | 0 | 0 | 0 | 100.0000 |
| goto | 431 | 431 | 431 | 0 | 0 | 0 | 100.0000 |
| label | 239 | 239 | 239 | 0 | 0 | 0 | 100.0000 |
| break | 1481 | 1481 | 1481 | 0 | 0 | 0 | 100.0000 |
| **all** | | | 3581715 | | | | 99.9994 |

Including the 3 parse-error files with a partial AST the new parser has 11 more missing constructs (2 names, 1 string, 1 local, 1 local name, 4 operators, 1 assignment, 1 local value; all inside the
error regions), see `tables.md`.

### Old parser (same oracle, for comparison)

### old parser, files without parse errors

| category | expected | AST | matched | missing | extra | value mismatch | agreement % |
|---|---:|---:|---:|---:|---:|---:|---:|
| name | 1281306 | 1281253 | 1281251 | 55 | 2 | 0 | 99.9957 |
| string | 251610 | 251608 | 251608 | 2 | 0 | 0 | 99.9992 |
| number | 93463 | 93462 | 93461 | 1 | 0 | 1 | 99.9979 |
| number_value | 93462 | 93462 | 2249 | 0 | 0 | 91213 | 2.4063 |
| nil | 15207 | 15203 | 15203 | 4 | 0 | 0 | 99.9737 |
| true | 17398 | 17398 | 17398 | 0 | 0 | 0 | 100.0000 |
| false | 8727 | 8727 | 8727 | 0 | 0 | 0 | 100.0000 |
| vararg | 2285 | 2314 | 2285 | 0 | 29 | 0 | 98.7468 |
| function | 45942 | 45942 | 45941 | 0 | 0 | 1 | 99.9978 |
| local | 85185 | 85185 | 85185 | 0 | 0 | 0 | 100.0000 |
| local_name | 96295 | 96295 | 96295 | 0 | 0 | 0 | 100.0000 |
| local_values | 85185 | 85185 | 85182 | 0 | 0 | 3 | 99.9965 |
| call | 182977 | 181574 | 181574 | 1403 | 0 | 0 | 99.2332 |
| method_call | 62070 | 62070 | 62070 | 0 | 0 | 0 | 100.0000 |
| dot | 263678 | 263667 | 263666 | 12 | 1 | 0 | 99.9954 |
| index | 28992 | 28988 | 28988 | 4 | 0 | 0 | 99.9862 |
| table | 87429 | 87429 | 87428 | 0 | 0 | 1 | 99.9989 |
| op | 133686 | 133682 | 133682 | 4 | 0 | 0 | 99.9970 |
| assign | 59279 | 59278 | 59277 | 1 | 0 | 1 | 99.9966 |
| return | 36960 | 36960 | 36960 | 0 | 0 | 0 | 100.0000 |
| for | 9103 | 9103 | 9103 | 0 | 0 | 0 | 100.0000 |
| for_iter | 7013 | 7013 | 6991 | 0 | 0 | 22 | 99.6863 |
| while | 905 | 905 | 905 | 0 | 0 | 0 | 100.0000 |
| repeat | 128 | 128 | 128 | 0 | 0 | 0 | 100.0000 |
| if | 44303 | 44303 | 44303 | 0 | 0 | 0 | 100.0000 |
| elseif | 4489 | 4489 | 4489 | 0 | 0 | 0 | 100.0000 |
| goto | 287 | 287 | 287 | 0 | 0 | 0 | 100.0000 |
| label | 137 | 137 | 137 | 0 | 0 | 0 | 100.0000 |
| break | 1141 | 1141 | 1141 | 0 | 0 | 0 | 100.0000 |
| **all** | | | 2903665 | | | | 99.9469 |

### Per project (new parser, files without parse errors)

| project | clean files | expected (all cats) | matched | missing | extra | mismatch | agreement % | files with any discrepancy |
|---|---:|---:|---:|---:|---:|---:|---:|---:|
| Kong_kong | 399 | 369869 | 369869 | 0 | 0 | 0 | 100.0000 | 0 |
| L3MON4D3_LuaSnip | 124 | 73653 | 73653 | 0 | 0 | 0 | 100.0000 | 0 |
| LuaLS_lua-language-server | 400 | 256537 | 256537 | 0 | 0 | 0 | 100.0000 | 0 |
| apache_apisix | 400 | 284415 | 284415 | 0 | 0 | 0 | 100.0000 | 0 |
| awesomeWM_awesome | 400 | 140161 | 140161 | 0 | 0 | 0 | 100.0000 | 0 |
| folke_lazy.nvim | 65 | 50072 | 50072 | 0 | 0 | 0 | 100.0000 | 0 |
| folke_snacks.nvim | 141 | 153929 | 153929 | 0 | 0 | 0 | 100.0000 | 0 |
| hrsh7th_nvim-cmp | 57 | 39127 | 39127 | 0 | 0 | 0 | 100.0000 | 0 |
| kikito_inspect.lua | 4 | 3405 | 3405 | 0 | 0 | 0 | 100.0000 | 0 |
| kikito_middleclass | 11 | 5038 | 5038 | 0 | 0 | 0 | 100.0000 | 0 |
| koreader_koreader | 400 | 547110 | 547110 | 0 | 0 | 0 | 100.0000 | 0 |
| lite-xl_lite-xl | 89 | 81559 | 81559 | 0 | 0 | 0 | 100.0000 | 0 |
| lua_lua | 34 | 86238 | 86236 | 1 | 0 | 1 | 99.9977 | 1 |
| luarocks_luarocks | 162 | 132640 | 132640 | 0 | 0 | 0 | 100.0000 | 0 |
| lunarmodules_Penlight | 115 | 75985 | 75985 | 0 | 0 | 0 | 100.0000 | 0 |
| lunarmodules_busted | 97 | 31112 | 31112 | 0 | 0 | 0 | 100.0000 | 0 |
| lunarmodules_luacheck | 133 | 65224 | 65224 | 0 | 0 | 0 | 100.0000 | 0 |
| luvit_luvit | 199 | 72671 | 72665 | 4 | 0 | 2 | 99.9917 | 2 |
| mpeterv_argparse | 15 | 13285 | 13285 | 0 | 0 | 0 | 100.0000 | 0 |
| neovim_nvim-lspconfig | 400 | 22699 | 22699 | 0 | 0 | 0 | 100.0000 | 0 |
| nvim-lualine_lualine.nvim | 119 | 42043 | 42043 | 0 | 0 | 0 | 100.0000 | 0 |
| nvim-telescope_telescope.nvim | 77 | 73732 | 73732 | 0 | 0 | 0 | 100.0000 | 0 |
| nvim-treesitter_nvim-treesitter | 77 | 24073 | 24073 | 0 | 0 | 0 | 100.0000 | 0 |
| openresty_lua-resty-core | 38 | 24571 | 24571 | 0 | 0 | 0 | 100.0000 | 0 |
| pkulchenko_ZeroBraneStudio | 324 | 308503 | 308503 | 0 | 0 | 0 | 100.0000 | 0 |
| rxi_json.lua | 6 | 2709 | 2709 | 0 | 0 | 0 | 100.0000 | 0 |
| rxi_lume | 3 | 7982 | 7982 | 0 | 0 | 0 | 100.0000 | 0 |
| tarantool_tarantool | 400 | 303229 | 303229 | 0 | 0 | 0 | 100.0000 | 0 |
| teal-language_tl | 257 | 167494 | 167480 | 13 | 0 | 1 | 99.9916 | 1 |
| xmake-io_xmake | 400 | 122672 | 122672 | 0 | 0 | 0 | 100.0000 | 0 |

## 5. Discrepancies, grouped by cause

### New parser (5346 files without parse errors): 22 discrepant constructs in 4 files, one cause

**(a) `local` declaration with more values than names: the extra values are dropped** (4 statements, 4 files: 4 `local_values`
mismatches, and everything inside the dropped expressions shows up as missing: 8 names, 2 strings, 1 number, 2 dots, 4 ops, 1 table).
`mk_vars` in `Parse_lua_tree_sitter.ml` pairs names with values and has `| [], _y :: _ys -> []`.
- `lua_lua/testes/gc.lua:661` `local a = 'xuxu'..(10+3)..'joao', {}` -> the `{}` table is not in the AST.
- `luvit_luvit/tests/test-tls-client-abort.lua:23-26` `local options = { port = ..., host = ... }, conn` -> `conn` missing.
- `luvit_luvit/tests/test-tls-client-abort2.lua:23-26` `local options = {...}, conn, onConnect, onError` -> 3 names missing.
- `teal-language_tl/teal/debug.lua:49` `local name = info.name or "<anon>", info.currentline > 0 and "@" .. info.currentline or ""`
  -> the whole second expression (2 dot accesses, `>`, `and`, `..`, `or`, `0`, `"@"`, `""`) is absent.

Verified with a reproducer, `-dump_ast` on `local a = f(), g()` / `local b, c = 1, 2, h()`: the AST has `a = f()`, `b = 1`, `c = 2`
and no `g`/`h` at all. The old parser gives exactly the same AST (3 of these 4 statements are also flagged with the old parser; the 4th,
gc.lua, is in a file the old parser cannot parse), so **this is a real translation bug but not a regression** of the migration.
Impact: code in the extra values is invisible to every rule (e.g. `local x = 1, os.execute(cmd)` would not match `os.execute(...)`).
In real code it is rare (4 statements out of 102,344 `local` declarations) since Lua evaluates and discards those values.

No other discrepancy: 0 missing / 0 extra / 0 mismatched constructs in all other categories (1.59M names, 296K strings, 122K numbers
with exact Int/Float kind and value, 55K functions with kind and parameter count, 224K calls and 76K method calls with argument count,
317K field accesses, 105K tables with exact field-kind counts, 174K operators with arity, 75K assignments with target/value counts,
46K returns with value counts, all loops/ifs/elseif-else structure, gotos, labels, breaks; per-file `else` counts 8596 = 8596).

### Old parser (same oracle; 5004 files without parse errors) - shows the oracle's sensitivity

| cause | evidence (examples) | class |
|---|---|---|
| numbers always `Float` (D06) | 91,213 `number_value` mismatches, e.g. `big.lua:12` `2` -> `Float 2` | (b) old design, fixed in new |
| statement-level chained/curried calls split into 2 statements (D09): `f(a)(b)`, `f 'a' 'b'`, `f{}{}`, `require 'x'()`; the second call is lost (`(b)` becomes an expression statement, `()` becomes `Id "..."`) | 1,403 missing calls + 29 extra `...`: `LuaLS/make.lua:25` `lm:source_set 'lpeglabel' {...}`, `LuaLS/make/bootstrap.lua:8` `assert(load(expr, "=(command line)"))()`, `busted/spec/modules/luacov_spec.lua:1` `require 'busted.modules.luacov'()` | (a) for old, fixed in new |
| generic `for` keeps only the first iterator expression (D22) | 22 `for_iter` mismatches + 4 missing `nil` + names: `busted/busted/context.lua:5` `for k, v in next, t, nil`, `awesome/lib/awful/prompt.lua:706` | (a) for old, fixed in new |
| `-- [[` opens a block comment (D02), code swallowed | `Kong/kong/tools/queue.lua:659` assignment after `-- [[ For testing purposes only` missing; `Penlight/lua/pl/luabalanced.lua:61-67` statements shifted | (a) for old, fixed in new |
| `1ULL` read as `1` + name `ULL` (D06) | `tarantool/test/engine-luatest/gh_6119_select_big_numbers_test.lua:34` | (a) for old, fixed in new |
| `[ [[x]] ] = v` table key read as a named key | `rxi_json.lua/test/test.lua:161` (0,0,7) vs (0,7,0) | (a) for old, fixed in new |
| parse errors (shebang, `<const>`, `goto` in 5.1-style code, LuaJIT numbers, `\z`, ...) | 346 files vs 6 for the new parser | (d) |
| the same `local a = x, y` loss | luvit x2, teal debug.lua:49 | (a), shared |

Oracle limitation (c) found and fixed during the run: the old parser stores a long string as one token `[[...]]` with fake
brackets; the first pass misclassified those (8,857 "missing strings"); the extractor now handles both representations and the
affected 1,590 old-parser files were re-run.

## 6. Verdict

**Real translation bugs found in the new parser: 1** (pre-existing, shared with the old parser, not a regression):

1. *`local` with more values than names drops the extra values* (4 occurrences in the sample).
   Reproducer:
   ```lua
   local a = f(), g()
   local b, c = 1, 2, h()
   ```
   AST: `DefStmt a = Call f`, `DefStmt b = 1`, `DefStmt c = 2`; the calls `g()` and `h()` are not in the AST.
   Fix: in `mk_vars`, keep the extra values, e.g. as `ExprStmt`s after the definitions, or translate such declarations like
   `assign` does for `a, b = ...` (tuple of all values).

No other lost, invented or garbled construct was found: across the 5346 files that parse, 3,581,715 keyed constructs match exactly
and the only 22 discrepant ones are explained by bug 1. Compared with the old parser, the new one fixes the call-chain splitting
(1,403 lost calls), the generic-`for` truncation, `-- [[` comment swallowing, LuaJIT number mis-lexing and 340 parse failures.

**Residual risk / limitations of the method.**
- It checks *presence, position and local shape* (arity, field kinds, operator, literal value, kind of function/loop/if/else), not
  the full tree shape: wrong nesting with all pieces present (e.g. operator precedence/associativity, which operand goes to which
  operator, which block a statement belongs to, the else-if chaining beyond "an else follows this branch") is not detected. Since the
  grammar has the same precedences as Lua and the converter maps `left/right` fields directly, the risk is low but not measured here.
- It does not compare semantic attributes (`Static`/`Const` attrs, `id_resolved`, `ParamRest` vs `Param`), nor `Lua 5.5 global`
  declarations in depth (only present in `lua_lua/testes`).
- String values are compared as raw source text (as the converter defines them); escape interpretation is out of scope by design.
- Expected-value counts for `assign`/`return`/`local_values` rely on a heuristic expression-list scanner; it had no false positives
  on this sample, but contrived code could fool it.
- The 6 files with parse errors are excluded from the bug verdict (their partial ASTs differ only inside the error regions).

Files: `/tmp/tc/lexical/lexcheck.py` (oracle), `run.py`, `merge.py`, `aggregate.py`, `peek.py`, `gen_summary.py`,
`merged_new.jsonl` / `merged_old.jsonl` (raw per-file results), `tables.md` (all tables incl. old-parser coverage per project),
`stats.json`, `summary.json`.
