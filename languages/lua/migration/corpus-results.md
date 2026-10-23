# Corpus results

Parsing statistics of the old and the new Lua parser on the 30 projects of
[projects.md](projects.md), computed with [corpus-parse-rate.sh](corpus-parse-rate.sh)
(`opengrep-core -lang lua -parsing_stats`). "Old" is opengrep at
`5814ab3` (Azganoth/tree-sitter-lua), "new" is this branch.

The numbers only count *reported* errors. The old parser also silently
mis-parsed valid code that is not counted here: Lua 5.5 `global`
declarations, `0b101`/`10ULL` literals, code after `-- [[` (see
[grammar-differences.md](grammar-differences.md)).

| Project | Files | Files with errors (old → new) | Lines with errors (old → new) | Parsing rate (old → new) |
|---|---|---|---|---|
| lua/lua | 35 | 29 → 0 | 500 → 0 | 97.33% → 100.00% |
| LuaLS/lua-language-server | 722 | 88 → 0 | 789 → 0 | 99.57% → 100.00% |
| Kong/kong | 1307 | 58 → 1 | 191 → 1 | 99.94% → 100.00% |
| apache/apisix | 415 | 13 → 0 | 18 → 0 | 99.98% → 100.00% |
| openresty/lua-resty-core | 38 | 3 → 0 | 3 → 0 | 99.97% → 100.00% |
| luarocks/luarocks | 165 | 19 → 0 | 28 → 0 | 99.92% → 100.00% |
| lunarmodules/Penlight | 114 | 9 → 0 | 13 → 0 | 99.95% → 100.00% |
| lunarmodules/busted | 99 | 9 → 1 | 13 → 3 | 99.84% → 99.96% |
| lunarmodules/luacheck | 133 | 15 → 3 | 31 → 7 | 99.86% → 99.97% |
| xmake-io/xmake | 2460 | 71 → 0 | 78 → 0 | 99.97% → 100.00% |
| awesomeWM/awesome | 883 | 34 → 0 | 51 → 0 | 99.95% → 100.00% |
| koreader/koreader | 597 | 50 → 0 | 119 → 0 | 99.95% → 100.00% |
| lite-xl/lite-xl | 89 | 2 → 0 | 2 → 0 | 99.99% → 100.00% |
| luvit/luvit | 198 | 1 → 0 | 1 → 0 | 100.00% → 100.00% |
| neovim/nvim-lspconfig | 862 | 3 → 0 | 3 → 0 | 100.00% → 100.00% |
| nvim-telescope/telescope.nvim | 77 | 8 → 0 | 14 → 0 | 99.94% → 100.00% |
| folke/lazy.nvim | 65 | 5 → 0 | 12 → 0 | 99.92% → 100.00% |
| nvim-lualine/lualine.nvim | 119 | 8 → 0 | 12 → 0 | 99.90% → 100.00% |
| hrsh7th/nvim-cmp | 57 | 2 → 0 | 5 → 0 | 99.95% → 100.00% |
| L3MON4D3/LuaSnip | 130 | 4 → 0 | 10 → 0 | 99.97% → 100.00% |
| folke/snacks.nvim | 140 | 11 → 0 | 18 → 0 | 99.95% → 100.00% |
| nvim-treesitter/nvim-treesitter | 79 | 8 → 2 | 9 → 12 | 99.89% → 99.85% |
| tarantool/tarantool | 2007 | 692 → 276 | 13720 → 20535 | 97.26% → 95.89% |
| teal-language/tl | 259 | 9 → 0 | 23 → 0 | 99.97% → 100.00% |
| pkulchenko/ZeroBraneStudio | 13284 | 1271 → 0 | 2214 → 0 | 99.95% → 100.00% |
| kikito/inspect.lua | 4 | 0 → 0 | 0 → 0 | 100.00% → 100.00% |
| rxi/json.lua | 6 | 1 → 0 | 1 → 0 | 99.88% → 100.00% |
| rxi/lume | 3 | 0 → 0 | 0 → 0 | 100.00% → 100.00% |
| kikito/middleclass | 11 | 1 → 0 | 1 → 0 | 99.92% → 100.00% |
| mpeterv/argparse | 15 | 5 → 0 | 352 → 0 | 91.25% → 100.00% |
| **Total** | 24373 | 2429 → 283 | 18231 → 20558 | 99.72% → 99.68% |
| **Total without tarantool** | 22366 | 1737 → 7 | 4511 → 23 | 99.92% → 100.00% |

## Remaining errors

All the files that still have errors are invalid Lua on purpose:

| Project | File | Why |
|---|---|---|
| tarantool/tarantool | 276 `test/**/*.test.lua` files | REPL transcripts for tarantool's test runner, not Lua: bare expressions such as `i0:bsize() > bsize` and `\` line continuations. The old grammar accepted any expression as a statement, so it reported fewer of them; the error lines are higher with the new parser because a statement containing an error is reported as a whole |
| Kong/kong | `spec/fixtures/invalid-module.lua` | "Invalid module (syntax error)" fixture |
| lunarmodules/busted | `spec/cl_compile_fail.lua` | "it should just fail when it is being compiled" |
| lunarmodules/luacheck | `spec/samples/python_code.lua` | Python code |
| lunarmodules/luacheck | `spec/samples/compound_operators.lua` | `+=` and similar operators, not Lua |
| lunarmodules/luacheck | `spec/samples/utf8_error.lua` | invalid UTF-8, a test of luacheck's error |
| nvim-treesitter/nvim-treesitter | `tests/indent/lua/cond.lua`, `func.lua` | incomplete code (indentation tests) |

## Bug found with the corpus

`luacheck/spec/lexer_spec.lua` revealed that with the upstream grammar
`"--[[x"` starts a block comment inside the string, silently hiding the
code up to the next `]]` (17 corpus files have such strings). Fixed in the
semgrep extension by making short strings single tokens; covered by the
migration rule `D07-short-strings/d07-comment-start-in-string`.

## Migration rules

[rules/](rules) (110 rules, see [run-rules.sh](run-rules.sh)):

| Parser | Pass | Fail | Pattern does not parse |
|---|---|---|---|
| old (first 109 rules) | 90 | 11 | 8 |
| new | 110 | 0 | 0 |

Two rules also failed with the new parser at first; both were
investigated:

* `D16-function-names/d16-deep-method-name`: `function T:m()` patterns
  never matched files where `self` doesn't appear, because the rule
  prefilter required the implicit `self` parameter. Fixed in
  `src/rule/Pattern.ml` (this affected the old parser too), regression test in
  `tests/relevant_rules/`.
* `D22-generic-for/d22-single-var`: wrong expectation in the rule (a
  metavariable in a pattern position also matches a tuple of names).
