# Differential oracle (old vs new parser + pattern queries) — report

(Written by the orchestrator from the agent's final message: the agent was not allowed to write .md files.)

## Method
1. AST differential: both binaries dumped all 5352 files (`-full_token_info -dump_ast -json`). On files both parse cleanly, by-design differences are normalized (numbers by source text, old `IdSpecial Self` / `OtherExpr next` -> plain names, new generic-for tuple -> first element, strings by value, block-bracket tokens ignored), then trees compared recursively with lists aligned by source position; plus node-kind histograms and the set of origin tokens.
2. Pattern-query differential: 42 generic rules (one per construct) run with both binaries, per project; match ranges compared per file and rule; an independent comment/string-stripping lexer gives a third count.
3. Extra: value/kind of all 122,208 number literals vs Lua rules (`numcheck.py`); `local` statements with more values than names (`localextra.py`).

## Coverage
5352 files dumped by both, no timeouts/crashes. Parse errors: old 369, new 6 (all 6 also fail with old; intentionally invalid). AST differential: 4983 files, ~6.2M nodes, all 30 projects. Rules: 4974 files (9 excluded: "Too many matches"/"Internal matching error", both binaries). The 363 old-only error files have no reference here.

## Key numbers
- 4809/4983 files identical after normalization.
- Histograms old->new: Call +1,311, ExprStmt -1,315, NAME -4, DefStmt/UseOuterDecl/OtherAttribute +2 (Lua 5.5 global); identical otherwise (NUM 92,953, String 385,117, FuncDef 20,399, Lambda 24,986, tables 86,823, DotAccess 260,432, operators 134,483).
- Rules (~2M matches): >= 99.9% identical match ranges except 3 rules with explained changes (neg, do_block, string).

| rule | old | new | lexical |
|---|---:|---:|---:|
| call | 234,295 | 235,606 | 234,342 |
| method_decl | 6,297 | 6,505 | 6,375 |
| method_call | 59,772 | 59,772 | 54,752 |
| func_decl, lambda, table, if, while, repeat, concat, not, len, eq, neq, plus | identical | identical | equal in all 4974 files |

Per-file agreement with the lexical count is higher for the new parser (call 4,903 vs 4,793 files; method_decl 4,928 vs 4,863).

## Differences (checked by hand)
- Chained calls split by the old parser (~1,300 calls, 174 files): `f 'x' {..}`, `f(x)(y)`, `require 'm'(x)`... (old bug).
- `next(...)` lost by the old AST; `-- [[` swallowing code; Lua 5.5 `global` (old bugs).
- Generic-for tuple; function body block ranges starting at `)` (by design).
- method_decl +208: old-binary quirk in multi-rule runs.
- Oracle limitations identical for both binaries.
- JSON dumper prints int64 > 2^62 and large floats wrong (text dump right).

## Behaviour changes due to integers being Int (not translation bugs)
1. `-$X` matches every positive integer literal (+85,585), from the generic equivalence in `src/matching/Generic_vs_generic.ml:954` ("(-) E vs -n"), which never applied to Floats.
2. `"..."` can match a variable bound to `c and 1 or 2` (+70), generic constant propagation (Python does the same).

## Real defects (a)
1. Extra values of a `local` declaration dropped (old and new): `local x = f(), g()` -> only `x = f()`. `mk_vars` (`| [], _y :: _ys -> []`).
2. Decimal integer overflow (new, 2 occurrences, `lua testes math.lua:394`): `10000000000000000000000` -> `L(Int None)`; Lua reads a float 1e22.
3. Hex literal wider than 64 bits (new, 2 occurrences, `tpack.lua:74`): `0x13121110090807060504030201` -> `L(Int None)`; Lua keeps the low 64 bits.

## Limitations
Sees only differences (shared defects found only via the lexical count); constructs only in old-error files covered only by new-side rule runs; normalizations would hide wrong block-bracket positions or string token ranges.
