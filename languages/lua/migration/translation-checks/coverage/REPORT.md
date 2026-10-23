# Coverage oracle (source token coverage) — report

(Written by the orchestrator from the agent's final message: the agent was not allowed to write .md files.)

## Method
Own Lua lexer (shebang, line/long comments, short strings incl. escapes/`\z`, long strings, numbers incl. hex/float/LuaJIT suffixes) plus a bracket/block context classifier. From the new parser's `-full_token_info -dump_ast -json`, collect all origin tokens (bytepos, text). Forward check: every source token that must be kept appears at the same offset with the same text. Reverse check: every AST origin token corresponds to a source token at its offset (else garbled/misplaced); duplicates reported. Tokens dropped by design (from the converter): `then`, `else`, the `end` of `if`, `::`, `=` in `local`/table fields, `[ ]` of keys, expression parentheses, `<`/`>` of attributes, `,` (except numeric for), `;` (except after `return`), `.` in `function a.b` names. Results confirm the split (kept contexts ~100%, dropped 0%). Extra `opcheck.py`: operator kind vs source, arity, operand side.

## How to run
`python3 luacov.py {new|old} ../files.txt raw/{new|old}.jsonl` (resumable); `python3 aggregate.py > raw/agg.md`; `python3 opcheck.py new ../files.txt raw/opcheck_new.jsonl 0 3`; `python3 luacov.py one new FILE`.

## Coverage
5352 files with both parsers, no timeouts. New: 5346 clean; 6 with parse errors, all invalid Lua (d). Old: 5004 clean, 348 with parse errors. Per project: `raw/agg.md`.

## Statistics (new, 5346 clean files)

| category | expected | AST | missing | extra |
|---|---:|---:|---:|---:|
| identifiers | 1,589,116 | 1,589,108 | 8 | 0 |
| keywords (kept) | 383,032 | 383,032 | 0 | 0 |
| nil/true/false | 52,739 | 52,739 | 0 | 0 |
| numbers | 122,209 | 122,208 | 1 | 0 |
| strings | 296,465 | 296,463 | 2 | 10 |
| `...` | 3,031 | 3,031 | 0 | 0 |
| operators | 173,744 | 173,740 | 4 | 0 |
| punctuation (kept) | 1,441,106 | 1,441,102 | 4 | 0 |
| **total** | **4,061,445** | **4,061,426** | **19** | **10** |

Agreement 99.9995%; 27/30 projects 100%. Old vs new on the 5002 files both parse: new misses 17 tokens, old 622; no token covered by old and missed by new. Operator check: 1782 files, 57,570 operators, 0 problems.

## Discrepancies

| cause | missing | extra | class |
|---|---:|---:|---|
| extra values of a `local` declaration dropped | 17 tokens / 4 statements | 0 | (a) |
| empty long string `[[]]`: content token and `]]` share an offset (oracle map) | 0 | 10 | (c) |
| `return function ... end, self, i;` context tracking (Penlight `pl/xml.lua:467,526`) | 0 | 0 | (c) |
| files with parse errors | 9 | 0 | (d) |
| dropped/duplicated by design | 933,134 | 0 | (b) |

## Verdict
One real bug: `local a = f(), g()` keeps only `f()` (`mk_vars`, `| [], _y :: _ys -> []`), pre-existing in the old parser. Occurrences: `lua_lua/testes/gc.lua:661`, `luvit_luvit/tests/test-tls-client-abort.lua:26`, `test-tls-client-abort2.lua:26`, `teal-language_tl/teal/debug.lua:49`. Otherwise no lost, garbled, misplaced or unexpectedly duplicated tokens, no regressions. Limitations: presence/placement, not tree structure (only operator kind/arity/side); dropped-by-design tokens and fake parts not checked; semantic values not checked.
