# Checking the Lua -> AST_generic translation

Five independent oracles that estimate whether the Lua translation loses or
garbles constructs (objects, functions, methods, parts of expressions), run on
a sample of the corpus of [../projects.md](../projects.md): 5352 files,
~1.13M lines, from all 30 projects (at most 400 files per project, without
the tarantool REPL transcripts; see [BRIEF.md](BRIEF.md), which also describes
the environment the scripts expect: the sample list in `/tmp/tc/files.txt`, the
`ogb` build container, and the old/new `opengrep-core` binaries).

Each directory has the tool, `REPORT.md` and `summary.json`.

| Oracle | What is compared | Files checked (new) | Agreement (new parser) | Old parser |
|---|---|---:|---|---|
| [lexical](lexical) | constructs found by an independent Lua lexer (names, strings, numbers, functions, calls, method calls, fields, tables, operators, assignments, returns, control flow) vs the same constructs in the AST, by byte position and properties | 5346 | 3,581,715 constructs, 99.9994%; 22 differences, all from one bug | 99.947% (chained calls split, generic-for truncated, `-- [[`, `1ULL`) |
| [external](external) | the AST of a reference parser, luaparse 0.3.1, vs AST_generic: 35 categories of facts at exact source positions, plus operand/callee positions and the enclosing function | 5301 (51 not supported by luaparse) | 3.62M facts 99.9995%, 1.63M structural facts 99.9993%; 11 differences, all from one bug | 301 files with parse errors, 649 calls lost |
| [coverage](coverage) | every source token that must be kept is in the AST at the same offset with the same text, and the reverse; operator kind/arity/operand side | 5346 | 4,061,445 tokens, 99.9995%; 19 missing, all from one bug (+10 oracle artifacts); operators: 57,570 checked, 0 problems | misses 622 tokens vs 17 for the new parser on the same files |
| [structure](structure) | structural invariants: node token ranges contiguous, children ordered and disjoint, top-level statements cover the source, block tree (function/if/while/for/repeat/do ... end) identical, forbidden shapes | 5346 | 55,195 functions and 125,721 blocks: 0 errors; 66 violations, all from one bug | 196,089 violations (function bodies, `OtherExpr`, empty names) |
| [differential](differential) | normalized old vs new ASTs (6.2M nodes, 4983 files) and 42 generic rules (~2M matches) with both binaries, plus an independent lexical count; number values vs Lua rules | 4983 / 4974 | 4809 identical files; every difference is an old-parser bug or by design; >= 99.9% of rule match ranges identical | old-parser bugs listed |

## Findings

All five found the same real bug, which the previous parser had too:
values without a name in a `local` declaration were dropped, `g()` in
`local a = f(), g()` (4 occurrences in the sample). Fixed: they are kept as
expression statements.

The differential oracle also found that integers beyond 64 bits had no value
(Lua reads a decimal one as a float and keeps the low 64 bits of a hex one).
Fixed.

After the fixes, the coverage oracle finds no uncovered token in the 4 files
with the bug.

Not translation bugs, worth knowing:

* `-$X` matches every positive integer literal: a generic equivalence of the
  matcher (`Generic_vs_generic.ml`, "(-) E vs -n") that never applied when Lua
  numbers were all floats.
* The JSON AST dump prints int64 values above 2^62 and floats above ~1e15 as
  0 (the text dump is right).

Limitations, shared by the methods: the meaning of nodes is only partly
checked (operators by the coverage oracle; Int/Float and values by the
lexical, external and differential oracles); name resolution and attributes
are not checked; Lua 5.4/5.5-only syntax is not supported by luaparse.
