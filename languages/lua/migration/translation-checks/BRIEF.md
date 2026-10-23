# Shared brief: checking the correctness of the Lua -> AST_generic translation

## Goal

opengrep (a semgrep fork, OCaml) translates Lua source code with a new tree-sitter grammar
(tree-sitter-grammars/tree-sitter-lua v0.5.0) into AST_generic. We want an independent,
automatic oracle that estimates whether the translation is *correct*: no lost objects
(functions, methods, tables, fields), no lost or garbled parts of expressions (calls, operators,
literals, names), right structure. You build ONE checking method (yours is described in your task),
run it on the shared sample of real code, and report statistics.

## Environment (read carefully)

- Host: Ubuntu, you run commands with Bash. `python3` (with `yaml`), `node`/`npm` (v25), `gcc`,
  `sudo apt-get install` work. Network access works (npm, apt, pip with `--break-system-packages`).
- The opengrep build lives in a docker container named `ogb` (Alpine). Run the binaries there:
  `docker exec ogb sh -c '...'`. The host dir `/tmp` is mounted as `/host-tmp` in the container,
  the repo `/workspaces/opengrep` as `/src`.
- Binaries (inside the container):
  - NEW parser: `/src/bin/opengrep-core` (branch lua-tree-sitter-migration).
  - OLD parser (previous Lua grammar, for comparison): `/host-tmp/opengrep-core-old`.
- Useful commands (inside the container):
  - AST as JSON, with token positions:
    `/src/bin/opengrep-core -lang lua -full_token_info -dump_ast -json FILE`
    (stdout = JSON; logs go to stderr; there may be a few log lines before the JSON on stdout,
    so parse from the first `{`). Tokens look like
    `{"token":{"OriginTok":{"str":"t","bytepos":6,"line":1,"column":6,"file":"..."}}}`;
    fake tokens look like `{"FakeTok":...}` / `"()"` without position.
    Without `-full_token_info` tokens are elided (`"()"`).
  - Text dump (OCaml-ish): `-dump_ast FILE`.
  - Run a rule: `opengrep-core -lang lua -rules RULE.yaml FILE -json` (one target file per call).
  - Parsing statistics of a dir: `opengrep-core -lang lua -parsing_stats -json DIR`.
  - Timeout: wrap long runs, a few files can be slow; use `timeout 20` per file.
- The sample: `/tmp/tc/files.txt` (host paths are the same with `/tmp` instead of `/host-tmp`;
  the list uses container paths `/host-tmp/corpus/...`). 5352 Lua files, ~1.13M lines, from 30
  real projects (Kong, apisix, LuaLS, xmake, awesome, koreader, neovim plugins, tarantool, teal,
  ZeroBrane, luarocks, Penlight, ...; LuaJIT, 5.1-5.4 code; at most 400 files per project).
  One path contains spaces and non-ASCII: read the list line by line, quote paths.
- A file whose parse has errors: the JSON dump still prints the (partial) AST; the exit code or
  stderr says there were errors. Track files with parse errors separately (don't count their
  mismatches as translation bugs, but report how many).

## How the translation works (so you know what to expect)

Converter: `/workspaces/opengrep/languages/lua/generic/Parse_lua_tree_sitter.ml` (read it).
Grammar differences / translation table: `/workspaces/opengrep/languages/lua/migration/grammar-differences.md`.
Highlights:
- `local a, b = x` -> one `DefStmt` (VarDef) per name, attrs `KeywordAttr Static`.
- `a, b = x, y` -> `ExprStmt (Assign (Tuple, Tuple))`; missing values become `nil` literals
  (so the AST can contain fake `nil`s that are not in the source).
- `function a.b:m(x)` -> `DefStmt` with a qualified name and an inserted parameter `self`
  (fake, not in the source). `local function f` -> DefStmt + Static attr. Anonymous -> `Lambda`.
- Calls -> `Call`; method calls `o:m(x)` -> `Call (N (IdQualified {name_last=m; QExpr o}))`.
- Binary/unary operators -> `Call (IdSpecial (Op ...))`. Parentheses are dropped.
- Tables -> `Container Dict` of `Container Tuple [key; value]`; positional fields get
  synthetic integer keys 1, 2, ... (fake); `x = v` gets a string key "x".
- Strings -> `L String` (value = source text between quotes; long strings normalized);
  numbers -> `L Int` / `L Float` / `L Imag`; `nil/true/false` -> literals.
- Numeric for -> `ForEach (PatId i, Call (IdSpecial (Op Range), [a; b; c]))`; generic for ->
  `ForEach (PatTuple/PatId, iter or Tuple)`. `repeat ... until c` -> `DoWhile (.., not c)`.
- `return a, b` -> `Return (Tuple [a; b])`. Comments and the shebang line are dropped.
- Lua 5.4/5.5: `<const>` -> `KeywordAttr Const`, `global x` -> `DefStmt UseOuterDecl`, etc.

## Rules for you

- Do NOT modify anything under `/workspaces/opengrep` and do NOT run dune/make/git there; do not
  rebuild opengrep. Other agents use the same container and binaries concurrently.
- Work only in your directory `/tmp/tc/<your-id>/` (create it). Put there: your tool (script
  or program), the raw results, and `REPORT.md`.
- Be efficient: 5352 files, the machine has 4 cores shared with 4 other agents. Run at most 2
  processes in parallel. If your method is expensive, you may process a subset, but say exactly
  which and why; prefer covering as many files/projects as possible.
- Validate your oracle: for the top discrepancies, look at the source and the AST yourself and
  classify each one as (a) a real translation bug, (b) an expected difference by design (see
  above: synthetic keys, inserted self, dropped parens, fake nil...), (c) a limitation of your
  oracle, (d) a parse error in that file. Only (a) matters for the final verdict; be strict and
  give concrete evidence (file, line, source snippet, what the AST contains).
- Old vs new: where it helps, run the same check with the old parser too, so we see whether a
  discrepancy is new.

## REPORT.md must contain

1. Method: what is compared to what, and why it detects lost/garbled constructs.
2. How to run it (command line).
3. Coverage: files processed / skipped / with parse errors / timeouts, per project.
4. Statistics table: per category (e.g. functions, methods, calls, tables, fields, strings,
   numbers, identifiers, operators, ...): expected count (oracle), AST count, missing, extra,
   agreement %; overall and per project (top projects at least).
5. Discrepancies: grouped by cause with counts, and 3-5 concrete examples each, classified
   (a)/(b)/(c)/(d) as above.
6. Verdict: list of real translation bugs found (with minimal reproducer: a few lines of Lua and
   what the AST wrongly contains), or a clear statement that none were found, with the
   estimated residual risk. Plus limitations of the method.

Also write `/tmp/tc/<your-id>/summary.json`: {"method": ..., "files_processed": N,
"files_with_parse_errors": N, "categories": {cat: {"expected": n, "actual": n, "missing": n,
"extra": n}}, "real_bugs": [{"title":..., "reproducer":..., "count": n}]}.

Your final message (to the orchestrator) must be a concise summary: method, coverage, key
numbers, real bugs found (with reproducers), and the path of your REPORT.md.
