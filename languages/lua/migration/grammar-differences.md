# Differences between the old and the new Lua grammar

* **Old**: [Azganoth/tree-sitter-lua](https://github.com/Azganoth/tree-sitter-lua) @ `a943944`
  plus the semgrep extension of `ocaml-tree-sitter-semgrep` (only
  `identifier: /\$?[a-zA-Z_][a-zA-Z0-9_]*/`).
* **New**: [tree-sitter-grammars/tree-sitter-lua](https://github.com/tree-sitter-grammars/tree-sitter-lua) `v0.5.0`
  plus the semgrep extension in
  [`semgrep-lua-new/grammar/semgrep-lua/grammar.js`](../tree-sitter/semgrep-lua-new/grammar/semgrep-lua/grammar.js).

Each difference has an id `Dnn` and a directory `rules/Dnn-*` with 2-3
synthetic rules (meaningless as security rules) and annotated targets. They
check that the construct is parsed and translated to AST_generic correctly.
[check-coverage.sh](check-coverage.sh) verifies that every difference listed
here has at least 2 rules.

The "Old behavior" column describes what happens with the old parser.
"Translation" says how the new CST is mapped to AST_generic.

## Lexical level

| Id | Construct | Old grammar | New grammar | Old behavior | Translation |
|---|---|---|---|---|---|
| D01 | Shebang `#!/usr/bin/env lua` | not supported | `hash_bang_line` at the start of `chunk` | parse error on line 1 | ignored |
| D02 | Comments | single external token. After `--`, blanks are skipped and then `[[` starts a block comment, so `-- [[` (a line comment in Lua) opens a block comment | `comment` with `start`/`content`/`end`. Block comments only for `--[[`/`--[==[` | the code after `-- [[` is swallowed until the next `]]` | ignored (extras) |
| D03 | Identifiers | `\$?[a-zA-Z_][a-zA-Z0-9_]*`, no word token | any char except control chars, blanks, operators and digits at the start (so `$X` and LuaJIT UTF-8 names are accepted); `identifier` is the word token (keyword extraction) | `local café = 1` is a parse error | `Id` |
| D04 | `self`, `next`, `_G`, `_VERSION` | dedicated nodes `self`, `next`, `global_variable` | plain identifiers | `next` was translated to `OtherExpr("next")`, `self` to `IdSpecial Self` | `self` in an expression -> `IdSpecial Self` (unchanged); `next`, `_G`, `_VERSION` -> `Id` |
| D05 | Contextual keyword `global` used as a name (`local global`, `{global = 1}`, `print(global)`) | ordinary identifier | `_contextual_keyword` alternative of `variable` and of table keys | worked | `Id "global"` / string key `"global"` |
| D06 | Numbers | decimal (no more than one leading `0`), hex, hex float | + binary `0b101`, LuaJIT `ULL`/`LL` suffixes, imaginary `3i`, any number of leading zeros | `0b101`, `10ULL` silently mis-parsed as a number followed by an identifier statement; `007` broken | integers -> `Int`, floats -> `Float`, `…i` -> `Imag` (before: always `Float`) |
| D07 | Short strings `"…"`, `'…'` | external token including the quotes; stops at a newline, so `\` + newline and `\z` + newline break | `string` with `start`/`content`/`end`; `content` is `string_content` made of raw text and `escape_sequence` nodes | parse error on line continuations | `String` whose value is the source text between the quotes (escapes are not interpreted, as before). The semgrep extension makes short strings single tokens: upstream, `"--[[x"` started a block comment inside the string, hiding the code up to the next `]]` |
| D08 | Long strings `[[…]]`, `[==[…]==]` and string call arguments `f[[x]]` | external token | external `_block_string_start/content/end` | worked | `String`; the first newline is dropped and line breaks are normalized to `\n` (as before) |

## Statements

| Id | Construct | Old grammar | New grammar | Old behavior | Translation |
|---|---|---|---|---|---|
| D09 | Expression statements | any expression is a statement (`alias($._expression, $.expression)`) | only `function_call` | invalid code silently accepted; it is the root cause of the mis-parses of D06/D13/D15 | `ExprStmt (Call …)` |
| D10 | Assignment `a, b = x, y` | `variable_declaration` with `variable_declarator`s | `assignment_statement` with `variable_list` / `expression_list` | ok | `Assign`, tuples for several targets/values, missing values become `nil` (as before) |
| D11 | `local a, b = x` | `local_variable_declaration` | `variable_declaration` = `local` + (`variable_list` \| `assignment_statement`) | ok | one `DefStmt VarDef` per name with `KeywordAttr Static("local")` (as before) |
| D12 | Lua 5.4 attributes `local x <const> = 1`, `<close>`, Lua 5.5 prefix attribute `local <const> a, b` | not supported | `attribute` in `variable_list` | parse error | `<const>` -> `KeywordAttr Const`, `<close>` and other names -> `OtherAttribute`; a prefix attribute applies to all names |
| D13 | Lua 5.5 `global a, b` / `global a = 1` / `global <const> a` | not supported | `global_declaration: variable_declaration` | silently mis-parsed as the expression statement `global` followed by an assignment | without value: `DefStmt UseOuterDecl` (as Python's `global`); with value: `DefStmt VarDef` with `OtherAttribute "global"` |
| D14 | Lua 5.5 `global function f() end` | not supported | `global_declaration: function_declaration` | silently mis-parsed | `DefStmt FuncDef` with `OtherAttribute "global"` |
| D15 | Lua 5.5 `global *`, `global <const> *` | not supported | `implicit_variable_declaration` | mis-parsed as a multiplication | `DirectiveStmt (OtherDirective "global" …)` |
| D16 | Function names `function a.b.c:m()` | `function_name` / `function_name_field` | nested `dot_index_expression` / `method_index_expression` in `name` | ok | qualified name; `:` adds the implicit `self` parameter (as before) |
| D17 | `local function f()` | `local_function` | `function_declaration` in field `local_declaration` | ok | `DefStmt FuncDef` with `KeywordAttr Static("local")` |
| D18 | Parameters | `(self \| ... \| id) {, id} [, ...]`, `self` is a dedicated node | `name_list [, vararg]`; Lua 5.5 named vararg `...args` | `...args` is a parse error | `Param`; `...` -> `ParamRest "..."` (target) / `ParamEllipsis` (pattern); `...args` -> `ParamRest args` |
| D19 | `if / elseif / else` | `elseif`, `else` nodes, statements inline | `elseif_statement`, `else_statement`, `block` | ok | nested `If` (as before) |
| D20 | `while`, `repeat … until`, `do … end` | statements inline | `block` | ok | `While`, `DoWhile (not cond)`, `Block` (as before) |
| D21 | Numeric `for i = a, b, c` | `for_statement` + `loop_expression` | `for_statement` + `for_numeric_clause` (`start`/`end`/`step`) | ok | `ForEach (i, Range(a, b, c))` (as before) |
| D22 | Generic `for k, v in e1, e2, e3` | `for_in_statement` + `loop_expression` | `for_statement` + `for_generic_clause` | the expressions after the first were dropped | `ForEach`; several expressions are kept as a tuple |
| D23 | `return [explist] [;]` | `return_statement` + `_empty_statement` | `return_statement` + `expression_list` | ok | `Return (Tuple …)` (as before) |
| D24 | `goto`, `::label::`, `break`, `;` | inline | `goto_statement`, `label_statement`, `break_statement`, `empty_statement` | ok | `Goto`, `Label`, `Break`, nothing |

## Expressions

| Id | Construct | Old grammar | New grammar | Old behavior | Translation |
|---|---|---|---|---|---|
| D25 | Vararg `...` | `spread` | `vararg_expression` | ok | `Id "..."` (target) / `Ellipsis` (pattern) |
| D26 | Calls `f(x)`, `f"s"`, `f{…}`, `f(x)(y)` | `function_call_statement` (`_prefix arguments`) | `function_call` (`name`, `arguments`) | ok | `Call` |
| D27 | Method calls `o:m(x)` | `_prefix ':' method arguments` | `method_index_expression` in `function_call.name` | ok | `Call (IdQualified [o; m])` (as before) |
| D28 | Field access `a.b` | `field_expression` / `property_identifier` | `dot_index_expression` | ok | `DotAccess` |
| D29 | Index `a[k]` | `_variable_declarator` alternative | `bracket_index_expression` | ok | `ArrayAccess` |
| D30 | Parentheses `(e)` | no node | `parenthesized_expression` | ok | the inner expression (as before) |
| D31 | Anonymous functions `function(…) … end` | `function_definition` | `function_definition` with `block` | ok | `Lambda` |
| D32 | Table constructors | `table`, `field`, `_field_sequence` | `table_constructor`, `field` with `name`/`value` | ok | `Container Dict` of key/values, positional keys `1, 2, …` (as before) |
| D33 | Binary operators | `binary_operation` | `binary_expression` with `left`/`operator`/`right`, same precedences | ok | `Call (IdSpecial Op …)` |
| D34 | Unary operators | `unary_operation` | `unary_expression` | ok | `Call (IdSpecial Op …)` |

## Semgrep extensions

| Id | Construct | Old grammar | New grammar | Old behavior | Translation |
|---|---|---|---|---|---|
| D35 | `...` as a statement | was an expression statement | `semgrep_ellipsis_statement` | ok | `ExprStmt Ellipsis` |
| D36 | Expression patterns (`$X + 1`, `"x"`, `a.b`) | expression statements | `semgrep_expression` entry point (`__SEMGREP_EXPRESSION` prefix added by the pattern parser when parsing as statements fails) | ok | `E expr` |
| D37 | `...` anywhere in a parameter list `function f(..., $X)` | `...` allowed only first or last | `...` allowed at any position | ok | `ParamEllipsis` |
| D38 | Deep ellipsis `<... e ...>` | not supported | `deep_ellipsis` | parse error | `DeepEllipsis` |
| D39 | Metavariable ellipsis `$...ARGS` | not supported | extra alternative of the `identifier` token | parse error | `Id "$...ARGS"` |
| D40 | Metavariables `$X` in all name positions | via the `identifier` override | the `identifier` token accepts `$` natively | ok | `Id "$X"` |
