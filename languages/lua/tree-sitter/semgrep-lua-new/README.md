# semgrep-lua-new

Lua parser for opengrep, based on
[tree-sitter-grammars/tree-sitter-lua](https://github.com/tree-sitter-grammars/tree-sitter-lua).

This folder is self-contained so that it can later be moved to its own
repository (as the other `semgrep-*` parsers):

* `grammar/tree-sitter-lua/`: unmodified copy of the upstream grammar (see `VERSION`).
* `scripts/esm-to-cjs.sh`: converts the upstream ES module grammar to CommonJS,
  so that it can be loaded by the tree-sitter CLI 0.22.6.
