#!/usr/bin/env bash
#
# Convert the upstream grammar.js from an ES module to a CommonJS module.
#
# usage: esm-to-cjs.sh INPUT_GRAMMAR_JS OUTPUT_GRAMMAR_JS
#
# tree-sitter-lua v0.5.0 uses 'export default grammar(...)', which the
# tree-sitter CLI 0.22.6 (the version matching the runtime linked by
# opengrep) cannot load: it loads grammars with require(). The semgrep
# extension also needs require('tree-sitter-lua/grammar').
#
set -eu -o pipefail

input=$1
output=$2

if ! grep -q '^export default grammar(' "$input"; then
  echo "$input: no 'export default grammar(' found" >&2
  exit 1
fi
if grep -Eq '^(import|export) ' "$input" \
   && [[ $(grep -Ec '^(import|export) ' "$input") -ne 1 ]]; then
  echo "$input: unsupported import/export statements" >&2
  exit 1
fi

mkdir -p "$(dirname "$output")"
sed -e 's/^export default grammar(/module.exports = grammar(/' "$input" \
  > "$output"
