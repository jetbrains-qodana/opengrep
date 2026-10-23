#!/usr/bin/env bash
#
# Regenerate the Lua parser from the grammars in grammar/.
#
# usage: scripts/regenerate.sh
#
# Environment:
#   TREE_SITTER        tree-sitter CLI, must be version 0.22.6, the version of
#                      the tree-sitter runtime linked by opengrep
#                      (default: tree-sitter)
#
set -eu -o pipefail

here=$(cd "$(dirname "$0")/.." && pwd)
tree_sitter=${TREE_SITTER:-tree-sitter}
expected_tree_sitter_version=0.22.6
expected_abi=14

error() {
  echo "Error: $*" >&2
  exit 1
}

version=$("$tree_sitter" --version | awk '{print $2}')
[[ "$version" == "$expected_tree_sitter_version" ]] \
  || error "tree-sitter $expected_tree_sitter_version is required, found $version"

work=$(mktemp -d)
trap 'rm -rf "$work"' EXIT

# Inputs: the upstream grammar as a CommonJS module that the semgrep
# extension can require(), and the external scanner.
"$here"/scripts/esm-to-cjs.sh \
  "$here"/grammar/tree-sitter-lua/grammar.js \
  "$work"/node_modules/tree-sitter-lua/grammar.js
mkdir -p "$work"/semgrep-lua/src
cp "$here"/grammar/semgrep-lua/grammar.js "$work"/semgrep-lua/
cp "$here"/grammar/tree-sitter-lua/src/scanner.c "$work"/semgrep-lua/src/
cd "$work"/semgrep-lua

echo "Generating src/grammar.json and src/parser.c"
"$tree_sitter" generate --no-bindings --abi "$expected_abi"
abi=$(awk '/#define LANGUAGE_VERSION/ {print $3; exit}' src/parser.c)
[[ "$abi" == "$expected_abi" ]] \
  || error "src/parser.c has ABI $abi, expected $expected_abi"
