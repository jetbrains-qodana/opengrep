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
#   OCAML_TREE_SITTER  code generator 'ocaml-tree-sitter' of
#                      https://github.com/opengrep/ocaml-tree-sitter-core
#                      (commit e737fd1 or later: multicore-friendly Parse.ml)
#                      (default: ocaml-tree-sitter)
#   OCAML_TREE_SITTER_VERSION  version or commit of ocaml-tree-sitter, recorded
#                      in fyi/versions
#
set -eu -o pipefail

here=$(cd "$(dirname "$0")/.." && pwd)
tree_sitter=${TREE_SITTER:-tree-sitter}
ocaml_tree_sitter=${OCAML_TREE_SITTER:-ocaml-tree-sitter}
expected_tree_sitter_version=0.22.6
expected_abi=14

# Names of the generated OCaml library. Temporary names are used while the
# previous Lua parser (submodule languages/lua/tree-sitter/semgrep-lua) is
# still in the tree, as dune doesn't allow two libraries with the same name.
lib_public_name=tree-sitter-lang.lua_new
lib_name=tree_sitter_lua_new
lib_module=Tree_sitter_lua_new
bin_public_name=parse-lua-new

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

# ocaml-tree-sitter works on a simplified grammar; parser.c must be
# generated from that same grammar so that the CST matches.
echo "Simplifying src/grammar.json"
mv src/grammar.json src/grammar.json.orig
"$ocaml_tree_sitter" simplify src/grammar.json.orig src/grammar.json
echo "Generating the definitive src/parser.c"
"$tree_sitter" generate src/grammar.json --no-bindings --abi "$expected_abi"
abi=$(awk '/#define LANGUAGE_VERSION/ {print $3; exit}' src/parser.c)
[[ "$abi" == "$expected_abi" ]] \
  || error "src/parser.c has ABI $abi, expected $expected_abi"

# The library: C sources and headers.
lib="$here"/lib
echo "Writing $lib"
rm -rf "$lib"
mkdir -p "$lib"
cp src/parser.c src/scanner.c "$lib"/
cp -R src/tree_sitter "$lib"/tree_sitter
sed -e "s/@LIB_PUBLIC_NAME@/$lib_public_name/" \
    -e "s/@LIB_NAME@/$lib_name/" \
    "$here"/templates/lib.dune.in > "$lib"/dune

echo "Generating the OCaml CST and parser"
mkdir -p "$work"/ocaml-src
"$ocaml_tree_sitter" gen lua src/grammar.json -d "$work"/ocaml-src
cp "$work"/ocaml-src/lib/*.ml "$work"/ocaml-src/lib/*.mli "$lib"/
sed -e "s/@LANG@/lua/g" "$here"/templates/bindings.c.in > "$lib"/bindings.c

# The parse-lua executable, to inspect the CST of a file:
#   dune exec -- parse-lua-new FILE
bin="$here"/bin
echo "Writing $bin"
rm -rf "$bin"
mkdir -p "$bin"
sed -e "s/@BIN_PUBLIC_NAME@/$bin_public_name/" \
    -e "s/@LIB_PUBLIC_NAME@/$lib_public_name/" \
    "$here"/templates/bin.dune.in > "$bin"/dune
sed -e "s/@LIB_MODULE@/$lib_module/" "$here"/templates/Main.ml.in \
  > "$bin"/Main.ml

# Informational copies of the source grammars and versions.
fyi="$here"/fyi
echo "Writing $fyi"
rm -rf "$fyi"
mkdir -p "$fyi"
cp src/grammar.json.orig "$fyi"/grammar.json
cp src/node-types.json "$fyi"/node-types.json
{
  echo "tree-sitter: $("$tree_sitter" --version)"
  echo "ABI: $expected_abi"
  echo "ocaml-tree-sitter: ${OCAML_TREE_SITTER_VERSION:-unknown}"
  echo
  cat "$here"/grammar/tree-sitter-lua/VERSION
} > "$fyi"/versions
