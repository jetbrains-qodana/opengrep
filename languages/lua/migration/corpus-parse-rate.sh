#!/usr/bin/env bash
#
# Parse the projects of corpus.txt and print their parsing statistics.
#
# usage: corpus-parse-rate.sh [OUTPUT_DIR]
# CORPUS_DIR: where the projects are cloned (default: /tmp/lua-corpus)
# OPENGREP_CORE: opengrep-core executable (default: the one built in this
#   repository)
#
# For each project, OUTPUT_DIR/<project>.json contains the output of
# 'opengrep-core -lang lua -parsing_stats -json'. A summary is printed in the
# markdown format of corpus-results.md.
#
set -eu -o pipefail
here=$(cd "$(dirname "$0")" && pwd)
root=$(cd "$here/../../.." && pwd)
exe=${OPENGREP_CORE:-$root/bin/opengrep-core}
corpus=${CORPUS_DIR:-/tmp/lua-corpus}
out=${1:-$corpus/stats}
mkdir -p "$corpus" "$out"

field() {
  # first occurrence of a numeric field in a json file (the "global" object)
  grep -m1 "\"$2\"" "$1" | sed -E 's/.*: *([0-9.e+-]+).*/\1/'
}

echo "| Project | Files | Files with errors | Lines | Lines with errors | Parsing rate |"
echo "|---|---|---|---|---|---|"
while read -r name commit; do
  dir="$corpus/${name//\//_}"
  if [[ ! -d "$dir/.git" ]]; then
    git init -q "$dir"
    git -C "$dir" remote add origin "https://github.com/$name"
  fi
  if [[ "$(git -C "$dir" rev-parse -q --verify HEAD 2>/dev/null)" != "$commit" ]]; then
    git -C "$dir" fetch -q --depth 1 origin "$commit"
    git -C "$dir" checkout -q FETCH_HEAD
  fi
  json="$out/${name//\//_}.json"
  "$exe" -lang lua -parsing_stats -json "$dir" > "$json" 2>/dev/null
  echo "| $name | $(field "$json" file_count) | $(field "$json" error_file_count)" \
       "| $(field "$json" line_count) | $(field "$json" error_line_count)" \
       "| $(field "$json" parsing_rate) |"
done < "$here/corpus.txt"
