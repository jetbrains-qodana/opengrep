#!/usr/bin/env bash
#
# Check that every difference listed in grammar-differences.md has at least
# 2 rules in rules/<Id>-*/, that every rule has a target and that every
# target has at least one 'ruleid:' annotation.
#
set -eu -o pipefail
cd "$(dirname "$0")"

errors=0
ids=$(grep -oE '^\| D[0-9]+ ' grammar-differences.md | tr -d '| ' | sort -u)
for id in $ids; do
  dirs=(rules/"$id"-*)
  if [[ ! -d "${dirs[0]}" ]]; then
    echo "$id: no rules directory"; errors=$((errors + 1)); continue
  fi
  n=$(ls "${dirs[0]}"/*.yaml 2>/dev/null | wc -l)
  if (( n < 2 )); then
    echo "$id: only $n rule(s)"; errors=$((errors + 1))
  fi
  for y in "${dirs[0]}"/*.yaml; do
    t=${y%.yaml}.lua
    if [[ ! -f "$t" ]]; then
      echo "$y: missing target $t"; errors=$((errors + 1))
    elif ! grep -q 'ruleid:' "$t"; then
      echo "$t: no 'ruleid:' annotation"; errors=$((errors + 1))
    fi
  done
done
for d in rules/*/; do
  id=$(basename "$d" | cut -d- -f1)
  grep -qx "$id" <<< "$ids" || { echo "$d: not in grammar-differences.md"; errors=$((errors + 1)); }
done
echo "$(wc -w <<< "$ids") differences, $(ls rules/*/*.yaml | wc -l) rules, $errors error(s)"
(( errors == 0 ))
