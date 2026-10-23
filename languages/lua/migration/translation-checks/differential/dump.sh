#!/bin/sh
# usage (inside container): dump.sh BIN TAG START END   -- dumps files START..END (1-based lines of files.txt)
BIN=$1; TAG=$2; S=$3; E=$4
D=/host-tmp/tc/differential/dumps/$TAG
sed -n "${S},${E}p" /host-tmp/tc/files.txt | awk -v s=$S '{print s+NR-1 "\t" $0}' | while IFS="$(printf '\t')" read -r i f; do
  [ -s "$D/$i.zst" ] && continue
  timeout 20 "$BIN" -lang lua -full_token_info -dump_ast -json "$f" 2>"$D/$i.err" | zstd -q -3 -o "$D/$i.zst.tmp"
  mv "$D/$i.zst.tmp" "$D/$i.zst"
done
