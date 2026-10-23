#!/bin/sh
# usage (in container): errs.sh BIN TAG  -> results/pstats/TAG.<batch>.log  (resumable, 25 files/batch)
BIN=$1; TAG=$2
D=/host-tmp/tc/differential/results/pstats; mkdir -p $D
N=$(wc -l < /host-tmp/tc/files.txt); b=0; s=1
while [ $s -le $N ]; do
  e=$((s+24)); out=$D/$TAG.$b.log
  if [ ! -s $out ] || ! grep -q "done parsing\|DONE" $out; then
    sed -n "${s},${e}p" /host-tmp/tc/files.txt | tr '\n' '\0' | xargs -0 "$BIN" -lang lua -parsing_stats > /dev/null 2> $out.tmp
    echo DONE >> $out.tmp; mv $out.tmp $out
  fi
  b=$((b+1)); s=$((e+1))
done
