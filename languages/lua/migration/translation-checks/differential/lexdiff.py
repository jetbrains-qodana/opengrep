import json,sys,collections
sys.argv += []
import compare_rules as C
old, mo, erro, so = C.load('old'); new, mn, errn, sn = C.load('new')
bad = set(f for f, t in list(erro.items()) + list(errn.items()) if t in ('Too many matches', 'Internal matching error', 'Timeout'))
clean=[f for f in C.clean if f not in bad]
out={}
for r in sys.argv[1].split(','):
  n=int(sys.argv[2])
  rows=[]
  for f in clean:
    L=C.lexof(f,r)
    if L is not None and new[f][r]!=L: rows.append((new[f][r]-L, old[f][r], new[f][r], L, f))
  rows.sort(key=lambda x:-abs(x[0]))
  out[r]=rows
  print(r, 'files new!=lex', len(rows), 'sum(new-lex)', sum(x[0] for x in rows))
  for x in rows[:n]: print('  ', x[1:4], x[4].replace('/host-tmp/corpus/',''))
json.dump(out, open('results/lexdiff.json','w'))
