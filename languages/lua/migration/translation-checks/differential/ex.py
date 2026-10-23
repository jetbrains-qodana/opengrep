import json,sys
import os; d=json.load(open(os.environ.get('AD','results/astdiff.json')))
pat=sys.argv[1]; n=int(sys.argv[2]) if len(sys.argv)>2 else 4
for k,v in d['kinds']:
    if pat in k:
        print('==',v,k)
        for e in d['examples'][k][:n]: print('   ',e['file'].replace('/host-tmp/corpus/',''),e['line'],e['col'],'|',e['src'][:120],'|',e.get('detail'))
