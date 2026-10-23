import json,sys
d=json.load(open('results/rules_compare.json'))
r=sys.argv[1]; n=int(sys.argv[2]) if len(sys.argv)>2 else 5
def src(f,l): 
    try: return open('/tmp'+f[9:],encoding='utf-8',errors='replace').read().split('\n')[l-1].strip()[:140]
    except: return '?'
for f,a,b,oa,nb in d['file_diffs'].get(r,[])[:n]:
    print('##',f.replace('/host-tmp/corpus/',''),'onlyOld',a,'onlyNew',b)
    for x in oa[:3]: print('   old', x, src(f,x[0]))
    for x in nb[:3]: print('   new', x, src(f,x[0]))
