import json, sys
# merge.py OUT IN...: keep last 'ok' (or final nojson/timeout) record per file
files=[l.rstrip('\n') for l in open('/tmp/tc/files.txt', encoding='utf-8') if l.strip()]
best={}
for f in sys.argv[2:]:
    for l in open(f, encoding='utf-8'):
        try: r=json.loads(l)
        except Exception: continue
        good = r.get('status')=='ok' or (r.get('status') in ('nojson','timeout') and r.get('final'))
        if good: best[r['file']]=r
        elif r['file'] not in best: pass
with open(sys.argv[1],'w',encoding='utf-8') as o:
    for fp in files:
        if fp in best: o.write(json.dumps(best[fp])+'\n')
missing=[fp for fp in files if fp not in best]
print('kept',len(best),'missing',len(missing))
open(sys.argv[1]+'.missing','w',encoding='utf-8').write(''.join(m+'\n' for m in missing))
