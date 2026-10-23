"""Count 'local n1..nk = e1..em' with m > k (extra values are dropped by the translation)."""
import json, collections
from lexcount import lex
FILES=[l.rstrip('\n') for l in open('/tmp/tc/files.txt',encoding='utf-8')]
eo=set(l.rstrip('\n') for l in open('results/errors_old.txt')); en=set(l.rstrip('\n') for l in open('results/errors_new.txt'))
VALEND={'NAME','NUM','STR'}; VALENDT={')',']','}','end','true','false','nil','...'}
STMT={'local','if','for','while','repeat','return','end','do','until','else','elseif','break','goto'}
tot=0; ex=[]; files=0; withcall=0
for f in FILES:
    if f in eo or f in en: continue
    T=lex(open('/tmp'+f[9:],encoding='utf-8',errors='replace').read()); n=len(T); found=False
    for k,(kind,t,line) in enumerate(T):
        if not (kind=='KW' and t=='local') or k+1>=n or T[k+1][1]=='function': continue
        j=k+1; names=0
        while j<n and T[j][0]=='NAME':
            names+=1; j+=1
            if j<n and T[j][1]=='<': j+=3
            if j<n and T[j][1]==',': j+=1
            else: break
        if j>=n or T[j][1]!='=': continue
        j+=1; vals=1; depth=0; prev=None; hascall=False
        while j<n:
            kd,tt,_=T[j]
            if depth==0 and kd=='KW' and tt=='function':
                # skip the function body
                d=1; j+=1
                while j<n and d>0:
                    if T[j][1] in ('function','if','do','repeat') and T[j][0]=='KW':
                        if not (T[j][1]=='do' and False): d+=1
                    elif T[j][1] in ('end','until') and T[j][0]=='KW': d-=1
                    j+=1
                # 'while/for ... do' count once: 'do' opens, while/for don't
                prev=('KW','end'); continue
            if kd=='OP' and tt in '([{': depth+=1
            elif kd=='OP' and tt in ')]}':
                if depth==0: break
                depth-=1
            elif depth==0:
                if kd=='KW' and tt in STMT: break
                if kd=='OP' and tt==';': break
                if prev and (prev[0] in VALEND or prev[1] in VALENDT) and (kd in ('NAME',) or (kd=='KW' and tt=='function')): break
                if kd=='OP' and tt==',': vals+=1
            if vals>names and kd=='OP' and tt=='(' : hascall=True
            prev=(kd,tt); j+=1
        if vals>names:
            tot+=1; found=True
            if hascall: withcall+=1
            if len(ex)<15: ex.append((f.replace('/host-tmp/corpus/',''),line,names,vals))
    files+=found
print(json.dumps({'local_stmts_with_extra_values':tot,'files':files,'extra_part_contains_call_approx':withcall,'examples':ex},indent=0))
json.dump({'local_stmts_with_extra_values':tot,'files':files,'examples':ex},open('results/localextra.json','w'))
