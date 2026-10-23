#!/usr/bin/env python3
"""Run rules.yaml with one binary over the sample, one batch per project.
usage: run_rules.py new|old"""
import json, sys, os, subprocess, collections
D = os.path.dirname(os.path.abspath(__file__))
BIN = {'new': '/src/bin/opengrep-core', 'old': '/host-tmp/opengrep-core-old'}[sys.argv[1]]
tag = sys.argv[1]
files = [l.rstrip('\n') for l in open('/tmp/tc/files.txt', encoding='utf-8')]
byp = collections.defaultdict(list)
for f in files:
    byp[f.split('/')[3]].append(f)
for pj, fs in sorted(byp.items(), reverse=len(sys.argv) > 2):
    out = f'{D}/rules_out/{tag}.{pj}.json'
    if os.path.exists(out) and os.path.getsize(out) > 0:
        continue
    tg = f'{D}/rules_out/targets.{pj}.{os.getpid()}.json'
    json.dump([["CodeTarget", {"path": f, "analyzer": "lua", "products": ["sast"]}] for f in fs], open(tg, 'w'))
    ctg = '/host-tmp' + tg[len('/tmp'):]
    crules = '/host-tmp' + D[len('/tmp'):] + '/rules.yaml'
    cmd = ['docker', 'exec', 'ogb', 'sh', '-c',
           f'{BIN} -rules {crules} -targets {ctg} -json -timeout 30 -max_target_bytes 0 2>/dev/null']
    r = subprocess.run(cmd, capture_output=True)
    s = r.stdout.decode('utf-8', 'replace')
    k = s.find('{"version"')
    open(out + '.tmp%d' % os.getpid(), 'w').write(s[k:] if k >= 0 else '')
    os.rename(out + '.tmp%d' % os.getpid(), out)
    print(pj, r.returncode, len(s), flush=True)
