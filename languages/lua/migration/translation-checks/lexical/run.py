#!/usr/bin/env python3
"""run.py new|old OUT.jsonl [start] [step]  -- runs the oracle over /tmp/tc/files.txt"""
import json, os, subprocess, sys, threading, time
sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
import lexcheck as L

BIN = {'new': '/src/bin/opengrep-core', 'old': '/host-tmp/opengrep-core-old'}

def one(parser, cpath):
    for attempt in range(4):
        try:
            rec = one1(parser, cpath)
        except OSError as e:
            time.sleep(10); continue
        if rec.get('status') in ('nojson', 'timeout'):
            st = rec.get('stderr', '')
            if rec.get('rc') in (2, 125, 126, 127, 128) or 'Resource temporarily' in st or 'vfork' in st:
                time.sleep(10); continue
            rec['final'] = True
        return rec
    rec['final'] = False
    return rec

def one1(parser, cpath):
    hpath = '/tmp' + cpath[len('/host-tmp'):]
    rec = {'file': cpath, 'project': cpath.split('/')[3], 'parser': parser}
    try:
        raw = open(hpath, 'rb').read()
    except OSError as e:
        rec['status'] = 'unreadable'; return rec
    t0 = time.time()
    p = subprocess.run(['docker', 'exec', 'ogb', 'timeout', '20', BIN[parser], '-lang', 'lua',
                        '-full_token_info', '-dump_ast', '-json', cpath],
                       stdout=subprocess.PIPE, stderr=subprocess.PIPE)
    rec['secs'] = round(time.time() - t0, 2)
    rec['rc'] = p.returncode
    err = p.stderr.decode('utf-8', 'replace')
    rec['parse_error'] = ('fail to fully parse' in err) or p.returncode not in (0,)
    if p.returncode in (124, 137):
        rec['status'] = 'timeout'; rec['stderr'] = err[-300:]; return rec
    try:
        js = L.parse_json_stdout(p.stdout)
    except Exception as e:
        js = None
    if js is None:
        rec['status'] = 'nojson'; rec['stderr'] = err[-500:]; return rec
    lo, lc, lerrs, ll = L.analyze_source(raw)
    ao, ac, al = L.analyze_ast(js)
    res = L.compare(lo, ao, ll, al)
    rec['status'] = 'ok'
    rec['lex_errors'] = lerrs[:5]
    rec['else'] = [lc.get('else', 0), ac.get('else', 0)]
    rec['astcounts'] = dict(ac)
    rec['cats'] = res
    return rec

def main():
    parser, out = sys.argv[1], sys.argv[2]
    start = int(sys.argv[3]) if len(sys.argv) > 3 else 0
    step = int(sys.argv[4]) if len(sys.argv) > 4 else 1
    files = [l.rstrip('\n') for l in open(os.environ.get('FILES','/tmp/tc/files.txt'), encoding='utf-8') if l.strip()]
    done = set()
    if os.path.exists(out):
        for l in open(out, encoding='utf-8'):
            try:
                r = json.loads(l)
                if r.get('status') == 'ok' or (r.get('status') in ('nojson', 'timeout') and r.get('final')):
                    done.add(r['file'])
            except Exception: pass
    with open(out, 'a', encoding='utf-8') as f:
        for k in range(start, len(files), step):
            fp = files[k]
            if fp in done: continue
            try:
                rec = one(parser, fp)
            except Exception as e:
                rec = {'file': fp, 'project': fp.split('/')[3], 'parser': parser,
                       'status': 'crash', 'error': repr(e)[:300]}
            f.write(json.dumps(rec) + '\n'); f.flush()

sys.setrecursionlimit(2000000)
threading.stack_size(1 << 29)
th = threading.Thread(target=main); th.start(); th.join()
