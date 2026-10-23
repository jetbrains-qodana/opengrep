#!/usr/bin/env python3
"""aggregate.py: results_new.jsonl results_old.jsonl -> stats.json + tables (markdown) on stdout"""
import json, sys
from collections import Counter, defaultdict

CATS = ['name', 'string', 'number', 'number_value', 'nil', 'true', 'false', 'vararg',
        'function', 'local', 'local_name', 'local_values', 'call', 'method_call', 'dot',
        'index', 'table', 'op', 'assign', 'return', 'for', 'for_iter', 'while', 'repeat',
        'if', 'elseif', 'goto', 'label', 'break']
K = ('expected', 'actual', 'matched', 'missing', 'extra', 'mismatch')


def load(p):
    recs = {}
    for l in open(p, encoding='utf-8'):
        r = json.loads(l)
        recs[r['file']] = r
    return recs


def agg(recs, clean_only=True, project=None):
    tot = defaultdict(Counter)
    for r in recs.values():
        if r.get('status') != 'ok':
            continue
        if clean_only and r.get('parse_error'):
            continue
        if project and r['project'] != project:
            continue
        for c, v in r['cats'].items():
            for k in K:
                tot[c][k] += v[k]
    return tot


def pct(m, e):
    return 100.0 * m / e if e else 100.0


def table(tot, title):
    out = ['### ' + title, '',
           '| category | expected | AST | matched | missing | extra | value mismatch | agreement % |',
           '|---|---:|---:|---:|---:|---:|---:|---:|']
    allm = alle = 0
    for c in CATS:
        v = tot.get(c, Counter())
        out.append('| %s | %d | %d | %d | %d | %d | %d | %.4f |' % (
            c, v['expected'], v['actual'], v['matched'], v['missing'], v['extra'], v['mismatch'],
            pct(v['matched'], max(v['expected'], v['actual']))))
        if c != 'number_value':
            allm += v['matched']; alle += max(v['expected'], v['actual'])
    out.append('| **all** | | | %d | | | | %.4f |' % (allm, pct(allm, alle)))
    return '\n'.join(out) + '\n'


def coverage(recs):
    per = defaultdict(Counter)
    for r in recs.values():
        p = r['project']
        per[p]['files'] += 1
        st = r.get('status')
        if st == 'ok':
            per[p]['processed'] += 1
            if r.get('parse_error'):
                per[p]['parse_errors'] += 1
        elif st == 'timeout':
            per[p]['timeouts'] += 1
        elif st == 'nojson':
            per[p]['no_ast'] += 1
        else:
            per[p]['other'] += 1
    return per


def file_bad(r):
    return sum(v['missing'] + v['extra'] + v['mismatch'] for c, v in r['cats'].items())


if __name__ == '__main__':
    new = load(sys.argv[1])
    old = load(sys.argv[2]) if len(sys.argv) > 2 else {}
    md = []
    for name, recs in (('new', new), ('old', old)):
        if not recs:
            continue
        cov = coverage(recs)
        md.append('## Coverage (%s parser)\n' % name)
        md.append('| project | files | processed (AST) | with parse errors | no AST | timeouts | other |')
        md.append('|---|---:|---:|---:|---:|---:|---:|')
        T = Counter()
        for p in sorted(cov):
            c = cov[p]
            T.update(c)
            md.append('| %s | %d | %d | %d | %d | %d | %d |' % (p, c['files'], c['processed'], c['parse_errors'], c['no_ast'], c['timeouts'], c['other']))
        md.append('| **total** | %d | %d | %d | %d | %d | %d |\n' % (T['files'], T['processed'], T['parse_errors'], T['no_ast'], T['timeouts'], T['other']))
        md.append(table(agg(recs, True), '%s parser, files without parse errors' % name))
        md.append(table(agg(recs, False), '%s parser, all files with an AST (incl. parse errors)' % name))
    # per project, new, clean files
    md.append('## Per project (new parser, clean files): constructs with a discrepancy\n')
    md.append('| project | clean files | expected (all cats) | matched | missing | extra | mismatch | agreement % | files with any discrepancy |')
    md.append('|---|---:|---:|---:|---:|---:|---:|---:|---:|')
    projs = sorted(set(r['project'] for r in new.values()))
    for p in projs:
        t = agg(new, True, p)
        e = sum(t[c]['expected'] for c in CATS if c != 'number_value')
        m = sum(t[c]['matched'] for c in CATS if c != 'number_value')
        mi = sum(t[c]['missing'] for c in CATS if c != 'number_value')
        ex = sum(t[c]['extra'] for c in CATS if c != 'number_value')
        mm = sum(t[c]['mismatch'] for c in CATS if c != 'number_value')
        nf = sum(1 for r in new.values() if r['project'] == p and r.get('status') == 'ok' and not r.get('parse_error'))
        nb = sum(1 for r in new.values() if r['project'] == p and r.get('status') == 'ok' and not r.get('parse_error') and file_bad(r))
        md.append('| %s | %d | %d | %d | %d | %d | %d | %.4f | %d |' % (p, nf, e, m, mi, ex, mm, pct(m, e + ex), nb))
    print('\n'.join(md))
    json.dump({'new': {c: dict(v) for c, v in agg(new, True).items()},
               'new_all': {c: dict(v) for c, v in agg(new, False).items()},
               'old': {c: dict(v) for c, v in agg(old, True).items()} if old else {}},
              open('stats.json', 'w'), indent=1)
