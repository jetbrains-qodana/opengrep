#!/usr/bin/env python3
"""Compare match counts of rules.yaml between old and new parser, and with lexical counts.
Writes results/rules_compare.json and prints tables."""
import json, os, collections, glob, sys

D = os.path.dirname(os.path.abspath(__file__))
FILES = [l.rstrip('\n') for l in open('/tmp/tc/files.txt', encoding='utf-8')]
proj = lambda f: f.split('/')[3]
eo = set(l.rstrip('\n') for l in open(f'{D}/results/errors_old.txt'))
en = set(l.rstrip('\n') for l in open(f'{D}/results/errors_new.txt'))
done = lambda tag, pj: os.path.exists(f'{D}/rules_out/{tag}.{pj}.json') and os.path.getsize(f'{D}/rules_out/{tag}.{pj}.json') > 0
clean = [f for f in FILES if f not in eo and f not in en and done('old', proj(f)) and done('new', proj(f))]
cleanset = set(clean)

def load(tag):
    cnt = collections.defaultdict(collections.Counter)  # file -> rule -> n
    matches = collections.defaultdict(set)  # (file, rule) -> set of (sline,scol,eline,ecol)
    errs = {}
    scanned = set()
    for fn in glob.glob(f'{D}/rules_out/{tag}.*.json'):
        s = open(fn).read()
        if not s:
            continue
        d = json.loads(s)
        for r in d['results']:
            key = (r['start']['line'], r['start']['col'], r['end']['line'], r['end']['col'])
            matches[(r['path'], r['check_id'])].add(key)
        for e in d['errors']:
            p = e.get('location', {}).get('path') or e.get('path')
            t = e.get('error_type'); errs[p] = t if isinstance(t, str) else t[0]
        scanned.update(d.get('paths', {}).get('scanned', []))
    for (f, rid), ks in matches.items():
        cnt[f][rid] = len(ks)
    return cnt, matches, errs, scanned

LEX = {}
for l in open(f'{D}/results/lexcounts.jsonl'):
    j = json.loads(l)
    LEX[j['file']] = j.get('c', {})

def lexof(f, rule):
    c = LEX.get(f, {})
    g = lambda k: c.get(k, 0)
    m = {
        'call': g('call'), 'method_call': g('method_call'), 'func_decl': g('func_decl'),
        'local_func': g('local_func'), 'method_decl': g('method_decl'), 'lambda': g('lambda'),
        'return': g('return'), 'table': g('table'), 'if': g('if') + g('elseif'),
        'numfor': g('numfor'), 'genfor2': g('genfor2'), 'while': g('while'), 'repeat': g('repeat'),
        'string': g('string'), 'concat': g('concat'), 'not': g('not') + g('until'), 'len': g('len'),
        'eq': g('eq'), 'neq': g('neq'), 'and': g('and'), 'or': g('or'), 'plus': g('plus'),
        'true': g('true'), 'false': g('false'), 'break': g('break'),
    }
    return m.get(rule)

def main():
    global clean
    old, mo, erro, so = load('old')
    new, mn, errn, sn = load('new')
    # files whose results are incomplete (too many matches, matching errors) are excluded
    bad = set(f for f, t in list(erro.items()) + list(errn.items()) if t in ('Too many matches', 'Internal matching error', 'Timeout'))
    clean = [f for f in clean if f not in bad]
    print('excluded (incomplete rule results):', len(bad))
    rules = [r['id'] for r in __import__('yaml').safe_load(open(f'{D}/rules.yaml'))['rules']]
    tab = {}
    perproj = collections.defaultdict(dict)
    filediff = collections.defaultdict(list)
    for r in rules:
        o = sum(old[f][r] for f in clean); n = sum(new[f][r] for f in clean)
        lx = [lexof(f, r) for f in clean]
        lxs = None if lx[0] is None else sum(lx)
        # files where old and new differ
        nd = 0; only_o = only_n = 0
        lex_agree_old = lex_agree_new = 0
        for f in clean:
            a, b = mo.get((f, r), set()), mn.get((f, r), set())
            if a != b:
                nd += 1
                only_o += len(a - b); only_n += len(b - a)
                filediff[r].append((f, len(a - b), len(b - a), sorted(a - b)[:5], sorted(b - a)[:5]))
            L = lexof(f, r)
            if L is not None:
                lex_agree_old += (old[f][r] == L); lex_agree_new += (new[f][r] == L)
        tab[r] = {'old': o, 'new': n, 'lex': lxs, 'files_differ': nd, 'match_ranges_only_old': only_o,
                  'match_ranges_only_new': only_n,
                  'files_lex_eq_old': lex_agree_old if lxs is not None else None,
                  'files_lex_eq_new': lex_agree_new if lxs is not None else None}
        for f in clean:
            p = proj(f)
            d = perproj[p].setdefault(r, [0, 0])
            d[0] += old[f][r]; d[1] += new[f][r]
    out = {'clean_files': len(clean), 'scanned_old': len(so), 'scanned_new': len(sn),
           'rule_errors_old': len(erro), 'rule_errors_new': len(errn),
           'table': tab, 'per_project': perproj,
           'file_diffs': {r: sorted(v, key=lambda x: -(x[1] + x[2]))[:30] for r, v in filediff.items()}}
    json.dump(out, open(f'{D}/results/rules_compare.json', 'w'), indent=1)
    print('clean files', len(clean), 'errors old/new in rule runs', len(erro), len(errn))
    print(f"{'rule':14} {'old':>8} {'new':>8} {'lex':>8} {'fdiff':>6} {'onlyO':>6} {'onlyN':>6} {'lex=old':>7} {'lex=new':>7}")
    for r in rules:
        t = tab[r]
        print(f"{r:14} {t['old']:8} {t['new']:8} {str(t['lex']):>8} {t['files_differ']:6} {t['match_ranges_only_old']:6} {t['match_ranges_only_new']:6} {str(t['files_lex_eq_old']):>7} {str(t['files_lex_eq_new']):>7}")

if __name__ == '__main__':
    main()
