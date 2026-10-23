#!/usr/bin/env python3
"""AST differential: old Lua parser vs new Lua parser (opengrep-core -dump_ast -json).

Usage: astdiff.py [--errors-old FILE] [--errors-new FILE] [--only N,...]
Reads dumps/{old,new}/<i>.zst (i = 1-based line of /tmp/tc/files.txt),
writes results/astdiff.json (+ per-file details in results/astdiff_files.jsonl).
"""
import json, sys, os, subprocess, collections, difflib, argparse, re

D = os.path.dirname(os.path.abspath(__file__))
FILES = [l.rstrip('\n') for l in open('/tmp/tc/files.txt', encoding='utf-8')]

def host(p):
    return '/tmp' + p[len('/host-tmp'):] if p.startswith('/host-tmp') else p

def project(p):
    return p.split('/')[3]

def load(tag, i):
    fn = f'{D}/dumps/{tag}/{i}.zst'
    if not os.path.exists(fn):
        return None, 'missing'
    raw = subprocess.run(['zstd', '-dc', fn], capture_output=True).stdout
    s = raw.decode('utf-8', 'replace')
    k = s.find('{')
    if k < 0:
        return None, 'empty'
    try:
        return json.loads(s[k:]), None
    except Exception as e:
        return None, 'badjson'

DROP = {'id_flags', 'id_resolved', 'id_resolved_alternative', 'id_type',
        'id_svalue', 'name_info', 'pinfo', 'id_info'}

class Tok:
    __slots__ = ('s', 'line', 'col', 'fake')
    def __init__(self, s, line, col, fake):
        self.s, self.line, self.col, self.fake = s, line, col, fake
    def __repr__(self):
        return f'Tok({self.s!r}@{self.line}:{self.col})' if not self.fake else f'Fake({self.s!r})'

# canonical node: ('Ctor', [args]) tuples-as-lists: N = [ctor, args]
def canon(x):
    if isinstance(x, dict):
        if 'token' in x and len(x) <= 2:
            t = x['token']
            if 'OriginTok' in t:
                o = t['OriginTok']
                return Tok(o['str'], o['line'], o['column'], False)
            if 'ExpandedTok' in t:
                o = t['ExpandedTok'][0]
                return Tok(o['str'], o['line'], o['column'], False)
            if 'FakeTok' in t:
                return Tok(t['FakeTok'][0], None, None, True)
            return Tok('?', None, None, True)
        if len(x) == 1:
            (k, v), = x.items()
            if k == 'some':
                return canon(v)
            if k[:1].isupper():
                args = v if isinstance(v, list) else [v]
                return ('C', k, [canon(a) for a in args])
        return ('R', {k: canon(v) for k, v in x.items() if k not in DROP})
    if isinstance(x, list):
        return ('L', [canon(a) for a in x])
    if isinstance(x, str):
        if x == '()':
            return Tok('()', None, None, True)
        if x[:1].isupper():
            return ('C', x, [])
        return ('S', x)
    return ('V', x)

def is_c(n, name=None):
    return isinstance(n, tuple) and n[0] == 'C' and (name is None or n[1] == name)

NORM = collections.Counter()

def norm(n, side):
    """apply by-design normalizations; returns new node"""
    if isinstance(n, Tok) or n is None:
        return n
    kind = n[0]
    if kind == 'L':
        return ('L', [norm(a, side) for a in n[1]])
    if kind == 'R':
        return ('R', {k: norm(v, side) for k, v in n[1].items()})
    if kind != 'C':
        return n
    c, args = n[1], n[2]
    # literals: numbers compared by source text
    if c == 'L' and args and is_c(args[0]) and args[0][1] in ('Int', 'Float', 'Imag'):
        lit = args[0]
        tok = lit[2][-1] if lit[2] else None
        if isinstance(tok, Tok) and not tok.fake:
            if side == 'old' and lit[1] == 'Float':
                NORM['old_Float_number'] += 1
            return ('C', 'NUM', [('S', tok.s), tok])
        # fake (synthetic table keys): keep the value
        val = lit[2][0] if lit[2] else None
        return ('C', 'NUMFAKE', [val])
    # names
    if c == 'IdSpecial' and args and is_c(args[0], 'Self'):
        NORM['old_IdSpecial_Self'] += 1
        return ('C', 'NAME', [('S', 'self'), args[1]])
    if c == 'OtherExpr' and args and isinstance(args[0], tuple) and args[0][0] == 'L' \
            and args[0][1] and isinstance(args[0][1][0], tuple) and args[0][1][0] == ('S', 'next'):
        NORM['old_OtherExpr_next'] += 1
        return ('C', 'NAME', [('S', 'next'), args[0][1][1]])
    if c == 'N' and args and is_c(args[0], 'Id'):
        idn = args[0][2][0]  # ('L',[('S',name), tok])
        return ('C', 'NAME', [idn[1][0], idn[1][1]])
    if c == 'ForEach' and len(args) == 3:
        it = args[2]
        if is_c(it, 'Container') and is_c(it[2][0], 'Tuple'):
            elems = it[2][1][1][1][1]  # bracket = ('L',[lb, ('L',elems), rb])
            if side == 'new' and len(elems) > 1:
                NORM['new_for_iter_tuple_truncated'] += 1
                args = [args[0], args[1], elems[0]]
        return ('C', c, [norm(a, side) for a in args])
    return ('C', c, [norm(a, side) for a in args])

def first_tok(n):
    if isinstance(n, Tok):
        return None if n.fake else n
    if isinstance(n, tuple):
        if n[0] == 'C':
            for a in n[2]:
                t = first_tok(a)
                if t: return t
        elif n[0] == 'L':
            for a in n[1]:
                t = first_tok(a)
                if t: return t
        elif n[0] == 'R':
            best = None
            for v in n[1].values():
                t = first_tok(v)
                if t and (best is None or (t.line, t.col) < (best.line, best.col)):
                    best = t
            return best
    return None

def label(n):
    if isinstance(n, Tok): return 'Tok'
    if n is None: return 'None'
    if n[0] == 'C':
        if n[1] in ('NAME', 'NUM'):
            return n[1]
        if n[1] in ('L',) and n[2] and is_c(n[2][0]):
            return 'L.' + n[2][0][1]
        if n[1] in ('Container', 'Call') and n[2] and is_c(n[2][0]):
            sub = n[2][0][1]
            if n[1] == 'Call' and sub == 'IdSpecial':
                return 'Call.Op'
            return n[1] + '.' + sub if n[1] == 'Container' else 'Call'
        return n[1]
    if n[0] == 'L': return 'List'
    if n[0] == 'R': return 'Rec'
    return n[0] + ':' + str(n[1])[:20]

def skey(n):
    """structural key for alignment (no tokens)"""
    if isinstance(n, Tok): return 'T'
    if n is None: return 'N'
    if n[0] == 'C':
        return n[1] + '(' + ','.join(skey(a) for a in n[2]) + ')'
    if n[0] == 'L':
        return '[' + ';'.join(skey(a) for a in n[1]) + ']'
    if n[0] == 'R':
        return '{' + ','.join(k + '=' + skey(v) for k, v in sorted(n[1].items())) + '}'
    return repr(n[1])

class Diff:
    def __init__(self):
        self.items = []  # (kind, ctx, old_label, new_label, tok)
    def add(self, kind, ctx, o, n, tok, detail=None):
        self.items.append((kind, ctx, o, n, tok, detail))

def pos_key(n):
    t = first_tok(n)
    return (t.line, t.col) if t else None

def cmp(o, n, ctx, d, toks):
    if isinstance(o, Tok) or isinstance(n, Tok):
        if isinstance(o, Tok) and isinstance(n, Tok):
            if toks and not o.fake and not n.fake and o.s != n.s:
                d.add('tokstr', ctx, 'tok', 'tok', o, (o.s[:60], n.s[:60]))
            elif toks and o.fake != n.fake:
                d.add('tokfake', ctx, 'fake' if o.fake else 'real', 'fake' if n.fake else 'real', o if not o.fake else n, (o.s[:30], n.s[:30]))
            return
        d.add('kind', ctx, label(o), label(n), first_tok(o) or first_tok(n))
        return
    if o is None or n is None:
        if o is not n:
            d.add('kind', ctx, label(o), label(n), first_tok(o) or first_tok(n))
        return
    if o[0] != n[0]:
        d.add('kind', ctx, label(o), label(n), first_tok(o) or first_tok(n))
        return
    k = o[0]
    if k == 'C':
        if o[1] != n[1]:
            d.add('kind', ctx, label(o), label(n), first_tok(o) or first_tok(n))
            return
        if len(o[2]) != len(n[2]):
            # a one-argument constructor whose argument is a list is dumped
            # as the list itself: align the elements
            cmp(('L', o[2]), ('L', n[2]), '>'.join((ctx + '>' + o[1]).split('>')[-3:]), d, toks)
            return
        if o[1] in ('NUM', 'NAME') and len(o[2]) < 2:
            if o != n:
                d.add('value', ctx, o[1] + '?', n[1] + '?', None, (repr(o)[:60], repr(n)[:60]))
            return
        if o[1] in ('NUM',):
            if o[2][0] != n[2][0]:
                d.add('value', ctx, 'NUM', 'NUM', o[2][1], (o[2][0][1], n[2][0][1]))
            return
        if o[1] == 'NAME':
            if o[2][0] != n[2][0]:
                d.add('value', ctx, 'NAME', 'NAME', o[2][1], (o[2][0][1], n[2][0][1]))
            return
        if o[1] == 'L' and o[2] and is_c(o[2][0], 'String') and is_c(n[2][0], 'String'):
            so, sn = strval(o[2][0]), strval(n[2][0])
            if so[0] != sn[0]:
                d.add('string_value', ctx, 'String', 'String', so[1] or sn[1], (so[0][:80], sn[0][:80]))
            return
        nctx = (ctx + '>' + o[1]).split('>')[-3:]
        nctx = '>'.join(nctx)
        for a, b in zip(o[2], n[2]):
            cmp(a, b, nctx, d, toks and o[1] not in ('Block',))
        return
    if k == 'L':
        A, B = o[1], n[1]
        if len(A) == len(B):
            for a, b in zip(A, B):
                cmp(a, b, ctx, d, toks)
            return
        # align by structural key + position
        ka = [(skey(a), pos_key(a)) for a in A]
        kb = [(skey(b), pos_key(b)) for b in B]
        pa = [p for _, p in ka]
        pb = [p for _, p in kb]
        sm = difflib.SequenceMatcher(None, pa, pb, autojunk=False)
        for tag, i1, i2, j1, j2 in sm.get_opcodes():
            if tag == 'equal':
                for a, b in zip(A[i1:i2], B[j1:j2]):
                    cmp(a, b, ctx, d, toks)
            else:
                common = min(i2 - i1, j2 - j1)
                for a, b in zip(A[i1:i1 + common], B[j1:j1 + common]):
                    cmp(a, b, ctx, d, toks)
                for a in A[i1 + common:i2]:
                    d.add('missing_in_new', ctx, label(a), '-', first_tok(a))
                for b in B[j1 + common:j2]:
                    d.add('extra_in_new', ctx, '-', label(b), first_tok(b))
        return
    if k == 'R':
        for key in sorted(set(o[1]) | set(n[1])):
            cmp(o[1].get(key), n[1].get(key), ctx + '.' + key if not ctx.endswith('.' + key) else ctx, d, toks)
        return
    if o != n:
        d.add('value', ctx, 'atom', 'atom', None, (str(o[1])[:40], str(n[1])[:40]))

def strval(n):
    try:
        mid = n[2][1][1]  # String(lq, (s, tok), rq)
        v = mid[0]
        return (v[1] if v[0] in ('S', 'C') else repr(v)), mid[1]
    except Exception:
        return (repr(n)[:80], None)

def strings_of(n, acc):
    """collect (line,col,value) of string literals"""
    if isinstance(n, tuple):
        if n[0] == 'C':
            if n[1] == 'L' and n[2] and is_c(n[2][0], 'String'):
                br = n[2][0][2][0]  # ('L',[lq, ('L',[('S',s),tok]), rq])
                try:
                    s = br[1][1][1][0][1]
                    t = br[1][1][1][1]
                    acc.append((t.line, t.col, s, t.fake))
                except Exception:
                    pass
            for a in n[2]:
                strings_of(a, acc)
        elif n[0] == 'L':
            for a in n[1]: strings_of(a, acc)
        elif n[0] == 'R':
            for a in n[1].values(): strings_of(a, acc)

def hist(n, h):
    if isinstance(n, tuple):
        if n[0] == 'C':
            h[label(n)] += 1
            for a in n[2]: hist(a, h)
        elif n[0] == 'L':
            for a in n[1]: hist(a, h)
        elif n[0] == 'R':
            for a in n[1].values(): hist(a, h)

def realtoks(n, acc):
    if isinstance(n, Tok):
        if not n.fake: acc.add((n.line, n.col, n.s))
    elif isinstance(n, tuple):
        if n[0] == 'C':
            for a in n[2]: realtoks(a, acc)
        elif n[0] == 'L':
            for a in n[1]: realtoks(a, acc)
        elif n[0] == 'R':
            for a in n[1].values(): realtoks(a, acc)

def main():
    sys.setrecursionlimit(1000000)
    ap = argparse.ArgumentParser()
    ap.add_argument('--errors-old', default=f'{D}/results/errors_old.txt')
    ap.add_argument('--errors-new', default=f'{D}/results/errors_new.txt')
    ap.add_argument('--only', default=None)
    ap.add_argument('--out', default=f'{D}/results/astdiff')
    args = ap.parse_args()
    eo = set(l.strip() for l in open(args.errors_old)) if os.path.exists(args.errors_old) else set()
    en = set(l.strip() for l in open(args.errors_new)) if os.path.exists(args.errors_new) else set()
    idx = range(1, len(FILES) + 1)
    if args.only:
        idx = [int(x) for x in args.only.split(',')]
    status = collections.Counter()
    per_proj = collections.defaultdict(collections.Counter)
    hist_old, hist_new = collections.Counter(), collections.Counter()
    kinds = collections.Counter()
    kinds_proj = collections.defaultdict(collections.Counter)
    examples = collections.defaultdict(list)
    tokdiff = collections.Counter()
    tokex = collections.defaultdict(list)
    fout = open(args.out + '_files.jsonl', 'w')
    for i in idx:
        f = FILES[i - 1]
        pj = project(f)
        per_proj[pj]['files'] += 1
        if f in eo or f in en:
            st = 'err_both' if (f in eo and f in en) else ('err_old_only' if f in eo else 'err_new_only')
            status[st] += 1; per_proj[pj][st] += 1
            continue
        jo, e1 = load('old', i)
        jn, e2 = load('new', i)
        if jo is None or jn is None:
            st = 'dump_fail_' + str(e1 if jo is None else 'ok') + '_' + str(e2 if jn is None else 'ok')
            status[st] += 1; per_proj[pj][st] += 1
            continue
        status['compared'] += 1; per_proj[pj]['compared'] += 1
        co, cn = canon(jo), canon(jn)
        no, nn = norm(co, 'old'), norm(cn, 'new')
        ho, hn = collections.Counter(), collections.Counter()
        hist(no, ho); hist(nn, hn)
        hist_old.update(ho); hist_new.update(hn)
        d = Diff()
        cmp(no, nn, '', d, True)
        # token sets
        to, tn = set(), set()
        realtoks(co, to); realtoks(cn, tn)
        lines = None
        for t in to - tn:
            tokdiff[('only_old', t[2] if not t[2][:1].isalnum() and len(t[2]) < 4 else ('<kw>' + t[2] if t[2] in KW else '<other>'))] += 1
            if len(tokex[('only_old', t[2] if t[2] in KW or len(t[2]) < 4 else '<other>')]) < 5:
                tokex[('only_old', t[2] if t[2] in KW or len(t[2]) < 4 else '<other>')].append((f, t[0], t[1], t[2][:40]))
        for t in tn - to:
            tokdiff[('only_new', t[2] if not t[2][:1].isalnum() and len(t[2]) < 4 else ('<kw>' + t[2] if t[2] in KW else '<other>'))] += 1
            if len(tokex[('only_new', t[2] if t[2] in KW or len(t[2]) < 4 else '<other>')]) < 5:
                tokex[('only_new', t[2] if t[2] in KW or len(t[2]) < 4 else '<other>')].append((f, t[0], t[1], t[2][:40]))
        fl = collections.Counter()
        for kind, ctx, ol, nl, tok, detail in d.items:
            key = f'{kind} | {ctx} | {ol} -> {nl}'
            kinds[key] += 1; kinds_proj[pj][key] += 1; fl[key] += 1
            if len(examples[key]) < 8:
                ln = tok.line if tok else None
                if lines is None:
                    try:
                        lines = open(host(f), encoding='utf-8', errors='replace').read().split('\n')
                    except Exception:
                        lines = []
                src = lines[ln - 1].strip()[:160] if ln and ln - 1 < len(lines) else ''
                examples[key].append({'file': f, 'line': ln, 'col': tok.col if tok else None, 'src': src, 'detail': detail})
        fout.write(json.dumps({'i': i, 'file': f, 'diffs': dict(fl), 'nodes_old': sum(ho.values()), 'nodes_new': sum(hn.values())}) + '\n')
    fout.close()
    res = {
        'status': status, 'per_project': per_proj,
        'hist_old': hist_old, 'hist_new': hist_new,
        'norm': NORM,
        'kinds': kinds.most_common(), 'kinds_per_project': kinds_proj,
        'examples': examples,
        'tokdiff': [[list(k), v] for k, v in tokdiff.most_common()],
        'tokex': {' '.join(k): v for k, v in tokex.items()},
    }
    json.dump(res, open(args.out + '.json', 'w'), indent=1, default=str)
    print(json.dumps(status))
    for k, v in kinds.most_common(60):
        print(v, k)

KW = {'and', 'break', 'do', 'else', 'elseif', 'end', 'false', 'for', 'function', 'goto', 'if', 'in',
      'local', 'nil', 'not', 'or', 'repeat', 'return', 'then', 'true', 'until', 'while', 'self', 'next'}

if __name__ == '__main__':
    main()
