#!/usr/bin/env python3
"""Check the value of every number literal of the NEW AST against the source text
(Lua semantics), and that a Float/Int/Imag kind matches the literal syntax.
Also counts, for the old AST, the literals (old: always Float)."""
import json, os, subprocess, collections, re, sys
D = os.path.dirname(os.path.abspath(__file__))
FILES = [l.rstrip('\n') for l in open('/tmp/tc/files.txt', encoding='utf-8')]
en = set(l.rstrip('\n') for l in open(f'{D}/results/errors_new.txt'))

def lua_value(s):
    l = s.lower()
    if l.endswith('i'):
        return 'Imag', None
    if re.search(r'u?ll$', l):
        l = re.sub(r'u?ll$', '', l)
        return 'Int', int(l, 16) if l.startswith('0x') else int(l)
    if l.startswith('0x'):
        if '.' in l or 'p' in l:
            return 'Float', float.fromhex(l)
        v = int(l, 16)
        # Lua wraps hex integers modulo 2^64
        v &= (1 << 64) - 1
        if v >= 1 << 63:
            v -= 1 << 64
        return 'Int', v
    if '.' in l or 'e' in l:
        return 'Float', float(l)
    v = int(l)
    if v >= 1 << 63:
        return 'Float', float(v)  # decimal integer overflow -> float in Lua
    return 'Int', v

def walk(x, acc):
    if isinstance(x, dict):
        if len(x) == 1:
            (k, v), = x.items()
            if k in ('Int', 'Float', 'Imag') and isinstance(v, list) and len(v) == 2 and isinstance(v[1], dict) \
                    and 'token' in v[1]:
                t = v[1]['token']
                if 'OriginTok' in t:
                    acc.append((k, v[0], t['OriginTok']['str'], t['OriginTok']['line'], t['OriginTok']['column']))
                return
        for v in x.values():
            walk(v, acc)
    elif isinstance(x, list):
        for v in x:
            walk(v, acc)

def main():
    sys.setrecursionlimit(1000000)
    stats = collections.Counter(); bad = collections.defaultdict(list)
    for i, f in enumerate(FILES, 1):
        if f in en:
            continue
        raw = subprocess.run(['zstd', '-dc', f'{D}/dumps/new/{i}.zst'], capture_output=True).stdout.decode('utf-8', 'replace')
        k = raw.find('{')
        if k < 0:
            continue
        acc = []
        walk(json.loads(raw[k:]), acc)
        for kind, val, s, line, col in acc:
            stats['literals'] += 1; stats['kind_' + kind] += 1
            try:
                ek, ev = lua_value(s)
            except Exception as e:
                stats['unparsable'] += 1; bad['unparsable'].append((f, line, s)); continue
            if ek != kind:
                stats['kind_mismatch'] += 1
                if len(bad['kind ' + ek + '->' + kind]) < 10: bad['kind ' + ek + '->' + kind].append((f, line, s, val))
                continue
            if kind == 'Int':
                v = val.get('some') if isinstance(val, dict) else val
                if v != ev:
                    key = 'int_value' if v is not None else 'int_none'
                    stats[key] += 1
                    if len(bad[key]) < 10: bad[key].append((f, line, s, v, ev))
            elif kind == 'Float':
                v = val.get('some') if isinstance(val, dict) else val
                if v is None or abs(v - ev) > 1e-9 * max(1, abs(ev)):
                    stats['float_value'] += 1
                    if len(bad['float_value']) < 10: bad['float_value'].append((f, line, s, v, ev))
    json.dump({'stats': stats, 'bad': bad}, open(f'{D}/results/numcheck.json', 'w'), indent=1)
    print(dict(stats)); print(json.dumps(bad, indent=1)[:3000])

if __name__ == '__main__':
    main()
