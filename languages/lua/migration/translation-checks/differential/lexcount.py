#!/usr/bin/env python3
"""Independent lexical counts of Lua constructs (comments and strings removed).

Used as a third opinion for the pattern-query differential.
lexcount.py -> results/lexcounts.jsonl  (one line per file of /tmp/tc/files.txt)
"""
import re, json, sys, os

D = os.path.dirname(os.path.abspath(__file__))
KW = {'and', 'break', 'do', 'else', 'elseif', 'end', 'false', 'for', 'function', 'goto', 'if', 'in',
      'local', 'nil', 'not', 'or', 'repeat', 'return', 'then', 'true', 'until', 'while'}
OPS = ['...', '..', '==', '~=', '<=', '>=', '<<', '>>', '//', '::']
NUM = re.compile(r'0[xX](?:[0-9a-fA-F]*\.?[0-9a-fA-F]*)(?:[pP][+-]?\d+)?(?:[uU]?[lL][lL]|i)?|'
                 r'(?:\d+\.?\d*|\.\d+)(?:[eE][+-]?\d+)?(?:[uU]?[lL][lL]|i)?')
NAME = re.compile(r'[A-Za-z_\u0080-\U0010ffff][A-Za-z0-9_\u0080-\U0010ffff]*')
LONGOPEN = re.compile(r'\[(=*)\[')


def lex(src):
    """yield (kind, text, line) with kind in NAME KW NUM STR OP"""
    toks = []
    i, n, line = 0, len(src), 1
    if src.startswith('#!') or src.startswith('#'):
        if src.startswith('#!'):
            j = src.find('\n')
            i = n if j < 0 else j
    while i < n:
        c = src[i]
        if c == '\n':
            line += 1; i += 1; continue
        if c in ' \t\r\f\v':
            i += 1; continue
        if src.startswith('--', i):
            m = LONGOPEN.match(src, i + 2)
            if m:
                close = ']' + m.group(1) + ']'
                j = src.find(close, m.end())
                j = n if j < 0 else j + len(close)
            else:
                j = src.find('\n', i)
                j = n if j < 0 else j
            line += src.count('\n', i, j); i = j; continue
        m = LONGOPEN.match(src, i)
        if m:
            close = ']' + m.group(1) + ']'
            j = src.find(close, m.end())
            j = n if j < 0 else j + len(close)
            toks.append(('STR', 'long', line)); line += src.count('\n', i, j); i = j; continue
        if c in '"\'':
            j = i + 1
            while j < n and src[j] != c:
                if src[j] == '\\':
                    if src[j + 1:j + 2] == 'z':
                        j += 2
                        while j < n and src[j] in ' \t\r\n':
                            j += 1
                        continue
                    j += 2; continue
                if src[j] == '\n':
                    break
                j += 1
            toks.append(('STR', 'short', line)); line += src.count('\n', i, j + 1); i = j + 1; continue
        if c.isdigit() or (c == '.' and i + 1 < n and src[i + 1].isdigit()):
            m = NUM.match(src, i)
            toks.append(('NUM', m.group(0), line)); i = m.end(); continue
        m = NAME.match(src, i)
        if m:
            w = m.group(0)
            toks.append(('KW' if w in KW else 'NAME', w, line)); i = m.end(); continue
        for op in OPS:
            if src.startswith(op, i):
                toks.append(('OP', op, line)); i += len(op); break
        else:
            toks.append(('OP', c, line)); i += 1
    return toks


def counts(toks):
    c = {}
    def inc(k, v=1):
        c[k] = c.get(k, 0) + v
    T = toks
    L = len(T)
    for k, (kind, t, line) in enumerate(T):
        prev = T[k - 1] if k > 0 else ('', '', 0)
        nxt = T[k + 1] if k + 1 < L else ('', '', 0)
        if kind == 'KW':
            if t in ('while', 'repeat', 'if', 'elseif', 'goto', 'until', 'return', 'nil', 'true', 'false', 'break', 'not', 'and', 'or'):
                inc(t)
            if t == 'function':
                if nxt[1] == '(':
                    inc('lambda')
                else:
                    inc('func_decl')
                    if prev[1] == 'local':
                        inc('local_func')
                    # method decl: function a.b:c
                    j = k + 1
                    while j + 1 < L and T[j][0] == 'NAME' and T[j + 1][1] in ('.', ':'):
                        if T[j + 1][1] == ':':
                            inc('method_decl'); break
                        j += 2
            if t == 'for':
                j = k + 1
                names = 0
                while j < L and T[j][0] == 'NAME':
                    names += 1
                    if T[j + 1][1] == ',':
                        j += 2
                    else:
                        j += 1; break
                if j < L and T[j][1] == '=':
                    inc('numfor')
                elif j < L and T[j][1] == 'in':
                    inc('genfor')
                    inc('genfor%d' % min(names, 3))
            if t == 'local':
                if nxt[1] != 'function':
                    inc('local_stmt')
            if t == 'do' and prev[1] not in ():
                inc('do_kw')
        elif kind == 'STR':
            inc('string')
            inc('string_' + t)
        elif kind == 'NUM':
            inc('number')
        elif kind == 'OP':
            if t in ('..', '==', '~=', '#', '+', '{', '<', '...'):
                inc({'..': 'concat', '==': 'eq', '~=': 'neq', '#': 'len', '+': 'plus', '{': 'table', '<': 'lt_or_attr', '...': 'vararg'}[t])
            if t == ':' and T[k - 1][0] == 'NAME' and nxt[0] == 'NAME' and k + 2 < L and \
                    (T[k + 2][1] in ('(', '{') or T[k + 2][0] == 'STR'):
                # method call, unless it is the name of a function declaration
                j = k - 1
                while j - 2 >= 0 and T[j - 1][1] == '.' and T[j - 2][0] == 'NAME':
                    j -= 2
                if not (j - 1 >= 0 and T[j - 1][1] == 'function'):
                    inc('method_call')
            if t == '.' and prev[0] in ('NAME', 'KW') and nxt[0] == 'NAME':
                if prev[0] == 'NAME' or prev[1] in ():
                    inc('dot_after_name')
            if t == '.':
                inc('dot')
            if t == '[' :
                inc('index_or_key')
            if t == '=' :
                inc('equals')
        # calls: an argument list after a prefix expression
        if (t == '(' and kind == 'OP') or kind == 'STR' or (t == '{' and kind == 'OP'):
            if prev[0] == 'NAME' or prev[1] in (')', ']'):
                # not 'function name(' (declaration)
                j = k - 1
                if prev[0] == 'NAME':
                    while j - 2 >= 0 and T[j - 1][1] in ('.', ':') and T[j - 2][0] == 'NAME':
                        j -= 2
                    if j - 1 >= 0 and T[j - 1][1] == 'function':
                        continue
                    if j - 1 >= 0 and T[j - 1][1] == 'local' and False:
                        continue
                if kind == 'STR' and prev[0] != 'NAME' and prev[1] not in (')', ']'):
                    continue
                inc('call')
    return c


def main():
    files = [l.rstrip('\n') for l in open('/tmp/tc/files.txt', encoding='utf-8')]
    out = open(f'{D}/results/lexcounts.jsonl', 'w')
    for i, f in enumerate(files, 1):
        h = '/tmp' + f[len('/host-tmp'):]
        try:
            src = open(h, encoding='utf-8', errors='replace').read()
        except Exception as e:
            out.write(json.dumps({'i': i, 'file': f, 'error': str(e)}) + '\n'); continue
        out.write(json.dumps({'i': i, 'file': f, 'c': counts(lex(src))}) + '\n')


if __name__ == '__main__':
    main()
