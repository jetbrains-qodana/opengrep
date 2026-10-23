#!/usr/bin/env python3
"""Lexical oracle for the Lua -> AST_generic translation.

An independent Lua lexer + a light token-level analysis computes, per file,
a set of keyed "constructs" (key = byte position of a characteristic source
token, value = a few properties such as the number of arguments). The same
constructs are extracted from the AST JSON dumped by opengrep-core
(-full_token_info -dump_ast -json) by walking it. Both sides are compared
per category: matched (same key, same value), missing (in source, not in
AST), extra (in AST, not in source), mismatched (same key, other value).
"""
import json
import re
import sys
from collections import Counter, defaultdict

KEYWORDS = set("""and break do else elseif end false for function goto if in
local nil not or repeat return then true until while""".split())

NAME_RE = re.compile(r'[A-Za-z_\x80-\xff][A-Za-z0-9_\x80-\xff]*')
HEX_RE = re.compile(r'0[xX][0-9a-fA-F]*(?:\.[0-9a-fA-F]*)?(?:[pP][+-]?[0-9]+)?')
BIN_RE = re.compile(r'0[bB][01]+')
DEC_RE = re.compile(r'(?:[0-9]+(?:\.[0-9]*)?|\.[0-9]+)(?:[eE][+-]?[0-9]+)?')
SUFFIX_RE = re.compile(r'(?:[uU]?[lL][lL]|[iI])')
LONG_OPEN_RE = re.compile(r'\[(=*)\[')
OPS3 = ('...',)
OPS2 = ('..', '==', '~=', '<=', '>=', '<<', '>>', '//', '::')
OPS1 = set('+-*/%^#&~|<>=(){}[];:,.')

BINOPS = set(['+', '-', '*', '/', '//', '%', '^', '..', '==', '~=', '<', '<=',
              '>', '>=', 'and', 'or', '&', '|', '~', '<<', '>>'])
UNOPS = set(['-', 'not', '#', '~'])


class Tok:
    __slots__ = ('kind', 'text', 'pos', 'line', 'value', 'flags')

    def __init__(self, kind, text, pos, line, value=None):
        self.kind = kind  # name kw str num op eof
        self.text = text
        self.pos = pos
        self.line = line
        self.value = value
        self.flags = set()

    def __repr__(self):
        return '%s:%r@%d' % (self.kind, self.text, self.pos)


class LexError(Exception):
    pass


def long_content(s):
    s = re.sub(r'\r\n|\n\r|\r', '\n', s)
    if s.startswith('\n'):
        s = s[1:]
    return s


def lex(src):
    """src: str decoded as latin-1 (1 char = 1 byte)."""
    toks = []
    i = 0
    n = len(src)
    line = 1
    errors = []
    # shebang / first line starting with '#'
    if src.startswith('#'):
        j = src.find('\n')
        i = n if j < 0 else j
    while i < n:
        c = src[i]
        if c == '\n':
            line += 1
            i += 1
            continue
        if c in ' \t\r\f\v':
            i += 1
            continue
        start = i
        sline = line
        if c == '-' and src.startswith('--', i):
            m = LONG_OPEN_RE.match(src, i + 2)
            if m:
                close = ']' + m.group(1) + ']'
                j = src.find(close, m.end())
                if j < 0:
                    errors.append(('unterminated long comment', sline))
                    j = n
                    end = n
                else:
                    end = j + len(close)
                line += src.count('\n', i, end)
                i = end
            else:
                j = src.find('\n', i)
                i = n if j < 0 else j
            continue
        if c == '[':
            m = LONG_OPEN_RE.match(src, i)
            if m:
                close = ']' + m.group(1) + ']'
                j = src.find(close, m.end())
                if j < 0:
                    errors.append(('unterminated long string', sline))
                    j = n
                    end = n
                else:
                    end = j + len(close)
                content = src[m.end():j]
                line += src.count('\n', i, end)
                toks.append(Tok('str', src[i:end], start, sline,
                                ('long', long_content(content), m.group(0))))
                i = end
                continue
        if c == '"' or c == "'":
            j = i + 1
            ok = False
            while j < n:
                d = src[j]
                if d == '\\':
                    if src[j + 1:j + 2] == 'z':
                        j += 2
                        while j < n and src[j] in ' \t\r\n\f\v':
                            j += 1
                        continue
                    if src[j + 1:j + 3] in ('\r\n', '\n\r'):
                        j += 3
                        continue
                    j += 2
                    continue
                if d == c:
                    ok = True
                    break
                if d == '\n':
                    break
                j += 1
            if not ok:
                errors.append(('unterminated string', sline))
                j = min(j, n)
                toks.append(Tok('str', src[i:j], start, sline, ('short', src[i + 1:j], c)))
                line += src.count('\n', i, j)
                i = j
                continue
            toks.append(Tok('str', src[i:j + 1], start, sline, ('short', src[i + 1:j], c)))
            line += src.count('\n', i, j + 1)
            i = j + 1
            continue
        if c.isdigit() or (c == '.' and i + 1 < n and src[i + 1].isdigit()):
            m = HEX_RE.match(src, i) if src.startswith(('0x', '0X'), i) else None
            if not m:
                m = BIN_RE.match(src, i) if src.startswith(('0b', '0B'), i) else None
            if not m:
                m = DEC_RE.match(src, i)
            j = m.end()
            ms = SUFFIX_RE.match(src, j)
            if ms and not (j + len(ms.group(0)) < n and
                           (src[j + len(ms.group(0))].isalnum() or src[j + len(ms.group(0))] == '_')):
                j = ms.end()
            toks.append(Tok('num', src[i:j], start, sline))
            i = j
            continue
        m = NAME_RE.match(src, i)
        if m:
            w = m.group(0)
            toks.append(Tok('kw' if w in KEYWORDS else 'name', w, start, sline))
            i = m.end()
            continue
        if src.startswith('...', i):
            toks.append(Tok('op', '...', start, sline))
            i += 3
            continue
        two = src[i:i + 2]
        if two in OPS2:
            toks.append(Tok('op', two, start, sline))
            i += 2
            continue
        if c in OPS1:
            toks.append(Tok('op', c, start, sline))
            i += 1
            continue
        errors.append(('unexpected char %r' % c, sline))
        i += 1
    toks.append(Tok('eof', '', n, line))
    return toks, errors


# ---------------------------------------------------------------------------
# token-level analysis


def is_op(t, s):
    return t.kind == 'op' and t.text == s


def is_kw(t, s):
    return t.kind == 'kw' and t.text == s


def compute_matches(toks):
    """bracket matches and block keyword matches (function/if/do/repeat -> end/until)"""
    match = {}
    st = []
    bst = []
    for i, t in enumerate(toks):
        if t.kind == 'op' and t.text in '([{':
            st.append(i)
        elif t.kind == 'op' and t.text in ')]}':
            if st:
                j = st.pop()
                match[j] = i
                match[i] = j
        elif t.kind == 'kw':
            if t.text in ('function', 'if', 'do', 'repeat'):
                bst.append(i)
            elif t.text == 'end' or t.text == 'until':
                if bst:
                    j = bst.pop()
                    match[j] = i
                    match[i] = j
    return match


class Analyzer:
    def __init__(self, toks):
        self.toks = toks
        self.match = compute_matches(toks)
        self.out = defaultdict(dict)  # cat -> key -> value
        self.counts = Counter()  # count-only categories

    def t(self, i):
        if i < 0:
            return Tok('bof', '', -1, 0)
        if i >= len(self.toks):
            return self.toks[-1]
        return self.toks[i]

    def skip_group(self, i):
        """i at an opening bracket or block keyword: index after its match"""
        j = self.match.get(i)
        if j is None:
            return len(self.toks) - 1
        return j + 1

    # count elements of a comma separated list between [a, b) (top level)
    def count_list(self, a, b):
        if a >= b:
            return 0
        n = 1
        i = a
        while i < b:
            t = self.toks[i]
            if t.kind == 'op' and t.text in '([{':
                i = self.skip_group(i)
                continue
            if is_kw(t, 'function'):
                i = self.skip_group(i)
                continue
            if is_op(t, ','):
                n += 1
            i += 1
        return n

    def explist_len(self, i):
        """forward expression list starting at token i; returns count"""
        toks = self.toks
        state = 'need'
        count = 0
        started = False
        while True:
            t = self.t(i)
            if state == 'need':
                if (t.kind == 'op' and t.text in ('-', '#', '~')) or is_kw(t, 'not'):
                    i += 1
                    continue
                if t.kind in ('name', 'num', 'str') or is_op(t, '...') or \
                        (t.kind == 'kw' and t.text in ('nil', 'true', 'false')):
                    if not started:
                        count += 1
                        started = True
                    state = 'after'
                    i += 1
                    continue
                if t.kind == 'op' and t.text in ('(', '{'):
                    if not started:
                        count += 1
                        started = True
                    i = self.skip_group(i)
                    state = 'after'
                    continue
                if is_kw(t, 'function'):
                    if not started:
                        count += 1
                        started = True
                    i = self.skip_group(i)
                    state = 'after'
                    continue
                return count
            else:
                if (t.kind == 'op' and t.text in BINOPS) or (t.kind == 'kw' and t.text in ('and', 'or')):
                    state = 'need'
                    i += 1
                    continue
                if t.kind == 'op' and t.text in ('.', ':'):
                    i += 2
                    continue
                if t.kind == 'op' and t.text in ('[', '(', '{'):
                    i = self.skip_group(i)
                    continue
                if t.kind == 'str':
                    i += 1
                    continue
                if is_op(t, ','):
                    started = False
                    state = 'need'
                    i += 1
                    continue
                return count

    def count_targets(self, eq):
        """backward count of the assignment targets before '=' at eq"""
        n = 1
        i = eq - 1
        while True:
            i = self.back_target(i)
            if i is None:
                return n
            if is_op(self.t(i), ','):
                n += 1
                i -= 1
                continue
            return n

    def back_target(self, i):
        """i at the last token of a target; returns the index before its start"""
        while i >= 0:
            t = self.toks[i]
            if t.kind == 'name':
                p = self.t(i - 1)
                if p.kind == 'op' and p.text in ('.', ':'):
                    i -= 2
                    continue
                return i - 1
            if t.kind == 'op' and t.text in (')', ']', '}'):
                j = self.match.get(i)
                if j is None:
                    return None
                p = self.t(j - 1)
                if t.text == ')' and not self.prefix_end(j - 1):
                    return j - 1
                i = j - 1
                continue
            if t.kind == 'str':
                i -= 1
                continue
            return None
        return None

    def prefix_end(self, i):
        if i < 0:
            return False
        t = self.toks[i]
        if t.kind == 'name':
            return 'decl' not in t.flags
        if t.kind == 'op' and t.text == ')':
            return 'params' not in t.flags
        if t.kind == 'op' and t.text == ']':
            return 'key' not in t.flags
        if t.kind == 'op' and t.text == '}':
            return 'callarg' in t.flags
        if t.kind == 'str':
            return 'callarg' in t.flags
        return False

    def expr_end(self, i):
        t = self.t(i)
        if t.kind in ('name', 'num', 'str'):
            return True
        if t.kind == 'op' and t.text in (')', ']', '}', '...'):
            return True
        if t.kind == 'kw' and t.text in ('nil', 'true', 'false', 'end'):
            return True
        return False

    def params(self, lp):
        """lp: '(' of a parameter list; returns number of params"""
        rp = self.match.get(lp)
        if rp is None:
            return 0
        self.toks[rp].flags.add('params')
        n = 0
        for k in range(lp + 1, rp):
            t = self.toks[k]
            if t.kind == 'name':
                t.flags.add('decl')
                if not is_op(self.toks[k - 1], '...'):
                    n += 1
            elif is_op(t, '...'):
                n += 1
        return n

    def run(self):
        toks = self.toks
        out = self.out
        # table frames: stack of dicts for '{' ; we process fields when we meet '{'
        block_stack = []  # for if-chains: index of the current if/elseif token
        N = len(toks)
        for i in range(N):
            t = toks[i]
            p = self.t(i - 1)
            if t.kind == 'eof':
                break
            if t.kind == 'name':
                out['name'][t.pos] = t.text
                if t.text == 'global' and 'decl' not in t.flags \
                        and not (p.kind == 'op' and p.text in ('.', ':', ',', '(', '[', '{', '=') ) \
                        and not (p.kind == 'kw' and p.text in ('local', 'function', 'return', 'and', 'or', 'not', 'for', 'in', 'if', 'elseif', 'while', 'until', 'goto')):
                    nt = self.t(i + 1)
                    if nt.kind == 'name' or is_kw(nt, 'function') or is_op(nt, '<') or is_op(nt, '*'):
                        self.do_global(i)
                continue
            if t.kind == 'str':
                kind, val, _ = t.value
                out['string'][t.pos] = val
                if p.kind == 'name' and is_op(self.t(i - 2), ':') and 'funcname' not in self.t(i - 2).flags:
                    t.flags.add('callarg')
                elif self.prefix_end(i - 1):
                    out['call'][t.pos] = 1
                    t.flags.add('callarg')
                continue
            if t.kind == 'num':
                out['number'][t.pos] = t.text
                continue
            if t.kind == 'kw':
                w = t.text
                if w in ('nil', 'true', 'false'):
                    out[w][t.pos] = w
                elif w == 'function':
                    self.do_function(i)
                elif w == 'local':
                    nt = self.t(i + 1)
                    if not is_kw(nt, 'function'):
                        self.do_local(i)
                elif w == 'return':
                    nt = self.t(i + 1)
                    out['return'][t.pos] = self.explist_len(i + 1)
                elif w == 'for':
                    self.do_for(i)
                elif w == 'while':
                    out['while'][t.pos] = 'while'
                elif w == 'repeat':
                    out['repeat'][t.pos] = 'repeat'
                elif w == 'if' or w == 'elseif':
                    # value: whether an 'else' directly follows this branch
                    out[w][t.pos] = False
                    if w == 'if':
                        block_stack.append(t.pos)
                    else:
                        if block_stack:
                            block_stack[-1] = t.pos
                elif w == 'else':
                    self.counts['else'] += 1
                    # find the owning if: the innermost open 'if' block
                    j = self.owner_if(i)
                    if j is not None:
                        out[toks[j].text][toks[j].pos] = True
                elif w == 'goto':
                    nt = self.t(i + 1)
                    if nt.kind == 'name':
                        nt.flags.add('decl')
                    out['goto'][t.pos] = nt.text
                elif w == 'break':
                    out['break'][t.pos] = 'break'
                elif w in ('not',):
                    out['op'][t.pos] = ('not', 1)
                elif w in ('and', 'or'):
                    out['op'][t.pos] = (w, 2)
                continue
            # operators / punctuation
            s = t.text
            if s == '...':
                out['vararg'][t.pos] = '...'
            elif s == '::':
                nt = self.t(i + 1)
                if nt.kind == 'name' and is_op(self.t(i + 2), '::') and 'labelend' not in t.flags:
                    out['label'][nt.pos] = nt.text
                    nt.flags.add('decl')
                    self.toks[i + 2].flags.add('labelend')
            elif s == '(':
                if 'paramsopen' in t.flags:
                    pass
                elif self.prefix_end(i - 1):
                    j = self.match.get(i)
                    n = self.count_list(i + 1, j) if j is not None else 0
                    if p.kind == 'name' and is_op(self.t(i - 2), ':'):
                        out['method_call'][self.t(i - 2).pos] = (p.text, n)
                    else:
                        out['call'][t.pos] = n
            elif s == '{':
                j = self.match.get(i)
                is_arg = self.prefix_end(i - 1)
                if is_arg:
                    if j is not None:
                        toks[j].flags.add('callarg')
                    if p.kind == 'name' and is_op(self.t(i - 2), ':'):
                        out['method_call'][self.t(i - 2).pos] = (p.text, 1)
                    else:
                        out['call'][t.pos] = 1
                out['table'][t.pos] = self.table_fields(i, j)
            elif s == '[':
                if 'keyopen' in t.flags:
                    pass
                else:
                    out['index'][t.pos] = '['
            elif s == '.':
                nt = self.t(i + 1)
                if 'funcname' not in t.flags and nt.kind == 'name':
                    out['dot'][t.pos] = nt.text
            elif s == ':':
                nt = self.t(i + 1)
                if 'funcname' not in t.flags and nt.kind == 'name':
                    a = self.t(i + 2)
                    if a.kind == 'str':
                        out['method_call'][t.pos] = (nt.text, 1)
                        a.flags.add('callarg')
            elif s == '=':
                if 'nonassign' not in t.flags:
                    out['assign'][t.pos] = (self.count_targets(i), self.explist_len(i + 1))
            elif s in ('-', '~'):
                if self.expr_end(i - 1):
                    out['op'][t.pos] = (s, 2)
                else:
                    out['op'][t.pos] = (s, 1)
            elif s == '#':
                out['op'][t.pos] = ('#', 1)
            elif s in BINOPS:
                if 'attr' in t.flags:
                    pass
                else:
                    out['op'][t.pos] = (s, 2)
        return out

    def owner_if(self, i):
        """for 'else' at i, the 'if'/'elseif' token of the same chain (last one)"""
        # walk back skipping nested blocks to the nearest if/elseif at the same level
        k = i - 1
        while k >= 0:
            t = self.toks[k]
            if t.kind == 'kw':
                if t.text in ('end', 'until'):
                    j = self.match.get(k)
                    if j is None:
                        return None
                    k = j - 1
                    continue
                if t.text in ('elseif', 'if'):
                    return k
                if t.text in ('function', 'do', 'repeat'):
                    return None
            k -= 1
        return None

    def table_fields(self, lb, rb):
        if rb is None:
            return (0, 0, 0)
        toks = self.toks
        npos = nnamed = nkeyed = 0
        i = lb + 1
        field_start = True
        while i < rb:
            t = toks[i]
            if field_start:
                if is_op(t, ',') or is_op(t, ';'):
                    i += 1
                    continue
                field_start = False
                if is_op(t, '['):
                    t.flags.add('keyopen')
                    k = self.match.get(i)
                    if k is not None:
                        toks[k].flags.add('key')
                        if is_op(self.t(k + 1), '='):
                            self.t(k + 1).flags.add('nonassign')
                    nkeyed += 1
                elif t.kind == 'name' and is_op(self.t(i + 1), '='):
                    t.flags.add('decl')
                    self.t(i + 1).flags.add('nonassign')
                    nnamed += 1
                else:
                    npos += 1
            if t.kind == 'op' and t.text in '([{':
                i = self.skip_group(i)
                continue
            if is_kw(t, 'function'):
                i = self.skip_group(i)
                continue
            if is_op(t, ',') or is_op(t, ';'):
                field_start = True
            i += 1
        return (npos, nnamed, nkeyed)

    def do_function(self, i):
        toks = self.toks
        t = toks[i]
        p = self.t(i - 1)
        nt = self.t(i + 1)
        if nt.kind == 'name':
            kind = 'stmt'
            if is_kw(p, 'local'):
                kind = 'local'
            elif p.kind == 'name' and p.text == 'global':
                kind = 'global'
            k = i + 1
            nt.flags.add('decl')
            nt.flags.add('funcname')
            k += 1
            while True:
                a = self.t(k)
                if a.kind == 'op' and a.text in ('.', ':') and self.t(k + 1).kind == 'name':
                    a.flags.add('funcname')
                    self.t(k + 1).flags.add('decl')
                    self.t(k + 1).flags.add('funcname')
                    if a.text == ':':
                        kind = 'method'
                    k += 2
                    continue
                break
            lp = k
        else:
            kind = 'anon'
            lp = i + 1
        if is_op(self.t(lp), '('):
            self.t(lp).flags.add('paramsopen')
            n = self.params(lp)
        else:
            n = -1
        self.out['function'][t.pos] = (kind, n)

    def do_local(self, i):
        toks = self.toks
        k = i + 1
        n = 0
        while True:
            a = self.t(k)
            if is_op(a, '<') and self.t(k + 1).kind == 'name' and is_op(self.t(k + 2), '>'):
                a.flags.add('attr')
                self.t(k + 1).flags.add('decl')
                self.t(k + 2).flags.add('attr')
                k += 3
                continue
            if a.kind == 'name':
                a.flags.add('decl')
                n += 1
                self.out['local_name'][a.pos] = a.text
                k += 1
                if is_op(self.t(k), '<') and self.t(k + 1).kind == 'name' and is_op(self.t(k + 2), '>'):
                    self.t(k).flags.add('attr')
                    self.t(k + 1).flags.add('decl')
                    self.t(k + 2).flags.add('attr')
                    k += 3
                if is_op(self.t(k), ','):
                    k += 1
                    continue
            break
        a = self.t(k)
        nv = 0
        if is_op(a, '='):
            a.flags.add('nonassign')
            nv = self.explist_len(k + 1)
        self.out['local'][toks[i].pos] = n
        self.out['local_values'][toks[i].pos] = (n, nv)

    def do_global(self, i):
        # Lua 5.5 'global [<attr>] names [= values]', 'global [<attr>] *'
        k = i + 1
        while True:
            a = self.t(k)
            if is_op(a, '<') and self.t(k + 1).kind == 'name' and is_op(self.t(k + 2), '>'):
                a.flags.add('attr')
                self.t(k + 1).flags.add('decl')
                self.t(k + 2).flags.add('attr')
                k += 3
                continue
            if is_op(a, '*'):
                a.flags.add('attr')
                return
            if a.kind == 'name':
                a.flags.add('decl')
                k += 1
                if is_op(self.t(k), '<') and self.t(k + 1).kind == 'name' and is_op(self.t(k + 2), '>'):
                    self.t(k).flags.add('attr')
                    self.t(k + 1).flags.add('decl')
                    self.t(k + 2).flags.add('attr')
                    k += 3
                if is_op(self.t(k), ','):
                    k += 1
                    continue
            break
        if is_op(self.t(k), '='):
            self.t(k).flags.add('nonassign')

    def do_for(self, i):
        toks = self.toks
        a = self.t(i + 1)
        b = self.t(i + 2)
        if a.kind == 'name' and is_op(b, '='):
            a.flags.add('decl')
            b.flags.add('nonassign')
            # count range expressions until 'do'
            n = self.explist_len(i + 3)
            self.out['for'][toks[i].pos] = ('num', n)
        else:
            k = i + 1
            n = 0
            while self.t(k).kind == 'name':
                self.t(k).flags.add('decl')
                n += 1
                k += 1
                if is_op(self.t(k), ','):
                    k += 1
            nexp = self.explist_len(k + 1) if is_kw(self.t(k), 'in') else 0
            self.out['for'][toks[i].pos] = ('gen', n)
            self.out['for_iter'][toks[i].pos] = nexp


def analyze_source(raw):
    src = raw.decode('latin-1')
    toks, errors = lex(src)
    an = Analyzer(toks)
    out = an.run()
    lines = {t.pos: t.line for t in toks}
    return out, an.counts, errors, lines


# ---------------------------------------------------------------------------
# AST side


def tok_of(x):
    """x: {"token": {...}} -> (pos, str, line) or None for fake"""
    if isinstance(x, dict):
        t = x.get('token')
        if isinstance(t, dict):
            o = t.get('OriginTok')
            if o is not None:
                return (o['bytepos'], fix(o['str']), o['line'])
            # ExpandedTok etc.
            for k in ('ExpandedTok',):
                if k in t:
                    v = t[k]
                    if isinstance(v, list) and v and isinstance(v[0], dict):
                        return (v[0]['bytepos'], fix(v[0]['str']), v[0]['line'])
    return None


def fix(s):
    try:
        return s.encode('utf-8', 'surrogateescape').decode('latin-1')
    except Exception:
        return s


def is_fake_nil(e):
    if isinstance(e, dict) and 'L' in e:
        l = e['L']
        if isinstance(l, dict) and 'Null' in l and tok_of(l['Null']) is None:
            return True
    return False


def tuple_elems(e):
    """Container Tuple with fake brackets -> elems, else None"""
    if isinstance(e, dict) and 'Container' in e:
        c = e['Container']
        if c[0] == 'Tuple':
            lb, elems, rb = c[1]
            if tok_of(lb) is None:
                return elems
    return None


def pname(p):
    """parameter -> (name, tokinfo)"""
    if isinstance(p, dict):
        if 'Param' in p:
            pn = p['Param'].get('pname')
            if pn and 'some' in pn:
                return pn['some'][0], tok_of(pn['some'][1])
        if 'ParamRest' in p:
            pn = p['ParamRest'][1].get('pname')
            if pn and 'some' in pn:
                return pn['some'][0], tok_of(pn['some'][1])
            return '...', None
        if 'ParamEllipsis' in p:
            return '...', None
    return None, None


def first_pos(e):
    best = None
    stack = [e]
    while stack:
        x = stack.pop()
        if isinstance(x, dict):
            t = tok_of(x)
            if t is not None:
                if best is None or t[0] < best:
                    best = t[0]
                continue
            stack.extend(x.values())
        elif isinstance(x, list):
            stack.extend(x)
    return best


class AstExtractor:
    def __init__(self):
        self.out = defaultdict(dict)
        self.lines = {}
        self.counts = Counter()
        self.alltoks = {}
        self.local_groups = Counter()
        self.local_valcount = Counter()
        self.nonname = set()

    def put(self, cat, tk, val):
        if tk is None:
            self.counts['fake_' + cat] += 1
            return
        pos = tk[0]
        if pos in self.out[cat] and self.out[cat][pos] != val:
            self.counts['dupkey_' + cat] += 1
        self.out[cat][pos] = val
        self.lines[pos] = tk[2]

    def walk(self, root):
        stack = [root]
        while stack:
            x = stack.pop()
            if isinstance(x, list):
                stack.extend(x)
                continue
            if not isinstance(x, dict):
                continue
            if 'token' in x and len(x) == 1:
                tk = tok_of(x)
                if tk is not None:
                    self.alltoks[tk[0]] = tk
                    self.lines[tk[0]] = tk[2]
                continue
            for k, v in x.items():
                h = getattr(self, 'n_' + k, None)
                if h is not None:
                    try:
                        h(v)
                    except (KeyError, IndexError, TypeError, AttributeError) as ex:
                        self.counts['walk_error_' + k] += 1
                stack.append(v)

    def n_Call(self, v):
        fn, args = v
        lb, al, rb = args
        n = len(al)
        if isinstance(fn, dict) and 'IdSpecial' in fn:
            sp, tk = fn['IdSpecial']
            tk = tok_of(tk)
            if isinstance(sp, dict) and 'Op' in sp:
                if tk is None:
                    self.counts['fake_op'] += 1
                    return
                if tk[1] in ('until', ','):
                    self.counts['op_synthetic_' + tk[1]] += 1
                    return
                self.put('op', tk, (tk[1], n))
                return
            self.counts['call_idspecial'] += 1
        if isinstance(fn, dict) and 'N' in fn and 'IdQualified' in fn['N']:
            q = fn['N']['IdQualified']
            mid = q.get('name_middle')
            if mid and 'some' in mid and 'QExpr' in mid['some']:
                colon = tok_of(mid['some']['QExpr'][1])
                name = fix(q['name_last'][0][0])
                self.put('method_call', colon, (name, n))
                return
        tk = tok_of(lb)
        if tk is None and al:
            a0 = al[0].get('Arg') if isinstance(al[0], dict) else None
            if a0 is not None:
                p = None
                if 'L' in a0 and 'String' in a0['L']:
                    s = a0['L']['String']
                    p = tok_of(s[0]) or tok_of(s[1][1])
                elif 'Container' in a0:
                    p = tok_of(a0['Container'][1][0])
                tk = p
        self.put('call', tk, n)

    def n_DotAccess(self, v):
        e, dot, fld = v
        name = None
        if isinstance(fld, dict) and 'FN' in fld:
            fn = fld['FN']
            if 'Id' in fn:
                name = fix(fn['Id'][0][0])
        self.put('dot', tok_of(dot), name)

    def n_ArrayAccess(self, v):
        e, br = v
        self.put('index', tok_of(br[0]), '[')

    def n_Container(self, v):
        kind, br = v
        if kind != 'Dict':
            return
        lb, elems, rb = br
        npos = nnamed = nkeyed = 0
        for el in elems:
            kv = tuple_elems(el)
            if kv is not None and len(kv) == 2:
                k = kv[0]
                if isinstance(k, dict) and 'L' in k:
                    l = k['L']
                    if 'Int' in l and tok_of(l['Int'][1]) is None:
                        npos += 1
                        continue
                    if 'String' in l and tok_of(l['String'][0]) is None and \
                            tok_of(l['String'][1][1]) is not None and \
                            not tok_of(l['String'][1][1])[1][:1] in ('"', "'"):
                        nnamed += 1
                        continue
                nkeyed += 1
            else:
                self.counts['table_nonpair_field'] += 1
                npos += 1
        self.put('table', tok_of(lb), (npos, nnamed, nkeyed))

    def fparams(self, fd):
        lp, ps, rp = fd['fparams']
        names = [pname(p) for p in ps]
        return names

    def n_Lambda(self, v):
        names = self.fparams(v)
        self.put('function', tok_of(v['fkind'][1]), ('anon', len(names)))

    def n_DefStmt(self, v):
        ent, d = v
        attrs = ent.get('attrs', [])
        static = None
        glob = False
        for a in attrs:
            if isinstance(a, dict) and 'KeywordAttr' in a and a['KeywordAttr'][0] == 'Static':
                static = tok_of(a['KeywordAttr'][1])
            if isinstance(a, dict) and 'OtherAttribute' in a and a['OtherAttribute'][0][0] == 'global':
                glob = True
        if isinstance(d, dict) and 'FuncDef' in d:
            fd = d['FuncDef']
            names = self.fparams(fd)
            kind = 'stmt'
            n = len(names)
            if static is not None:
                kind = 'local'
            elif glob:
                kind = 'global'
            elif names and names[0][0] == 'self' and names[0][1] is not None and names[0][1][1] == ':':
                kind = 'method'
                n -= 1
            self.put('function', tok_of(fd['fkind'][1]), (kind, n))
        elif isinstance(d, dict) and 'VarDef' in d:
            name = ent['name']
            tk = None
            if 'EN' in name and 'Id' in name['EN']:
                tk = tok_of(name['EN']['Id'][0][1])
                nm = fix(name['EN']['Id'][0][0])
            if static is not None:
                self.local_groups[static[0]] += 1
                self.lines[static[0]] = static[2]
                vi = d['VarDef'].get('vinit')
                if vi:
                    self.local_valcount[static[0]] += 1
                if tk is not None:
                    self.put('local_name', tk, nm)

    def n_Return(self, v):
        tk, e, sc = v
        n = 0
        if e and 'some' in e:
            el = tuple_elems(e['some'])
            n = len(el) if el is not None else 1
        self.put('return', tok_of(tk), n)

    def n_For(self, v):
        tk, hdr, body = v
        tk = tok_of(tk)
        if isinstance(hdr, dict) and 'ForEach' in hdr:
            pat, _in, e = hdr['ForEach']
            if isinstance(e, dict) and 'Call' in e and isinstance(e['Call'][0], dict) and \
                    'IdSpecial' in e['Call'][0] and e['Call'][0]['IdSpecial'][0] == {'Op': 'Range'}:
                self.put('for', tk, ('num', len(e['Call'][1][1])))
            else:
                if 'PatTuple' in pat:
                    nv = len(pat['PatTuple'][1])
                else:
                    nv = 1
                self.put('for', tk, ('gen', nv))
                el = tuple_elems(e)
                self.put('for_iter', tk, len(el) if el is not None else 1)
        else:
            self.put('for', tk, ('other', 0))

    def n_While(self, v):
        self.put('while', tok_of(v[0]), 'while')

    def n_DoWhile(self, v):
        self.put('repeat', tok_of(v[0]), 'repeat')

    def n_If(self, v):
        tk, c, th, el = v
        tk = tok_of(tk)
        has_else = False
        if el and 'some' in el:
            e = el['some']
            if isinstance(e, dict) and 'Block' in e:
                has_else = True
                self.counts['else'] += 1
        if tk is None:
            self.counts['fake_if'] += 1
            return
        self.put(tk[1] if tk[1] in ('if', 'elseif') else 'if', tk, has_else)

    def n_Goto(self, v):
        self.put('goto', tok_of(v[0]), fix(v[1][0]))

    def n_Label(self, v):
        self.put('label', tok_of(v[0][1]), fix(v[0][0]))

    def n_Break(self, v):
        self.put('break', tok_of(v[0]), 'break')

    def n_Assign(self, v):
        l, tk, r = v
        le = tuple_elems(l)
        nl = len(le) if le is not None else 1
        re_ = tuple_elems(r)
        if re_ is not None:
            nr = len([x for x in re_ if not is_fake_nil(x)])
        else:
            nr = 0 if is_fake_nil(r) else 1
        self.put('assign', tok_of(tk), (nl, nr))

    def n_L(self, v):
        if not isinstance(v, dict):
            return
        for k, a in v.items():
            if k == 'String':
                lb, (val, tk), rb = a
                lbt = tok_of(lb)
                tkt = tok_of(tk)
                val = fix(val)
                if tkt is None:
                    self.counts['fake_string'] += 1
                    return
                if lbt is not None and tkt[1][:1] not in ('"', "'"):
                    self.nonname.add(tkt[0])
                if lbt is not None:
                    self.put('string', lbt, val)
                elif tkt[1][:1] in ('"', "'"):
                    self.put('string', tkt, val)
                elif LONG_OPEN_RE.match(tkt[1]):
                    # old parser: a long string is a single token [[...]]
                    self.put('string', tkt, val)
                else:
                    self.counts['named_key_string'] += 1
            elif k in ('Int', 'Float', 'Imag'):
                if k == 'Imag':
                    tk = tok_of(a[1]) if isinstance(a, list) else None
                    val = None
                else:
                    tk = tok_of(a[1])
                    val = a[0].get('some') if isinstance(a[0], dict) else None
                if tk is None:
                    self.counts['fake_' + k] += 1
                    return
                self.put('number', tk, tk[1])
                self.out['number_kind'][tk[0]] = (k, val)
            elif k in ('Null', 'Bool'):
                if k == 'Null':
                    tk = tok_of(a)
                    if tk is None:
                        self.counts['fake_nil'] += 1
                        return
                    self.put('nil', tk, 'nil')
                else:
                    tk = tok_of(a[1])
                    if tk is None:
                        self.counts['fake_bool'] += 1
                        return
                    b = a[0] in (True, 'true')
                    self.put('true' if b else 'false', tk, 'true' if b else 'false')

    def n_N(self, v):
        if isinstance(v, dict) and 'Id' in v:
            if v['Id'][0][0] == '...':
                self.put('vararg', tok_of(v['Id'][0][1]), '...')

    def n_ParamRest(self, v):
        tk = tok_of(v[0])
        if tk is not None and tk[1] == '...':
            self.put('vararg', tk, '...')

    def n_ParamEllipsis(self, v):
        tk = tok_of(v)
        if tk is not None:
            self.put('vararg', tk, '...')

    def finish(self):
        out = self.out
        for pos, n in self.local_groups.items():
            out['local'][pos] = n
            out['local_values'][pos] = (n, self.local_valcount[pos])
        for pos, (p, s, line) in self.alltoks.items():
            if NAME_RE.fullmatch(s) and s not in KEYWORDS and pos not in self.nonname:
                out['name'][pos] = s
        return out


def analyze_ast(js):
    ex = AstExtractor()
    ex.walk(js)
    out = ex.finish()
    return out, ex.counts, ex.lines


# ---------------------------------------------------------------------------
# number value expectations


def expected_number(text):
    """-> (kind, value) as Lua would read it, value None if not computed"""
    s = text
    low = s.lower()
    if low.endswith('i') and not low.startswith('0x'):
        return ('Imag', None)
    if low.endswith('ll'):
        digits = re.sub(r'[uU]?[lL][lL]$', '', s)
        try:
            return ('Int', int(digits, 0) if not digits.lower().startswith('0b') else int(digits[2:], 2))
        except ValueError:
            return ('Int', None)
    if low.startswith('0x'):
        if '.' in low or 'p' in low:
            try:
                return ('Float', float.fromhex(s))
            except ValueError:
                return ('Float', None)
        return ('Int', int(s, 16))
    if low.startswith('0b'):
        return ('Int', int(s[2:], 2))
    if '.' in low or 'e' in low:
        return ('Float', float(s))
    return ('Int', int(s, 10))


CATS = ['name', 'string', 'number', 'nil', 'true', 'false', 'vararg', 'function',
        'local', 'local_name', 'local_values', 'call', 'method_call', 'dot', 'index',
        'table', 'op', 'assign', 'return', 'for', 'for_iter', 'while', 'repeat', 'if',
        'elseif', 'goto', 'label', 'break']


def compare(lexout, astout, lexlines, astlines, maxex=8):
    res = {}
    for cat in CATS:
        e = lexout.get(cat, {})
        a = astout.get(cat, {})
        matched = missing = extra = mism = 0
        exs = []
        for k, v in e.items():
            if k in a:
                if a[k] == v:
                    matched += 1
                else:
                    mism += 1
                    if len(exs) < maxex:
                        exs.append(('mismatch', lexlines.get(k), k, repr(v)[:80], repr(a[k])[:80]))
            else:
                missing += 1
                if len(exs) < maxex:
                    exs.append(('missing', lexlines.get(k), k, repr(v)[:80], None))
        for k, v in a.items():
            if k not in e:
                extra += 1
                if len(exs) < maxex:
                    exs.append(('extra', astlines.get(k), k, None, repr(v)[:80]))
        res[cat] = {'expected': len(e), 'actual': len(a), 'matched': matched,
                    'missing': missing, 'extra': extra, 'mismatch': mism, 'ex': exs}
    # number kind/value
    nk = astout.get('number_kind', {})
    bad = 0
    exs = []
    total = 0
    for pos, text in lexout.get('number', {}).items():
        if pos not in nk:
            continue
        total += 1
        try:
            ek, ev = expected_number(text)
        except Exception:
            continue
        ak, av = nk[pos]
        ok = (ek == ak)
        if ok and ev is not None and ak != 'Imag':
            if ek == 'Int':
                ok = (av == ev) or (av is None and ev >= 2 ** 62) or (ev >= 2 ** 62)
            else:
                ok = (av is not None and (av == ev or abs(av - ev) <= 1e-9 * abs(ev))) or \
                    (av is None and ev in (float('inf'),)) or \
                    (av == 0 and abs(ev) >= 1e15)  # the JSON dumper prints big floats as 0
        if not ok:
            bad += 1
            if len(exs) < maxex:
                exs.append(('mismatch', lexlines.get(pos), pos, repr((text, ek, ev))[:80], repr((ak, av))[:80]))
    res['number_value'] = {'expected': total, 'actual': total, 'matched': total - bad,
                           'missing': 0, 'extra': 0, 'mismatch': bad, 'ex': exs}
    return res


def parse_json_stdout(b):
    s = b.decode('utf-8', 'surrogateescape')
    i = s.find('{')
    if i < 0:
        return None
    return json.loads(s[i:])


if __name__ == '__main__':
    # debug: lexcheck.py SRC JSON
    sys.setrecursionlimit(1000000)
    raw = open(sys.argv[1], 'rb').read()
    lo, lc, errs, ll = analyze_source(raw)
    js = parse_json_stdout(open(sys.argv[2], 'rb').read())
    ao, ac, al = analyze_ast(js)
    r = compare(lo, ao, ll, al)
    for c, v in r.items():
        if v['expected'] or v['actual']:
            print('%-14s exp=%d act=%d ok=%d miss=%d extra=%d mism=%d' % (
                c, v['expected'], v['actual'], v['matched'], v['missing'], v['extra'], v['mismatch']))
            for x in v['ex']:
                print('     ', x)
    print('lex errors', errs, 'lexcounts', dict(lc), 'astcounts', dict(ac))
