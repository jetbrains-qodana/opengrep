"""Small Lua lexer (5.1-5.5 + LuaJIT numbers) working on bytes.
Returns a list of tokens (start, end, kind, text) with byte offsets.
kind in: name, kw, num, str, lstr, op. Comments, whitespace and the
shebang line are skipped."""
import re

KEYWORDS = {
    "and", "break", "do", "else", "elseif", "end", "false", "for",
    "function", "goto", "if", "in", "local", "nil", "not", "or", "repeat",
    "return", "then", "true", "until", "while",
}
OPS3 = [b"..."]
OPS2 = [b"..", b"==", b"~=", b"<=", b">=", b"<<", b">>", b"//", b"::"]
OPS1 = set(b"+-*/%^#&~|<>=(){}[];:,.")

_long_open = re.compile(rb"\[(=*)\[")
_ws = set(b" \t\r\n\f\v")


def _name_char(c):
    return (48 <= c <= 57) or (65 <= c <= 90) or (97 <= c <= 122) or c == 95 or c >= 0x80


def _name_start(c):
    return (65 <= c <= 90) or (97 <= c <= 122) or c == 95 or c >= 0x80


def lex(src: bytes):
    toks = []
    n = len(src)
    i = 0
    if src.startswith(b"#"):
        j = src.find(b"\n")
        i = n if j < 0 else j
    while i < n:
        c = src[i]
        if c in _ws:
            i += 1
            continue
        if src.startswith(b"--", i):
            m = _long_open.match(src, i + 2)
            if m:
                close = b"]" + m.group(1) + b"]"
                j = src.find(close, m.end())
                i = n if j < 0 else j + len(close)
            else:
                j = src.find(b"\n", i)
                i = n if j < 0 else j
            continue
        if c == 0x5B:  # [
            m = _long_open.match(src, i)
            if m:
                close = b"]" + m.group(1) + b"]"
                j = src.find(close, m.end())
                e = n if j < 0 else j + len(close)
                toks.append((i, e, "lstr", src[i:e]))
                i = e
                continue
        if c in (0x22, 0x27):  # " '
            j = i + 1
            while j < n and src[j] != c:
                if src[j] == 0x5C:
                    if j + 1 < n and src[j + 1] == 0x7A:  # \z skips whitespace
                        j += 2
                        while j < n and src[j] in _ws:
                            j += 1
                        continue
                    if src.startswith(b"\r\n", j + 1):
                        j += 3
                        continue
                    j += 2
                    continue
                if src[j] == 0x0A:  # unfinished string
                    break
                j += 1
            e = min(j + 1, n)
            toks.append((i, e, "str", src[i:e]))
            i = e
            continue
        if (48 <= c <= 57) or (c == 0x2E and i + 1 < n and 48 <= src[i + 1] <= 57):
            j = i
            hexa = src[i:i + 2].lower() == b"0x"
            if hexa:
                j += 2
            while j < n:
                d = src[j]
                if _name_char(d) and d < 0x80 or d == 0x2E:
                    if d == 0x2E and src.startswith(b"..", j):
                        break
                    j += 1
                    if j < n and src[j] in (0x2B, 0x2D) and src[j - 1] in (
                            (b"pP" if hexa else b"eE")):
                        j += 1
                else:
                    break
            toks.append((i, j, "num", src[i:j]))
            i = j
            continue
        if _name_start(c):
            j = i + 1
            while j < n and _name_char(src[j]):
                j += 1
            w = src[i:j]
            try:
                ws = w.decode()
            except UnicodeDecodeError:
                ws = None
            toks.append((i, j, "kw" if ws in KEYWORDS else "name", w))
            i = j
            continue
        for op in OPS3 + OPS2:
            if src.startswith(op, i):
                toks.append((i, i + len(op), "op", op))
                i += len(op)
                break
        else:
            toks.append((i, i + 1, "op" if c in OPS1 else "other", src[i:i + 1]))
            i += 1
    return toks


def match_blocks(toks):
    """Match block openers with their closers.
    Returns list of constructs (kind, open_idx, close_idx or None, parent_construct_index)
    where kind in function/if/while/for/do/repeat; the 'do' of while/for is
    recorded as 'do_idx' in the loop construct."""
    cons = []  # dicts
    stack = []
    for k, t in enumerate(toks):
        if t[2] != "kw":
            continue
        w = t[3]
        if w in (b"function", b"if", b"while", b"for", b"repeat"):
            c = {"kind": w.decode(), "open": k, "close": None,
                 "parent": stack[-1] if stack else None, "do": None}
            cons.append(c)
            stack.append(len(cons) - 1)
        elif w == b"do":
            if stack and cons[stack[-1]]["kind"] in ("while", "for") and cons[stack[-1]]["do"] is None:
                cons[stack[-1]]["do"] = k
            else:
                cons.append({"kind": "do", "open": k, "close": None,
                             "parent": stack[-1] if stack else None, "do": None})
                stack.append(len(cons) - 1)
        elif w == b"end":
            if stack and cons[stack[-1]]["kind"] != "repeat":
                cons[stack.pop()]["close"] = k
            elif stack:
                # unbalanced: an 'end' inside repeat; error
                pass
        elif w == b"until":
            if stack and cons[stack[-1]]["kind"] == "repeat":
                cons[stack.pop()]["close"] = k
    return cons, len(stack)
