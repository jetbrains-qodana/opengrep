#!/usr/bin/env python3
"""Source token coverage oracle for the Lua -> AST_generic translation.

For each Lua file:
  1. tokenize the source with an independent Lua lexer (comments, short/long
     strings, numbers incl. LuaJIT suffixes, shebang), and give every token a
     syntactic *context* (e.g. '=' of an assignment vs '=' of a table field,
     '(' of a call vs '(' of a parenthesized expression) with a light
     bracket/block-stack analysis;
  2. run opengrep-core -full_token_info -dump_ast -json (new or old parser)
     and collect every OriginTok (bytepos, str);
  3. check that every source token is represented in the AST at the same byte
     offset with the same text, and conversely that every AST token is the
     text of a source token at that offset (garbled / misplaced) and how often
     it occurs (duplicates).

Usage: luacov.py {new|old} FILELIST OUT.jsonl [--start N --end M]
       luacov.py one {new|old} FILE      (debug: print uncovered tokens)
"""
import json, re, subprocess, sys, os, collections

BIN = {"new": "/src/bin/opengrep-core", "old": "/host-tmp/opengrep-core-old"}

KEYWORDS = {"and", "break", "do", "else", "elseif", "end", "false", "for",
            "function", "goto", "if", "in", "local", "nil", "not", "or",
            "repeat", "return", "then", "true", "until", "while"}
LITERAL_KW = {"nil", "true", "false"}
WORD_OPS = {"and", "or", "not"}
SYM_OPS = {"+", "-", "*", "/", "//", "%", "^", "#", "&", "~", "|", "<<", ">>",
           "..", "==", "~=", "<", "<=", ">", ">="}
SYMBOLS = ["...", "..", "==", "~=", "<=", ">=", "<<", ">>", "//", "::",
           "+", "-", "*", "/", "%", "^", "#", "&", "~", "|", "<", ">", "=",
           "(", ")", "{", "}", "[", "]", ";", ":", ",", "."]

RE_WS = re.compile(rb"[ \t\r\n\f\v]+")
RE_NAME = re.compile(rb"[A-Za-z_\x80-\xff][A-Za-z0-9_\x80-\xff]*")
RE_HEX = re.compile(rb"0[xX](?:[0-9a-fA-F]*\.?[0-9a-fA-F]*)(?:[pP][+-]?[0-9]+)?(?:[uU]?[lL][lL]|[iI])?")
RE_DEC = re.compile(rb"(?:[0-9]+\.?[0-9]*|\.[0-9]+)(?:[eE][+-]?[0-9]+)?(?:[uU]?[lL][lL]|[iI])?")
RE_LONGOPEN = re.compile(rb"\[(=*)\[")
RE_SYM = re.compile(b"|".join(re.escape(s.encode()) for s in SYMBOLS))


class Tok:
    __slots__ = ("pos", "text", "kind", "ctx", "line", "parts", "prefix_end")

    def __init__(self, pos, text, kind, line):
        self.pos, self.text, self.kind, self.line = pos, text, kind, line
        self.ctx = None
        self.parts = None  # long string: (open, content, close) positions
        self.prefix_end = False


def lex(src):
    toks, i, n, line = [], 0, len(src), 1
    lexerr = 0
    if src.startswith(b"#"):  # shebang / first line comment
        j = src.find(b"\n")
        i = n if j < 0 else j
    while i < n:
        m = RE_WS.match(src, i)
        if m:
            line += src.count(b"\n", i, m.end()); i = m.end(); continue
        c = src[i:i + 1]
        if src.startswith(b"--", i):
            m = RE_LONGOPEN.match(src, i + 2)
            if m:
                close = b"]" + m.group(1) + b"]"
                j = src.find(close, m.end())
                j = n if j < 0 else j + len(close)
            else:
                j = src.find(b"\n", i)
                j = n if j < 0 else j
            line += src.count(b"\n", i, j); i = j; continue
        if c == b"[":
            m = RE_LONGOPEN.match(src, i)
            if m:
                close = b"]" + m.group(1) + b"]"
                j = src.find(close, m.end())
                if j < 0:
                    lexerr += 1; j = n - len(close)
                t = Tok(i, src[i:j + len(close)], "string", line)
                t.parts = (i, m.group(0), m.end(), src[m.end():j], j, close)
                toks.append(t)
                line += src.count(b"\n", i, j + len(close)); i = j + len(close); continue
        if c in (b'"', b"'"):
            j = i + 1
            while j < n:
                d = src[j:j + 1]
                if d == b"\\":
                    if src[j + 1:j + 2] == b"z":
                        m = RE_WS.match(src, j + 2)
                        j = m.end() if m else j + 2
                        continue
                    j += 2; continue
                if d == c:
                    j += 1; break
                if d == b"\n":
                    lexerr += 1; break
                j += 1
            toks.append(Tok(i, src[i:j], "string", line))
            line += src.count(b"\n", i, j); i = j; continue
        if c.isdigit() or (c == b"." and src[i + 1:i + 2].isdigit()):
            m = RE_HEX.match(src, i) if src[i:i + 2].lower() == b"0x" else RE_DEC.match(src, i)
            toks.append(Tok(i, m.group(0), "number", line)); i = m.end(); continue
        m = RE_NAME.match(src, i)
        if m:
            w = m.group(0)
            k = "keyword" if w.decode("latin-1") in KEYWORDS else "ident"
            toks.append(Tok(i, w, k, line)); i = m.end(); continue
        m = RE_SYM.match(src, i)
        if m:
            toks.append(Tok(i, m.group(0), "sym", line)); i = m.end(); continue
        toks.append(Tok(i, c, "other", line)); lexerr += 1; i += 1
    return toks, lexerr


# ---------------------------------------------------------------------------
# context classification
# ---------------------------------------------------------------------------
NOT_STMT_START_PREV = {"=", ",", "(", "[", "{", ".", ":", "local", "return",
                       "in", "if", "elseif", "while", "until", "for",
                       "function", "goto", "and", "or", "not"} | SYM_OPS


def classify(toks):
    """Give every token a ctx string. ctx naming: <class>:<detail>."""
    stack = []        # entries: [kind, extra]  kind in ( [ { function if do repeat
    func_pending = False   # after 'function': next '(' is the parameter list
    attlist = False        # inside 'local'/'global' name list
    for_state = None       # None | 'name' | 'eq' | 'range' (waiting for 1st comma)
    for_depth = 0
    ret_depth = None
    n = len(toks)
    for idx, t in enumerate(toks):
        s = t.text.decode("latin-1")
        prev = toks[idx - 1] if idx else None
        prevs = prev.text.decode("latin-1") if prev else None
        nxt = toks[idx + 1].text.decode("latin-1") if idx + 1 < n else None
        prev_pe = prev.prefix_end if prev else False
        if attlist:
            pk = prev.ctx if prev else None
            ok = False
            if t.kind == "ident":
                ok = prevs in (",", "<") or pk in ("keyword:local", "keyword:global", "punct:>-attrib")
            elif s in (",", "<"):
                ok = prev.kind == "ident" or (s == "<" and pk in ("keyword:local", "keyword:global")) or pk == "punct:>-attrib"
            elif s == ">":
                ok = prev.kind == "ident" and idx >= 2 and toks[idx - 2].ctx == "punct:<-attrib"
            elif s in ("=", "*"):
                ok = True
            if not ok:
                attlist = False
        if t.kind == "ident":
            if (s == "global" and (prevs is None or prevs not in NOT_STMT_START_PREV)
                    and nxt is not None and (toks[idx + 1].kind == "ident" or nxt in ("function", "<", "*"))):
                t.ctx = "keyword:global"
                if nxt != "function":
                    attlist = True
                continue
            t.ctx = "ident"
            t.prefix_end = True
            if for_state == "name":
                for_state = "eq" if nxt == "=" else None
            continue
        if t.kind == "number":
            t.ctx = "number"; continue
        if t.kind == "string":
            t.ctx = "string:long" if t.parts else "string:short"
            t.prefix_end = prev_pe  # f"x" is a call, a prefix expression
            continue
        if t.kind == "other":
            t.ctx = "other"; continue
        if t.kind == "keyword":
            if s in LITERAL_KW:
                t.ctx = "literal:" + s
            elif s in WORD_OPS:
                t.ctx = "op:" + s
            else:
                t.ctx = "keyword:" + s
            if s == "function":
                func_pending = True
                stack.append(["function", None])
            elif s == "if":
                stack.append(["if", None])
            elif s == "do":
                stack.append(["do", None])
            elif s == "repeat":
                stack.append(["repeat", None])
            elif s == "end":
                kind = None
                while stack:
                    k = stack.pop()[0]
                    if k in ("function", "if", "do"):
                        kind = k; break
                t.ctx = "keyword:end(%s)" % (kind or "?")
            elif s == "until":
                while stack:
                    if stack.pop()[0] == "repeat":
                        break
            elif s == "local":
                if nxt != "function":
                    attlist = True
            elif s == "for":
                for_state = "name"; for_depth = len(stack)
            elif s == "return":
                ret_depth = len(stack)
            if s in ("end", "else", "elseif", "until"):
                ret_depth = None
            continue
        # symbols
        if s in ("(", "[", "{"):
            if s == "(":
                if func_pending:
                    ctx = "punct:(params"; func_pending = False
                elif prev_pe:
                    ctx = "punct:(call"
                else:
                    ctx = "punct:(paren"
            elif s == "[":
                ctx = "punct:[index" if prev_pe else "punct:[fieldkey"
            else:
                ctx = "punct:{table-args" if prev_pe else "punct:{table"
            t.ctx = ctx
            stack.append([s, ctx])
            attlist = False if s != "(" else attlist
            continue
        if s in (")", "]", "}"):
            opener = {")": "(", "]": "[", "}": "{"}[s]
            ctx = None
            while stack:
                k, c = stack.pop()
                if k == opener:
                    ctx = c; break
            if ctx is None:
                t.ctx = "punct:%s?" % s
            else:
                t.ctx = "punct:" + s + ctx[7:]
                if s == ")":
                    t.prefix_end = ctx != "punct:(params"
                elif s == "]":
                    t.prefix_end = ctx == "punct:[index"
                else:
                    t.prefix_end = ctx == "punct:{table-args"
            continue
        top = stack[-1][1] if stack else None
        if s == "=":
            if attlist:
                t.ctx = "punct:=decl"; attlist = False
            elif for_state == "eq" and len(stack) == for_depth:
                t.ctx = "punct:=for"; for_state = "range"
            elif top in ("punct:{table", "punct:{table-args"):
                t.ctx = "punct:=field"
            else:
                t.ctx = "punct:=assign"
            continue
        if s == ",":
            if for_state == "range" and len(stack) == for_depth:
                t.ctx = "punct:,for-range"; for_state = None
            elif top in ("punct:{table", "punct:{table-args"):
                t.ctx = "punct:,field-sep"
            else:
                t.ctx = "punct:,"
            continue
        if s == ";":
            if ret_depth is not None and len(stack) == ret_depth:
                t.ctx = "punct:;return"; ret_depth = None
            elif top in ("punct:{table", "punct:{table-args"):
                t.ctx = "punct:;field-sep"
            else:
                t.ctx = "punct:;"
            attlist = False
            continue
        if attlist and s in ("<", ">"):
            t.ctx = "punct:%s-attrib" % s; continue
        if attlist and s == "*":
            t.ctx = "punct:*global"; continue
        if s in SYM_OPS:
            t.ctx = "op:" + s
            continue
        if func_pending and s in (".", ":"):
            t.ctx = "punct:%sfuncname" % s; continue
        if s in (".", ":", "::", "..."):
            t.ctx = "punct:" + s
            if s == "...":
                t.ctx = "vararg"
            continue
        t.ctx = "punct:" + s
    return toks


# contexts whose token is dropped by design by the NEW converter
# (Parse_lua_tree_sitter.ml): see REPORT.md.
DROPPED_BY_DESIGN = {
    "keyword:then", "keyword:else", "keyword:end(if)", "punct:::",
    "punct:=decl", "punct:=field", "punct:.funcname", "punct:,", "punct:,field-sep",
    "punct:;", "punct:;field-sep", "punct:<-attrib", "punct:>-attrib",
    "punct:(paren", "punct:)paren", "punct:[fieldkey", "punct:]fieldkey",
}


def category(ctx):
    if ctx.startswith("keyword:end"):
        return "keyword"
    return ctx.split(":")[0]


# ---------------------------------------------------------------------------
# AST tokens
# ---------------------------------------------------------------------------
RE_ORIG = re.compile(r'"OriginTok":\{"str":("(?:[^"\\]|\\.)*"),"bytepos":(\d+)')


def ast_tokens(out):
    txt = out.decode("utf-8", "surrogateescape")
    res = collections.Counter()
    for m in RE_ORIG.finditer(txt):
        s = json.loads(m.group(1)).encode("utf-8", "surrogateescape")
        res[(int(m.group(2)), s)] += 1
    return res, ("{" in txt)


def run_parser(which, path):
    cmd = ["docker", "exec", "ogb", "timeout", "20", BIN[which], "-lang", "lua",
           "-full_token_info", "-dump_ast", "-json", path]
    p = subprocess.run(cmd, stdout=subprocess.PIPE, stderr=subprocess.PIPE)
    err = p.stderr.decode("utf-8", "replace")
    status = "ok"
    if p.returncode == 124:
        status = "timeout"
    elif p.returncode != 0 or "fail to fully parse" in err:
        status = "parse_error"
    return p.stdout, status, p.returncode


def analyze(src, ast):
    toks, lexerr = lex(src)
    classify(toks)
    starts = {}
    for t in toks:
        starts[t.pos] = t
        if t.parts:  # long string inner tokens of the new grammar
            o, ot, cp, ct, clp, clt = t.parts
            starts.setdefault(cp, ("lsinner", ct))
            starts.setdefault(clp, ("lsinner", clt))
    cov = collections.Counter(); exp = collections.Counter()
    uncovered = []
    for t in toks:
        exp[t.ctx] += 1
        ok = ast.get((t.pos, t.text), 0) > 0
        if not ok and t.parts:
            o, ot, cp, ct, clp, clt = t.parts
            ok = ast.get((o, ot), 0) > 0
        if ok:
            cov[t.ctx] += 1
        else:
            uncovered.append(t)
    # reverse direction
    bad, dups = [], collections.Counter()
    for (pos, s), k in ast.items():
        st = starts.get(pos)
        if isinstance(st, tuple):
            ok = (s == st[1]) or (st[1] == b"" and s == b"")
            ctx = "string:long-inner"
        elif st is None:
            ok, ctx = False, None
        else:
            ctx = st.ctx
            ok = s == st.text or (st.parts and s == st.parts[1])
        if not ok:
            bad.append((pos, s, src[pos:pos + max(len(s), 1)], ctx))
        elif k > 1:
            dups[ctx if ctx else s.decode("latin-1")] += k - 1
    return toks, lexerr, exp, cov, uncovered, bad, dups


def line_of(src, pos):
    return src.count(b"\n", 0, pos) + 1


def process(which, path):
    hostp = path.replace("/host-tmp/", "/tmp/", 1)
    with open(hostp, "rb") as f:
        src = f.read()
    out, status, rc = run_parser(which, path)
    ast, has_json = ast_tokens(out)
    if not has_json and status != "timeout":
        raise RuntimeError("no JSON output, rc=%d" % rc)
    toks, lexerr, exp, cov, uncovered, bad, dups = analyze(src, ast)
    unc = [{"pos": t.pos, "line": t.line, "ctx": t.ctx,
            "text": t.text[:60].decode("utf-8", "replace")}
           for t in uncovered if t.ctx not in DROPPED_BY_DESIGN or which == "old"]
    sig_lines = sorted({t.line for t in uncovered
                        if category(t.ctx) in ("ident", "number", "string", "op", "literal", "vararg")})
    return {
        "file": path, "project": path.split("/")[3], "status": status, "rc": rc,
        "json": has_json, "lexerr": lexerr, "ntok": len(toks),
        "ast_tokens": sum(ast.values()),
        "expected": dict(exp), "covered": dict(cov),
        "uncovered": unc[:300], "n_uncovered_listed": len(unc),
        "uncovered_sig_lines": sig_lines[:500], "n_uncovered_sig_lines": len(sig_lines),
        "bad_ast_tokens": [{"pos": p, "line": line_of(src, p), "ast": s[:60].decode("utf-8", "replace"),
                            "src": sx[:60].decode("utf-8", "replace"), "ctx": c} for p, s, sx, c in bad[:100]],
        "n_bad": len(bad), "dups": dict(dups),
    }


def main():
    if sys.argv[1] == "one":
        which, path = sys.argv[2], sys.argv[3]
        r = process(which, path)
        print(json.dumps({k: v for k, v in r.items()}, indent=1, ensure_ascii=False))
        return
    which, flist, outp = sys.argv[1], sys.argv[2], sys.argv[3]
    files = [l.rstrip("\n") for l in open(flist, encoding="utf-8") if l.strip()]
    done = set()
    if os.path.exists(outp):
        for l in open(outp, encoding="utf-8"):
            try:
                r0 = json.loads(l)
                if r0.get("status") != "tool_error" and (r0.get("json") or r0.get("status") == "timeout"):
                    done.add(r0["file"])
            except Exception:
                pass
    with open(outp, "a", encoding="utf-8") as o:
        for i, p in enumerate(files):
            if p in done:
                continue
            import time
            for attempt in range(5):
                try:
                    r = process(which, p); break
                except Exception as e:
                    r = {"file": p, "project": p.split("/")[3], "status": "tool_error", "error": repr(e)}
                    time.sleep(3)
            if False:
                r = {"file": p, "project": p.split("/")[3], "status": "tool_error", "error": repr(e)}
            o.write(json.dumps(r, ensure_ascii=False) + "\n"); o.flush()
            if i % 200 == 0:
                print(which, i, file=sys.stderr, flush=True)


if __name__ == "__main__":
    main()
