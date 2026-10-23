"""Extract facts [cat, bytepos, desc] from an opengrep AST_generic JSON dump
(-full_token_info -dump_ast -json), in the same vocabulary as extract_ref.js."""
import json, re, sys

OPS2 = {"Or": "or", "And": "and", "Lt": "<", "LtE": "<=", "Eq": "==", "NotEq": "~=",
        "GtE": ">=", "Gt": ">", "BitOr": "|", "BitXor": "~", "BitAnd": "&", "LSL": "<<",
        "LSR": ">>", "Plus": "+", "Minus": "-", "Mult": "*", "Div": "/", "FloorDiv": "//",
        "Mod": "%", "Concat": "..", "Pow": "^"}
OPS1 = {"Not": "not", "Length": "#", "Minus": "-", "BitNot": "~"}


def tok(t):
    """(bytepos, str) of a real token, else None"""
    if isinstance(t, dict) and "token" in t:
        o = t["token"]
        if isinstance(o, dict) and "OriginTok" in o:
            return o["OriginTok"]["bytepos"], o["OriginTok"]["str"]
    return None


def toks(n):
    st = [n]
    while st:
        x = st.pop()
        if isinstance(x, dict):
            t = tok(x)
            if t:
                yield t
                continue
            st.extend(x.values())
        elif isinstance(x, list):
            st.extend(x)


def span(n):
    lo, hi = None, None
    for p, s in toks(n):
        e = p + len(s.encode("latin1", "replace")) if False else p + len(s.encode("utf-8"))
        lo = p if lo is None or p < lo else lo
        hi = e if hi is None or e > hi else hi
    return lo, hi


def ctor(n):
    if isinstance(n, dict) and len(n) == 1:
        k = next(iter(n))
        return k, n[k]
    return None, None


def some(x):
    if isinstance(x, dict) and "some" in x:
        return x["some"]
    return None


class Extractor:
    def __init__(self):
        self.facts = []
        self.notes = []  # by-design / value checks
        self.fn = [-1]

    def F(self, c, p, d=""):
        if p is not None:
            if c in SCOPED:
                self.facts.append(["scope:" + c, p, self.fn[-1]])
            self.facts.append([c, p, d])

    def is_ident(self, x):
        if isinstance(x, list) and len(x) == 2 and isinstance(x[0], str):
            t = tok(x[1])
            return t is not None and t[1] == x[0]
        return False

    def params(self, fparams):
        lp, ps, rp = fparams
        n = 0
        for p in ps:
            k, v = ctor(p)
            if k == "Param":
                pn = some(v["pname"])
                t = tok(pn[1]) if pn else None
                if t and t[1] == ":" and pn[0] == "self":
                    self.notes.append(["self_inserted", t[0]])
                    continue
                n += 1
                if t:
                    self.F("param", t[0], pn[0])
                    if pn[0] != "...":
                        self.F("ident", t[0], pn[0])
            elif k == "ParamRest":
                n += 1
                t = tok(v[0])
                pn = some(v[1]["pname"])
                if t:
                    self.F("param", t[0], "...")
                    self.F("vararg", t[0])
                if pn and pn[0] != "...":  # ...args
                    tt = tok(pn[1])
                    if tt:
                        self.F("ident", tt[0], pn[0])
            else:
                n += 1
                self.visit(p)
        return n

    def funcdef(self, fd):
        n = self.params(fd["fparams"])
        t = tok(fd["fkind"][1])
        self.fn.append(t[0] if t else None)
        self.visit(fd["fbody"])
        self.fn.pop()
        return n

    def name_parts(self, name):
        """returns (list of (str,pos)), last pos"""
        k, v = ctor(name)
        if k == "Id":
            t = tok(v[0][1])
            return [(v[0][0], t[0] if t else None)]
        if k == "IdQualified":
            parts = []
            mid = some(v["name_middle"])
            if mid:
                mk, mv = ctor(mid)
                if mk == "QDots":
                    for (s, t), _ in mv:
                        tt = tok(t)
                        parts.append((s, tt[0] if tt else None))
                else:
                    self.visit(mid)
            (s, t), _ = v["name_last"]
            tt = tok(t)
            parts.append((s, tt[0] if tt else None))
            return parts
        self.visit(name)
        return []

    def number(self, kind, v):
        t = tok(v[1])
        if not t:
            return
        self.F("number", t[0], t[1])
        raw = t[1]
        self.F("number_kind", t[0], kind)
        # value check
        exp = lua_number_value(raw)
        if kind in ("Int", "Float"):
            got = some(v[0]) if isinstance(v[0], dict) else v[0]
            if exp is not None and got is not None and not same_num(exp, got):
                self.notes.append(["number_value_mismatch", t[0], raw, got])

    def string(self, v):
        l, (s, t), r = v
        tt = tok(t)
        lo, _ = span(v)
        if tt is None:
            return
        if tok(l) is None and tt[1] == s and not tt[1][:1] in "\"'":
            # a name used as a string (table key x = v)
            self.F("ident", tt[0], s)
            return
        self.F("string", lo, s)

    def visit(self, n):
        if isinstance(n, list):
            if self.is_ident(n):
                s = n[0]
                p = tok(n[1])[0]
                if s == "...":
                    self.F("vararg", p)
                else:
                    self.F("ident", p, s)
                return
            for x in n:
                self.visit(x)
            return
        if not isinstance(n, dict):
            return
        k, v = ctor(n)
        if k is None:
            for x in n.values():
                self.visit(x)
            return
        h = getattr(self, "h_" + k, None)
        if h:
            h(v)
        else:
            self.visit(v)

    # ---- handlers
    def h_DefStmt(self, v):
        ent, d = v
        dk, dv = ctor(d)
        attrs = ent.get("attrs", [])
        is_local = any(ctor(a)[0] == "KeywordAttr" and a["KeywordAttr"][0] == "Static" for a in attrs)
        name = ent["name"]
        nk, nv = ctor(name)
        if dk == "FuncDef":
            parts = self.name_parts(nv)
            for s, p in parts:
                self.F("ident", p, s)
            fps = dv["fparams"][1]
            is_method = any(ctor(p)[0] == "Param" and some(p["Param"]["pname"]) and
                            some(p["Param"]["pname"])[0] == "self" and
                            (tok(some(p["Param"]["pname"])[1]) or (0, ""))[1] == ":" for p in fps)
            n = self.funcdef(dv)
            if parts:
                if is_local:
                    kind, qn = "func_local", parts[-1][0]
                elif is_method:
                    kind = "func_method"
                    qn = ".".join(s for s, _ in parts[:-1]) + ":" + parts[-1][0]
                else:
                    kind, qn = "func_global", ".".join(s for s, _ in parts)
                self.F(kind, parts[-1][1], "%s/%d" % (qn, n))
            self.visit(attrs)
        elif dk == "VarDef":
            parts = self.name_parts(nv)
            init = some(dv["vinit"])
            for s, p in parts:
                self.F("ident", p, s)
                self.F("local_var" if is_local else "global_var", p, s + ("=" if init is not None else ""))
            if init is not None:
                self.visit(init)
            self.visit(attrs)
        else:
            self.visit(v)

    def h_Lambda(self, fd):
        t = tok(fd["fkind"][1])
        n = self.funcdef(fd)
        self.F("func_anon", t[0] if t else None, n)

    def h_Call(self, v):
        fn, args = v
        fk, fv = ctor(fn)
        al = args[1]
        if fk == "IdSpecial":
            op, t = fv
            ok, ov = ctor(op)
            tt = tok(t)
            if ok == "Op":
                if ov == "Range":
                    pass
                elif tt and ov == "Not" and tt[1] == "until":
                    self.notes.append(["repeat_not", tt[0]])
                elif len(al) == 2 and ov in OPS2:
                    self.F("binop", tt[0] if tt else None, OPS2[ov])
                    self.F("binop_shape", tt[0] if tt else None, "%s,%s" % (span(al[0])[0], span(al[1])[0]))
                elif len(al) == 1 and ov in OPS1:
                    self.F("unop", tt[0] if tt else None, OPS1[ov])
                    self.F("unop_shape", tt[0] if tt else None, span(al[0])[0])
                else:
                    self.F("other_op", tt[0] if tt else None, ov)
                self.visit(al)
                return
        lo, hi = span(n_call := {"Call": v})
        if fk == "N" and ctor(fv)[0] == "IdQualified" and ctor(some(fv["IdQualified"]["name_middle"]) or {})[0] == "QExpr":
            q = fv["IdQualified"]
            (s, t), _ = q["name_last"]
            self.F("method_call", hi, "%s/%d" % (s, len(al)))
            callee = span(some(q["name_middle"])["QExpr"][0])[0]
        else:
            self.F("call", hi, len(al))
            callee = span(fn)[0]
        self.F("call_shape", hi, "%s;%s" % (callee, ",".join(str(span(a)[0]) for a in al)))
        self.visit(fn)
        self.visit(al)

    def h_Container(self, v):
        kind, (lb, items, rb) = v
        if kind == "Dict":
            t = tok(lb)
            self.F("table", t[0] if t else None, len(items))
            for it in items:
                ik, iv = ctor(it)
                if ik == "Container" and iv[0] == "Tuple" and len(iv[1][1]) == 2:
                    key, val = iv[1][1]
                    kk, kv = ctor(key)
                    lk, lv = ctor(kv) if kk == "L" else (None, None)
                    if lk == "Int" and tok(lv[1]) is None:
                        self.F("field_pos", span(val)[0])
                        self.visit(val)
                        continue
                    if lk == "String" and tok(lv[0]) is None:
                        tt = tok(lv[1][1])
                        if tt and tt[1] == lv[1][0]:
                            self.F("field_named", tt[0], tt[1])
                            self.F("ident", tt[0], tt[1])
                            self.visit(val)
                            continue
                    self.F("field_bracket", span(key)[0])
                    self.visit(key)
                    self.visit(val)
                else:
                    self.F("field_other", span(it)[0])
                    self.visit(it)
            return
        self.visit(items)

    def h_Assign(self, v):
        lhs, t, rhs = v
        def items(e):
            k, x = ctor(e)
            if k == "Container" and x[0] == "Tuple" and tok(x[1][0]) is None:
                return x[1][1]
            return [e]
        ts = items(lhs)
        vs = items(rhs)
        real = [e for e in vs if not is_fake_nil(e)]
        if len(real) != len(vs):
            self.notes.append(["fake_nil", span(lhs)[0], len(vs) - len(real)])
        self.F("assign", span(lhs)[0], "%d=%d" % (len(ts), len(real)))
        self.visit(lhs)
        self.visit(rhs)

    def h_Return(self, v):
        t, e, _ = v
        tt = tok(t)
        e = some(e)
        if e is None:
            n = 0
        else:
            k, x = ctor(e)
            n = len(x[1][1]) if k == "Container" and x[0] == "Tuple" and tok(x[1][0]) is None else 1
        self.F("return", tt[0] if tt else None, n)
        if e is not None:
            self.visit(e)

    def h_If(self, v):
        t, cond, then_, else_ = v
        tt = tok(t)
        if tt and tt[1] == "if":
            ne, he, cur = 0, False, some(else_)
            while cur is not None:
                ck, cv = ctor(cur)
                if ck == "If" and (tok(cv[0]) or (0, ""))[1] == "elseif":
                    ne += 1
                    cur = some(cv[3])
                else:
                    he = True
                    break
            self.F("if", tt[0], "%d%s" % (ne, "+else" if he else ""))
        elif tt and tt[1] == "elseif":
            self.F("elseif", tt[0])
        self.visit([cond, then_, else_])

    def h_While(self, v):
        t = tok(v[0])
        self.F("while", t[0] if t else None)
        self.visit(v[1])
        self.body(v[2])

    def h_DoWhile(self, v):
        t = tok(v[0])
        self.F("repeat", t[0] if t else None)
        self.body(v[1])
        self.visit(v[2])

    def body(self, b):
        k, x = ctor(b)
        if k == "Block":
            self.visit(x[1])
        else:
            self.visit(b)

    def h_Block(self, v):
        t = tok(v[0])
        if t and t[1] == "do":
            self.F("do", t[0])
        self.visit(v[1])

    def pat_vars(self, p):
        k, x = ctor(p)
        if k == "PatId":
            (s, t), _ = x
            tt = tok(t)
            if tt:
                self.F("for_var", tt[0], s)
                self.F("ident", tt[0], s)
            return 1
        if k == "PatTuple":
            return sum(self.pat_vars(q) for q in x[1])
        self.visit(p)
        return 1

    def h_For(self, v):
        t, header, body = v
        tt = tok(t)
        hk, hv = ctor(header)
        if hk == "ForEach":
            pat, _, e = hv
            nv = self.pat_vars(pat)
            ek, ev = ctor(e)
            if ek == "Call" and ctor(ev[0])[0] == "IdSpecial" and ev[0]["IdSpecial"][0] == {"Op": "Range"}:
                self.F("for_num", tt[0] if tt else None, len(ev[1][1]))
            else:
                if ek == "Container" and ev[0] == "Tuple" and tok(ev[1][0]) is None:
                    ne = len(ev[1][1])
                else:
                    ne = 1
                self.F("for_gen", tt[0] if tt else None, "%din%d" % (nv, ne))
            self.visit(e)
        else:
            self.F("for_other", tt[0] if tt else None, hk)
            self.visit(header)
        self.body(body)

    def h_Break(self, v):
        t = tok(v[0])
        self.F("break", t[0] if t else None)

    def h_Goto(self, v):
        t = tok(v[0])
        self.F("goto", t[0] if t else None, v[1][0])
        self.visit(v[1])

    def h_Label(self, v):
        (s, t), st = v
        tt = tok(t)
        self.F("label", tt[0] if tt else None, s)
        self.visit(v[0])
        self.visit(st)

    def h_L(self, v):
        k, x = ctor(v)
        if k == "String":
            self.string(x)
        elif k in ("Int", "Float"):
            self.number(k, x)
        elif k == "Imag":
            t = tok(x[1])
            if t:
                self.F("number", t[0], t[1])
                self.F("number_kind", t[0], "Imag")
        elif k == "Bool":
            t = tok(x[1])
            if t:
                self.F("bool", t[0], t[1])
        elif k == "Null":
            t = tok(x)
            if t:
                self.F("nil", t[0])
        else:
            self.visit(v)

    def h_DotAccess(self, v):
        e, t, fn = v
        self.visit(e)
        fk, fv = ctor(fn)
        if fk == "FN" and ctor(fv)[0] == "Id":
            (s, tt), _ = fv["Id"]
            p = tok(tt)
            if p:
                self.F("dot_access", p[0], s)
                self.F("ident", p[0], s)
        else:
            self.visit(fn)

    def h_ArrayAccess(self, v):
        e, (lb, idx, rb) = v
        self.F("index_access", span(idx)[0])
        self.visit(e)
        self.visit(idx)

    def h_IdSpecial(self, v):
        sp, t = v
        tt = tok(t)
        sk = sp if isinstance(sp, str) else ctor(sp)[0]
        if tt and sk == "Self":
            self.F("ident", tt[0], "self")
        elif tt:
            self.F("idspecial", tt[0], json.dumps(sp))

    def h_OtherExpr(self, v):
        # old parser: next -> OtherExpr("next")
        (s, t), rest = v
        tt = tok(t)
        if tt and tt[1] == s:
            self.F("ident", tt[0], s)
        self.notes.append(["other_expr", tt[0] if tt else None, s])
        self.visit(rest)


SCOPED = {"call", "method_call", "assign", "local_var", "return", "if", "while", "repeat", "for_num", "for_gen", "func_anon", "func_global", "func_local", "func_method", "table", "binop", "string"}


def is_fake_nil(e):
    k, x = ctor(e)
    if k == "L":
        lk, lv = ctor(x)
        return lk == "Null" and tok(lv) is None
    return False


def lua_number_value(raw):
    s = raw.lower()
    s = re.sub(r"(u?ll)$", "", s)
    try:
        if s.startswith("0x"):
            if "." in s or "p" in s:
                return float.fromhex(s)
            v = int(s, 16)
            return v - (1 << 64) if v >= (1 << 63) else v  # Lua 5.3 wraps
        if "." in s or "e" in s:
            return float(s)
        return int(s)
    except ValueError:
        return None


def same_num(a, b):
    try:
        if a == b:
            return True
        if isinstance(a, float) or isinstance(b, float):
            return abs(float(a) - float(b)) <= 1e-9 * max(1.0, abs(float(a)))
        # int vs int with 63-bit OCaml wrap
        return (a - b) % (1 << 63) == 0 or (a - b) % (1 << 64) == 0
    except (OverflowError, TypeError):
        return False


def extract(js):
    x = Extractor()
    x.visit(js)
    return x.facts, x.notes


def load_dump(text):
    i = text.find("{")
    if i < 0:
        return None
    return json.loads(text[i:])


if __name__ == "__main__":
    js = load_dump(open(sys.argv[1]).read())
    f, n = extract(js)
    for x in sorted(f, key=lambda a: a[1]):
        print(x)
    print(n)
