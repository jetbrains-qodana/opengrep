"""Structural invariant checker for the Lua -> AST_generic translation.

Usage: python3 check.py SRC.lua AST.json  -> prints one JSON line of results.
(The driver run.py calls check_file() directly.)
"""
import bisect
import json
import sys
from collections import Counter, defaultdict

from lualex import lex, match_blocks

sys.setrecursionlimit(200000)

ALWAYS_DROP = {b"(", b")", b",", b";", b"then", b"else", b"::"}
SHARED_OK = {b")", b"until", b"...", b"local", b"global", b"repeat"}
CONSTRUCT_KW = {b"function", b"if", b"while", b"for", b"repeat", b"do"}
STMT_CTORS = {"ExprStmt", "DefStmt", "Return", "If", "While", "DoWhile", "For",
              "Block", "Label", "Goto", "Break", "Continue", "DirectiveStmt",
              "OtherStmt", "OtherStmtWithStmt"}
MAXEX = 6


def droppable_mask(toks, cons):
    """Indices of source tokens that the translation drops by design."""
    n = len(toks)
    drop = [False] * n
    txt = [t[3] for t in toks]
    for k in range(n):
        if txt[k] in ALWAYS_DROP:
            drop[k] = True
    # 'end' of an if
    for c in cons:
        if c["kind"] == "if" and c["close"] is not None:
            drop[c["close"]] = True
    # attribs <const>
    for k in range(1, n - 2):
        if txt[k] == b"<" and toks[k + 1][2] == "name" and txt[k + 2] == b">" and \
                (toks[k - 1][2] == "name" or txt[k - 1] in (b"local", b"global")):
            drop[k] = drop[k + 2] = True
    # dots in function names: function a.b.c:m
    for k in range(n):
        if txt[k] == b"function":
            j = k + 1
            while j + 1 < n and toks[j][2] == "name" and txt[j + 1] in (b".", b":"):
                drop[j + 1] = True
                if txt[j + 1] == b":":
                    break
                j += 2
    # '=' and '[' ']' in table fields, '=' of local/global declarations
    stack = []
    for k in range(n):
        t = txt[k]
        if t in (b"{", b"(", b"["):
            if t == b"[" and stack and stack[-1][0] == b"{" and txt[k - 1] in (b"{", b",", b";"):
                stack.append((b"K", k))
            else:
                stack.append((t, k))
        elif t in (b"}", b")", b"]"):
            if stack:
                o, ok = stack.pop()
                if o == b"K":
                    drop[ok] = drop[k] = True
        elif t == b"=" and stack and stack[-1][0] == b"{":
            drop[k] = True
        elif t in (b"local", b"global") and k + 1 < n and txt[k + 1] != b"function":
            j = k + 1
            while j < n:
                if txt[j] == b"<" and j + 2 < n and txt[j + 2] == b">":
                    j += 3
                    continue
                if toks[j][2] == "name" or txt[j] == b",":
                    j += 1
                    continue
                break
            if j < n and txt[j] == b"=":
                drop[j] = True
    return drop


class Ctx:
    def __init__(self, src, toks, label):
        self.src = src
        self.toks = toks
        self.starts = [t[0] for t in toks]
        self.label = label
        self.cnt = Counter()          # violations by invariant
        self.ex = defaultdict(list)   # examples
        self.tally = Counter()        # informational tallies
        self.occ = defaultdict(list)  # src idx -> list of parent ctors
        self.ghost = 0
        self.constructs = {}          # (kind, open idx) -> dict
        self.cstack = []

    def line_of(self, idx):
        if idx is None or idx < 0 or idx >= len(self.toks):
            return 0
        return self.src.count(b"\n", 0, self.toks[idx][0]) + 1

    def snippet(self, lo, hi):
        if lo is None:
            return ""
        a = self.toks[lo][0]
        b = self.toks[min(hi, len(self.toks) - 1)][1]
        s = self.src[a:b][:160].decode("utf-8", "replace")
        return s.replace("\n", "\\n")

    def viol(self, inv, idx=None, hi=None, msg=""):
        self.cnt[inv] += 1
        if len(self.ex[inv]) < MAXEX:
            self.ex[inv].append({"line": self.line_of(idx), "msg": msg,
                                 "src": self.snippet(idx, hi if hi is not None else idx)})

    def tok_idx(self, bp):
        k = bisect.bisect_right(self.starts, bp) - 1
        if k < 0 or bp >= self.toks[k][1]:
            return None
        return k


class Info:
    __slots__ = ("lo", "hi", "nd")

    def __init__(self, lo=None, hi=None, nd=None):
        self.lo, self.hi, self.nd = lo, hi, nd if nd is not None else set()

    def add(self, o):
        if o.lo is None:
            return
        if self.lo is None or o.lo < self.lo:
            self.lo = o.lo
        if self.hi is None or o.hi > self.hi:
            self.hi = o.hi
        if len(o.nd) > len(self.nd):
            o.nd |= self.nd
            self.nd = o.nd
        else:
            self.nd |= o.nd


def is_variant(x):
    if isinstance(x, dict) and len(x) == 1:
        k = next(iter(x))
        return k[:1].isupper() and k not in ("OriginTok", "FakeTok", "ExpandedTok", "Ab")
    return False


class Checker:
    def __init__(self, ctx, drop, ndprefix):
        self.c = ctx
        self.drop = drop
        self.P = ndprefix
        self.grouped = set()

    # ---- tokens ------------------------------------------------------
    def token(self, t, parent):
        c = self.c
        if "OriginTok" in t:
            o = t["OriginTok"]
            if o.get("str", None) == "":
                c.viol("missing_token_inserted", None, msg=f"empty OriginTok in {parent} at line {o.get('line')}")
            k = c.tok_idx(o["bytepos"])
            if k is None:
                c.ghost += 1
                c.viol("ghost_token", None, msg=f"token {o.get('str')!r} at byte {o['bytepos']} (line {o.get('line')}) not in a source token")
                return Info()
            c.occ[k].append(parent)
            inf = Info(k, k)
            if not self.drop[k]:
                inf.nd.add(k)
            return inf
        if "FakeTok" in t:
            c.tally["fake_tok:" + parent] += 1
        return Info()

    # ---- generic walk -----------------------------------------------
    def walk(self, x, parent="Pr"):
        """Walk x; returns Info. For variant nodes, check invariants."""
        if isinstance(x, dict):
            if "token" in x and len(x) == 1:
                return self.token(x["token"], parent)
            if "OriginTok" in x or "FakeTok" in x:
                return self.token(x, parent)
            if is_variant(x):
                return self.variant(x)
            inf = Info()
            for k, v in x.items():
                if k in ("id_info", "name_info", "pinfo", "id_flags", "id_resolved",
                         "id_resolved_alternative", "id_type", "id_svalue"):
                    continue
                inf.add(self.walk(v, parent))
            return inf
        if isinstance(x, list):
            inf = Info()
            for v in x:
                inf.add(self.walk(v, parent))
            return inf
        return Info()

    def items(self, x, parent, out):
        """Flatten payload into ordered list of (Info, desc) of direct items."""
        if isinstance(x, list):
            for v in x:
                self.items(v, parent, out)
        elif isinstance(x, dict) and not is_variant(x) and not ("token" in x and len(x) == 1) \
                and "OriginTok" not in x and "FakeTok" not in x:
            for k, v in x.items():
                if k in ("id_info", "name_info", "pinfo", "id_flags", "id_resolved",
                         "id_resolved_alternative", "id_type", "id_svalue"):
                    continue
                self.items(v, parent, out)
        elif isinstance(x, dict):
            desc = next(iter(x)) if is_variant(x) else "tok"
            out.append((self.walk(x, parent), desc, x))

    def check_order(self, kind, its, where):
        c = self.c
        prev = None
        for inf, desc, _ in its:
            if inf.lo is None:
                continue
            if prev is not None:
                if prev.hi > inf.lo or (prev.hi == inf.lo and c.toks[inf.lo][3] not in SHARED_OK
                                        and c.toks[inf.lo][2] != "lstr"):
                    c.viol("order_overlap:" + kind, min(prev.lo, inf.lo), max(prev.hi, inf.hi),
                           msg=f"{where}: item {desc} [{inf.lo},{inf.hi}] vs previous [{prev.lo},{prev.hi}]")
                elif prev.hi == inf.lo:
                    c.tally["shared_boundary:" + c.toks[inf.lo][3].decode()] += 1
            prev = inf

    def contiguity(self, kind, inf, key=None):
        if inf.lo is None:
            return
        expected = self.P[inf.hi + 1] - self.P[inf.lo]
        if expected != len(inf.nd):
            c = self.c
            miss = [k for k in range(inf.lo, inf.hi + 1) if not self.drop[k] and k not in inf.nd]
            ms = " ".join(c.toks[k][3][:20].decode("utf-8", "replace") for k in miss[:8])
            v = ("contiguity:" + kind, inf.lo, inf.hi, f"{len(miss)} source token(s) inside the node not in its subtree: {ms}")
            if key is not None:
                self.pending[key] = v
            else:
                c.viol(*v)

    def variant(self, x):
        c = self.c
        kind, pay = next(iter(x.items()))
        c.tally["node:" + kind] += 1
        # constructs stack (function/loop/block)
        opened = None
        if kind in ("FuncDef", "Lambda"):
            ft = pay["fkind"][1]
            opened = self.reg_construct("function", ft, pay, kind)
        elif kind in ("While", "For", "DoWhile", "If"):
            if kind in ("While", "For"):
                self.loop_bodies.add(id(pay[2]))
            opened = self.reg_construct({"While": "while", "For": "for", "DoWhile": "repeat", "If": "if"}[kind],
                                        pay[0], pay, kind)
        elif kind == "Block" and id(x) not in self.loop_bodies:
            opened = self.reg_construct("do", pay[0], pay, kind)
        its = []
        self.items(pay, kind, its)
        if opened is not None:
            c.cstack.pop()
        inf = Info()
        for i, _, _ in its:
            inf.add(i)
        self.shapes(kind, pay, its)
        # ordering of direct items
        if kind == "DefStmt":
            self.check_defstmt(pay, its)
        elif kind == "IdQualified":
            pass  # name_last, name_top, name_middle: checked via QDots below
        elif kind == "Call" and its and its[0][1] == "IdSpecial":
            op = its[0][0]
            args = [i for i in its[1:] if i[0].lo is not None]
            seq = []
            if len(args) >= 2:
                seq = [args[0], its[0]] + args[1:]
            else:
                seq = its
            self.check_order("Call(op)", seq, "Call op")
        elif kind == "FuncDef" or kind == "Lambda":
            its2 = [i for i in its if not (i[1] == "Param" and self.is_self(i[2]))]
            self.check_order(kind, its2, kind)
        elif kind in ("ParamRest", "Pr", "Block"):
            pass  # Pr/Block: statements are checked in groups()
        else:
            self.check_order(kind, its, kind)
        # contiguity
        if kind == "Pr":
            if self.P[-1] and inf.lo is None:
                c.viol("lost_toplevel", 0, len(c.toks) - 1, msg="no tokens in AST")
            full = Info(0 if c.toks else None, len(c.toks) - 1 if c.toks else None, inf.nd)
            self.contiguity("Pr(file)", full)
        elif kind == "DefStmt":
            self.contiguity(kind, inf, key=id(x))
        elif kind == "FuncDef":
            lp = self.tokidx_of(pay["fparams"][0])
            if lp is not None:
                self.contiguity(kind, Info(lp, inf.hi, {k for k in inf.nd if k >= lp}))
        else:
            self.contiguity(kind, inf)
        if kind in ("Pr", "Block"):
            self.groups(pay if kind == "Pr" else pay[1], its)
        return inf

    def is_self(self, p):
        try:
            pn = p["Param"]["pname"]["some"]
            return pn[0] == "self" and pn[1]["token"]["OriginTok"]["str"] == ":"
        except Exception:
            return False

    def reg_construct(self, ckind, tok, pay, kind):
        c = self.c
        t = tok.get("token", tok) if isinstance(tok, dict) else None
        if not t or "OriginTok" not in t:
            return None  # e.g. fake bracket blocks of if branches
        k = c.tok_idx(t["OriginTok"]["bytepos"])
        if k is None:
            return None
        w = c.toks[k][3]
        if ckind == "do" and w != b"do":
            return None  # loop bodies / function bodies are blocks too
        if ckind == "if" and w == b"elseif":
            return None
        rec = {"kind": ckind, "ast": kind, "parent": c.cstack[-1] if c.cstack else None, "pay": pay}
        key = (ckind, k)
        if key in c.constructs:
            c.viol("construct_twice", k, k, msg=f"{ckind} at token {k} translated twice")
        c.constructs[key] = rec
        c.cstack.append(key)
        return key

    def pure(self, x, out):
        if isinstance(x, dict):
            for k, v in x.items():
                if k == "OriginTok":
                    i = self.c.tok_idx(v["bytepos"])
                    if i is not None:
                        out.add(i)
                else:
                    self.pure(v, out)
        elif isinstance(x, list):
            for v in x:
                self.pure(v, out)
        return out

    def pinfo(self, x):
        s = self.pure(x, set())
        return Info(min(s), max(s)) if s else Info()

    def check_defstmt(self, pay, its):
        c = self.c
        ent, d = pay[0], pay[1]
        name = self.pinfo(ent.get("name"))
        attrs = self.pinfo(ent.get("attrs"))
        if name.lo is None:
            c.viol("shape:DefStmt_without_name", None, msg=json.dumps(ent)[:200])
            return
        kw = [k for k in self.pure(ent.get("attrs"), set()) if c.toks[k][3] in (b"local", b"global")]
        if kw and max(kw) >= name.lo:
            c.viol("order_overlap:DefStmt(attrs/name)", attrs.lo, name.hi)
        dk = next(iter(d)) if isinstance(d, dict) else str(d)
        if dk == "FuncDef":
            fk = d["FuncDef"]["fkind"][1]["token"]
            if "OriginTok" in fk:
                fi = c.tok_idx(fk["OriginTok"]["bytepos"])
                lp = d["FuncDef"]["fparams"][0]["token"]
                li = c.tok_idx(lp["OriginTok"]["bytepos"]) if "OriginTok" in lp else None
                if fi is None or not (fi < name.lo and (li is None or name.hi < li)):
                    c.viol("order_overlap:DefStmt(function/name/params)", fi, li)
        elif dk == "VarDef":
            v = d["VarDef"]["vinit"]
            if v:
                vinf = self.pinfo(v)
                if vinf.lo is not None and vinf.lo <= name.hi:
                    c.viol("order_overlap:DefStmt(name/init)", name.lo, vinf.hi)

    def groups(self, stmts, its):
        """Group DefStmts sharing their local/global token; check groups' contiguity
        and order of statements."""
        c = self.c
        sts = [i for i in its if i[1] in STMT_CTORS or i[1] == "tok"]
        # its for Pr is the stmt list; for Block includes brackets (tok)
        groups = []
        for inf, desc, x in sts:
            if desc == "tok":
                groups.append((None, [(inf, None)]))
                continue
            share = None
            if desc == "DefStmt":
                kw = [k for k in self.pure(x["DefStmt"][0].get("attrs"), set())
                      if c.toks[k][3] in (b"local", b"global")]
                share = kw[0] if kw else None
            if groups and share is not None and groups[-1][0] == share:
                groups[-1][1].append((inf, x))
            else:
                groups.append((share, [(inf, x)]))
        ginfs = []
        for share, mem in groups:
            g = Info()
            if len(mem) > 1:
                for inf, x in mem:
                    nd = {k for k in self.pure(x, set()) if not self.drop[k]}
                    g.add(Info(inf.lo, inf.hi, nd))
            else:
                g = Info(mem[0][0].lo, mem[0][0].hi)
            if len(mem) > 1:
                c.tally["defstmt_groups"] += 1
                for _, x in mem:
                    self.pending.pop(id(x), None)
                self.contiguity("DefStmt(group)", g)
            ginfs.append((g, "stmt", None))
        for _, x in [m for _, mm in groups for m in mm if m[1] is not None]:
            v = self.pending.pop(id(x), None)
            if v:
                c.viol(*v)
        self.check_order("stmt_list", ginfs, "statements")

    def shapes(self, kind, pay, its):
        c = self.c
        if kind == "Id":
            try:
                (s, tk), _ = pay
                t = tk["token"]
                if s == "":
                    c.viol("shape:empty_Id", None, msg=json.dumps(t)[:150])
                if "OriginTok" not in t:
                    c.viol("shape:Id_with_fake_token", None, msg=s)
                elif t["OriginTok"]["str"] != s:
                    k = c.tok_idx(t["OriginTok"]["bytepos"])
                    c.viol("shape:Id_text_differs_from_token", k, k, msg=f"{s!r} vs {t['OriginTok']['str']!r}")
            except Exception as e:
                c.viol("shape:Id_bad", None, msg=str(e))
        elif kind in ("Ellipsis", "DeepEllipsis", "TypedMetavar", "DisjExpr"):
            c.viol("shape:pattern_node_in_target:" + kind, its[0][0].lo if its else None)
        elif kind.startswith("Other") or kind in ("RawExpr", "RawStmt", "Todo"):
            c.tally["other:" + kind + ":" + (str(pay[0])[:40] if isinstance(pay, list) else "")] += 1
            if kind not in ("OtherAttribute", "OtherDirective"):
                c.viol("shape:Other_node:" + kind, None, msg=str(pay)[:150])
        elif kind == "Call":
            ck = next(iter(pay[0])) if isinstance(pay[0], dict) else "?"
            if ck == "N":
                ck = "N." + next(iter(pay[0]["N"]))
            c.tally["callee:" + ck] += 1
            if ck in ("L", "Container"):
                lo = its[0][0].lo if its else None
                c.viol("shape:Call_with_literal_callee", lo, its[-1][0].hi if its else None)
        elif kind == "Assign":
            def lhs_kinds(e, out):
                k = next(iter(e))
                if k == "Container" and e[k][0] == "Tuple":
                    for y in e[k][1][1]:
                        lhs_kinds(y, out)
                else:
                    out.append(k)
            out = []
            lhs_kinds(pay[0], out)
            for k in out:
                c.tally["assign_lhs:" + k] += 1
                if k not in ("N", "DotAccess", "ArrayAccess"):
                    c.viol("shape:bad_assign_target:" + k, its[0][0].lo if its else None)
        elif kind == "IdQualified":
            try:
                last = pay["name_last"][0][0]
                if last == "":
                    c.viol("shape:IdQualified_empty_part", None)
                mid = pay.get("name_middle")
                if mid and "some" in mid and "QDots" in mid["some"]:
                    for (nm, _), _ in mid["some"]["QDots"]:
                        if nm == "":
                            c.viol("shape:IdQualified_empty_part", None)
            except Exception as e:
                c.viol("shape:IdQualified_bad", None, msg=str(e))
        elif kind == "Container" and pay[0] == "Dict":
            for e in pay[1][1]:
                k = next(iter(e))
                if k != "Container" or e[k][0] != "Tuple" or len(e[k][1][1]) != 2:
                    c.viol("shape:Dict_entry_not_pair", None, msg=k)
        elif kind == "Block":
            pass

    # ---- block structure --------------------------------------------
    def tokidx_of(self, t):
        t = t.get("token", t)
        if "OriginTok" in t:
            return self.c.tok_idx(t["OriginTok"]["bytepos"])
        return None

    def compare_constructs(self, cons):
        c = self.c
        lex_by_key = {}
        for i, lc in enumerate(cons):
            lex_by_key[(lc["kind"], lc["open"])] = i
        for i, lc in enumerate(cons):
            key = (lc["kind"], lc["open"])
            c.tally["lex:" + lc["kind"]] += 1
            rec = c.constructs.get(key)
            if rec is None:
                c.viol("block:construct_missing:" + lc["kind"], lc["open"], lc["close"])
                continue
            # parent
            lp = cons[lc["parent"]] if lc["parent"] is not None else None
            lpk = (lp["kind"], lp["open"]) if lp else None
            if rec["parent"] != lpk:
                c.viol("block:wrong_parent:" + lc["kind"], lc["open"], lc["close"],
                       msg=f"lexer parent {lpk}, AST parent {rec['parent']}")
            pay = rec["pay"]
            close = None
            if rec["ast"] in ("FuncDef", "Lambda"):
                b = pay["fbody"]["FBStmt"]["Block"]
                close = self.tokidx_of(b[2])
                bo = self.tokidx_of(b[0])
                rp = self.tokidx_of(pay["fparams"][2])
                if bo != rp:
                    c.viol("block:function_body_not_from_rparen", lc["open"], lc["close"])
            elif rec["ast"] in ("While", "For"):
                b = pay[2]["Block"]
                close = self.tokidx_of(b[2])
                if lc["do"] is not None and self.tokidx_of(b[0]) != lc["do"]:
                    c.viol("block:loop_body_not_from_do", lc["open"], lc["close"])
            elif rec["ast"] == "DoWhile":
                close = self.tokidx_of(pay[1]["Block"][2])
            elif rec["ast"] == "Block":
                close = self.tokidx_of(pay[2])
            if rec["ast"] != "If":
                if close != lc["close"]:
                    c.viol("block:close_mismatch:" + lc["kind"], lc["open"], lc["close"],
                           msg=f"AST close token {close}, lexer {lc['close']}")
        for key, rec in c.constructs.items():
            if key not in lex_by_key:
                c.viol("block:construct_extra:" + key[0], key[1], key[1])

    def run(self, ast, cons):
        self.pending = {}
        self.loop_bodies = set()
        self.walk(ast)
        for v in self.pending.values():
            self.c.viol(*v)
        self.compare_constructs(cons)
        # inside each lexer construct span, all AST tokens of the construct subtree
        # are within the span (checked via contiguity + close match); also each
        # source 'end' used at most once
        for k, ps in self.c.occ.items():
            if len(ps) > 1:
                t = self.c.toks[k][3]
                tag = t.decode("utf-8", "replace") if t in SHARED_OK or len(t) < 3 else t[:12].decode("utf-8", "replace")
                ctx = "/".join(sorted(set(ps)))
                if t in SHARED_OK or self.c.toks[k][2] == "lstr" or \
                        (t in (b"const", b"close") and set(ps) == {"KeywordAttr"} or set(ps) == {"OtherAttribute"}):
                    self.c.tally[f"dup_expected:{tag}:{ctx}"] += 1
                else:
                    self.c.viol("dup_token", k, k, msg=f"{tag!r} used {len(ps)} times in {ctx}")
        # presence
        for k, t in enumerate(self.c.toks):
            if k not in self.c.occ:
                key = t[3].decode("utf-8", "replace") if t[2] in ("kw", "op", "other") else t[2]
                if self.drop[k]:
                    self.c.tally["dropped:" + key] += 1
                else:
                    self.c.viol("absent_token:" + key, k, k)


def check(src: bytes, ast, label=""):
    toks = lex(src)
    cons, unclosed = match_blocks(toks)
    drop = droppable_mask(toks, cons)
    P = [0]
    for d in drop:
        P.append(P[-1] + (0 if d else 1))
    ctx = Ctx(src, toks, label)
    ctx.tally["src_tokens"] = len(toks)
    ctx.tally["lex_unclosed"] = unclosed
    ch = Checker(ctx, drop, P)
    ch.run(ast, cons)
    return ctx


if __name__ == "__main__":
    src = open(sys.argv[1], "rb").read()
    s = open(sys.argv[2]).read()
    ast = json.loads(s[s.index("{"):])
    ctx = check(src, ast)
    print(json.dumps({"viol": ctx.cnt, "ex": ctx.ex, "tally": ctx.tally}, indent=1))
