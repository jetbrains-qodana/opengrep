#!/usr/bin/env python3
"""Supplementary check (same token-position idea, applied to operators):
for every Call(IdSpecial(Op op, tok), args) in the AST JSON check that
  - the operator kind matches the source text of tok (e.g. '//' -> FloorDiv),
  - unary/binary arity matches the source (unary if the previous source token
    cannot end an expression),
  - operands are on the right side: for a binary op every origin token of
    arg1 is before tok and every token of arg2 after; for a unary op the
    operand is after tok.
Usage: opcheck.py {new|old} FILELIST OUT.jsonl [K M]  (process files i with i % M == K)
"""
import json, sys, os, collections, subprocess, time
sys.path.insert(0, "/tmp/tc/coverage")
from luacov import lex, classify, BIN

BINOP = {"or": "Or", "and": "And", "<": "Lt", "<=": "LtE", "==": "Eq", "~=": "NotEq",
         ">=": "GtE", ">": "Gt", "|": "BitOr", "~": "BitXor", "&": "BitAnd", "<<": "LSL",
         ">>": "LSR", "+": "Plus", "-": "Minus", "*": "Mult", "/": "Div", "//": "FloorDiv",
         "%": "Mod", "..": "Concat", "^": "Pow"}
UNOP = {"not": "Not", "#": "Length", "-": "Minus", "~": "BitNot"}


def tokpos(x, acc):
    if isinstance(x, dict):
        o = x.get("OriginTok")
        if o is not None and isinstance(o, dict) and "bytepos" in o:
            acc.append(o["bytepos"])
        else:
            for v in x.values():
                tokpos(v, acc)
    elif isinstance(x, list):
        for v in x:
            tokpos(v, acc)


def walk(x, out):
    if isinstance(x, dict):
        c = x.get("Call")
        if isinstance(c, list) and len(c) == 2 and isinstance(c[0], dict) and "IdSpecial" in c[0]:
            sp = c[0]["IdSpecial"]
            if isinstance(sp[0], dict) and "Op" in sp[0]:
                o = sp[1].get("token", {}).get("OriginTok")
                args = c[1][1] if isinstance(c[1], list) and len(c[1]) == 3 else c[1]
                spans = []
                for a in args:
                    acc = []; tokpos(a, acc); spans.append((min(acc), max(acc)) if acc else None)
                out.append((sp[0]["Op"], o["str"] if o else None, o["bytepos"] if o else None, spans))
        for v in x.values():
            walk(v, out)
    elif isinstance(x, list):
        for v in x:
            walk(v, out)


def check(which, path):
    src = open(path.replace("/host-tmp/", "/tmp/", 1), "rb").read()
    toks, _ = lex(src); classify(toks)
    bypos = {t.pos: (i, t) for i, t in enumerate(toks)}
    p = subprocess.run(["docker", "exec", "ogb", "timeout", "20", BIN[which], "-lang", "lua",
                        "-full_token_info", "-dump_ast", "-json", path], stdout=subprocess.PIPE, stderr=subprocess.PIPE)
    txt = p.stdout.decode("utf-8", "surrogateescape")
    i = txt.find("{")
    if i < 0:
        if p.returncode == 124:
            return {"file": path, "status": "timeout"}
        raise RuntimeError("no json rc=%d" % p.returncode)
    ast = json.loads(txt[i:])
    status = "ok" if p.returncode == 0 else "parse_error"
    calls = []; walk(ast, calls)
    stats = collections.Counter(); probs = []
    for op, s, pos, spans in calls:
        if s is None:
            stats["fake_tok"] += 1; continue
        if (op, s) in (("Range", ","), ("Not", "until")):
            stats["by_design:" + op] += 1; continue
        it = bypos.get(pos)
        if it is None:
            probs.append(("op_tok_not_at_source_token", op, s, pos)); continue
        idx, t = it
        prev = toks[idx - 1] if idx else None
        unary_src = prev is None or not (prev.prefix_end or prev.kind in ("number", "string") or
                                          prev.ctx in ("literal:nil", "literal:true", "literal:false", "vararg",
                                                       "keyword:end(function)", "punct:}table"))
        if s == "not":
            unary_src = True
        exp = (UNOP if unary_src else BINOP).get(s)
        arity = 1 if unary_src else 2
        stats["ops"] += 1
        if exp != op:
            probs.append(("op_kind", op, s, pos, exp))
        if len(spans) != arity:
            probs.append(("arity", op, s, pos, len(spans), arity))
        elif arity == 2:
            if spans[0] is None or spans[1] is None or not (spans[0][1] < pos < spans[1][0]):
                probs.append(("operand_side", op, s, pos, spans))
        elif spans[0] is None or not spans[0][0] > pos:
            probs.append(("operand_side", op, s, pos, spans))
    return {"file": path, "status": status, "stats": dict(stats), "problems": probs[:100], "n_problems": len(probs)}


def main():
    which, flist, outp = sys.argv[1:4]
    k, m = (int(sys.argv[4]), int(sys.argv[5])) if len(sys.argv) > 5 else (0, 1)
    files = [l.rstrip("\n") for l in open(flist, encoding="utf-8") if l.strip()]
    done = set()
    if os.path.exists(outp):
        for l in open(outp, encoding="utf-8"):
            try:
                r = json.loads(l)
                if r["status"] != "tool_error": done.add(r["file"])
            except Exception:
                pass
    with open(outp, "a", encoding="utf-8") as o:
        for i, f in enumerate(files):
            if i % m != k or f in done:
                continue
            for _ in range(5):
                try:
                    r = check(which, f); break
                except Exception as e:
                    r = {"file": f, "status": "tool_error", "error": repr(e)}; time.sleep(3)
            o.write(json.dumps(r, ensure_ascii=False) + "\n"); o.flush()


if __name__ == "__main__":
    main()
