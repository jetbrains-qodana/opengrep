#!/usr/bin/env python3
"""Aggregate raw/{new,old}.jsonl into tables (stdout, markdown) + summary.json."""
import json, collections, sys
sys.path.insert(0, "/tmp/tc/coverage")
from luacov import DROPPED_BY_DESIGN, category

D = "/tmp/tc/coverage/"


def load(w):
    rs = {}
    for l in open(D + "raw/%s.jsonl" % w, encoding="utf-8"):
        r = json.loads(l); rs[r["file"]] = r
    return rs


new, old = load("new"), load("old")
CATS = ["ident", "keyword", "literal", "number", "string", "vararg", "op", "punct"]


def agg(rs, files, by_ctx=False):
    E, C, X = collections.Counter(), collections.Counter(), collections.Counter()
    for f in files:
        r = rs[f]
        for k, v in r.get("expected", {}).items():
            key = k if by_ctx else (("dropped-by-design" if k in DROPPED_BY_DESIGN else category(k)))
            E[key] += v
        for k, v in r.get("covered", {}).items():
            key = k if by_ctx else (("dropped-by-design" if k in DROPPED_BY_DESIGN else category(k)))
            C[key] += v
        for b in r.get("bad_ast_tokens", []):
            c = b["ctx"]
            key = (c or "no-source-token") if by_ctx else ("no-source-token" if c is None else category(c))
            X[key] += 1
    return E, C, X


def table(E, C, X, keys=None):
    keys = keys or sorted(set(E) | set(X))
    out = ["| category | expected (source) | in AST | missing | extra (garbled/misplaced) | agreement % |",
           "|---|---:|---:|---:|---:|---:|"]
    te = tc = tx = 0
    for k in keys:
        e, c, x = E[k], C[k], X[k]
        if k != "dropped-by-design":
            te += e; tc += c; tx += x
        pct = "%.4f" % (100.0 * c / e) if e else "-"
        out.append("| %s | %d | %d | %d | %d | %s |" % (k, e, c, e - c, x, pct))
    out.append("| **total (kept tokens)** | %d | %d | %d | %d | %.4f |" % (te, tc, te - tc, tx, 100.0 * tc / te if te else 0))
    return "\n".join(out)


def status_table():
    projs = sorted({r["project"] for r in new.values()})
    out = ["| project | files | new ok | new parse err | new timeout | old ok | old parse err | old timeout |",
           "|---|---:|---:|---:|---:|---:|---:|---:|"]
    tot = collections.Counter()
    for p in projs:
        fs = [f for f, r in new.items() if r["project"] == p]
        cn = collections.Counter(new[f]["status"] for f in fs)
        co = collections.Counter(old[f]["status"] for f in fs if f in old)
        row = [len(fs), cn["ok"], cn["parse_error"], cn["timeout"], co["ok"], co["parse_error"], co["timeout"]]
        for i, v in enumerate(row): tot[i] += v
        out.append("| %s | %s |" % (p, " | ".join(map(str, row))))
    out.append("| **total** | %s |" % " | ".join(str(tot[i]) for i in range(7)))
    return "\n".join(out)


def main():
    files = list(new)
    ok_new = [f for f in files if new[f]["status"] == "ok"]
    pe_new = [f for f in files if new[f]["status"] == "parse_error"]
    both_ok = [f for f in ok_new if f in old and old[f]["status"] == "ok"]
    print("files: new", len(new), "old", len(old))
    print(collections.Counter(r["status"] for r in new.values()), collections.Counter(r["status"] for r in old.values()))
    print("\n## status per project\n"); print(status_table())
    keys = CATS + ["other", "dropped-by-design"]
    for name, rs, fs in [("NEW, files parsed without error", new, ok_new),
                         ("NEW, files with parse errors", new, pe_new),
                         ("NEW, files OK in both parsers", new, both_ok),
                         ("OLD, files OK in both parsers", old, both_ok),
                         ("OLD, all files OK in old", old, [f for f in old if old[f]["status"] == "ok"])]:
        print("\n## %s (%d files)\n" % (name, len(fs)))
        print(table(*agg(rs, fs), keys=keys))
    for name, rs, fs in [("NEW per context, ok files", new, ok_new), ("OLD per context, both-ok files", old, both_ok)]:
        print("\n## %s\n" % name)
        E, C, X = agg(rs, fs, by_ctx=True)
        print(table(E, C, X))
    # per project (new, ok files)
    print("\n## NEW per project (ok files): kept tokens\n")
    print("| project | ok files | kept tokens | missing | extra | missing ident/literal/op | agreement % |\n|---|---:|---:|---:|---:|---:|---:|")
    for p in sorted({r["project"] for r in new.values()}):
        fs = [f for f in ok_new if new[f]["project"] == p]
        E, C, X = agg(new, fs)
        e = sum(E[k] for k in keys[:-1]); c = sum(C[k] for k in keys[:-1]); x = sum(X.values())
        sig = sum(E[k] - C[k] for k in ("ident", "literal", "number", "string", "op", "vararg"))
        print("| %s | %d | %d | %d | %d | %d | %s |" % (p, len(fs), e, e - c, x, sig, "%.4f" % (100.0 * c / e) if e else "-"))
    # uncovered tokens in ok files, grouped
    print("\n## NEW uncovered kept tokens in ok files (grouped by ctx,text)\n")
    g = collections.Counter(); ex = collections.defaultdict(list)
    for f in ok_new:
        for u in new[f]["uncovered"]:
            k = (u["ctx"], u["text"][:30]); g[k] += 1
            if len(ex[k]) < 5: ex[k].append("%s:%d" % (f, u["line"]))
    for k, v in g.most_common(60):
        print(v, k, ex[k])
    print("\n## NEW bad AST tokens in ok files\n")
    g = collections.Counter(); ex = collections.defaultdict(list)
    for f in ok_new:
        for b in new[f]["bad_ast_tokens"]:
            k = (b["ctx"], b["ast"][:30], b["src"][:30]); g[k] += 1
            if len(ex[k]) < 5: ex[k].append("%s:%d" % (f, b["line"]))
    for k, v in g.most_common(60):
        print(v, k, ex[k])
    print("\n## NEW duplicates (ok files)\n")
    dd = collections.Counter()
    for f in ok_new:
        for k, v in new[f]["dups"].items(): dd[k] += v
    for k, v in dd.most_common(40): print(v, k)
    print("\n## NEW parse-error files: uncovered sig lines\n")
    for f in pe_new:
        r = new[f]
        print(f, "n_sig_lines", r["n_uncovered_sig_lines"], r["uncovered_sig_lines"][:15])
    # lines file
    with open(D + "raw/new_uncovered_sig_lines.json", "w") as o:
        json.dump({f: {"status": new[f]["status"], "lines": new[f]["uncovered_sig_lines"],
                       "n": new[f]["n_uncovered_sig_lines"]} for f in files if new[f].get("n_uncovered_sig_lines")}, o, indent=0)
    with open(D + "raw/old_uncovered_sig_lines.json", "w") as o:
        json.dump({f: {"status": old[f]["status"], "lines": old[f]["uncovered_sig_lines"],
                       "n": old[f]["n_uncovered_sig_lines"]} for f in old if old[f].get("n_uncovered_sig_lines")}, o, indent=0)
    # old regressions vs new: tokens covered by old but not by new (both ok)
    print("\n## kept-context tokens covered by OLD but not by NEW (both-ok files)\n")
    cnt = collections.Counter(); ex = collections.defaultdict(list)
    for f in both_ok:
        for k, v in old[f]["covered"].items():
            if k in DROPPED_BY_DESIGN: continue
            d = v - new[f]["covered"].get(k, 0)
            if d > 0:
                cnt[k] += d; ex[k].append(f)
    for k, v in cnt.most_common(): print(v, k, ex[k][:5])
    # summary.json
    E, C, X = agg(new, ok_new)
    cats = {k: {"expected": E[k], "actual": C[k], "missing": E[k] - C[k], "extra": X[k]} for k in keys}
    json.dump({"method": "source token coverage: every token of an independent Lua lexer (with syntactic context) "
                         "must appear in the -full_token_info AST JSON at the same byte offset with the same text; "
                         "reverse check for garbled/misplaced AST tokens; counts over files parsed without error",
               "files_processed": len(new), "files_with_parse_errors": len(pe_new),
               "files_timeout": sum(1 for r in new.values() if r["status"] == "timeout"),
               "categories": cats, "real_bugs": []},
              open(D + "summary.json.auto", "w"), indent=1)


main()
