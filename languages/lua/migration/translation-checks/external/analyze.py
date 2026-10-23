"""Aggregate results_{new,old}.jsonl: coverage, per-category stats, per project,
discrepancy grouping. Prints markdown-ish text; writes agg_<which>.json."""
import json, sys, collections
from compare import CATS

HERE = "/tmp/tc/external"


def proj(f):
    return f.split("/host-tmp/corpus/")[1].split("/")[0]


def load(which):
    return [json.loads(l) for l in open(f"{HERE}/results_{which}.jsonl")]


def agg(rows, include_parse_errors=False):
    cov = collections.defaultdict(collections.Counter)
    tot = collections.defaultdict(collections.Counter)
    per = collections.defaultdict(lambda: collections.defaultdict(collections.Counter))
    disc = []
    notes = collections.Counter()
    for r in rows:
        p = proj(r["file"])
        cov[p]["files"] += 1
        if not r["ref_ok"]:
            cov[p]["ref_fail"] += 1
            continue
        if r.get("timeout"):
            cov[p]["timeout"] += 1
            continue
        if "stats" not in r:
            cov[p]["no_ast"] += 1
            continue
        if r.get("parse_error"):
            cov[p]["parse_error"] += 1
            if not include_parse_errors:
                continue
        cov[p]["compared"] += 1
        for c, s in r["stats"].items():
            for k, v in s.items():
                tot[c][k] += v
                per[p][c][k] += v
        for k, v in r.get("notes", {}).items():
            notes[k] += v
        for d in r.get("disc", []):
            disc.append([r["file"]] + d)
    return cov, tot, per, disc, notes


def table(tot, cats=CATS + ["number_kind"]):
    lines = ["| category | expected | actual | matched | missing | extra | desc-mismatch | agreement % |",
             "|---|---:|---:|---:|---:|---:|---:|---:|"]
    E = A = M = 0
    for c in cats:
        s = tot.get(c)
        if not s:
            continue
        e = s["expected"]
        agr = 100.0 * s["matched"] / max(e, s["actual"], 1)
        lines.append(f"| {c} | {e} | {s['actual']} | {s['matched']} | {s['missing']} | {s['extra']} | {s['desc_mismatch']} | {agr:.3f} |")
        if c != "number_kind":
            E += e; A += s["actual"]; M += s["matched"]
    lines.append(f"| **all (w/o number_kind)** | {E} | {A} | {M} | | | | {100.0*M/max(E,A,1):.4f} |")
    return "\n".join(lines)


if __name__ == "__main__":
    which = sys.argv[1]
    rows = load(which)
    cov, tot, per, disc, notes = agg(rows)
    print(table(tot))
    print(notes)
    g = collections.Counter((d[1], d[2]) for d in disc)
    print(g.most_common(40))
    json.dump({"cov": cov, "tot": tot, "notes": notes, "disc_groups": [[list(k), v] for k, v in g.most_common()]},
              open(f"{HERE}/agg_{which}.json", "w"), indent=1)


def report_tables():
    from compare import SHAPE, SCOPE
    out = []
    res = {}
    for which in ("new", "old"):
        rows = load(which)
        res[which] = agg(rows)
    cov = res["new"][0]
    covo = res["old"][0]
    out.append("### Coverage per project (new parser; old in parentheses)\n")
    out.append("| project | files | luaparse failed (skipped) | opengrep parse errors | timeouts/no AST | compared |")
    out.append("|---|---:|---:|---:|---:|---:|")
    T = collections.Counter(); TO = collections.Counter()
    for p in sorted(cov):
        c, o = cov[p], covo.get(p, collections.Counter())
        T.update(c); TO.update(o)
        out.append(f"| {p} | {c['files']} | {c['ref_fail']} | {c['parse_error']} ({o['parse_error']}) | {c['timeout']+c['no_ast']} ({o['timeout']+o['no_ast']}) | {c['compared']} ({o['compared']}) |")
    out.append(f"| **total** | {T['files']} | {T['ref_fail']} | {T['parse_error']} ({TO['parse_error']}) | {T['timeout']+T['no_ast']} ({TO['timeout']+TO['no_ast']}) | {T['compared']} ({TO['compared']}) |")
    for which in ("new", "old"):
        cov, tot, per, disc, notes = res[which]
        out.append(f"\n### Per-category statistics, {which} parser (files without parse errors)\n")
        out.append(table(tot))
        out.append("\nStructure checks (tree shape and enclosing function):\n")
        out.append(table(tot, SHAPE + SCOPE))
        out.append(f"\nBy-design notes counted in the AST ({which}): {dict(notes)}\n")
        g = collections.Counter((d[1], d[2]) for d in disc)
        out.append(f"Discrepancy groups ({which}): " + ", ".join(f"{a}/{b}: {n}" for (a, b), n in g.most_common()) + "\n")
        out.append(f"\n### Per project, {which} parser: agreement over all base categories\n")
        out.append("| project | expected facts | matched | missing | extra | desc-mismatch | agreement % |")
        out.append("|---|---:|---:|---:|---:|---:|---:|")
        for p in sorted(per, key=lambda p: -sum(per[p][c]["expected"] for c in CATS)):
            E = sum(per[p][c]["expected"] for c in CATS); A = sum(per[p][c]["actual"] for c in CATS)
            M = sum(per[p][c]["matched"] for c in CATS)
            mi = sum(per[p][c]["missing"] for c in CATS); ex = sum(per[p][c]["extra"] for c in CATS)
            dm = sum(per[p][c]["desc_mismatch"] for c in CATS)
            out.append(f"| {p} | {E} | {M} | {mi} | {ex} | {dm} | {100.0*M/max(E,A,1):.4f} |")
    open(f"{HERE}/report_tables.md", "w").write("\n".join(out) + "\n")
    s = {}
    tot = res["new"][1]
    for c in CATS + SHAPE + SCOPE + ["number_kind"]:
        if c in tot:
            s[c] = {k: tot[c][k] for k in ("expected", "actual", "missing", "extra", "desc_mismatch")}
    return res, s
