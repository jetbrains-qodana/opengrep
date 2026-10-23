"""Aggregate results_<parser>.jsonl -> agg_<parser>.json (+ printed tables)."""
import json
import sys
from collections import Counter, defaultdict

from lualex import lex, match_blocks
from check import droppable_mask

parser = sys.argv[1]
recs = [json.loads(l) for l in open(f"/tmp/tc/structure/results_{parser}.jsonl", encoding="utf-8")]

OPS = {"+", "-", "*", "/", "//", "%", "^", "..", "==", "~=", "<", "<=", ">", ">=",
       "&", "|", "~", "<<", ">>", "#", "and", "or", "not"}


def cat_of(t):
    k, s = t[2], t[3].decode("utf-8", "replace")
    if k == "name":
        return "identifiers"
    if k in ("str", "lstr"):
        return "strings"
    if k == "num":
        return "numbers"
    if s in OPS:
        return "operators"
    if s in ("nil", "true", "false"):
        return "nil/true/false"
    if s == "...":
        return "vararg"
    if k == "kw":
        return "kw:" + s
    return "punct:" + s


def absent_cat(key):
    if key == "name":
        return "identifiers"
    if key in ("str", "lstr"):
        return "strings"
    if key == "num":
        return "numbers"
    if key in OPS:
        return "operators"
    if key in ("nil", "true", "false"):
        return "nil/true/false"
    if key == "...":
        return "vararg"
    from lualex import KEYWORDS
    if key in KEYWORDS:
        return "kw:" + key
    return "punct:" + key


status = Counter()
proj = defaultdict(Counter)
viol_clean = Counter()
viol_err = Counter()
viol_proj = defaultdict(Counter)
files_viol = Counter()
examples = defaultdict(list)
tok_expected = Counter()
tok_absent = Counter()
tok_expected_proj = defaultdict(Counter)
tok_absent_proj = defaultdict(Counter)
cons_expected = Counter()
tally = Counter()
for r in recs:
    p = r["project"]
    proj[p]["files"] += 1
    st = r["status"]
    status[st] += 1
    proj[p][st] += 1
    if st != "ok":
        continue
    pe = r.get("parse_error")
    if pe:
        proj[p]["parse_error"] += 1
        status["parse_error"] += 1
        for k, v in r["viol"].items():
            viol_err[k] += v
        continue
    proj[p]["clean"] += 1
    for k, v in r["tally"].items():
        tally[k] += v
    for k, v in r["viol"].items():
        viol_clean[k] += v
        viol_proj[p][k] += v
        files_viol[k] += 1
        for e in r["ex"].get(k, []):
            if len(examples[k]) < 12:
                examples[k].append(dict(e, file=r["file"]))
    # token-kind totals from the source (non-droppable tokens)
    src = open("/tmp" + r["file"][len("/host-tmp"):], "rb").read()
    toks = lex(src)
    cons, _ = match_blocks(toks)
    drop = droppable_mask(toks, cons)
    for t, d in zip(toks, drop):
        if not d:
            c = cat_of(t)
            tok_expected[c] += 1
            tok_expected_proj[p][c] += 1
    for k, v in r["viol"].items():
        if k.startswith("absent_token:"):
            c = absent_cat(k[len("absent_token:"):])
            tok_absent[c] += v
            tok_absent_proj[p][c] += v
    for c in cons:
        cons_expected[c["kind"]] += 1

constructs = {}
for kind in ("function", "if", "while", "for", "repeat", "do"):
    miss = viol_clean["block:construct_missing:" + kind]
    extra = viol_clean["block:construct_extra:" + kind]
    wrong = viol_clean["block:close_mismatch:" + kind] + viol_clean["block:wrong_parent:" + kind]
    exp = cons_expected[kind]
    constructs[kind] = {"expected": exp, "actual": exp - miss + extra, "missing": miss,
                        "extra": extra, "wrong_close_or_parent": wrong}

tokcats = {}
for c in sorted(tok_expected, key=lambda c: -tok_expected[c]):
    tokcats[c] = {"expected": tok_expected[c], "actual": tok_expected[c] - tok_absent[c],
                  "missing": tok_absent[c], "extra": 0}

out = {"status": status, "projects": proj, "viol_clean": viol_clean, "viol_parse_error_files": viol_err,
       "files_with_violation": files_viol, "viol_proj": viol_proj, "examples": examples,
       "tokcats": tokcats, "tok_absent_proj": tok_absent_proj, "tok_expected_proj": {p: sum(v.values()) for p, v in tok_expected_proj.items()},
       "constructs": constructs, "tally": tally}
json.dump(out, open(f"/tmp/tc/structure/agg_{parser}.json", "w"), indent=1)
print("status", dict(status))
print("violations (clean files):")
for k, v in viol_clean.most_common():
    print(f"  {k:55s} {v:7d}  files={files_viol[k]}")
print("constructs", json.dumps(constructs))
