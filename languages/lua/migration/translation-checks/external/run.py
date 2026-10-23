"""Driver: for each file with a successful luaparse parse (ref.jsonl), dump the
AST with the new (and old) opengrep-core, extract facts and compare.
Usage: python3 run.py new|old [MAXFILES]   -> results_<which>.jsonl
2 worker processes."""
import json, subprocess, sys, os
from multiprocessing import Pool
sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
from extract_ast import extract, load_dump
from compare import compare

BIN = {"new": "/src/bin/opengrep-core", "old": "/host-tmp/opengrep-core-old"}
which = sys.argv[1]
HERE = "/tmp/tc/external"


def work(line):
    r = json.loads(line)
    f = r["file"]
    res = {"file": f, "ref_ok": r["ok"]}
    if not r["ok"]:
        res["ref_error"] = r.get("error")
        return res
    try:
        out = subprocess.run(["docker", "exec", "ogb", "timeout", "20", BIN[which], "-lang", "lua",
                              "-full_token_info", "-dump_ast", "-json", f],
                             capture_output=True, timeout=60)
    except subprocess.TimeoutExpired:
        res["timeout"] = True
        return res
    res["rc"] = out.returncode
    if out.returncode in (124, 137):
        res["timeout"] = True
        return res
    stdout = out.stdout.decode("utf-8", "replace")
    res["parse_error"] = out.returncode != 0 or b"fail to fully parse" in out.stderr
    try:
        js = load_dump(stdout)
    except Exception as e:
        js = None
        res["json_error"] = str(e)[:200]
    if js is None:
        res["no_ast"] = True
        return res
    try:
        facts, notes = extract(js)
    except Exception as e:
        res["extract_error"] = repr(e)[:300]
        return res
    st, disc = compare(r["facts"], facts)
    res["stats"] = st
    res["disc"] = disc[:400]
    res["ndisc"] = len(disc)
    nc = {}
    for n in notes:
        nc[n[0]] = nc.get(n[0], 0) + 1
    res["notes"] = nc
    res["number_value_mismatch"] = [n for n in notes if n[0] == "number_value_mismatch"][:20]
    return res


if __name__ == "__main__":
    lines = open(f"{HERE}/ref.jsonl").read().splitlines()
    if len(sys.argv) > 2:
        lines = lines[: int(sys.argv[2])]
    done = set()
    if os.path.exists(f"{HERE}/results_{which}.jsonl"):
        for l in open(f"{HERE}/results_{which}.jsonl"):
            try:
                done.add(json.loads(l)["file"])
            except Exception:
                pass
    lines = [l for l in lines if json.loads(l)["file"] not in done]
    with open(f"{HERE}/results_{which}.jsonl", "a") as out, Pool(int(os.environ.get("NPROC", "2"))) as pool:
        for i, res in enumerate(pool.imap(work, lines, chunksize=4)):
            out.write(json.dumps(res) + "\n")
            if i % 250 == 0:
                print(which, i, flush=True)
