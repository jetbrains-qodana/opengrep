"""Driver: python3 run.py new|old [START END]  -> results_<parser>.jsonl
Runs the parser in the ogb container on every file of /tmp/tc/files.txt
(2 workers), checks the structural invariants, writes one JSON line per file."""
import json
import os
import subprocess
import sys
import time
from multiprocessing import Pool

sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
from check import check  # noqa

BIN = {"new": "/src/bin/opengrep-core", "old": "/host-tmp/opengrep-core-old"}


def project(p):
    parts = p.split("/")
    return parts[parts.index("corpus") + 1]


def one(args):
    parser, cpath = args
    hpath = "/tmp" + cpath[len("/host-tmp"):]
    rec = {"file": cpath, "project": project(cpath)}
    t0 = time.time()
    try:
        r = subprocess.run(["docker", "exec", "ogb", "timeout", "20", BIN[parser], "-lang", "lua",
                            "-full_token_info", "-dump_ast", "-json", cpath],
                           capture_output=True, timeout=60)
    except subprocess.TimeoutExpired:
        rec["status"] = "timeout"
        return rec
    rec["rc"] = r.returncode
    err = r.stderr.decode("utf-8", "replace")
    out = r.stdout.decode("utf-8", "replace")
    if r.returncode in (124, 137, 143):
        rec["status"] = "timeout"
        return rec
    rec["parse_error"] = r.returncode != 0 or "fail to fully parse" in err or "parse error" in err.lower()
    i = out.find("{")
    if i < 0:
        rec["status"] = "no_ast"
        rec["err"] = err[-300:]
        return rec
    try:
        ast = json.loads(out[i:])
    except Exception as e:
        rec["status"] = "bad_json"
        rec["err"] = str(e)[:200]
        return rec
    try:
        src = open(hpath, "rb").read()
        ctx = check(src, ast)
    except RecursionError:
        rec["status"] = "checker_recursion"
        return rec
    except Exception as e:
        import traceback
        rec["status"] = "checker_error"
        rec["err"] = traceback.format_exc()[-600:]
        return rec
    rec["status"] = "ok"
    rec["viol"] = ctx.cnt
    rec["ex"] = ctx.ex
    rec["tally"] = ctx.tally
    rec["secs"] = round(time.time() - t0, 2)
    return rec


def main():
    parser = sys.argv[1]
    files = [l.rstrip("\n") for l in open("/tmp/tc/files.txt", encoding="utf-8") if l.strip()]
    if len(sys.argv) > 3:
        files = files[int(sys.argv[2]):int(sys.argv[3])]
    outp = f"/tmp/tc/structure/results_{parser}.jsonl"
    done = set()
    if os.path.exists(outp):
        for l in open(outp, encoding="utf-8"):
            try:
                done.add(json.loads(l)["file"])
            except Exception:
                pass
    todo = [(parser, f) for f in files if f not in done]
    with open(outp, "a", encoding="utf-8") as fo, Pool(2) as pool:
        for n, rec in enumerate(pool.imap_unordered(one, todo, chunksize=4)):
            fo.write(json.dumps(rec) + "\n")
            fo.flush()
            if n % 200 == 0:
                print(n, len(todo), time.strftime("%X"), flush=True)


if __name__ == "__main__":
    sys.setrecursionlimit(200000)
    main()
