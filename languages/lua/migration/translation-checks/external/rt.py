import sys, json, subprocess
sys.path.insert(0, '/tmp/tc/external')
from extract_ast import extract, load_dump
from compare import compare
f = sys.argv[1]; binary = sys.argv[2] if len(sys.argv) > 2 else '/src/bin/opengrep-core'
cf = f.replace('/tmp', '/host-tmp', 1)
subprocess.run(['node', '/tmp/tc/external/extract_ref.js', '/dev/stdin', '/tmp/tc/external/rt.jsonl'], input=cf + "\n", text=True, check=True)
ref = json.loads(open('/tmp/tc/external/rt.jsonl').readline())
out = subprocess.run(['docker', 'exec', 'ogb', binary, '-lang', 'lua', '-full_token_info', '-dump_ast', '-json', cf], capture_output=True, text=True)
facts, notes = extract(load_dump(out.stdout))
print('rc', out.returncode, ref.get('ok'), ref.get('error'))
st, disc = compare(ref['facts'], facts)
for c in sorted(st): print(c, st[c])
for d in disc: print(d)
print(notes)
