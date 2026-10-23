"""Compare reference facts with AST facts.
compare(ref, ast) -> {cat: {"expected", "actual", "matched", "missing", "extra", "desc_mismatch"}},
                     list of discrepancy records (cat, kind, pos, ref_desc, ast_desc)"""
from collections import defaultdict

# categories compared (others are informative only)
CATS = ["func_global", "func_local", "func_method", "func_anon", "param", "call", "method_call",
        "table", "field_pos", "field_named", "field_bracket", "local_var", "assign", "return",
        "if", "elseif", "while", "repeat", "do", "for_num", "for_gen", "for_var", "break", "goto",
        "label", "binop", "unop", "string", "number", "bool", "nil", "vararg", "dot_access",
        "index_access", "ident"]
SHAPE = ["binop_shape", "unop_shape", "call_shape"]
SCOPE = ["scope:" + c for c in ["func_global", "func_local", "func_method", "func_anon", "call", "method_call",
         "table", "local_var", "assign", "return", "if", "while", "repeat", "for_num", "for_gen", "binop", "string"]]


def compare(ref, ast):
    stats = {}
    disc = []
    R = defaultdict(lambda: defaultdict(list))
    A = defaultdict(lambda: defaultdict(list))
    for c, p, d in ref:
        R[c][p].append(str(d))
    for c, p, d in ast:
        A[c][p].append(str(d))
    for c in set(R) | set(A):
        st = dict(expected=0, actual=0, matched=0, missing=0, extra=0, desc_mismatch=0)
        rc, ac = R.get(c, {}), A.get(c, {})
        for p in set(rc) | set(ac):
            rl, al = sorted(rc.get(p, [])), sorted(ac.get(p, []))
            st["expected"] += len(rl)
            st["actual"] += len(al)
            # exact matches first
            al2 = list(al)
            rest_r = []
            for d in rl:
                if d in al2:
                    al2.remove(d)
                    st["matched"] += 1
                else:
                    rest_r.append(d)
            # same position, different descriptor
            while rest_r and al2:
                dr, da = rest_r.pop(0), al2.pop(0)
                st["desc_mismatch"] += 1
                disc.append((c, "desc", p, dr, da))
            for d in rest_r:
                st["missing"] += 1
                disc.append((c, "missing", p, d, None))
            for d in al2:
                st["extra"] += 1
                disc.append((c, "extra", p, None, d))
        stats[c] = st
    return stats, disc
