import json
st=json.load(open('stats.json'))
cats={c:{k:v.get(k,0) for k in ('expected','actual','matched','missing','extra','mismatch')} for c,v in st['new'].items()}
json.dump({
 "method": "lexical oracle: independent Python Lua lexer + token-level analysis keyed by byte position, compared with constructs extracted from the opengrep-core -full_token_info -dump_ast -json AST (new parser), also run on the old parser",
 "files_processed": 5349, "files_total": 5352, "files_without_ast": 3,
 "files_with_parse_errors": 6,
 "categories": cats,
 "categories_scope": "new parser, the 5346 files without parse errors; 'mismatch' = same position, different value (arity, kind, field counts, literal value)",
 "real_bugs": [{
   "title": "local declaration with more values than names drops the extra values (mk_vars in Parse_lua_tree_sitter.ml); not a regression, the old parser does the same",
   "reproducer": "local a = f(), g()   -- AST: DefStmt a = f(); the call g() is absent from the AST (also: local b, c = 1, 2, h() drops h())",
   "count": 4}],
 "old_parser": {"files_with_parse_errors": 346, "categories": st['old']}
}, open('summary.json','w'), indent=1)
