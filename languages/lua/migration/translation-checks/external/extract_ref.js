// Reference extractor: parses Lua files with luaparse and prints, per file, a
// JSON line {file, ok, version, error, facts: [[cat, pos, desc], ...]}.
// pos = byte offset (file read as latin1 so that JS indices are bytes).
// Usage: node extract_ref.js LISTFILE OUT.jsonl   (container paths in LIST)
const lp = require("luaparse");
const fs = require("fs");
const readline = require("readline");

const [listFile, outFile] = process.argv.slice(2);
const out = fs.createWriteStream(outFile);

function skipWsComments(src, i, alsoChars) {
  for (;;) {
    while (i < src.length && (/\s/.test(src[i]) || (alsoChars && alsoChars.includes(src[i])))) i++;
    if (src.startsWith("--", i)) {
      const m = /^--\[(=*)\[/.exec(src.slice(i, i + 64));
      if (m) {
        const close = "]" + m[1] + "]";
        const j = src.indexOf(close, i + m[0].length);
        i = j < 0 ? src.length : j + close.length;
      } else {
        const j = src.indexOf("\n", i);
        i = j < 0 ? src.length : j + 1;
      }
      continue;
    }
    return i;
  }
}
// first real token of an expression (luaparse ranges may start at a '(' of
// a parenthesized sub-expression; AST_generic drops parentheses)
function coreStart(src, n) { return skipWsComments(src, n.range[0], "("); }

function analyze(src) {
  const facts = [];
  const fnStack = [-1];
  const F = (c, p, d) => { if (SCOPED.has(c)) facts.push(["scope:" + c, p, fnStack[fnStack.length - 1]]); F0(c, p, d); };
  const F0 = (c, p, d) => facts.push([c, p, d === undefined ? "" : (typeof d === "string" && /[\x80-\xff]/.test(d) ? Buffer.from(d, "latin1").toString("utf8") : d)]);
  const cs = (x) => coreStart(src, x);
  function nameOf(n) {
    if (!n) return "?";
    if (n.type === "Identifier") return n.name;
    if (n.type === "MemberExpression") return nameOf(n.base) + n.indexer + n.identifier.name;
    return "?";
  }
  function params(ps) {
    for (const p of ps) {
      if (p.type === "Identifier") F("param", p.range[0], p.name);
      else F("param", p.range[0], "...");
    }
  }
  function visit(n) {
    if (!n || typeof n !== "object") return;
    if (Array.isArray(n)) { n.forEach(visit); return; }
    switch (n.type) {
      case "FunctionDeclaration": {
        const np = n.parameters.length;
        if (!n.identifier) F("func_anon", n.range[0], np);
        else {
          const id = n.identifier;
          const last = id.type === "MemberExpression" ? id.identifier : id;
          for (let x = id; x && x.type === "MemberExpression"; x = x.base) x._fname = true;
          let kind = n.isLocal ? "func_local" : (id.type === "MemberExpression" && id.indexer === ":" ? "func_method" : "func_global");
          F(kind, last.range[0], nameOf(id) + "/" + np);
        }
        params(n.parameters);
        fnStack.push(n.isLocal && n.identifier ? skipWsComments(src, n.range[0] + 5) : n.range[0]);
        for (const k in n) if (k !== "range" && k !== "type") { const v = n[k]; if (v && typeof v === "object") visit(v); }
        fnStack.pop();
        return;
      }
      case "LocalStatement":
        n.variables.forEach((v, i) => F("local_var", v.range[0], v.name + (i < n.init.length ? "=" : "")));
        break;
      case "AssignmentStatement":
        F("assign", coreStart(src, n.variables[0]), n.variables.length + "=" + n.init.length);
        break;
      case "ReturnStatement":
        F("return", n.range[0], n.arguments.length);
        break;
      case "IfStatement": {
        const ne = n.clauses.filter(c => c.type === "ElseifClause").length;
        const he = n.clauses.some(c => c.type === "ElseClause");
        F("if", n.range[0], ne + (he ? "+else" : ""));
        for (const c of n.clauses) if (c.type === "ElseifClause") F("elseif", c.range[0]);
        break;
      }
      case "WhileStatement": F("while", n.range[0]); break;
      case "RepeatStatement": F("repeat", n.range[0]); break;
      case "DoStatement": F("do", n.range[0]); break;
      case "ForNumericStatement":
        F("for_num", n.range[0], n.step ? 3 : 2);
        F("for_var", n.variable.range[0], n.variable.name);
        break;
      case "ForGenericStatement":
        F("for_gen", n.range[0], n.variables.length + "in" + n.iterators.length);
        n.variables.forEach(v => F("for_var", v.range[0], v.name));
        break;
      case "BreakStatement": F("break", n.range[0]); break;
      case "GotoStatement": F("goto", n.range[0], n.label.name); break;
      case "LabelStatement": F("label", n.label.range[0], n.label.name); break;
      case "CallExpression": case "StringCallExpression": case "TableCallExpression": {
        const nargs = n.type === "CallExpression" ? n.arguments.length : 1;
        const args = n.type === "CallExpression" ? n.arguments : [n.argument || n.arguments];
        const meth = n.base.type === "MemberExpression" && n.base.indexer === ":";
        if (meth) F("method_call", n.range[1], n.base.identifier.name + "/" + nargs);
        else F("call", n.range[1], nargs);
        F("call_shape", n.range[1], cs(meth ? n.base.base : n.base) + ";" + args.map(cs).join(","));
        break;
      }
      case "MemberExpression":
        if (n.indexer === "." && !n._fname) F("dot_access", n.identifier.range[0], n.identifier.name);
        break;
      case "IndexExpression": F("index_access", coreStart(src, n.index)); break;
      case "TableConstructorExpression":
        F("table", n.range[0], n.fields.length);
        for (const f of n.fields) {
          if (f.type === "TableValue") F("field_pos", coreStart(src, f.value));
          else if (f.type === "TableKeyString") F("field_named", f.key.range[0], f.key.name);
          else F("field_bracket", coreStart(src, f.key));
        }
        break;
      case "BinaryExpression": case "LogicalExpression": {
        const i = skipWsComments(src, n.left.range[1], ")");
        F("binop", i, n.operator);
        F("binop_shape", i, cs(n.left) + "," + cs(n.right));
        break;
      }
      case "UnaryExpression": F("unop", n.range[0], n.operator); F("unop_shape", n.range[0], cs(n.argument)); break;
      case "StringLiteral": F("string", n.range[0], stringValue(n.raw)); break;
      case "NumericLiteral": {
        F("number", n.range[0], n.raw);
        const r = n.raw.toLowerCase();
        const hex = r.startsWith("0x");
        F("number_kind", n.range[0], /i$/.test(r) ? "Imag" : (hex ? /[.p]/.test(r) : /[.e]/.test(r)) ? "Float" : "Int");
        break;
      }
      case "BooleanLiteral": F("bool", n.range[0], n.raw); break;
      case "NilLiteral": F("nil", n.range[0]); break;
      case "VarargLiteral": F("vararg", n.range[0]); break;
      case "Identifier": F("ident", n.range[0], n.name); break;
    }
    for (const k in n) if (k !== "range" && k !== "type" && k !== "_fname") {
      const v = n[k];
      if (v && typeof v === "object") visit(v);
    }
  }
  visit(lp_ast);
  return facts;
}
let lp_ast = null;
const SCOPED = new Set(["call", "method_call", "assign", "local_var", "return", "if", "while", "repeat", "for_num", "for_gen", "func_anon", "func_global", "func_local", "func_method", "table", "binop", "string"]);
function stringValue(raw) {
  if (raw[0] === '"' || raw[0] === "'") return raw.slice(1, -1);
  const m = /^\[(=*)\[/.exec(raw);
  let s = raw.slice(m[0].length, raw.length - m[0].length);
  s = s.replace(/\r\n|\n\r|\r/g, "\n");
  if (s.startsWith("\n")) s = s.slice(1);
  return s;
}

const versions = ["5.3", "LuaJIT", "5.1"];
(async () => {
  const rl = readline.createInterface({ input: fs.createReadStream(listFile) });
  for await (const line of rl) {
    if (!line) continue;
    const host = line.replace(/^\/host-tmp/, "/tmp");
    let src;
    try { src = fs.readFileSync(host, "latin1"); } catch (e) { out.write(JSON.stringify({ file: line, ok: false, error: "read: " + e.message }) + "\n"); continue; }
    // shebang: blank it, keeping the offsets
    if (src.startsWith("#")) { const j = src.indexOf("\n"); src = " ".repeat(j < 0 ? src.length : j) + (j < 0 ? "" : src.slice(j)); }
    let res = null, err = null;
    for (const v of versions) {
      try {
        lp_ast = lp.parse(src, { ranges: true, comments: false, luaVersion: v, encodingMode: "pseudo-latin1", extendedIdentifiers: true, scope: false });
        res = { file: line, ok: true, version: v, facts: analyze(src) };
        break;
      } catch (e) { if (!err) err = String(e.message || e); }
    }
    if (!res) res = { file: line, ok: false, error: err };
    out.write(JSON.stringify(res) + "\n");
  }
  out.end();
})();
