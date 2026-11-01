function f()
  local t = source()
  local a, b = t.value, sanitize(t)
  -- ruleid: lua-mig-d19-local-list-evaluation-order
  sink(a)
  -- ok: lua-mig-d19-local-list-evaluation-order
  sink(t.value)
end
