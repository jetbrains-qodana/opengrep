local x, y = taint_source(), 1
-- ruleid: lua-mig-d15-local-list-taint-first-value
sink(x)
-- ok: lua-mig-d15-local-list-taint-first-value
sink(y)
