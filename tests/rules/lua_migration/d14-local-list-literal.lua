local a, b = "unsafe", 0
-- ruleid: lua-mig-d14-local-list-literal
sink(a)
local c, d = 1, "unsafe"
-- ruleid: lua-mig-d14-local-list-literal
sink(d)
-- ok: lua-mig-d14-local-list-literal
sink(c)
