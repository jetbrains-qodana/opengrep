local x = source()
local x, y = x, 0
-- ruleid: lua-mig-d14-local-list-taint-outer-value
sink(x)
-- ok: lua-mig-d14-local-list-taint-outer-value
sink(y)
