local a, b = 1, 2
-- ruleid: lua-mig-d15-local-list-numeric-const
sink(a)
-- ruleid: lua-mig-d15-local-list-numeric-const
sink(b)
local c, d = 3, 4
-- ok: lua-mig-d15-local-list-numeric-const
sink(c)
