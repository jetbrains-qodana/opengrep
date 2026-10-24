-- ruleid: lua-mig-d30-precedence
local a = (d30_a + d30_b) * 2
-- ok: lua-mig-d30-precedence
local b = d30_a + d30_b * 2
