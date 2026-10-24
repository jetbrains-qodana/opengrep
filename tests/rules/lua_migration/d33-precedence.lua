-- ruleid: lua-mig-d33-precedence
local a = 1 + 2 * 3
-- ok: lua-mig-d33-precedence
local b = (1 + 2) * 3
