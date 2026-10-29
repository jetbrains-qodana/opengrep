-- ruleid: lua-mig-d13-global-with-value
global d13_a = 1
-- a list of several names is a tuple assignment
-- ok: lua-mig-d13-global-with-value
global d13_b, d13_c = 1, 2
-- ok: lua-mig-d13-global-with-value
local d13_d = 1
-- ok: lua-mig-d13-global-with-value
d13_e = 1
