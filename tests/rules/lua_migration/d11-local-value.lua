-- ruleid: lua-mig-d11-local-value
local a = d11_v
-- a list of several names is a tuple assignment
-- ok: lua-mig-d11-local-value
local b, c = d11_v, 2
-- ok: lua-mig-d11-local-value
d11_g = d11_v
