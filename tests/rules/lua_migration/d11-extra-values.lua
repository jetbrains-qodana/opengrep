-- ruleid: lua-mig-d11-extra-values
local a = 1, d11_side_effect()
-- ruleid: lua-mig-d11-extra-values
local b, c = 1, 2, d11_side_effect()
-- ok: lua-mig-d11-extra-values
local d = 1, d11_other()
