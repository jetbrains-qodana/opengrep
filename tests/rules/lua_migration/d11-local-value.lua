-- ruleid: lua-mig-d11-local-value
local a = d11_v
-- local b, c = d11_v, 2 is one definition of the pattern of its names, as Lua
-- assigns all the values at once: the one-name pattern local $X = d11_v
-- matches one-name declarations only, a list is matched by
-- local $X, $Y = d11_v, $V
-- ok: lua-mig-d11-local-value
local b, c = d11_v, 2
-- ok: lua-mig-d11-local-value
d11_g = d11_v
