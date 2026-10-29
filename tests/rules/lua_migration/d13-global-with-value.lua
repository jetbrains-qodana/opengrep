-- ruleid: lua-mig-d13-global-with-value
global d13_a = 1
-- global d13_b, d13_c = 1, 2 is one definition of the pattern of its names, as Lua
-- assigns all the values at once: the one-name pattern global $X = $V
-- matches one-name declarations only, a list is matched by
-- global $X, $Y = $V, $W
-- ok: lua-mig-d13-global-with-value
global d13_b, d13_c = 1, 2
-- ok: lua-mig-d13-global-with-value
local d13_d = 1
-- ok: lua-mig-d13-global-with-value
d13_e = 1
