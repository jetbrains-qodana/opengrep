-- local a, b = 1, 2 is one definition of the pattern of its names, as Lua
-- assigns all the values at once: the one-name pattern local $X = 2
-- matches one-name declarations only, a list is matched by
-- local $X, $Y = $V, 2
-- ok: lua-mig-d11-second-value
local a, b = 1, 2
-- ok: lua-mig-d11-second-value
local c = 1
