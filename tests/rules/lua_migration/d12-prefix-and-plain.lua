-- local <const> a, b = 1, 2 is one definition of the pattern of its names, as Lua
-- assigns all the values at once: the one-name pattern local $X = 2
-- matches one-name declarations only, a list is matched by
-- local $X, $Y = $V, 2
-- ok: lua-mig-d12-prefix-and-plain
local <const> a, b = 1, 2
-- ruleid: lua-mig-d12-prefix-and-plain
local c <const> = 2
-- ruleid: lua-mig-d12-prefix-and-plain
local d <close> = 2
