-- the declarations of a local list are sequential: in local x, y = y, x the
-- second value is the new x (Lua reads the previous x)
local x, y = 1, 2
print(x, y)
-- ERROR: match
local x, y = y, x
print(x, y)
local u, v = u, v
