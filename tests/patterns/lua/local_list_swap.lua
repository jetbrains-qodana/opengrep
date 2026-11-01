-- the values are read before the new names are declared
-- ERROR: match
local x, y = 1, 2
local x, y = y, x
return x, y
