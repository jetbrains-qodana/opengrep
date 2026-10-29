-- the values are read before the new names are declared, also when only
-- some names have an attribute
-- ERROR: match
local x, y = 1, 2
local x <const>, y = y, x
