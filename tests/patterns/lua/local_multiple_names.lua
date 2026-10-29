-- a pattern with two names matches the first two names of a local list
-- ERROR: match
local a, b = f()
local c = f()
-- ERROR: match
local d, e, g = f()
local h, i = 1, 2
