-- the last call gives the values of b and c
-- ERROR: match
local a, b, c = x, f()
-- ok:
local d, e, g = x, y, f()
