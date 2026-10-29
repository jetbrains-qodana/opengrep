-- each name gets the value at its position, the extra values are still evaluated
-- ERROR: match
local a, b = g(), 1
-- ERROR: match
local c, d = 1, g()
-- ERROR: match
local e, h = f(), 2, g()
local i, j = f()
-- ERROR: match
local k = f(), g()
