-- every value is kept
-- ERROR: match
local a = f(), g()
-- ERROR: match
local b, c = f(), g()
-- ERROR: match
local d, e, h = x, g()
