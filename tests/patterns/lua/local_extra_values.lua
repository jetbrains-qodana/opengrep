-- the values without a name are still evaluated: they are not dropped
-- ERROR: match
local a = f(), g()
-- ERROR: match
local b, c = 1, 2, g()
-- ERROR: match
global d = 1, g()
local e = f(), h()
