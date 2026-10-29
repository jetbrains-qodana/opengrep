-- a local list is one declaration per name: the statement after the
-- second name follows a pattern of two names
-- ERROR: match
local a, b = f()
if not a then return end
local c, d, e = f()
if not c then return end
local g, h = f()
print(g)
