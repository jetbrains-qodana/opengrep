-- a key of a table constructor is not an assignment
local t = { [math.huge] = 1 }

-- ERROR: match
math.huge = 1
