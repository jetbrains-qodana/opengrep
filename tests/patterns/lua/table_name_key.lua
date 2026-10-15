-- { x = v } is { ["x"] = v }

-- ERROR: match
local a = { x = 1 }
-- ERROR: match
local b = { ["x"] = 1 }
local c = { y = 1 }
