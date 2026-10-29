-- ERROR: match
local f, err = io.open("a")

local g = io.open("b")

-- ERROR: match
local h <close>, e2 = io.open("c", "r")
