-- TODO: only x has <const>, so the list is one definition per name and,
-- as before the tuple definitions, the 2nd value reads the new x
local x, y = 1, 2
-- ERROR: match
local x <const>, y = y, x
