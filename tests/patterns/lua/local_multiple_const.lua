-- the attribute of a name in a local list is the attribute of that name only
-- ERROR: match
local a <const>, b = 1, 2
-- ERROR: match
local c, d <const> = 1, 2
local e, f = 1, 2
-- ERROR: match
local <const> g, h = 1, 2
