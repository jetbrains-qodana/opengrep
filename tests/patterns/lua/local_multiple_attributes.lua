-- the attribute of a name in a local list is the attribute of that name only
-- ERROR: match
local f <close>, err = io.open(p)
local g, err2 <close> = io.open(q)
-- ERROR: match
local a, h <close> = 1, io.open(r)
local i <const>, j = 1, io.open(s)
