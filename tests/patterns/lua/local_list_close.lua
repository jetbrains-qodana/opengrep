-- the <close> of a name of a list is kept
-- ERROR: match
local f <close>, err = io.open(p)
local g, err2 = io.open(p)
