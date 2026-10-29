-- a pattern with one name matches each name of a local list with its value
-- ERROR: match
local f, err = io.open(p)
-- ERROR: match
local g = io.open(q)
-- ERROR: match
local a, b, c = io.open(r)
-- ERROR: match
local d, e = x, io.open(s)
local h, i = io.close(t)
