-- a name with <close> (or <const>: final does not tell them apart) in the
-- pattern matches only a name with one of them in the list
-- ERROR: match
local f <close>, err = io.open(p)
local g, err2 = io.open(p)
local h, k <close> = io.open(p)
-- ERROR: match
local m <const>, n = io.open(p)
