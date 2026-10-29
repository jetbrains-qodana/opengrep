-- a list where only f has <close> is a two-name definition; f is final,
-- which a pattern does not test
-- ERROR: match
local f <close>, err = io.open(p)
-- ERROR: match
local g, err2 = io.open(p)
