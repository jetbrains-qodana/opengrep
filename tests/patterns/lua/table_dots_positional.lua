-- positional fields after '...' can be at any position
-- ERROR: match
local t1 = {a, b}
-- ERROR: match
local t2 = {x, y, b, z}
-- ERROR: match
local t3 = {b}
local t4 = {a, c}
-- the key of b is unknown, so a named field matches too
-- ERROR: match
local t5 = {k = b}
