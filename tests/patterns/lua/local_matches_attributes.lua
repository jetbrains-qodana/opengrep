-- a plain pattern matches declarations with attributes
-- ERROR: match
local a <const> = 2
-- ERROR: match
local b <close> = 2
-- ERROR: match
local c = 2
local d = 3
