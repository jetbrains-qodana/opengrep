-- the values are read before the new names are declared, also when only
-- some names have an attribute
-- ERROR: match
local x, y = 1, 2
local x <const>, y = y, x

-- known limitation: per-name attributes are not matched, so the <const> of
-- the pattern also matches a list without it
-- ERROR: match
local p, q = 1, 2
local p, q = q, p
