-- the values are read before the new names are declared, also when only
-- some names have an attribute
-- ERROR: match
local x, y = 1, 2
local x <const>, y = y, x

-- the <const> of the pattern needs a <const> or <close> (final does not
-- tell them apart) on that name of the list
local p, q = 1, 2
local p, q = q, p
