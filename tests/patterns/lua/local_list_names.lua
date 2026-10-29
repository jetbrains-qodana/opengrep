-- ERROR: match
local a, b = f()
-- ERROR: match
local fd <close>, err = io.open(p)
-- ok: three names
local c, d, e = x, f()
-- ok: two values
local g, h = f(), g()
-- ok: one name
local i = f(), g()
