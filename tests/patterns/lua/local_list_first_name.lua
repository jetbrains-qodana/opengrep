-- ERROR: match
local a = f(), g()
-- ok: a list of two names is not a one-name declaration
local b, c = f()
-- ok:
local d, e = f(), g()
