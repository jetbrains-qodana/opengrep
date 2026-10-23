-- an assignment pattern matches global and local declarations
-- ERROR: match
global a = secret
-- ERROR: match
local b = secret
-- ERROR: match
c = secret
d = public
