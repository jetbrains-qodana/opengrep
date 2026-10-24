-- a pattern starting with # is the length operator, not a shebang line
-- ERROR: match
if #t > 0 then end
-- ERROR: match
local nonempty = #list > 0
if #t == 0 then end
