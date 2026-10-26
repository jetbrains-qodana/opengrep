local a = "--[[ not a comment"
local b = 'x\n--[=[ neither'
-- ERROR: match
after()
local c = "]] ]=]"
