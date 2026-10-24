local a = "--[[ not a comment"
local b = "x\n--[=[ not a comment either"
-- ruleid: lua-mig-d07-comment-start-in-string
d07_after()
local c = "]] ]=]"
