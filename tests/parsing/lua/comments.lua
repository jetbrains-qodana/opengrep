-- line comment
--[[ block
comment ]]
--[==[ block comment with ]] inside ]==]
-- [[ this is a line comment, not a block comment
print("not hidden")
-- ]]
local x = 1 -- trailing comment
local y = --[[ inline ]] 2
