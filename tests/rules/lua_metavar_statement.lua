-- ruleid: lua-identical-branches
if c then x() else x() end
-- ok: lua-identical-branches
if c then x() else y() end

-- the match starts at the statement before the function
-- ruleid: lua-statement-before-function
local a = 1
function target() end

function g()
  -- ruleid: lua-last-local-of-block
  local unused = compute()
end

function h()
  -- ok: lua-last-local-of-block
  local used = compute()
  print(used)
end
