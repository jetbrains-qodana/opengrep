local a <const>, b = 42, 0
local c <const>, d <const> = 42, 0
function check()
  -- ruleid: lua-mig-d14-local-list-const
  sink(a)
  -- ok: lua-mig-d14-local-list-const
  sink(b)
  -- ruleid: lua-mig-d14-local-list-const
  sink(c)
end
