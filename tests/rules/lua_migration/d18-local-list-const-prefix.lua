local a <const>, b = 42, 0, side_effect()
local c <const>, d, e = 42, f()
function check()
  -- ruleid: lua-mig-d18-local-list-const-prefix
  sink(a)
  -- ruleid: lua-mig-d18-local-list-const-prefix
  sink(c)
end
