local a <const>, b <const> = 42, 0
function check()
  -- ruleid: lua-mig-d14-local-list-const
  sink(a)
end
-- a definition of several names has the attributes that all the names
-- have, so here a is not known to be <const>
local c <const>, d = 42, 0
function check2()
  -- todoruleid: lua-mig-d14-local-list-const
  sink(c)
  -- ok: lua-mig-d14-local-list-const
  sink(d)
end
