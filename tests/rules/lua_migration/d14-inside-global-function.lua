global function d14_f()
  -- ruleid: lua-mig-d14-inside-global-function
  d14_call()
end
local function d14_g()
  -- ok: lua-mig-d14-inside-global-function
  d14_call()
end
