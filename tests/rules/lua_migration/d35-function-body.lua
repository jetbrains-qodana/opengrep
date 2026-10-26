-- ruleid: lua-mig-d35-function-body
function d35_f(a)
  local x = a
  d35_danger()
end
-- ok: lua-mig-d35-function-body
function d35_g(a)
  d35_safe()
end
