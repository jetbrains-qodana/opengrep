global <const> g1, g2 = 1, 2
function f()
  -- ruleid: lua-mig-d19-global-const-list
  sink(g2)
end
