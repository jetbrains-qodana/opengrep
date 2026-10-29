local run <const>, unused = os.execute, 0
function g(cmd)
  -- ruleid: lua-mig-d18-local-list-const-symbolic
  run(cmd)
end
