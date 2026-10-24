-- ruleid: lua-mig-d21-loop-variable
for k = 1, n do d21_use(k) end
-- ok: lua-mig-d21-loop-variable
for k = 1, n do d21_use(j) end
