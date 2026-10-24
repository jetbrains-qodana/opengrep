-- ruleid: lua-mig-d22-explicit-iterator
for k, v in next, t do end
-- ok: lua-mig-d22-explicit-iterator
for k, v in pairs(t) do end
