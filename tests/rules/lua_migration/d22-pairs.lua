-- ruleid: lua-mig-d22-pairs
for k, v in pairs(t) do end
-- ok: lua-mig-d22-pairs
for k, v in ipairs(t) do end
