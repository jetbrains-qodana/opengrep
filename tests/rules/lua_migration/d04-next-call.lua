-- ruleid: lua-mig-d04-next-call
local k = next(d04_t)
-- ruleid: lua-mig-d04-next-call
if next(d04_t, nil) == nil then end
-- ok: lua-mig-d04-next-call
nexty(d04_t)
