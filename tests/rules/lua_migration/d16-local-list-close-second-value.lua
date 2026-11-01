local f <close>, err = source()
-- ruleid: lua-mig-d16-local-list-close-second-value
sink(err)
local g <close>, e2 = safe()
-- ok: lua-mig-d16-local-list-close-second-value
sink(e2)
