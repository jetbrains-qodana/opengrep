-- ruleid: lua-mig-d12-const
local a <const> = 1
-- ok: lua-mig-d12-const
local b = 1
-- ok: lua-mig-d12-const
local c <close> = d12_open()
