-- ruleid: lua-mig-d32-ellipsis-and-keys
d32_e({ a = 1, debug = true; b = 2 })
-- ruleid: lua-mig-d32-ellipsis-and-keys
d32_e({ ["debug"] = true })
-- ok: lua-mig-d32-ellipsis-and-keys
d32_e({ debug = false })
