-- ruleid: lua-mig-d08-first-newline
d08_eq([[
abc]])
-- ruleid: lua-mig-d08-first-newline
d08_eq([=[abc]=])
-- ok: lua-mig-d08-first-newline
d08_eq([[ abc]])
