-- ok: lua-unpack-not-in-last-position
local ok1 = { b, unpack(a) }
-- ok: lua-unpack-not-in-last-position
local ok2 = f(b, unpack(a))
-- ok: lua-unpack-not-in-last-position
local ok3 = o:m(b, table.unpack(a))
-- ruleid: lua-unpack-not-in-last-position
local bad1 = { unpack(a), b }
-- ruleid: lua-unpack-not-in-last-position
local bad2 = f(unpack(a), b)
-- ruleid: lua-unpack-not-in-last-position
local bad3 = o:m(table.unpack(a), b)
