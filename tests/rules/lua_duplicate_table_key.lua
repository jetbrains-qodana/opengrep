-- positional fields keep their position across '...' in a pattern:
-- '{[1] = $B, ..., $X1, ...}' is about the first positional field

-- ok: lua-duplicate-table-key
local ok1 = { [1] = "a", [2] = "b" }
-- ok: lua-duplicate-table-key
local ok2 = { "a", "b", [3] = "c" }
-- ruleid: lua-duplicate-table-key
local bad1 = { "a", [1] = "b" }
-- ruleid: lua-duplicate-table-key
local bad2 = { [1] = "a", "b" }
-- ruleid: lua-duplicate-table-key
local bad3 = { x = 1, y = 2, x = 3 }
-- ruleid: lua-duplicate-table-key
local bad4 = { "a", "b", [2] = "c" }
