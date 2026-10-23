-- Lua 5.4 attributes of local variables
local x <const> = 42
local f <close> = io.open("file")
local a <const>, b, c <close> = 1, 2, setmetatable({}, { __close = print })
-- Lua 5.5: an attribute before the names applies to all of them
local <const> d, e = 3, 4
