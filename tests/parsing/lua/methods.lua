local T = {}
function T.new(x) return setmetatable({ x = x }, { __index = T }) end
function T:get() return self.x end
function T.a.b:c(y) self.y = y end
local self = T.new(1)
local next = next
print(self:get(), next({}))
