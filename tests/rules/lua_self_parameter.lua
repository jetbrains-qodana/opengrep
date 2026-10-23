-- self is the name of a parameter, explicit or implicit in T:m()

local function k(self)
  -- ok: assignment-to-undeclared-global
  self = 1
end

function T:m()
  -- ok: assignment-to-undeclared-global
  self = 2
end

local function p(a)
  -- ok: assignment-to-undeclared-global
  a = 3
end

-- ruleid: assignment-to-undeclared-global
undeclared = 4
