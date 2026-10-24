-- Lua 5.5 global declarations
global print, string
global <const> pi = 3.14
global counter, total = 0, 0
global function helper(...args)
  return select("#", ...), args
end
do
  global *
  undeclared = 1
end
do
  global <const> *
end
-- 'global' is still a name elsewhere
local global = 1
local t = { global = global }
print(global, t.global)
