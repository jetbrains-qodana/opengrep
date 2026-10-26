-- ERROR: match
function f(...args) return args end
function g(...args) return 1 end
-- the unnamed vararg is a rest parameter named '...'
-- ERROR: match
function h(...) return ... end
