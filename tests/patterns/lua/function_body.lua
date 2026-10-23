-- a block pattern also matches the body of a function, which goes from the
-- ) of its parameters to its end, not from the keyword function

function f(
  a
-- ERROR: match
)
  x()
end

local g = function(
  a,
  b
-- ERROR: match
) return a end

-- ERROR: match
do
  y()
end
