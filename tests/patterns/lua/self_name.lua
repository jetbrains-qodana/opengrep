-- self is the name of the first parameter of a method, so the self it
-- returns is that parameter

-- ERROR: match
function T:m()
  return self
end

-- ERROR: match
function T.n(s)
  return s
end

function T:k(x)
  return x
end

function T.j(s)
  return self
end
