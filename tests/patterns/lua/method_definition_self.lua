-- function T:m(x) is function T.m(self, x)

-- ERROR: match
function T:m(x)
  return x
end

-- ERROR: match
function T.n(self, x)
  return x
end

function T.k(x)
  return x
end
