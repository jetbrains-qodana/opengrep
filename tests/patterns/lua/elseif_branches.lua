if a then
  other()
elseif b then
  -- ERROR: match
  sink(1)
else
  other()
end

if a then
  other()
elseif b then
  -- ERROR: match
  sink(2)
elseif c then
  -- ERROR: match
  sink(3)
end
