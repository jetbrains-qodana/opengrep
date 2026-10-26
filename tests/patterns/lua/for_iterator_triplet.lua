-- the iterator, state and initial value are all kept
-- ERROR: match
for k, v in next, t do end
for k, v in pairs(t) do end
