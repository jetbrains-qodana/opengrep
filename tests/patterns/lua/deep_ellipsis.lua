-- ERROR: match
sink(wrap(source()))
-- ERROR: match
sink("a" .. source() .. "b")
sink(other())
