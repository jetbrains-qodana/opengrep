-- ERROR: match
f, err = io.open("a")

f = io.open("b")

-- ERROR: match
g, err = io.open("c", "r")
