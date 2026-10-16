-- ERROR: match
foo = foo

foo = {}

-- ERROR: match
foo = foo

qux = foo:bar() == foo:baz()

qux = foo:bar() == foo:bar()

local a, b, c = 1, 2, 3

-- a multiple assignment is an assignment of tuples, as in Python: a, b, c = a
-- is (a, b, c) = (a, nil, nil), which assigns a to a
-- ERROR: match
a, b, c = a

-- x = a, b, c is (x) = (a, b, c), which is not an instance of $X = $X
a = a, b, c
