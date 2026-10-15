-- ERROR: match
foo = foo

foo = {}

-- ERROR: match
foo = foo

qux = foo:bar() == foo:baz()

qux = foo:bar() == foo:bar()

local a, b, c = 1, 2, 3

-- a multiple assignment is an assignment of tuples, as in Python, so it is
-- not an instance of $X = $X
a, b, c = a

a = a, b, c
