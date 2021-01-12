a = 2

if a == 2:
    b = 10
else:
    b = 15

# b should exist in the global environment to match Python's behaviour
assert b == 10
