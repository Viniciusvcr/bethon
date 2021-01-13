def foo():
    b = 2
    assert b == 2


b = 5
foo()
assert b != 2
assert b == 5
