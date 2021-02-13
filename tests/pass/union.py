from dataclasses import dataclass

# Info: Unions are defined like in Python 3.10

a: int | str = 10  # can be 10 because of int
b: int | str = "test"  # can be "test" because of str

c: 10 | bool = 10
d: 10 | bool = True

assert a == 10
assert b == "test"
assert c == 10
assert d == True


def foo(x: float | int) -> float | int:
    return x


# todo add unions to comparisons
# assert foo(10.0) == 10.0


@dataclass
class IntOrFloat:
    x: int | float


e = IntOrFloat(10)
f = IntOrFloat(10.0)
