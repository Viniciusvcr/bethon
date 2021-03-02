from dataclasses import dataclass

# Info: Unions are defined like in Python 3.10

a: int | str = 10  # can be 10 because of int
b: int | str = "test"  # can be "test" because of str

assert a == 10
assert b == "test"


def foo(x: float | int) -> float | int:
    return x


@dataclass
class IntOrFloat:
    x: int | float


e = IntOrFloat(10)
f = IntOrFloat(10.0)
