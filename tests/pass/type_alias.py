from dataclasses import dataclass

temperature = int

a: temperature = 1
b = 1

assert a == b

int_or_float = int | float

c: int_or_float = 10
d: int_or_float = 10.0

assert c == 10
assert d == 10.0


@dataclass
class Point:
    x: int
    y: int


str_or_Point = str | Point

hello = "hello"
e: str_or_Point = "hello"
origin = Point(0, 0)
f: str_or_Point = Point(0, 0)

assert e == "hello"
assert hello == e
assert f == Point(0, 0)
assert origin == f

g = f

assert g == f
assert origin == g
