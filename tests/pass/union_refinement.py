from dataclasses import dataclass


@dataclass
class Point:
    x: int
    y: int


temperatura = int | str | Point


def f(x: temperatura) -> int:
    if isinstance(x, int):
        return 1
    else:
        if isinstance(x, Point):
            return x.x
        else:
            return 0


p1 = Point(20, 0)

x = f(10)
y = f("test")
z = f(p1)

assert x == 1
assert y == 0
assert z == p1.x
