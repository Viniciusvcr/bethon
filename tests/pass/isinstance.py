from dataclasses import dataclass

temp = int | float


def f(x: temp) -> int:
    if isinstance(x, int):
        return 1
    else:
        return 0


assert f(10) == 1
assert f(10.0) == 0


def g() -> int:
    return 0


@dataclass
class Point:
    x: int
    y: int


@dataclass
class APoint:
    x: int | float
    y: int | float


a = 10
b = g()
p1 = Point(0, 0)
ap1 = APoint(0, 10.0)
ap2 = APoint(0.0, 10)


assert isinstance(b, int)
assert isinstance(a, int)
assert not isinstance(a, float)
assert not isinstance(a, str)
assert not isinstance(a, bool)

assert isinstance(p1, Point)
assert not isinstance(p1, int)
assert isinstance(p1.x, int)
assert isinstance(p1.y, int)


assert isinstance(ap1, APoint)
assert isinstance(ap1.x, int)
assert not isinstance(ap1.x, float)
assert isinstance(ap1.y, float)
assert not isinstance(ap1.y, int)

assert isinstance(ap2, APoint)
assert isinstance(ap2.x, float)
assert not isinstance(ap2.x, int)
assert isinstance(ap2.y, int)
assert not isinstance(ap2.y, float)
