from dataclasses import dataclass


@dataclass
class IntPoint:
    x: int
    y: int


@dataclass
class FloatPoint:
    x: float
    y: float


Point = IntPoint | FloatPoint


def addPoints(x: Point, y: Point) -> Point:
    if isinstance(x, FloatPoint):
        if isinstance(y, FloatPoint):
            return FloatPoint(x.x + y.x, x.y + y.y)
        return FloatPoint(x.x + y.x, x.y + y.y)
    else:
        if isinstance(y, FloatPoint):
            return FloatPoint(x.x + y.x, x.y + y.y)
        return IntPoint(x.x + y.x, x.y + y.y)


a = FloatPoint(20.0, 10.0)
b = IntPoint(10, 20)

c = FloatPoint(5.0, 5.0)
d = FloatPoint(10.0, 10.0)

assert addPoints(a, b) == FloatPoint(30.0, 30.0)
assert addPoints(b, a) == FloatPoint(30.0, 30.0)
assert addPoints(IntPoint(5, 5), IntPoint(10, 10)) == IntPoint(15, 15)
assert addPoints(c, d) == FloatPoint(15.0, 15.0)
