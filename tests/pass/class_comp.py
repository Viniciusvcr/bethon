from dataclasses import dataclass


@dataclass
class Point:
    x: float
    y: float


@dataclass
class Test:
    x: int


@dataclass
class Point2:
    x: float
    y: float


p1 = Point(10.0, 20.0)
p2 = Point(10.0, 20.0)
p3 = Point(0.0, 0.0)
p4 = Point2(10.0, 20.0)
t1 = Test(20)


assert p1 == p2
assert not (p1 != p2)
assert p1 != p3
assert not (p1 == p3)
assert p1 != t1
assert not (p1 == t1)
assert not (p1 == p4)
assert p1 != p4
