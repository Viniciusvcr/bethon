from dataclasses import dataclass


# dataclasses create a default constructor respecting positional arguments
@dataclass
class Point:
    x: float
    y: float


# implicit type declaration
p1 = Point(10.0, 20.0)
# explicit
p2: Point = Point(20.0, 30.0)

assert p1.x == 10.0
assert p1.y == 20.0
assert p2.x == 20.0
assert p2.y == 30.0
