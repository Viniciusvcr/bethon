from dataclasses import dataclass


@dataclass
class Point:
    x: float
    y: float


p1 = Point(10.0, 20.0)
p2: Point = Point(30.0, 40.0)

p1
p2
