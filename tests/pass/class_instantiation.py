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

# todo add assertion after GetExpression implementation
