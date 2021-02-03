from dataclasses import dataclass


# dataclasses create a default constructor respecting positional arguments
@dataclass
class Point:
    x: float
    y: float


# implicit type declaration with wrong arity (insufficient)
p1 = Point(10.0)
