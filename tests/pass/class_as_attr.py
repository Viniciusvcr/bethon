from dataclasses import dataclass


@dataclass
class Point:
    x: float
    y: float


# w_pos has another class as type
@dataclass
class Person:
    name: str
    age: int
    w_pos: Point


# this function calls both constructors
def build_person(name: str, age: int, x: float, y: float) -> Person:
    return Person(name, age, Point(x, y))


# this functions expects an instance of Point
def _build_person(name: str, age: int, w_pos: Point) -> Person:
    return Person(name, age, w_pos)


p1 = build_person("Vinícius", 21, 10.0, 20.0)
p2 = _build_person("Vinícius", 21, Point(30.0, 40.0))

assert p1.name == "Vinícius"
assert p1.age == 21
assert p1.w_pos.x == 10.0
assert p1.w_pos.y == 20.0

assert p2.name == "Vinícius"
assert p2.age == 21
assert p2.w_pos.x == 30.0
assert p2.w_pos.y == 40.0
