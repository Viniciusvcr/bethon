from dataclasses import dataclass


@dataclass
class Point:
    x: float
    y: float


p1 = Point(10.0, 20.0)

print(1)
print(10.2)
print("Hello, world!")
print(None)

a = 20
print(a)
print(a + 80)

print(p1)
print(Point(30.0, 40.0))


def foo() -> int:
    return 10


print(foo, foo())
