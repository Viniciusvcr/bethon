from dataclasses import dataclass


@dataclass
class Point:
    x: float
    y: float


p1 = Point(10.0, 20.0)

print(1)
print(10.2)
print("Hello, world!")
print("Hello, " + "world!")

hello_world1 = "Hello, world!"
hello_world2 = "Hello" + "," + " " + "world!"

print(hello_world2)
print(hello_world1 == hello_world2)
print(hello_world1 > hello_world2)
print(hello_world1 < hello_world2)
print(hello_world1 <= hello_world2)
print(hello_world1 >= hello_world2)

print(None)

a = 20
b = 21
print(a)
print(a + 80)
print(a > b)

print(p1)
print(Point(30.0, 40.0))


def foo() -> int:
    return 10


print(foo, foo())
