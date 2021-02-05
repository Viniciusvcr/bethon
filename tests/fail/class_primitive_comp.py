from dataclasses import dataclass


def foo():
    return None


@dataclass
class Point:
    x: float
    y: float


p1 = Point(10.0, 20.0)


print(p1 != 10)
print(p1 == 10)


print(p1 != 10.0)
print(p1 == 10.0)


print(p1 != "10")
print(p1 == "10")

print(p1 == None)
print(p1 != None)

print(p1 == True)
print(p1 != False)

print(p1 == foo)
print(p1 != foo)

print(p1 == foo())
print(p1 != foo())
