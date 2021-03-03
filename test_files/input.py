temperatura = int | float


def f(x: temperatura):
    if isinstance(x, int):
        print("int")
    else:
        print("float")


a = 10
b = 10.0

f(b)
f(a)
