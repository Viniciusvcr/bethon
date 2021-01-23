def f() -> int:
    return g()


# cannot call 'f' because it's depending on definition of 'g', which has not yet occurred
f()


def g() -> int:
    return a


a = 10
