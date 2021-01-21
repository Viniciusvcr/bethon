def f(x: bool) -> str:
    if x:
        return "teste"
    else:
        b = "10"

    return b


def g(x: bool) -> int:
    if (x):
        return 10
    else:
        return 20


def h() -> int:
    return 10


def i(x: bool) -> int:
    if x:
        if x:
            return 10
        else:
            return 20
    else:
        return 30


def j(x: bool) -> int:
    if x:
        if x:
            return 10
        else:
            return 20

    return 30


def k():
    a = 10


def l(a: int) -> int:
    if a == 2:
        return 2
    else:
        c = 10

    if a == 10:
        return 10
    else:
        return a


# dead_code, but valid
def m(a: int) -> int:
    if a == 2:
        return 2
    else:
        return 10

    if a == 10:
        c = 10
    else:
        return a
