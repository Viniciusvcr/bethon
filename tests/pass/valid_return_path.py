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
