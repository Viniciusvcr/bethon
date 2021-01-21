def f(x: bool) -> str:
    if x:
        return "teste"
    else:
        b = "10"


def g(x: bool) -> int:
    if (x):
        a = 20
    else:
        return 20


def h() -> int:
    s = 10


def i(x: bool) -> int:
    if x:
        if x:
            return 10
        else:
            a = 20
    else:
        return 30


def j(x: bool) -> int:
    if x:
        if x:
            return 10
        else:
            return 20
