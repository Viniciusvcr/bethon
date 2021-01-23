def foo() -> int:
    def inner_fun() -> int:
        return another_inner_fun()

    def another_inner_fun() -> int:
        return 25

    return g() + inner_fun()


def g() -> int:
    return a


a = 10
assert foo() == a + 25
