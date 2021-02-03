def foo() -> int:
    def inner_fun() -> int:
        return another_inner_fun()

    def another_inner_fun() -> int:
        return 25

    return g() + inner_fun()


a = 10


def g() -> int:
    return a  # 'a' is not in the same scope, but it is accessible


assert foo() == a + 25
