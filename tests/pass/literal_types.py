from dataclasses import dataclass


def f() -> 20:
    """ This function only returns 20"""
    return 20


assert f() == 20


def g(x: 10.0) -> float:
    """ 'x' is 10, but can be returned to int type """
    return x


a: 10.0 = 10.0  # 'a' can only be 10
assert g(a) == 10.0  # 'g' can be called with 'a'
assert g(10.0) == 10.0  # 'g' can be called with 'a'


@dataclass
class Test:
    x: "Hello, world"  # 'x' can only be "Hello, world"


b: "Hello, world" = "Hello, world"  # Type of b is the literal string
Test("Hello, world")
Test(b)
