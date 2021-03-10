from dataclasses import dataclass


@dataclass
class Cons:
    first: int
    rest: List


List = None | Cons


def sum(lst: List) -> int:
    if not isinstance(lst, Cons):
        return 0
    else:
        return lst.first + sum(lst.rest)


assert sum(None) == 0
assert sum(Cons(10, None)) == 10
assert sum(Cons(40, Cons(10, None))) == 50
