from dataclasses import dataclass


@dataclass
class Cons:
    first: int
    rest: None | Cons  # todo mudar aqui


lista = Cons(10, Cons(20, None))


def sum(lst: None | Cons) -> int:
    if isinstance(lst, None):
        return 0
    else:
        return lst.first + sum(lst.rest)


assert sum(None) == 0
assert sum(Cons(10, None)) == 10
assert sum(Cons(40, Cons(10, None))) == 50
