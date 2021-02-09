def is_odd(x: int) -> bool:
    if x == 0:
        return False
    else:
        return is_even(x - 1)


def is_even(x: int) -> bool:
    if x == 0:
        return True
    else:
        return is_odd(x - 1)


assert is_even(1) == False
assert is_even(5) == False
assert is_even(2) == True
assert is_even(0) == True
assert is_odd(1) == True
assert is_odd(5) == True
assert is_odd(2) == False
assert is_odd(0) == False
