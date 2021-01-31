# 'param' is valid as is a function parameter and
# will always be bound
def foo(param: int) -> str:
    a = 20  # valid, declared in the functions "global" scope

    # 'b' is declared inside both if and else:
    # valid in the outer environment
    if a == param:
        b = "equal"
    else:
        b = "not equal"

    return b  # 'b' can be used, as is valid


# 'x' is valid because is declared in the global scope
x = 10

# 'a' is declared inside both if and else:
# valid in the outer environment
if x == 10:
    a = x
else:
    a = 20


# asserting the values are right
assert a == x
assert foo(a) == "not equal"
