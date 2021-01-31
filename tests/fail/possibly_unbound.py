# 'param' is valid as is a function parameter and
# will always be bound
def foo(param: int) -> str:
    a = 20  # valid, declared in the functions "global" scope

    # 'b' is declared only inside if:
    # not valid in the outer environment
    if a == param:
        b = "equal"

    return b  # 'b' cannot be used, might not be declared -> error


# 'x' is valid because is declared in the global scope
x = 10

# 'a' is declared inside if branch.
# 'b'  is declared inside else branch.
# both are not valid in the outer environment: only one of them will be declared on runtime -> error

if x == 10:
    a = x
else:
    b = 20
