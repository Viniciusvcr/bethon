def fib(n: int) -> int:
    if (n <= 1):
        return n

    return fib(n - 2) + fib(n - 1)


assert fib(0) == 0
assert fib(1) == 1
assert fib(1) == fib(2)
assert fib(10) == 55
