def f() -> float:
    return 10.0


# not allowed, expects variable or class field access
assert isinstance(f(), int)
