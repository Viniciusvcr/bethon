temperatura = int | str


def f(x: temperatura) -> int:
    if isinstance(x, int):
        return x
    else:
        return x  # 'x' is refined to be of type 'str' -> error with function return type
