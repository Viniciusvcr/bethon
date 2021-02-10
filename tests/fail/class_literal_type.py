from dataclasses import dataclass


@dataclass
class Test:
    x: "Hello, world"  # 'x' can only be "Hello, world"


b = "Hello, world"  # Type of b is str
Test(b)  # only accepts literal type "Hello, world"
