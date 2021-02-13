from dataclasses import dataclass


@dataclass
class IntOrFloat:
    x: int | float


e = IntOrFloat("test")
