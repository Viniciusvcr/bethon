from dataclasses import dataclass


# 'w_pos' type does not exist -> error
@dataclass
class Person:
    name: str
    age: int
    w_pos: Point
