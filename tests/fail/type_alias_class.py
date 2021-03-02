from dataclasses import dataclass


@dataclass
class Point:
    x: int
    y: int


str_or_Point = str | Point

f: str_or_Point = 10
