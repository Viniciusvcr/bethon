from dataclasses import dataclass


@dataclass
class Point:
    x: int
    y: int


@dataclass
class APoint:
    x: int
    y: int


str_or_Point = str | Point

f: str_or_Point = APoint(0, 0)  # APoint not assignable to str | Point
