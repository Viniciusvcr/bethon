from enum import IntEnum  # IntEnum being used for comparisons


class Result(IntEnum):
    Ok = 1
    Err = 2


assert Result.Ok == 1
assert Result.Err == 2
assert Result.Ok != 2
assert Result.Ok == Result.Ok
assert Result.Ok != Result.Err
