from dataclasses import dataclass

Name = str

STEVE: Name = "Steve"
DAN: Name = "Dan"
RAHUL: Name = "Rahul"


@dataclass
class ListOfName:
    first: Name
    rest: Cons


Cons = None | ListOfName

STEVE_DAN_RAHUL: ListOfName = ListOfName(STEVE, ListOfName(DAN, ListOfName(RAHUL, None)))

    
