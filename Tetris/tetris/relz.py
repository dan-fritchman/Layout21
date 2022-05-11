from enum import Enum, auto


class RelZ(Enum):
    """ Relative Z-Axis Reference to one Layer `Above` or `Below` another """

    Above = auto()
    Below = auto()

    def other(self) -> "RelZ":
        if self == RelZ.Above:
            return RelZ.Below
        if self == RelZ.Below:
            return RelZ.Above
        raise ValueError

