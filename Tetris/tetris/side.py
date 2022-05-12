from enum import Enum, auto


class Side(Enum):
    Top = auto()
    Bottom = auto()
    Left = auto()
    Right = auto()

    # Get the side rotated 90 degrees clockwise
    def cw_90(self) -> "Side":
        if self == Side.Top:
            return Side.Right
        if self == Side.Right:
            return Side.Bottom
        if self == Side.Bottom:
            return Side.Left
        if self == Side.Left:
            return Side.Top
        raise ValueError

    # Get the side rotated 90 degrees counter-clockwise
    def ccw_90(self) -> "Side":
        if self == Side.Top:
            return Side.Left
        if self == Side.Left:
            return Side.Bottom
        if self == Side.Bottom:
            return Side.Right
        if self == Side.Right:
            return Side.Top
        raise ValueError

