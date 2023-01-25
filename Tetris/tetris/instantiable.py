from typing import Union

from .cell import Cell
from .array import Array
from .group import Group

# Instantiable Types Union
# Primarily used as the `of` target of each `Instance`
Instantiable = Union[Cell, Array, Group]
