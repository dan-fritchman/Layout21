#
# # Interfaces Module
#
# Describing Cells in terms of their IO Interfaces
#

from typing import List
from enum import Enum, auto

from pydantic.dataclasses import dataclass


class PortKind(Enum):
    # Flat Scalar Port, e.g. `clk`
    Scalar = auto()
    # Array-Based Port, e.g. `data[31:0]`
    Array = auto()
    # Instance of a Hierarchical Bundle
    Bundle = auto()


# # Port
#
# Logical port, as in a netlist or HDL description.
# Includes scalar, vector (bus), and bundle-valued ports.
# Does not include physical/ geometric information.
#
@dataclass
class Port:
    # Port Name
    name: str
    # Port Type  Content
    kind: PortKind


@dataclass
class Bundle:
    name: str
    ports: List[Port]
