from typing import Tuple, Union

from pydantic.dataclasses import dataclass

from .side import Side


@dataclass
class AlignSide:
    # Side-to-side alignment
    side: Side


@dataclass
class AlignCenter:
    # Center-aligned
    ...  # No data


@dataclass
class AlignPorts:
    # Port-to-port alignment
    ports: Tuple[str, str]


# Alignment types
Align = Union[AlignSide, AlignCenter, AlignPorts]
