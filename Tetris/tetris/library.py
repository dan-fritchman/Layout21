#
# # Layout Library Module
#

from typing import List, Set, Dict
from dataclasses import field

from pydantic.dataclasses import dataclass

from .cell import Cell


# # Layout Library
#
# A combination of cell definitions, sub-libraries, and metadata
#
@dataclass
class Library:
    # Library Name
    name: str
    # Cell Definitions
    cells: Dict[str, Cell] = field(default_factory=dict)

    # FIXME: `raw` stuff
    # # [raw.Library] Definitions
    # rawlibs: List[raw.Library]
    # # Export to a [raw.Library]
    # def to_raw(self, stack: validate.ValidStack) -> LayoutResult<Ptr<raw.Library>> :
    #     conv.raw.RawExporter.convert(self, stack)
    # # Add a [raw.Library]
    # def add_rawlib(self, rawlib: raw.Library) -> Ptr<raw.Library> :
    #     self.rawlibs.insert(rawlib)

    # Add a [Cell]
    def add_cell(self, cell: Cell) -> None:
        self.cells.append(cell)

    # Create an ordered list in which dependent cells follow their dependencies.
    def dep_order(self) -> List[Cell]:
        return DepOrder.order(self)


# # Dependency-Orderer
#
# Creates an ordered list in which dependent cells follow their dependencies.
# FIXME: migrate to utils.DepOrder
#
@dataclass
class DepOrder:
    lib: Library
    stack: List[Cell]
    seen: Set[Cell]  # FIXME! hashing of these aint gonna happen

    def order(lib: Library) -> List[Cell]:
        myself = DepOrder(lib=lib, stack=list(), seen=set())
        for cell in myself.lib.cells.keys():
            myself.push(cell)
        return myself.stack

    def push(self, cellname: str):
        # If the Cell hasn't already been visited, depth-first search it
        if cellname in self.seen:
            return  # Already done

        # Read the cell-pointer
        cell = self.lib.cells[cellname]
        # If the cell has an implementation, visit its [Instance]s before inserting it
        if cell.layout is not None:
            for inst in cell.layout.instances:
                self.push(inst.cell.name)

        # And insert the cell (pointer) itself
        self.seen.insert(cellname)
        self.stack.push(cell)

