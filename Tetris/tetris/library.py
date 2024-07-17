#
# # Layout Library Module
#

from typing import List, Set, Dict
from dataclasses import field

from pydantic.dataclasses import dataclass

from .cell import Cell


@dataclass
class Library:
    """
    # # Layout Library
    #
    # A combination of cell definitions, sub-libraries, and metadata
    #
    """

    name: str  # Library Name
    cells: Dict[str, Cell] = field(default_factory=dict)
    # Cell Definitions

    # FIXME: `raw` stuff
    # # [raw.Library] Definitions
    # rawlibs: List[raw.Library]
    # # Export to a [raw.Library]
    # def to_raw(self, stack: validate.ValidStack) -> LayoutResult<Ptr<raw.Library>> :
    #     conv.raw.RawExporter.convert(self, stack)
    # # Add a [raw.Library]
    # def add_rawlib(self, rawlib: raw.Library) -> Ptr<raw.Library> :
    #     self.rawlibs.add(rawlib)

    def add_cell(self, cell: Cell) -> Cell:
        """# Add a [Cell]"""
        if not isinstance(cell, Cell):
            raise TypeError
        if cell.name in self.cells:
            raise RuntimeError
        self.cells[cell.name] = cell
        return cell

    # Create an ordered list in which dependent cells follow their dependencies.
    def dep_order(self) -> List[Cell]:
        return DepOrder.order(self)


# # Dependency-Orderer
# Creates an ordered list in which dependent cells follow their dependencies.
@dataclass
class DepOrder:
    lib: Library
    order: List[Cell] = field(default_factory=list)
    done: Set[str] = field(default_factory=set)
    pending: Set[str] = field(default_factory=set)

    def order(lib: Library) -> List[Cell]:
        myself = DepOrder(lib=lib, order=[], done=set(), pending=set())
        for cell in lib.cells.values():
            myself.process(cell)
        return myself.order

    def process(self, cell: Cell):
        # If the Cell hasn't already been visited, depth-first search it
        if cell.name in self.done:
            return  # Already done
        if cell.name in self.pending:
            raise RuntimeError(f"Cycle in cell dependencies: {cell.name}")
        self.pending.add(cell.name)

        # If the cell has an implementation, visit its [Instance]s before inserting it
        if cell.layout is not None:
            for inst in cell.layout.instances:
                self.process(inst.of)

        # And insert the cell (pointer) itself
        self.pending.remove(cell.name)
        self.done.add(cell.name)
        self.order.append(cell)
