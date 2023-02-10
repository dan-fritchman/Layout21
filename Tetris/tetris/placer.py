"""
# Tetris Placer

Converts potentially relatively-placed attributes to absolute positions.
"""

# Std-Lib Imports
from typing import List, Tuple, Union, Optional, Set
from dataclasses import field

# PyPi Imports
from dataclasses import dataclass

# Local imports
from .cell import Cell
from .instance import Instance
from .reflect import Reflect
from .bbox import BoundBox
from .layout import Layout
from .coords import Dir, PrimPitches, UnitSpeced, Xy
from .library import Library
from .placement import Placeable, RelativePlace, AbsPlace, Side
from .separation import SepBy
from .stack import ValidStack
from .abstract import Abstract
from .align import Align, AlignSide
from .library import Library
from .stack import ValidStack

StackEntry = Union[Library, Cell, Layout, Abstract, Instance]  # FIXME: maybe more

# # Placer
# Converts all potentially-relatively-placed attributes to absolute positions.
@dataclass
class Placer:
    lib: Library
    stack: Optional[ValidStack]
    ctx: List[StackEntry]

    # [Placer] public API entrypoint.
    # Modify and return [Library] `lib`, converting all [RelativePlace]s to absolute locations.
    @classmethod
    def place(cls, lib: Library, stack: ValidStack) -> Tuple[Library, ValidStack]:
        this = cls(
            lib,
            stack,
            ctx=[],
        )
        this.place_lib()
        return (this.lib, this.stack)

    # Primary internal implementation method. Update placements for [Library] `self.lib`.
    def place_lib(self) -> None:
        self.ctx.append(self.lib)
        # Iterate over all the library's cells, updating their instance-placements.
        for cell in self.lib.dep_order():
            self.ctx.append(cell)
            if cell.layout is not None:
                self.place_layout(cell.layout)
            self.ctx.pop()
        self.ctx.pop()

    # Update placements for [Layout] `layout`
    def place_layout(self, layout: Layout) -> None:
        self.ctx.append(layout)

        # Move `instances` and `places` into one vector of [Placeable]s
        places: List[Placeable] = layout.instances
        # places.extend(layout.places)
        layout.places = []

        # Iterate over `places` in dependency order, updating any relative-places to absolute.
        ordered = PlaceOrder.get_ordered(places)
        for place in ordered:
            if isinstance(place, Instance):
                inst = place
                if isinstance(inst.loc, RelativePlace):
                    # Convert to an absolute location
                    inst.loc.resolved = self.resolve_instance_place(inst)

                # Add the now-absolute-placed inst to the `instances` list
                # FIXME: do this or nah?
                # layout.instances.append(inst_ptr.clone())

            # if isinstance(place, Array):
            #     array_inst = ptr.write()
            #     if Place.Rel(ref rel) = array_inst.loc {
            #         # Convert to an absolute location
            #         abs = self.resolve_array_place(*array_inst, rel)
            #         array_inst.loc = Place.Abs(abs)
            #     }
            #     # And flatten its instances
            #     children = self.flatten_array_inst(*array_inst)
            #     children = children.into_iter().map(|i| Ptr.new(i))
            #     layout.instances.extend(children)
            # }
            # Placeable.Assign(ref ptr) => {
            #     assn = ptr.read()
            #     abs: TrackCross = self.resolve_assign_place(assn.loc)
            #     new_assn = stack.Assign {
            #         net: assn.net.clone(),
            #         at: abs,
            #     }
            #     layout.assignments.append(new_assn)
            # }
            # Placeable.Group(_) => unimplemented!(),
            # Placeable.Port { .. } => (), # Nothing to do, at least until hitting something that *depends* on the Port location

        self.ctx.pop()

    # Flatten an [ArrayInstance] to a vector of Cell Instances.
    # Instance location must be absolute by call-time.
    # def flatten_array_inst(
    #     self,
    #     array_inst: ArrayInstance,
    # ) -> LayoutResult<Vec<Instance>> {
    #     # Read the child-Instances from the underlying [Array] definition
    #     children = {
    #         array = array_inst.array.read()
    #         self.flatten_array(*array, array_inst.name)
    #     }
    #     # Get its initial location
    #     loc = array_inst.loc.abs()
    #     # Translate each child to our location and reflection
    #     for child in children.iter_mut() {
    #         childloc = child.loc.abs_mut()
    #         if array_inst.reflect_horiz {
    #             # Reflect horizontally
    #             childloc.x *= -1_isize
    #             child.reflect_horiz = !child.reflect_horiz
    #         }
    #         if array_inst.reflect_vert {
    #             # Reflect vertically
    #             childloc.y *= -1_isize
    #             child.reflect_vert = !child.reflect_vert
    #         }
    #         # Translate its location
    #         *childloc += *loc

    # Flatten an [Array] to a vector of Cell Instances.
    # def flatten_array(self, array: Array, prefix: str) -> LayoutResult<Vec<Instance>> {
    #     insts = Vec.with_capacity(array.count)

    #     # Get the separations in each dimension
    #     xsep = match array.sep.x {
    #         None => PrimPitches.x(0),
    #         Some(SepBy.UnitSpeced(u)) => {
    #             match u {
    #                 UnitSpeced.PrimPitches(p) => p.clone(),
    #                 _ => unimplemented!(), # TODO: other units
    #             }
    #         }
    #         Some(SepBy.SizeOf(_)) => unimplemented!(),
    #     }
    #     ysep = match array.sep.y {
    #         None => PrimPitches.y(0),
    #         Some(SepBy.UnitSpeced(u)) => {
    #             match u {
    #                 UnitSpeced.PrimPitches(p) => p.clone(),
    #                 _ => unimplemented!(), # TODO: other units
    #             }
    #         }
    #         Some(SepBy.SizeOf(_)) => unimplemented!(),
    #     }
    #     sep = Xy.new(xsep, ysep)

    #     # Initialize our location to the array-origin
    #     loc = (0, 0).into()
    #     for i in 0..array.count {
    #         match array.unit {
    #             Arrayable.Instance(cell) => {
    #                 i = Instance {
    #                     inst_name: format!("{}[{}]", prefix, i), # `arrayname[i]`
    #                     cell: cell.clone(),
    #                     loc: Place.Abs(loc),
    #                     reflect_horiz: false,
    #                     reflect_vert: false,
    #                 }
    #                 insts.append(i)
    #             }
    #             Arrayable.Array(arr) => {
    #                 # Create a new [ArrayInstance] at the current location,
    #                 # largely for sake of reusing `flatten_array_inst`
    #                 # to get its located, flattened children.
    #                 i = ArrayInstance {
    #                     name: format!("{}[{}]", prefix, i), # `arrayname[i]`
    #                     array: arr.clone(),
    #                     loc: Place.Abs(loc),
    #                     reflect_horiz: false,
    #                     reflect_vert: false,
    #                 }
    #                 # (Potentially recursively) flatten that short-lived [ArrayInstance]
    #                 children = self.flatten_array_inst(i)
    #                 # And add its children to ours
    #                 insts.extend(children)
    #             }
    #             Arrayable.Group(_arr) => unimplemented!(),
    #         }
    #         # Increment the location by our (two-dimensional) increment.
    #         loc += sep

    #     return insts

    # Resolve a location of [ArrayInstance] `inst` relative to its [RelativePlace] `rel`.
    # def resolve_array_place(
    #     self,
    #     _inst: ArrayInstance,
    #     _rel: RelativePlace,
    # ) -> LayoutResult<Xy<PrimPitches>> {
    #     # FIXME: this should just need the same stuff as `resolve_instance_place`,
    #     # once we have a consolidated version of [Instance] that covers Arrays.
    #     todo!()
    # }
    def resolve_instance_place(self, inst: Instance) -> AbsPlace:
        """# Resolve a location of [Instance] `inst` relative to its [RelativePlace] `rel`."""
        self.ctx.append(inst)

        # Get the relative-to instance's bounding box
        if not isinstance(inst.loc, RelativePlace):
            raise RuntimeError("Expected RelativePlace")
        place_relative_to_this_bbox = inst.loc.to.boundbox()

        # The coordinate axes here are referred to as `side`, corresponding to `inst.loc.side`, and `align`, corresponding to `inst.loc.align`.
        # Mapping these back to (x,y) happens at the very end.
        # FIXME: checks that `side` and `align` are orthogonal should come earlier

        # Collect the two sides of the placed instance dictated by `inst.loc.to`
        left = right = top = bottom = None

        if inst.loc.side == Side.Left:
            right = place_relative_to_this_bbox.left
        elif inst.loc.side == Side.Right:
            left = place_relative_to_this_bbox.right
        elif inst.loc.side == Side.Top:
            bottom = place_relative_to_this_bbox.top
        elif inst.loc.side == Side.Bottom:
            top = place_relative_to_this_bbox.bottom
        else:
            raise ValueError

        if inst.loc.align.side == Side.Left:
            left = place_relative_to_this_bbox.left
        elif inst.loc.align.side == Side.Right:
            right = place_relative_to_this_bbox.right
        elif inst.loc.align.side == Side.Top:
            top = place_relative_to_this_bbox.top
        elif inst.loc.align.side == Side.Bottom:
            bottom = place_relative_to_this_bbox.bottom
        else:
            raise ValueError

        origin = something(inst=inst, top=top, bottom=bottom, left=left, right=right)
        return AbsPlace(origin)


def something(
    inst: Instance,
    top: Optional[PrimPitches],
    bottom: Optional[PrimPitches],
    left: Optional[PrimPitches],
    right: Optional[PrimPitches],
) -> Xy:
    # What we know at this point:
    # * The cell's bounding box
    # * *Either* the top or bottom edge of the instance
    # * *Either* the right or left edge of the instance
    # * The instance's reflection state
    # Now find its origin
    ...
    cell_size = inst.boundbox_size()

    if top is None:
        top = bottom + cell_size.y
    if bottom is None:
        bottom = top - cell_size.y
    if left is None:
        left = right - cell_size.x
    if right is None:
        right = left + cell_size.x

    bbox = BoundBox(mins=Xy(x=left, y=bottom), maxs=Xy(x=right, y=top))
    return get_origin_something(bbox, inst.reflect)


def get_origin_something(bbox: BoundBox, reflect: Reflect) -> Xy:
    """# Get the origin of an object with bounding box `bbox` and reflection-state `reflect`."""
    x = bbox.right if reflect.horiz else bbox.left
    y = bbox.top if reflect.vert else bbox.bottom
    return Xy(x=x, y=y)


@dataclass
class PlaceOrder:
    placeables: List[Placeable]
    order: List[Placeable] = field(default_factory=list)
    pending: Set[str] = field(default_factory=set)
    done: Set[str] = field(default_factory=set)

    @classmethod
    def get_ordered(cls, placeables: List[Placeable]) -> List[Placeable]:
        order = cls(placeables)
        for p in placeables:
            order.push(p)
        return order.order

    def push(self, item: Placeable):
        # Depth-first search dependent Instance placements
        if item.name in self.done:
            return  # Already processed

        # Check for cycles, indicated if `item` is in the pending-set, i.e. an open recursive stack-frame.
        if item.name in self.pending:
            self.fail()

        self.pending.add(item.name)
        # Process the Item, dependencies first
        self.process(item)
        # Check that `item` hasn't (somehow) been removed from the pending-set
        self.pending.remove(item.name)

        # And insert the Item itself
        self.done.add(item.name)
        self.order.append(item)

    def process(self, item: Placeable) -> None:
        # Process [Instance]-pointer `item`
        if isinstance(item, Instance):
            if isinstance(item.loc, RelativePlace):
                self.push(item.loc.to)  # Visit the dependency first
            elif isinstance(item.loc, AbsPlace):
                return  # Nothing to do
            else:
                raise TypeError(f"Unexpected location type {item.loc}")
        else:
            raise TypeError  # FIXME!

        # Placeable.Array(ref p) => {
        #     if Place.Rel(rel) = p.read().loc {
        #         self.order.append(rel.to) # Visit the dependency first
        #     }
        # }
        # Placeable.Group(ref p) => {
        #     if Place.Rel(rel) = p.read().loc {
        #         self.order.append(rel.to) # Visit the dependency first
        #     }
        # }
        # Placeable.Assign(a) => {
        #     # Always relative, append it unconditionally
        #     a = a.read()
        #     self.order.append(a.loc.to)
        # }
        # Placeable.Port { ref inst, .. } => {
        #     if Place.Rel(rel) = inst.read().loc {
        #         self.order.append(rel.to) # Visit the dependency first

    def fail(self, msg: Optional[str] = None):
        raise RuntimeError(msg or "")
