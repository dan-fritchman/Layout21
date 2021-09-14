//!
//! # Tetris Placer
//!
//! Converts potentially relatively-placed attributes to absolute positions.
//!

// Std-lib
use std::collections::HashSet;

// Local imports
use crate::bbox::HasBoundBox;
use crate::cell::{Instance, LayoutImpl};
use crate::coords::{PrimPitches, Xy};
use crate::library::Library;
use crate::placement::{Place, Placeable, RelativePlace, Side};
use crate::raw::{Dir, LayoutError, LayoutResult};
use crate::stack::Stack;
use crate::utils::Ptr;

pub struct Placer {
    lib: Library,
    stack: Stack,
}
impl Placer {
    ///
    /// [Placer] public API entrypoint.
    /// Modify and return [Library] `lib`, converting all [RelativePlace]s to absolutes.
    ///
    pub fn place(lib: Library, stack: Stack) -> LayoutResult<(Library, Stack)> {
        let mut this = Self { lib, stack };
        this.place_lib()?;
        Ok((this.lib, this.stack))
    }
    /// Primary internal implementation method. Update placements for [Library] `self.lib`.
    fn place_lib(&mut self) -> LayoutResult<()> {
        // Iterate over all the library's cells, updating their instance-placements.
        for cellptr in &self.lib.dep_order() {
            let cell = cellptr.write()?;
            if let Some(ref layout) = cell.layout {
                self.place_layout(layout)?;
            }
        }
        Ok(())
    }
    /// Update placements for [LayoutImpl] `layout`
    fn place_layout(&mut self, layout: &LayoutImpl) -> LayoutResult<()> {
        // Iterate over the layout's instances in dependency order,
        // updating any relative-places to absolute.
        for inst_ptr in DepOrder::order(layout)?.iter() {
            let mut inst = inst_ptr.write()?;
            inst.loc = match &inst.loc {
                Place::Abs(a) => Place::Abs(*a), // Already done
                Place::Rel(ref rel) => {
                    // Convert to an absolute location
                    let abs = self.resolve_instance_place(&*inst, rel)?;
                    Place::Abs(abs)
                }
            }
        }
        Ok(())
    }
    /// Resolve a location of [Instance] `inst` relative to its [RelativePlace] `rel`.
    fn resolve_instance_place(
        &mut self,
        inst: &Instance,
        rel: &RelativePlace,
    ) -> LayoutResult<Xy<PrimPitches>> {
        let to = match rel.to {
            Placeable::Instance(ref ptr) => ptr,
            _ => unimplemented!(), // FIXME: other variants TBC
        };
        // The coordinate axes here are referred to as `side`, corresponding to `rel.side`, and `align`, corresponding to `rel.align`.
        // Mapping these back to (x,y) happens at the very end.
        // FIXME: checks that `side` and `align` are orthogonal should come earlier

        // Get the relative-to instance's bounding box
        let to_inst = to.read()?;
        let bbox = to_inst.boundbox()?;
        // Get its edge-coordinates in each axis
        let mut side_coord = bbox.side(rel.side);
        let mut align_coord = bbox.side(rel.align);
        let side_axis = match rel.side {
            Side::Left | Side::Right => Dir::Horiz,
            Side::Top | Side::Bottom => Dir::Vert,
        };
        let align_axis = side_axis.other();
        // Sort out whether the instance needs a reflection-based offset in each axis
        let offset_side = match rel.side {
            Side::Left | Side::Bottom => !inst.reflected(side_axis),
            Side::Top | Side::Right => inst.reflected(side_axis),
        };
        let offset_align = match rel.align {
            Side::Left | Side::Bottom => inst.reflected(align_axis),
            Side::Top | Side::Right => !inst.reflected(align_axis),
        };
        // Add in any reflection-based offsets
        if offset_side || offset_align {
            let inst_size = inst.boundbox_size()?;
            if offset_side {
                side_coord = side_coord - inst_size[side_axis];
            }
            if offset_align {
                align_coord = align_coord - inst_size[align_axis];
            }
        }
        let res = match rel.side {
            Side::Left | Side::Right => Xy::new(side_coord, align_coord),
            Side::Top | Side::Bottom => Xy::new(align_coord, side_coord),
        };
        Ok(res)
    }
}

///
/// # Placement Dependency-Orderer
///
/// Orders the [Instance]s in a [LayoutImpl] by their placement dependencies.
/// Holds a shared reference to the [LayoutImpl] during traversal,
/// then returns an ordered vector of pointers to its [Instance]s.
///
#[derive(Debug)]
pub struct DepOrder {
    /// Ordered, completed items
    stack: Vec<Ptr<Instance>>,
    /// Hash-set of completed items, for quick membership tests
    seen: HashSet<Ptr<Instance>>,
    /// Hash-set of pending items, for cycle detection
    pending: HashSet<Ptr<Instance>>,
}
impl DepOrder {
    fn order(layout: &LayoutImpl) -> LayoutResult<Vec<Ptr<Instance>>> {
        let len = layout.instances.len();
        let mut this = Self {
            stack: Vec::with_capacity(len),
            seen: HashSet::with_capacity(len),
            pending: HashSet::new(),
        };
        for inst in layout.instances.iter() {
            this.push(inst)?;
        }
        Ok(this.stack)
    }
    fn push(&mut self, ptr: &Ptr<Instance>) -> LayoutResult<()> {
        // Depth-first search dependent Instance placements
        if !self.seen.contains(ptr) {
            // Check for cycles, indicated if `ptr` is in the pending-set, i.e. an open recursive stack-frame.
            if self.pending.contains(ptr) {
                return Err(LayoutError::Tbd);
            }
            self.pending.insert(ptr.clone());
            // Process the Instance, particularly its dependencies
            self.process(ptr)?;
            // Check that `ptr` hasn't (somehow) been removed from the pending-set
            let removed = self.pending.remove(ptr);
            if !removed {
                return Err(LayoutError::Tbd);
            }
            // And insert the Instance (pointer) itself
            self.seen.insert(ptr.clone());
            self.stack.push(ptr.clone());
        }
        Ok(())
    }
    /// Process `item`
    fn process(&mut self, item: &Ptr<Instance>) -> LayoutResult<()> {
        // Read the instance-pointer
        let inst = item.read()?;
        // If its place is relative, visit its dependencies first
        match &inst.loc {
            Place::Rel(rel) => {
                match rel.to {
                    Placeable::Instance(ref ptr) => self.push(ptr)?,
                    _ => unimplemented!(), // FIXME: other variants TBC
                };
            }
            Place::Abs(_) => (), // Nothing to traverse
        }
        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::outline::Outline;
    use crate::tests::{exports, SampleStacks};

    #[test]
    fn test_place1() -> LayoutResult<()> {
        // Most basic smoke-test
        Placer::place(Library::new("plib"), SampleStacks::empty()?)?;
        Ok(())
    }
    #[test]
    fn test_place2() -> LayoutResult<()> {
        // Initial test of relative placement
        let mut lib = Library::new("test_place2");
        // Create a unit cell which we'll instantiate a few times
        let unit = LayoutImpl::new("unit", 0, Outline::rect(3, 7)?).into();
        let unit = lib.cells.add(unit);
        // Create an initial instance
        let i0 = Instance {
            inst_name: "i0".into(),
            cell: unit.clone(),
            loc: (47, 51).into(),
            reflect_horiz: false,
            reflect_vert: false,
        };
        // Create the parent cell which instantiates it
        let mut parent = LayoutImpl::new("parent", 0, Outline::rect(100, 100)?);
        let i0 = parent.instances.add(i0);
        // Create another Instance, placed relative to `i0`
        let i1 = Instance {
            inst_name: "i1".into(),
            cell: unit.clone(),
            loc: Place::Rel(RelativePlace {
                to: Placeable::Instance(i0.clone()),
                side: Side::Right,
                align: Side::Bottom,
            }),
            reflect_horiz: false,
            reflect_vert: false,
        };
        let i1 = parent.instances.add(i1);
        let parent = lib.cells.add(parent.into());

        // The real code-under-test: run placement
        let (lib, stack) = Placer::place(lib, SampleStacks::empty()?)?;

        // Checks on results
        assert_eq!(lib.cells.len(), 2);
        // Note these next two checks do *pointer* equality, not value equality
        assert_eq!(lib.cells[0], unit);
        assert_eq!(lib.cells[1], parent);
        // Now check the locations
        {
            let inst = i0.read()?;
            assert_eq!(*inst.loc.abs()?, Xy::<PrimPitches>::from((47, 51)));
        }
        {
            let inst = i1.read()?;
            assert_eq!(*inst.loc.abs()?, Xy::<PrimPitches>::from((50, 51)));
        }
        exports(lib, stack)
    }
    #[test]
    fn test_place3() -> LayoutResult<()> {
        // Test each relative side and alignment
        let mut lib = Library::new("test_place3");
        // Create a big center cell
        let big = LayoutImpl::new("big", 0, Outline::rect(75, 25)?).into();
        let big = lib.cells.add(big);
        // Create the parent cell which instantiates it
        let mut parent = LayoutImpl::new("parent", 0, Outline::rect(225, 75)?);
        // Create an initial instance
        let ibig = Instance {
            inst_name: "ibig".into(),
            cell: big.clone(),
            loc: (30, 20).into(),
            reflect_horiz: false,
            reflect_vert: false,
        };
        let ibig = parent.instances.add(ibig);
        // Create a unit cell which we'll instantiate a few times around `ibig`
        let lil = LayoutImpl::new("lil", 0, Outline::rect(11, 4)?).into();
        let lil = lib.cells.add(lil);

        // Relative-placement-adder closure
        let mut add_inst = |inst_name: &str, side, align| {
            let i = Instance {
                inst_name: inst_name.into(),
                cell: lil.clone(),
                loc: Place::Rel(RelativePlace {
                    to: Placeable::Instance(ibig.clone()),
                    side,
                    align,
                }),
                reflect_horiz: false,
                reflect_vert: false,
            };
            parent.instances.add(i)
        };
        // Add a bunch of em
        let i1 = add_inst("i1", Side::Left, Side::Bottom);
        let i2 = add_inst("i2", Side::Right, Side::Bottom);
        let i3 = add_inst("i3", Side::Bottom, Side::Left);
        let i4 = add_inst("i4", Side::Bottom, Side::Right);
        let i5 = add_inst("i5", Side::Left, Side::Top);
        let i6 = add_inst("i6", Side::Right, Side::Top);
        let i7 = add_inst("i7", Side::Top, Side::Left);
        let i8 = add_inst("i8", Side::Top, Side::Right);

        // Add `parent` to the library
        let _parent = lib.cells.add(parent.into());

        // The real code under test: run placement
        let (lib, stack) = Placer::place(lib, SampleStacks::empty()?)?;

        // And test the placed results
        let bigbox = ibig.read()?.boundbox()?;
        {
            let ibox = i1.read()?.boundbox()?;
            assert_eq!(ibox.side(Side::Right), bigbox.side(Side::Left));
            assert_eq!(ibox.side(Side::Bottom), bigbox.side(Side::Bottom));
        }
        {
            let ibox = i2.read()?.boundbox()?;
            assert_eq!(ibox.side(Side::Left), bigbox.side(Side::Right));
            assert_eq!(ibox.side(Side::Bottom), bigbox.side(Side::Bottom));
        }
        {
            let ibox = i3.read()?.boundbox()?;
            assert_eq!(ibox.side(Side::Top), bigbox.side(Side::Bottom));
            assert_eq!(ibox.side(Side::Left), bigbox.side(Side::Left));
        }
        {
            let ibox = i4.read()?.boundbox()?;
            assert_eq!(ibox.side(Side::Top), bigbox.side(Side::Bottom));
            assert_eq!(ibox.side(Side::Right), bigbox.side(Side::Right));
        }
        {
            let ibox = i5.read()?.boundbox()?;
            assert_eq!(ibox.side(Side::Right), bigbox.side(Side::Left));
            assert_eq!(ibox.side(Side::Top), bigbox.side(Side::Top));
        }
        {
            let ibox = i6.read()?.boundbox()?;
            assert_eq!(ibox.side(Side::Left), bigbox.side(Side::Right));
            assert_eq!(ibox.side(Side::Top), bigbox.side(Side::Top));
        }
        {
            let ibox = i7.read()?.boundbox()?;
            assert_eq!(ibox.side(Side::Bottom), bigbox.side(Side::Top));
            assert_eq!(ibox.side(Side::Left), bigbox.side(Side::Left));
        }
        {
            let ibox = i8.read()?.boundbox()?;
            assert_eq!(ibox.side(Side::Bottom), bigbox.side(Side::Top));
            assert_eq!(ibox.side(Side::Right), bigbox.side(Side::Right));
        }
        exports(lib, stack)
    }
}
