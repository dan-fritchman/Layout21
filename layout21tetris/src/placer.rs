//!
//! # Tetris Placer
//!
//! Converts potentially relatively-placed attributes to absolute positions.
//!

// Std-lib
use std::collections::{HashMap, HashSet};

// Local imports
use crate::cell::{CellBag, Instance, LayoutImpl};
use crate::coords::{PrimPitches, Xy};
use crate::library::Library;
use crate::placement::{Place, Placeable, RelativePlace, Side};
use crate::raw::{Dir, LayoutResult};
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
        for inst_ptr in DepOrder::order(layout).iter() {
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
    ///
    fn resolve_instance_place(
        &mut self,
        inst: &Instance,
        rel: &RelativePlace,
    ) -> LayoutResult<Xy<PrimPitches>> {
        let to = match rel.to {
            Placeable::Instance(ref ptr) => ptr,
            _ => unimplemented!(), // FIXME: other variants TBC
        };
        let to_inst = to.read()?;
        let bbox = to_inst.boundbox()?;

        // FIXME: checks that `side` and `align` are orthogonal should come earlier

        // The coordinate axes here are referred to as `adjacent`, corresponding to `rel.side`,
        // and `align`, corresponding to `rel.align`.
        // Mapping these back to (x,y) happens at the very end.
        let adjacent_axis = match rel.side {
            Side::Left | Side::Right => Dir::Horiz,
            Side::Top | Side::Bottom => Dir::Vert,
        };
        let align_axis = adjacent_axis.other();

        let something = match rel.side {
            Side::Left | Side::Bottom => bbox.0,
            Side::Top | Side::Right => bbox.1,
        };
        let (mut adjacent_coord, mut align_coord) = match adjacent_axis {
            Dir::Horiz => (something.x, something.y),
            Dir::Vert => (something.y, something.x),
        };
        // Sort out whether the instance needs an offset in each axis
        let offset_adjacent = match rel.side {
            Side::Left | Side::Bottom => !inst.reflected(adjacent_axis),
            Side::Top | Side::Right => inst.reflected(adjacent_axis),
        };
        let offset_align = match rel.align {
            Side::Left | Side::Bottom => !inst.reflected(align_axis),
            Side::Top | Side::Right => inst.reflected(align_axis),
        };
        // Add in any reflection-based offsets
        if offset_adjacent || offset_align {
            let inst_size = inst.boundbox_size()?;
            if offset_adjacent {
                adjacent_coord = adjacent_coord + inst_size[adjacent_axis];
            }
            if offset_align {
                align_coord = align_coord - inst_size[align_axis];
            }
        }
        let res = match rel.side {
            Side::Left | Side::Right => Xy::new(adjacent_coord, align_coord),
            Side::Top | Side::Bottom => Xy::new(align_coord, adjacent_coord),
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
pub struct DepOrder<'c> {
    layout: &'c LayoutImpl,
    stack: Vec<Ptr<Instance>>,
    seen: HashSet<Ptr<Instance>>,
}
impl<'c> DepOrder<'c> {
    fn order(layout: &'c LayoutImpl) -> Vec<Ptr<Instance>> {
        let mut this = Self {
            layout,
            stack: Vec::new(),
            seen: HashSet::new(),
        };
        for inst in layout.instances.iter() {
            this.push(inst);
        }
        this.stack
    }
    fn push(&mut self, ptr: &Ptr<Instance>) {
        // Depth-first search dependent Instance placements
        // FIXME: cycle detection
        if !self.seen.contains(ptr) {
            // Process the Instance, particularly its dependencies
            self.process(ptr);
            // And insert the Instance (pointer) itself
            self.seen.insert(ptr.clone());
            self.stack.push(ptr.clone());
        }
    }
    /// Process `item`
    fn process(&mut self, item: &Ptr<Instance>) {
        // Read the instance-pointer
        let inst = item.read().unwrap();
        // If its place is relative, visit its dependencies first
        match &inst.loc {
            Place::Rel(rel) => {
                match rel.to {
                    Placeable::Instance(ref ptr) => self.push(ptr),
                    _ => unimplemented!(), // FIXME: other variants TBC
                };
            }
            Place::Abs(_) => (), // Nothing to traverse
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::outline::Outline;
    use crate::tests::{exports, stack};

    #[test]
    fn test_place1() -> LayoutResult<()> {
        // Most-basic smoke-test
        let stack = stack()?;
        Placer::place(Library::new("plib"), stack)?;
        Ok(())
    }
    #[test]
    fn test_place2() -> LayoutResult<()> {
        // Initial test of relative placement
        let mut lib = Library::new("plib");
        // Create a unit cell which we'll instantiate a few times
        let unit = LayoutImpl::new("unit", 0, Outline::rect(3, 7)?).into();
        let unit = lib.cells.add(unit);
        // Create an initial instance, at the origin
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
        // Now the new stuff: relative placements
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
        let (lib, _stack) = Placer::place(lib, stack()?)?;
        assert_eq!(lib.cells.len(), 2);
        // Note these next two checks do *pointer* equality, not value equality
        assert_eq!(lib.cells[0], unit);
        assert_eq!(lib.cells[1], parent);
        // Now check the locations
        let i0read = i0.read()?;
        assert_eq!(*i0read.loc.abs()?, Xy::<PrimPitches>::from((47, 51)));
        let i1read = i1.read()?;
        assert_eq!(*i1read.loc.abs()?, Xy::<PrimPitches>::from((50, 51)));
        exports(lib)
    }
    #[test]
    fn test_place3() -> LayoutResult<()> {
        // FIXME: Test each relative side, alignment, and orientation
        /// ## !?!?! ## Start here ## !?!?! ##
        Ok(())
    }
}
