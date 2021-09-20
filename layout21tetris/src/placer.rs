//!
//! # Tetris Placer
//!
//! Converts potentially relatively-placed attributes to absolute positions.
//!

// Local imports
use crate::bbox::HasBoundBox;
use crate::cell::{CellBag, Instance, LayoutImpl};
use crate::coords::{PrimPitches, UnitSpeced, Xy};
use crate::library::Library;
use crate::placement::{Place, Placeable, RelativePlace, SepBy, Separation, Side};
use crate::raw::{Dir, LayoutError, LayoutResult};
use crate::stack::Stack;
use crate::utils::{DepOrder, DepOrderer, ErrorContext, ErrorHelper, Ptr};

pub struct Placer {
    lib: Library,
    stack: Stack,
    ctx: Vec<ErrorContext>,
}
impl Placer {
    ///
    /// [Placer] public API entrypoint.
    /// Modify and return [Library] `lib`, converting all [RelativePlace]s to absolutes.
    ///
    pub fn place(lib: Library, stack: Stack) -> LayoutResult<(Library, Stack)> {
        let mut this = Self {
            lib,
            stack,
            ctx: Vec::new(),
        };
        this.place_lib()?;
        Ok((this.lib, this.stack))
    }
    /// Primary internal implementation method. Update placements for [Library] `self.lib`.
    fn place_lib(&mut self) -> LayoutResult<()> {
        self.ctx.push(ErrorContext::Library(self.lib.name.clone()));
        // Iterate over all the library's cells, updating their instance-placements.
        for cellptr in &self.lib.dep_order() {
            let cell = cellptr.write()?;
            if let Some(ref layout) = cell.layout {
                self.ctx.push(ErrorContext::Cell(cell.name.clone()));
                self.place_layout(layout)?;
                self.ctx.pop();
            }
        }
        self.ctx.pop();
        Ok(())
    }
    /// Update placements for [LayoutImpl] `layout`
    fn place_layout(&mut self, layout: &LayoutImpl) -> LayoutResult<()> {
        self.ctx.push(ErrorContext::Impl);
        // Iterate over the layout's instances in dependency order,
        // updating any relative-places to absolute.
        for inst_ptr in InstPlaceOrder::order(layout.instances.as_slice())? {
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
        self.ctx.pop();
        Ok(())
    }
    /// Resolve a location of [Instance] `inst` relative to its [RelativePlace] `rel`.
    fn resolve_instance_place(
        &mut self,
        inst: &Instance,
        rel: &RelativePlace,
    ) -> LayoutResult<Xy<PrimPitches>> {
        self.ctx
            .push(ErrorContext::Instance(inst.inst_name.clone()));
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
                if inst.reflected(side_axis) {
                    side_coord = side_coord + inst_size[side_axis];
                } else {
                    side_coord = side_coord - inst_size[side_axis];
                }
            }
            if offset_align {
                if inst.reflected(align_axis) {
                    align_coord = align_coord + inst_size[align_axis];
                } else {
                    align_coord = align_coord - inst_size[align_axis];
                }
            }
        }
        // Add in our separation
        if rel.sep.z.is_some() {
            self.fail("Z-axis separation is invalid for Instances")?;
        }
        if rel.sep.dir(align_axis).is_some() {
            self.fail("Separation in the alignment-axis is invalid for Instances")?;
        }
        // Get the side-axis separation
        let sep_side_axis = match &rel.sep.dir(side_axis) {
            None => PrimPitches::new(side_axis, 0),
            Some(SepBy::SizeOf(cellptr)) => {
                let cell = cellptr.read()?;
                cell.boundbox_size()?[side_axis]
            }
            Some(SepBy::UnitSpeced(ref u)) => {
                match u {
                    UnitSpeced::DbUnits(_) => self.fail("Invalid separation units: DbUnits")?,
                    UnitSpeced::LayerPitches(_) => {
                        // Do a buncha coordinate transformations
                        todo!()
                    }
                    UnitSpeced::PrimPitches(ref p) => {
                        if p.dir != side_axis {
                            self.fail(format!("Separation {:?} specified in invalid axis", u))?;
                        }
                        p.clone()
                    }
                }
            }
        };
        // Invert the separation if necessary
        let sep_side_axis = match &rel.side {
            Side::Top | Side::Right => sep_side_axis,
            Side::Left | Side::Bottom => sep_side_axis.negate(),
        };
        // And finally add it in
        side_coord = side_coord + sep_side_axis;
        // Move back to (x,y) coordinates
        let res = match rel.side {
            Side::Left | Side::Right => Xy::new(side_coord, align_coord),
            Side::Top | Side::Bottom => Xy::new(align_coord, side_coord),
        };
        self.ctx.pop();
        Ok(res)
    }
}
impl ErrorHelper for Placer {
    type Error = LayoutError;
    fn err(&self, msg: impl Into<String>) -> LayoutError {
        LayoutError::Export {
            message: msg.into(),
            stack: self.ctx.clone(),
        }
    }
}

/// Empty struct for implementing the [DepOrder] trait for relative-placed [Instance]s.
struct InstPlaceOrder;
impl DepOrder for InstPlaceOrder {
    type Item = Ptr<Instance>;
    type Error = LayoutError;

    /// Process [Instance]-pointer `item`
    fn process(item: &Ptr<Instance>, orderer: &mut DepOrderer<Self>) -> LayoutResult<()> {
        // Read the instance-pointer
        let inst = item.read()?;
        // If its place is relative, visit its dependencies first
        match &inst.loc {
            Place::Rel(rel) => {
                match rel.to {
                    Placeable::Instance(ref ptr) => orderer.push(ptr)?,
                    _ => unimplemented!(), // FIXME: other variants TBC
                };
            }
            Place::Abs(_) => (), // Nothing to traverse
        }
        Ok(())
    }
    fn fail() -> Result<(), Self::Error> {
        Err(LayoutError::msg("Instance placement ordering error"))
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
                sep: Separation::default(),
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

        // Get the sample data
        let SampleLib {
            ibig,
            big,
            lil,
            mut lib,
            mut parent,
        } = SampleLib::get()?;
        lib.name = "test_place3".into();

        // Relative-placement-adder closure
        let mut add_inst = |inst_name: &str, side, align| {
            let i = Instance {
                inst_name: inst_name.into(),
                cell: lil.clone(),
                loc: Place::Rel(RelativePlace {
                    to: Placeable::Instance(ibig.clone()),
                    side,
                    align,
                    sep: Separation::default(),
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
        let ibox = i1.read()?.boundbox()?;
        assert_eq!(ibox.side(Side::Right), bigbox.side(Side::Left));
        assert_eq!(ibox.side(Side::Bottom), bigbox.side(Side::Bottom));
        let ibox = i2.read()?.boundbox()?;
        assert_eq!(ibox.side(Side::Left), bigbox.side(Side::Right));
        assert_eq!(ibox.side(Side::Bottom), bigbox.side(Side::Bottom));
        let ibox = i3.read()?.boundbox()?;
        assert_eq!(ibox.side(Side::Top), bigbox.side(Side::Bottom));
        assert_eq!(ibox.side(Side::Left), bigbox.side(Side::Left));
        let ibox = i4.read()?.boundbox()?;
        assert_eq!(ibox.side(Side::Top), bigbox.side(Side::Bottom));
        assert_eq!(ibox.side(Side::Right), bigbox.side(Side::Right));
        let ibox = i5.read()?.boundbox()?;
        assert_eq!(ibox.side(Side::Right), bigbox.side(Side::Left));
        assert_eq!(ibox.side(Side::Top), bigbox.side(Side::Top));
        let ibox = i6.read()?.boundbox()?;
        assert_eq!(ibox.side(Side::Left), bigbox.side(Side::Right));
        assert_eq!(ibox.side(Side::Top), bigbox.side(Side::Top));
        let ibox = i7.read()?.boundbox()?;
        assert_eq!(ibox.side(Side::Bottom), bigbox.side(Side::Top));
        assert_eq!(ibox.side(Side::Left), bigbox.side(Side::Left));
        let ibox = i8.read()?.boundbox()?;
        assert_eq!(ibox.side(Side::Bottom), bigbox.side(Side::Top));
        assert_eq!(ibox.side(Side::Right), bigbox.side(Side::Right));
        exports(lib, stack)
    }
    #[test]
    fn test_place4() -> LayoutResult<()> {
        // Test size-of separation

        // Get the sample data
        let SampleLib {
            ibig,
            big,
            lil,
            mut lib,
            mut parent,
        } = SampleLib::get()?;
        lib.name = "test_place4".into();

        // Relative-placement-adder closure
        let mut add_inst = |inst_name: &str, side, sep| {
            let i = Instance {
                inst_name: inst_name.into(),
                cell: lil.clone(),
                loc: Place::Rel(RelativePlace {
                    to: Placeable::Instance(ibig.clone()),
                    side,
                    align: side.cw_90(), // Leave out `align` as an arg, set one-turn CW of `side`
                    sep,
                }),
                reflect_horiz: false,
                reflect_vert: false,
            };
            parent.instances.add(i)
        };
        // Add a bunch of em
        let sep_x = Separation::x(SepBy::SizeOf(lil.clone()));
        let i1 = add_inst("i1", Side::Left, sep_x.clone());
        let i2 = add_inst("i2", Side::Right, sep_x.clone());
        let sep_y = Separation::y(SepBy::SizeOf(lil.clone()));
        let i3 = add_inst("i3", Side::Bottom, sep_y.clone());
        let i4 = add_inst("i4", Side::Top, sep_y.clone());
        // Add `parent` to the library
        let _parent = lib.cells.add(parent.into());

        // The real code under test: run placement
        let (lib, stack) = Placer::place(lib, SampleStacks::empty()?)?;

        // And test the placed results
        let lilsize = lil.read()?.boundbox_size()?;
        let bigbox = ibig.read()?.boundbox()?;
        let ibox = i1.read()?.boundbox()?;
        assert_eq!(ibox.side(Side::Top), bigbox.side(Side::Top));
        assert_eq!(ibox.side(Side::Right), bigbox.side(Side::Left) - lilsize.x);
        let ibox = i2.read()?.boundbox()?;
        assert_eq!(ibox.side(Side::Bottom), bigbox.side(Side::Bottom));
        assert_eq!(ibox.side(Side::Left), bigbox.side(Side::Right) + lilsize.x);
        let ibox = i3.read()?.boundbox()?;
        assert_eq!(ibox.side(Side::Left), bigbox.side(Side::Left));
        assert_eq!(ibox.side(Side::Top), bigbox.side(Side::Bottom) - lilsize.y);
        let ibox = i4.read()?.boundbox()?;
        assert_eq!(ibox.side(Side::Right), bigbox.side(Side::Right));
        assert_eq!(ibox.side(Side::Bottom), bigbox.side(Side::Top) + lilsize.y);

        exports(lib, stack)
    }
    #[test]
    fn test_place5() -> LayoutResult<()> {
        // Test separation by units

        // Get the sample data
        let SampleLib {
            ibig,
            big,
            lil,
            mut lib,
            mut parent,
        } = SampleLib::get()?;
        lib.name = "test_place5".into();

        // Relative-placement-adder closure
        let mut add_inst = |inst_name: &str, side, sep| {
            let i = Instance {
                inst_name: inst_name.into(),
                cell: lil.clone(),
                loc: Place::Rel(RelativePlace {
                    to: Placeable::Instance(ibig.clone()),
                    side,
                    align: side.ccw_90(), // Leave out `align` as an arg, set one-turn CCW of `side`
                    sep,
                }),
                reflect_horiz: false,
                reflect_vert: false,
            };
            parent.instances.add(i)
        };
        // Add a bunch of em
        let dx = PrimPitches::new(Dir::Horiz, 1);
        let sep_x = Separation::x(SepBy::UnitSpeced(dx.clone().into()));
        let i1 = add_inst("i1", Side::Left, sep_x.clone());
        let i2 = add_inst("i2", Side::Right, sep_x.clone());
        let dy = PrimPitches::new(Dir::Vert, 5);
        let sep_y = Separation::y(SepBy::UnitSpeced(dy.clone().into()));
        let i3 = add_inst("i3", Side::Bottom, sep_y.clone());
        let i4 = add_inst("i4", Side::Top, sep_y.clone());
        // Add `parent` to the library
        let _parent = lib.cells.add(parent.into());

        // The real code under test: run placement
        let (lib, stack) = Placer::place(lib, SampleStacks::empty()?)?;

        // And test the placed results
        let lilsize = lil.read()?.boundbox_size()?;
        let bigbox = ibig.read()?.boundbox()?;
        let ibox = i1.read()?.boundbox()?;
        assert_eq!(ibox.side(Side::Bottom), bigbox.side(Side::Bottom));
        assert_eq!(ibox.side(Side::Right), bigbox.side(Side::Left) - dx);
        let ibox = i2.read()?.boundbox()?;
        assert_eq!(ibox.side(Side::Top), bigbox.side(Side::Top));
        assert_eq!(ibox.side(Side::Left), bigbox.side(Side::Right) + dx);
        let ibox = i3.read()?.boundbox()?;
        assert_eq!(ibox.side(Side::Right), bigbox.side(Side::Right));
        assert_eq!(ibox.side(Side::Top), bigbox.side(Side::Bottom) - dy);
        let ibox = i4.read()?.boundbox()?;
        assert_eq!(ibox.side(Side::Left), bigbox.side(Side::Left));
        assert_eq!(ibox.side(Side::Bottom), bigbox.side(Side::Top) + dy);

        exports(lib, stack)
    }
    pub struct SampleLib {
        pub lib: Library,
        pub big: Ptr<CellBag>,
        pub ibig: Ptr<Instance>,
        pub lil: Ptr<CellBag>,
        pub parent: LayoutImpl,
    }
    impl SampleLib {
        /// Get a sample library with test cells `big`, `lil`, and `parent`.
        /// Designed for adding instances of `lil` relative to `big` all around `parent`.
        pub fn get() -> LayoutResult<SampleLib> {
            let mut lib = Library::new("_rename_me_plz_");
            // Create a big center cell
            let big = LayoutImpl::new("big", 0, Outline::rect(11, 12)?).into();
            let big = lib.cells.add(big);
            // Create the parent cell which instantiates it
            let mut parent = LayoutImpl::new("parent", 0, Outline::rect(40, 35)?);
            // Create an initial instance
            let ibig = Instance {
                inst_name: "ibig".into(),
                cell: big.clone(),
                loc: (16, 15).into(),
                reflect_horiz: false,
                reflect_vert: false,
            };
            let ibig = parent.instances.add(ibig);
            // Create a unit cell which we'll instantiate a few times around `ibig`
            let lil = LayoutImpl::new("lil", 0, Outline::rect(2, 1)?).into();
            let lil = lib.cells.add(lil);
            Ok(SampleLib {
                lib,
                big,
                ibig,
                lil,
                parent,
            })
        }
    }
}
