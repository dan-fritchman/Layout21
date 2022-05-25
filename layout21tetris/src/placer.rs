//!
//! # Tetris Placer
//!
//! Converts potentially relatively-placed attributes to absolute positions.
//!

// Std-Lib Imports
use std::convert::TryFrom;

// Local imports
use crate::array::{Array, ArrayInstance, Arrayable};
use crate::bbox::HasBoundBox;
use crate::coords::{LayerPitches, PrimPitches, UnitSpeced, Xy};
use crate::library::Library;
use crate::placement::{Align, Place, Placeable, RelativePlace, SepBy, Side};
use crate::raw::{Dir, LayoutError, LayoutResult};
use crate::utils::{DepOrder, DepOrderer, ErrorContext, ErrorHelper, Ptr};
use crate::validate::ValidStack;
use crate::{
    abs, stack,
    tracks::{TrackCross, TrackRef},
};
use crate::{instance::Instance, layout::Layout};

/// # Placer
/// Converts all potentially-relatively-placed attributes to absolute positions.
pub struct Placer {
    lib: Library,
    stack: ValidStack,
    ctx: Vec<ErrorContext>,
}
impl Placer {
    /// [Placer] public API entrypoint.
    /// Modify and return [Library] `lib`, converting all [RelativePlace]s to absolute locations.
    pub fn place(lib: Library, stack: ValidStack) -> LayoutResult<(Library, ValidStack)> {
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
            let mut cell = cellptr.write()?;
            self.ctx.push(ErrorContext::Cell(cell.name.clone()));
            if let Some(ref mut layout) = cell.layout {
                self.place_layout(layout)?;
            }
            self.ctx.pop();
        }
        self.ctx.pop();
        Ok(())
    }
    /// Update placements for [Layout] `layout`
    fn place_layout(&mut self, layout: &mut Layout) -> LayoutResult<()> {
        self.ctx.push(ErrorContext::Impl);

        // Move `instances` and `places` into one vector of [Placeable]s
        let mut places: Vec<Placeable> = layout
            .instances
            .drain(..)
            .map(|i| Placeable::Instance(i))
            .collect();
        places.extend(layout.places.drain(..));

        // Iterate over `places` in dependency order, updating any relative-places to absolute.
        let mut ordered = PlaceOrder::order(&places)?;
        for place in ordered.drain(..) {
            match place {
                Placeable::Instance(ref inst_ptr) => {
                    let mut inst = inst_ptr.write()?;
                    if let Place::Rel(ref rel) = inst.loc {
                        // Convert to an absolute location
                        let abs = self.resolve_instance_place(&*inst, rel)?;
                        inst.loc = Place::Abs(abs);
                    }
                    // Add the now-absolute-placed inst to the `instances` list
                    layout.instances.push(inst_ptr.clone());
                }
                Placeable::Array(ref ptr) => {
                    let mut array_inst = ptr.write()?;
                    if let Place::Rel(ref rel) = array_inst.loc {
                        // Convert to an absolute location
                        let abs = self.resolve_array_place(&*array_inst, rel)?;
                        array_inst.loc = Place::Abs(abs);
                    }
                    // And flatten its instances
                    let children = self.flatten_array_inst(&*array_inst)?;
                    let children = children.into_iter().map(|i| Ptr::new(i));
                    layout.instances.extend(children);
                }
                Placeable::Assign(ref ptr) => {
                    let assn = ptr.read()?;
                    let abs: TrackCross = self.resolve_assign_place(&assn.loc)?;
                    let new_assn = stack::Assign {
                        net: assn.net.clone(),
                        at: abs,
                    };
                    layout.assignments.push(new_assn);
                }
                Placeable::Group(_) => unimplemented!(),
                Placeable::Port { .. } => (), // Nothing to do, at least until hitting something that *depends* on the Port location
            }
        }
        self.ctx.pop();
        Ok(())
    }
    /// Flatten an [ArrayInstance] to a vector of Cell Instances.
    /// Instance location must be absolute by call-time.
    fn flatten_array_inst(&mut self, array_inst: &ArrayInstance) -> LayoutResult<Vec<Instance>> {
        // Read the child-Instances from the underlying [Array] definition
        let mut children = {
            let array = array_inst.array.read()?;
            self.flatten_array(&*array, &array_inst.name)?
        };
        // Get its initial location
        let loc = array_inst.loc.abs()?;
        // Translate each child to our location and reflection
        for child in children.iter_mut() {
            let childloc = child.loc.abs_mut()?;
            if array_inst.reflect_horiz {
                // Reflect horizontally
                childloc.x *= -1_isize;
                child.reflect_horiz = !child.reflect_horiz;
            }
            if array_inst.reflect_vert {
                // Reflect vertically
                childloc.y *= -1_isize;
                child.reflect_vert = !child.reflect_vert;
            }
            // Translate its location
            *childloc += *loc;
        }
        Ok(children)
    }
    /// Flatten an [Array] to a vector of Cell Instances.
    fn flatten_array(&mut self, array: &Array, prefix: &str) -> LayoutResult<Vec<Instance>> {
        let mut insts = Vec::with_capacity(array.count);

        // Get the separations in each dimension
        let xsep = match array.sep.x {
            None => PrimPitches::x(0),
            Some(SepBy::UnitSpeced(u)) => {
                match u {
                    UnitSpeced::PrimPitches(p) => p.clone(),
                    _ => unimplemented!(), // TODO: other units
                }
            }
            Some(SepBy::SizeOf(_)) => unimplemented!(),
        };
        let ysep = match array.sep.y {
            None => PrimPitches::y(0),
            Some(SepBy::UnitSpeced(u)) => {
                match u {
                    UnitSpeced::PrimPitches(p) => p.clone(),
                    _ => unimplemented!(), // TODO: other units
                }
            }
            Some(SepBy::SizeOf(_)) => unimplemented!(),
        };
        let sep = Xy::new(xsep, ysep);

        // Initialize our location to the array-origin
        let mut loc = (0, 0).into();
        for i in 0..array.count {
            match &array.unit {
                Arrayable::Instance(cell) => {
                    let i = Instance {
                        inst_name: format!("{}[{}]", prefix, i), // `arrayname[i]`
                        cell: cell.clone(),
                        loc: Place::Abs(loc),
                        reflect_horiz: false,
                        reflect_vert: false,
                    };
                    insts.push(i);
                }
                Arrayable::Array(arr) => {
                    // Create a new [ArrayInstance] at the current location,
                    // largely for sake of reusing `flatten_array_inst`
                    // to get its located, flattened children.
                    let i = ArrayInstance {
                        name: format!("{}[{}]", prefix, i), // `arrayname[i]`
                        array: arr.clone(),
                        loc: Place::Abs(loc),
                        reflect_horiz: false,
                        reflect_vert: false,
                    };
                    // (Potentially recursively) flatten that short-lived [ArrayInstance]
                    let children = self.flatten_array_inst(&i)?;
                    // And add its children to ours
                    insts.extend(children);
                }
                Arrayable::Group(_arr) => unimplemented!(),
            };
            // Increment the location by our (two-dimensional) increment.
            loc += sep;
        }
        Ok(insts)
    }
    /// Resolve a location of [ArrayInstance] `inst` relative to its [RelativePlace] `rel`.
    fn resolve_array_place(
        &mut self,
        _inst: &ArrayInstance,
        _rel: &RelativePlace,
    ) -> LayoutResult<Xy<PrimPitches>> {
        // FIXME: this should just need the same stuff as `resolve_instance_place`,
        // once we have a consolidated version of [Instance] that covers Arrays.
        todo!()
    }
    /// Resolve a location of [Instance] `inst` relative to its [RelativePlace] `rel`.
    fn resolve_instance_place(
        &mut self,
        inst: &Instance,
        rel: &RelativePlace,
    ) -> LayoutResult<Xy<PrimPitches>> {
        self.ctx
            .push(ErrorContext::Instance(inst.inst_name.clone()));

        // Get the relative-to instance's bounding box
        let bbox = match rel.to {
            Placeable::Instance(ref ptr) => ptr.read()?.boundbox()?,
            Placeable::Array(ref ptr) => ptr.read()?.boundbox()?,
            Placeable::Group(_) => unimplemented!(),
            Placeable::Assign(_) => unimplemented!(),
            Placeable::Port { .. } => unimplemented!(),
        };
        // The coordinate axes here are referred to as `side`, corresponding to `rel.side`, and `align`, corresponding to `rel.align`.
        // Mapping these back to (x,y) happens at the very end.
        // FIXME: checks that `side` and `align` are orthogonal should come earlier

        // Get its edge-coordinates in each axis
        let mut side_coord = bbox.side(rel.side);
        let align_side = match rel.align {
            Align::Side(s) => s,
            _ => unimplemented!(),
        };
        let mut align_coord = bbox.side(align_side);
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
        let offset_align = match align_side {
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
    /// Resolve the location of a track-crossing at `rel`
    fn resolve_assign_place(&mut self, rel: &RelativePlace) -> LayoutResult<TrackCross> {
        let port_loc = match &rel.to {
            Placeable::Port { inst, port } => self.locate_instance_port(&*inst.read()?, port)?,
            Placeable::Instance(_) => unimplemented!(),
            Placeable::Array(_) => unimplemented!(),
            Placeable::Group(_) => unimplemented!(),
            Placeable::Assign(_) => unimplemented!(),
        };
        let ref_cross: (TrackRef, TrackRef) = match port_loc {
            PortLoc::ZTopEdge { track, range } => {
                let ortho_track = match rel.align {
                    Align::Side(s) => {
                        match s {
                            Side::Bottom | Side::Left => range.0.track, // FIXME: reflection support
                            Side::Top | Side::Right => range.1.track,   // FIXME: reflection support
                        }
                    }
                    Align::Center => (range.0.track + range.1.track) / 2,
                    Align::Ports(_, _) => unreachable!(),
                };
                (
                    track.clone(),
                    TrackRef {
                        layer: range.0.layer,
                        track: ortho_track,
                    },
                )
            }
            _ => unimplemented!(),
        };

        // Sort out separation (in tracks) in (x, y)
        let _sep_x = match &rel.sep.x {
            Some(_) => unimplemented!(),
            None => 0_usize,
        };
        let _sep_y = match &rel.sep.y {
            Some(_) => unimplemented!(),
            None => 0_usize,
        };
        // Sort out the layer-based z-separation
        let sep_z = match &rel.sep.z {
            Some(i) => *i,
            None => 0,
        };
        let newlayer = isize::try_from(ref_cross.0.layer)? + sep_z;
        let newlayer = usize::try_from(newlayer)?;

        // Of the two layers in `ref_cross`, one will be in parallel with `newlayer`, and one will be orthogonal to it.
        // Set `par` as the parallel track, and `cross` as the orthogonal track.
        let (par, cross) = {
            let newlayer_dir = self.stack.metal(newlayer)?.spec.dir;
            if newlayer_dir == self.stack.metal(ref_cross.0.layer)?.spec.dir {
                (ref_cross.0, ref_cross.1)
            } else if newlayer_dir == self.stack.metal(ref_cross.1.layer)?.spec.dir {
                (ref_cross.1, ref_cross.0)
            } else {
                return self.fail("Invalid non-crossing TrackCross");
            }
        };

        // Get the track on `newlayer` closest to `par`
        let new_track: TrackRef = self.convert_track_layer(&par, newlayer)?;
        // And turn the combination into our result [TrackCross]
        let rv = TrackCross::new(new_track, cross);
        Ok(rv)
    }
    /// Resolve a location of [Instance] `inst` relative to its [RelativePlace] `rel`.
    fn locate_instance_port(&mut self, inst: &Instance, portname: &str) -> LayoutResult<PortLoc> {
        // Port locations are only valid for Cells with Abs definitions. Otherwise fail.
        let cell = inst.cell.read()?; // Note `cell` is alive for the duration of this function
        let abs = self.unwrap(
            cell.abs.as_ref(),
            format!(
                "Cannot Location Port {} on Cell {} with no Abs View",
                portname, cell.name
            ),
        )?;
        // Get the Port-object, or fail
        let port = self.unwrap(
            abs.port(portname),
            format!("Cell {} has no Port {}", cell.name, portname),
        )?;

        let loc = match &port.kind {
            abs::PortKind::Edge { .. /*layer, track, side*/ } => unimplemented!(),
            abs::PortKind::ZTopEdge { track, side, into } => {
                let top_metal = self.unwrap(cell.top_metal()?, "No metal layers")?;
                let (dir, nsignals) = {
                    // Get relevant data from our [Layer], and quickly drop a reference to it.
                    let layer = self.stack.metal(top_metal)?;
                    (layer.spec.dir, layer.period_data.signals.len())
                };
                let port_track = {
                    let layer_pitches =
                        self.layer_pitches(top_metal, inst.loc.abs()?[!dir].into())?;

                    // Get the port's track-index, combining in the instance-location
                    let (_, period_tracks) = (layer_pitches * nsignals).into_inner();
                    let period_tracks = usize::try_from(period_tracks)?;
                    if inst.reflected(!dir) {
                        period_tracks - track - 1
                    } else {
                        period_tracks + track
                    }
                };

                // Sort out the orthogonal-axis range.
                let ortho_layer = match into.1 {
                    stack::RelZ::Above => top_metal + 1,
                    stack::RelZ::Below => top_metal - 1,
                };
                let ortho_range = {
                    let layer = &self.stack.metal(ortho_layer)?;
                    let nsignals = layer.period_data.signals.len();

                    // Find the origin and size in `ortho_layer` tracks
                    // FIXME: probably make this a method, and/or a track-index `HasUnits`
                    let loc = inst.loc.abs()?[dir];
                    let loc = self.layer_pitches(ortho_layer, loc.into())?;
                    let (_, loc) = (loc * nsignals).into_inner();
                    let loc = usize::try_from(loc)?;
                    let size = inst.boundbox_size()?[dir];
                    let size = self.layer_pitches(ortho_layer, size.into())?;
                    let (_, size) = (size * nsignals).into_inner();
                    let size = usize::try_from(size)?;

                    // Pull out the number of tracks in the orthogonal-axis
                    let into = into.0;
                    // And sort out the orthogonal range, based on location, size, and `into` tracks 
                    match (side, inst.reflected(dir)) {
                        (abs::Side::BottomOrLeft, false)=> (loc, loc+into),
                        (abs::Side::BottomOrLeft, true)=> (loc-into, loc),
                        (abs::Side::TopOrRight, false)=> (loc+into, loc+size),
                        (abs::Side::TopOrRight, true)=> (loc-size, loc-into),
                    }
                };

                // From all that, create a [PortLoc]
                PortLoc::ZTopEdge {
                    track: TrackRef {
                        layer: top_metal,
                        track: port_track,
                    },
                    range: (
                        TrackRef {
                            layer: ortho_layer,
                            track: ortho_range.0,
                        },
                        TrackRef {
                            layer: ortho_layer,
                            track: ortho_range.1
                        },
                    ),
                }
            }
            abs::PortKind::ZTopInner { .. } => unimplemented!(),
        };
        Ok(loc)
    }
    /// Convert a [TrackRef] to the closest track on another same-direction layer `to_layer`.
    fn convert_track_layer(
        &mut self,
        trackref: &TrackRef,
        to_layer: usize,
    ) -> LayoutResult<TrackRef> {
        if trackref.layer == to_layer {
            // Same layer, no conversion needed
            return Ok(trackref.clone());
        }
        let track_layer_dir = &self.stack.metal(trackref.layer)?.spec.dir;
        let to_layer_dir = &self.stack.metal(to_layer)?.spec.dir;
        if track_layer_dir != to_layer_dir {
            // Orthogonal layers. Fail.
            self.fail(format!(
                "Cannot convert between tracks on {:?} layer {} and {:?} layer {}",
                track_layer_dir, trackref.layer, to_layer_dir, to_layer
            ))?;
        }
        // Normal case -  actually do some work.
        // First find the starting-track's center in [DbUnits]
        let track_center = self.stack.metal(trackref.layer)?.center(trackref.track)?;
        // And get the corresponding track-index on the other layer
        let to_layer_index = self.stack.metal(to_layer)?.track_index(track_center)?;
        Ok(TrackRef {
            layer: to_layer,
            track: to_layer_index,
        })
    }
    /// Convert a [UnitSpeced] distance `dist` into [LayerPitches] on layer `layer_index`.
    /// Fails if `dist` is not an integer multiple of the pitch of `layer_index`.
    fn layer_pitches(
        &mut self,
        layer_index: usize,
        dist: UnitSpeced,
    ) -> LayoutResult<LayerPitches> {
        let layer = &self.stack.metal(layer_index)?;
        let layer_pitch = layer.pitch;
        let num = match dist {
            UnitSpeced::DbUnits(_) => unimplemented!(),
            UnitSpeced::LayerPitches(_) => unimplemented!(),
            UnitSpeced::PrimPitches(p) => {
                let dir = layer.spec.dir;
                let prim_pitch = self.stack.prim.pitches[dir.other()];
                if layer_pitch % prim_pitch != 0 {
                    self.fail(format!(
                        "Invalid Conversion: Primitive (pitch={:?}) to Layer {} (pitch={:?})",
                        prim_pitch, layer_index, layer_pitch
                    ))?;
                }
                p.num * (layer_pitch / prim_pitch)
            }
        };
        Ok(LayerPitches::new(layer_index, num))
    }
}

/// Resolved Locations corresponding to [abs::PortKind]s
#[derive(Debug, Clone)]
pub enum PortLoc {
    Edge {
        /// Port Track
        track: TrackRef,
        /// Crossing Track
        at: TrackRef,
    },
    ZTopEdge {
        /// Port Track
        track: TrackRef,
        /// Extent on an adjacent layer
        range: (TrackRef, TrackRef),
    },
    ZTopInner {
        /// Locations
        locs: Vec<Tbd>,
    },
}
#[derive(Debug, Clone)]
pub struct Tbd;
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
struct PlaceOrder;
impl DepOrder for PlaceOrder {
    type Item = Placeable;
    type Error = LayoutError;

    /// Process [Instance]-pointer `item`
    fn process(item: &Placeable, orderer: &mut DepOrderer<Self>) -> LayoutResult<()> {
        match item {
            Placeable::Instance(ref p) => {
                let inst = p.read()?;
                if let Place::Rel(rel) = &inst.loc {
                    orderer.push(&rel.to)?; // Visit the dependency first
                }
            }
            Placeable::Array(ref p) => {
                if let Place::Rel(rel) = &p.read()?.loc {
                    orderer.push(&rel.to)?; // Visit the dependency first
                }
            }
            Placeable::Group(ref p) => {
                if let Place::Rel(rel) = &p.read()?.loc {
                    orderer.push(&rel.to)?; // Visit the dependency first
                }
            }
            Placeable::Assign(a) => {
                // Always relative, push it unconditionally
                let a = a.read()?;
                orderer.push(&a.loc.to)?;
            }
            Placeable::Port { ref inst, .. } => {
                if let Place::Rel(rel) = &inst.read()?.loc {
                    orderer.push(&rel.to)?; // Visit the dependency first
                }
            }
        };
        Ok(())
    }
    fn fail() -> Result<(), Self::Error> {
        Err(LayoutError::msg("Placement ordering error"))
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::cell::Cell;
    use crate::outline::Outline;
    use crate::placement::{Place, Placeable, RelAssign, RelativePlace, SepBy, Separation, Side};
    use crate::tests::{exports, stacks::SampleStacks};

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
        let unit = Layout::new("unit", 0, Outline::rect(3, 7)?);
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
        let mut parent = Layout::new("parent", 0, Outline::rect(100, 100)?);
        let i0 = parent.instances.add(i0);
        // Create another Instance, placed relative to `i0`
        let i1 = Instance {
            inst_name: "i1".into(),
            cell: unit.clone(),
            loc: Place::Rel(RelativePlace {
                to: Placeable::Instance(i0.clone()),
                side: Side::Right,
                align: Align::Side(Side::Bottom),
                sep: Separation::default(),
            }),
            reflect_horiz: false,
            reflect_vert: false,
        };
        let i1 = parent.instances.add(i1);
        let parent = lib.cells.add(parent);

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
            // big,
            lil,
            mut lib,
            mut parent,
            ..
        } = SampleLib::get()?;
        lib.name = "test_place3".into();
        let relto = Placeable::Instance(ibig.clone());

        // Relative-placement-adder closure
        let mut add_inst = |inst_name: &str, side, align| {
            let i = Instance {
                inst_name: inst_name.into(),
                cell: lil.clone(),
                loc: Place::Rel(RelativePlace {
                    to: relto.clone(),
                    side,
                    align: Align::Side(align),
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
        let _parent = lib.cells.add(parent);

        // The real code under test: run placement
        let (lib, stack) = Placer::place(lib, SampleStacks::pdka()?)?;

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
            // big,
            lil,
            mut lib,
            mut parent,
            ..
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
                    align: Align::Side(side.cw_90()), // Leave out `align` as an arg, set one-turn CW of `side`
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
        let _parent = lib.cells.add(parent);

        // The real code under test: run placement
        let (lib, stack) = Placer::place(lib, SampleStacks::pdka()?)?;

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
            // big,
            lil,
            mut lib,
            mut parent,
            ..
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
                    align: Align::Side(side.ccw_90()), // Leave out `align` as an arg, set one-turn CCW of `side`
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
        let _parent = lib.cells.add(parent);

        // The real code under test: run placement
        let (lib, stack) = Placer::place(lib, SampleStacks::pdka()?)?;

        // And test the placed results
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
    #[test]
    fn test_place6() -> LayoutResult<()> {
        // Test port-relative placement

        // Get the sample data
        let SampleLib {
            // ibig,
            // big,
            lil,
            mut lib,
            mut parent,
            ..
        } = SampleLib::get()?;
        lib.name = "test_place6".into();

        // Relative-placement-adder closure
        let mut add_inst = |inst_name: &str| {
            let i = Instance {
                inst_name: inst_name.into(),
                cell: lil.clone(),
                loc: (0, 0).into(),
                reflect_horiz: false,
                reflect_vert: false,
            };
            parent.instances.add(i)
        };
        // Add a `lil`
        let i1 = add_inst("i1");

        // The "code under test": add a relative-placed `Assign`.
        parent.places.push(Placeable::Assign(Ptr::new(RelAssign {
            net: "NETPPP".into(),
            loc: RelativePlace {
                to: Placeable::Port {
                    inst: i1.clone(),
                    port: "PPP".into(),
                },
                align: Align::Center,
                side: Side::Left, // FIXME: kinda nonsense
                sep: Separation::z(2),
            },
        })));
        // Add `parent` to the library
        let parent = lib.cells.add(parent);

        // The real code under test: run placement
        let (lib, stack) = Placer::place(lib, SampleStacks::pdka()?)?;

        {
            let p = parent.read()?;
            let parent_layout = p.layout.as_ref().unwrap();
            assert_eq!(parent_layout.places.len(), 0);
            assert_eq!(parent_layout.instances.len(), 2);
            assert_eq!(parent_layout.cuts.len(), 0);
            assert_eq!(parent_layout.assignments.len(), 1);
            let assn = &parent_layout.assignments[0];
            assert_eq!(assn.net, "NETPPP");
            assert_eq!(assn.at.track.layer, 2);
            assert_eq!(assn.at.track.track, 0);
            assert_eq!(assn.at.cross.layer, 1);
            assert_eq!(assn.at.cross.track, 1);
        }
        exports(lib, stack)
    }
    pub struct SampleLib {
        pub lib: Library,
        pub big: Ptr<Cell>,
        pub ibig: Ptr<Instance>,
        pub lil: Ptr<Cell>,
        pub parent: Layout,
    }
    impl SampleLib {
        /// Get a sample library with test cells `big`, `lil`, and `parent`.
        /// Designed for adding instances of `lil` relative to `big` all around `parent`.
        pub fn get() -> LayoutResult<SampleLib> {
            let mut lib = Library::new("_rename_me_plz_");
            // Create a big center cell
            let big = Layout::new("big", 1, Outline::rect(11, 12)?);
            let big = lib.cells.add(big);
            // Create the parent cell which instantiates it
            let mut parent = Layout::new("parent", 3, Outline::rect(40, 35)?);
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
            let mut lil = Cell::new("lil");
            lil.layout = Some(Layout::new("lil", 1, Outline::rect(2, 1)?));
            let mut lil_abs = abs::Abstract::new("lil", 1, Outline::rect(2, 1)?);
            lil_abs.ports.push(abs::Port {
                name: "PPP".into(),
                kind: abs::PortKind::ZTopEdge {
                    track: 0,
                    side: abs::Side::BottomOrLeft,
                    into: (2, stack::RelZ::Above),
                },
            });
            lil.abs = Some(lil_abs);
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
