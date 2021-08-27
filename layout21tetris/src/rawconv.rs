//!
//! # Raw-Layout Conversion Module
//!

// Std-lib
use std::collections::{HashMap, HashSet};
use std::convert::TryFrom;
use std::fmt::Debug;

// Crates.io
use slotmap::{new_key_type, SlotMap};

// Local imports
use crate::cell::{Instance, LayoutImpl};
use crate::coords::{DbUnits, HasUnits, PrimPitches, UnitSpeced, Xy};
use crate::library::Library;
use crate::outline::Outline;
use crate::raw::{self, Dir, HasErrors, LayoutError, LayoutResult, Point};
use crate::stack::{LayerPeriod, RelZ, Stack, Track, TrackError, TrackSegmentType};
use crate::utils::Ptr;
use crate::{abstrakt, cell, validate};

// Create key-types for each internal type stored in [SlotMap]s
new_key_type! {
    /// Keys for [ValidAssign] entries
    pub struct AssignKey;
}
/// A short-lived Cell, largely organized by layer
#[derive(Debug, Clone)]
struct TempCell<'lib> {
    /// Reference to the source [Cell]
    cell: &'lib LayoutImpl,
    /// Reference to the source [Library]
    lib: &'lib Library,
    /// Instances and references to their definitions
    instances: Vec<&'lib Instance>,
    /// Cuts, arranged by Layer
    cuts: Vec<Vec<validate::ValidCut<'lib>>>,
    /// Validated Assignments
    assignments: SlotMap<AssignKey, validate::ValidAssign<'lib>>,
    /// Assignments, arranged by Layer
    top_assns: Vec<Vec<AssignKey>>,
    /// Assignments, arranged by Layer
    bot_assns: Vec<Vec<AssignKey>>,
}
/// Temporary arrangement of data for a [Layer] within a [Cell]
#[derive(Debug, Clone)]
struct TempCellLayer<'lib> {
    /// Reference to the validated metal layer
    layer: &'lib validate::ValidMetalLayer<'lib>,
    /// Reference to the parent cell
    cell: &'lib TempCell<'lib>,
    /// Instances which intersect with this layer and period
    instances: Vec<&'lib Instance>,
    /// Pitch per layer-period
    pitch: DbUnits,
    /// Number of layer-periods
    nperiods: usize,
    /// Spanning distance in the layer's "infinite" dimension
    span: DbUnits,
}

/// Short-Lived structure of the stuff relevant for converting a single LayerPeriod,
/// on a single Layer, in a single Cell.
#[derive(Debug, Clone)]
struct TempPeriod<'lib> {
    periodnum: usize,
    cell: &'lib TempCell<'lib>,
    layer: &'lib TempCellLayer<'lib>,
    /// Instance Blockages
    blockages: Vec<(PrimPitches, PrimPitches, &'lib Instance)>,
    cuts: Vec<&'lib validate::ValidCut<'lib>>,
    top_assns: Vec<AssignKey>,
    bot_assns: Vec<AssignKey>,
}

/// # Dependency-Orderer
///
/// Ideally an iterator, but really just a struct that creates an in-order [Vec] at creation time.
#[derive(Debug)]
pub struct DepOrder<'lib> {
    lib: &'lib Library,
    stack: Vec<Ptr<cell::CellBag>>,
    seen: HashSet<Ptr<cell::CellBag>>, // FIXME: should be able to use [Ptr]
}
impl<'lib> DepOrder<'lib> {
    fn order(lib: &'lib Library) -> Vec<Ptr<cell::CellBag>> {
        // FIXME: now that this uses `name` as the `seen`-set,
        // move to check for uniqueness of names *before* running it.
        let mut myself = Self {
            lib,
            stack: Vec::new(),
            seen: HashSet::new(),
        };
        for cell in myself.lib.cells.as_slice() {
            myself.push(cell);
        }
        myself.stack
    }
    fn push(&mut self, ptr: &Ptr<cell::CellBag>) {
        // If the Cell hasn't already been visited, depth-first search it
        if !self.seen.contains(&ptr) {
            // Read the cell-pointer
            let cell = ptr.read().unwrap();
            // If the cell has an implementation, visit its [Instance]s before inserting it
            if let Some(layout) = &cell.layout {
                for inst in &layout.instances {
                    self.push(&inst.cell);
                }
            }
            // And insert the cell (pointer) itself
            self.seen.insert(Ptr::clone(ptr));
            self.stack.push(Ptr::clone(ptr));
        }
    }
}

/// # Converter from [Library] and constituent elements to [raw::Library]
pub struct RawExporter<'lib> {
    /// Source [Library]
    lib: Library,
    /// Source (validated) [Stack]
    stack: validate::ValidStack<'lib>,
    /// HashMap from source [CellBag] to exported [CellKey],
    /// largely for lookup during conversion of [Instance]s
    rawcells: HashMap<Ptr<cell::CellBag>, raw::CellKey>,
}
impl<'lib> RawExporter<'lib> {
    /// Convert the combination of a [Library] `lib` and [Stack] `stack` to a [raw::Library].
    /// Both `lib` and `stack` are consumed in the process.
    pub fn convert(lib: Library, stack: Stack) -> LayoutResult<Ptr<raw::Library>> {
        // FIXME: validate the library too
        let stack = validate::StackValidator::validate(stack)?;

        let mut myself = Self {
            lib,
            stack,
            rawcells: HashMap::new(),
        };
        myself.convert_stack()?;
        myself.convert_lib()
    }
    /// "Convert" our [Stack]. Really just checks a few properties are valid.
    fn convert_stack(&self) -> LayoutResult<()> {
        // Require our [Stack] specify both:
        // (a) set of [raw::Layers], and
        // (b) a boundary layer
        // Both these fields are frequently `unwrap`ed hereafter.
        if !self.stack.rawlayers.is_some() {
            return self.fail("Raw export failed: no [raw::Layers] specified");
        }
        if !self.stack.boundary_layer.is_some() {
            return self.fail("Raw export failed: no [raw::Layers] specified");
        }
        Ok(())
    }
    /// Convert everything in our [Library]
    fn convert_lib(&mut self) -> LayoutResult<Ptr<raw::Library>> {
        // Get our starter raw-lib, either anew or from any we've imported
        let rawlibptr = if self.lib.rawlibs.len() == 0 {
            // Create a new [raw::Library]
            let mut rawlib = raw::Library::new(&self.lib.name, self.stack.units);
            rawlib.layers = Ptr::clone(self.stack.rawlayers.as_ref().unwrap());
            Ok(Ptr::new(rawlib))
        } else if self.lib.rawlibs.len() == 1 {
            // Pop the sole raw-library, and use it as a starting point
            let rawlibptr = self.lib.rawlibs.pop().unwrap();
            let mut rawlib = rawlibptr.write()?;
            rawlib.name = self.lib.name.to_string();
            if rawlib.units != self.stack.units {
                // Check that the units match, or fail
                return self.fail(format!(
                    "NotImplemented: varying units between raw and tetris libraries: {:?} vs {:?}",
                    rawlib.units, self.stack.units,
                ));
            }
            drop(rawlib);
            Ok(rawlibptr)
        } else {
            // Multiple raw-libraries will require some merging
            // Probably not difficult, but not done yet either.
            self.fail("NotImplemented: with multiple [raw::Library")
        }?;
        // Get write-access to the raw-lib
        let mut rawlib = rawlibptr.write()?;
        // Convert each defined [Cell] to a [raw::Cell]
        for srcptr in DepOrder::order(&self.lib).iter() {
            let rawkey = self.convert_cell(&*srcptr.read()?, &mut rawlib.cells)?;
            self.rawcells.insert(Ptr::clone(srcptr), rawkey);
        }
        drop(rawlib);
        Ok(rawlibptr)
    }
    /// Convert a [CellBag] to a [raw::Cell] and add to `rawcells`.
    /// In reality only one of the cell-views is converted,
    /// generally the "most specific" available view.
    fn convert_cell(
        &self,
        cell: &cell::CellBag,
        rawcells: &mut SlotMap<raw::CellKey, raw::Cell>,
    ) -> LayoutResult<raw::CellKey> {
        if let Some(ref x) = cell.raw {
            // Raw definitions store the "key" pointer
            // Just return a copy of it
            Ok(x.cell.clone())
        } else if let Some(ref x) = cell.layout {
            Ok(rawcells.insert(self.convert_layout_impl(x)?))
        } else if let Some(ref x) = cell.abstrakt {
            Ok(rawcells.insert(self.convert_abstract(x)?))
        } else {
            self.fail(format!(
                "No Abstract or Implementation found for cell {}",
                cell.name
            ))
        }
    }
    /// Convert to a raw layout cell
    fn convert_layout_impl(&self, layout: &LayoutImpl) -> LayoutResult<raw::Cell> {
        if layout.outline.x.len() > 1 {
            return Err(LayoutError::Str(
                "Non-rectangular outline; conversions not supported (yet)".into(),
            ));
        };
        let mut elems: Vec<raw::Element> = Vec::new();
        // Re-organize the cell into the format most helpful here
        let temp_cell = self.temp_cell(layout)?;
        // Convert a layer at a time, starting from bottom
        for layernum in 0..layout.top_layer {
            // Organize the cell/layer combo into temporary conversion format
            let temp_layer = self.temp_cell_layer(&temp_cell, &self.stack.metals[layernum])?;
            // Convert each "layer period" one at a time
            for periodnum in 0..temp_layer.nperiods {
                // Again, re-organize into the relevant objects for this "layer period"
                let temp_period = self.temp_cell_layer_period(&temp_layer, periodnum)?;
                // And finally start doing stuff!
                elems.extend(self.convert_cell_layer_period(&temp_period)?);
            }
        }
        // Convert our [Outline] into a polygon
        elems.push(self.convert_outline(&layout.outline)?);
        // Convert our [Instance]s
        let insts = layout
            .instances
            .iter()
            .map(|inst| self.convert_instance(inst).unwrap()) // FIXME: error handling
            .collect();
        // Aaaand create our new [raw::Cell]
        Ok(raw::Cell {
            name: layout.name.clone(),
            insts,
            elems,
            ..Default::default()
        })
    }
    /// Convert an [Instance] to a [raw::Instance]
    fn convert_instance(&self, inst: &Instance) -> LayoutResult<raw::Instance> {
        // Get the raw-cell pointer from our mapping.
        // Note this requires dependent cells be converted first, depth-wise.
        let rawkey = self.unwrap(
            self.rawcells.get(&inst.cell),
            format!("Internal Error Exporting Instance {}", inst.inst_name),
        )?;
        // Primarily scale the location of each instance by our pitches
        Ok(raw::Instance {
            inst_name: inst.inst_name.clone(),
            cell: rawkey.clone(),
            p0: self.convert_xy(&inst.loc),
            reflect: inst.reflect,
            angle: inst.angle,
        })
    }
    /// Create a [TempCell], organizing [Cell] data in more-convenient fashion for conversion
    fn temp_cell<'a>(&'a self, layout: &'a LayoutImpl) -> LayoutResult<TempCell<'a>> {
        // Collect references to its instances
        let instances: Vec<&Instance> = layout
            .instances
            .iter()
            // .map(|inst| &inst)
            .collect();
        // Validate `cuts`, and arrange them by layer
        let mut cuts: Vec<Vec<validate::ValidCut>> = vec![vec![]; layout.top_layer];
        for cut in layout.cuts.iter() {
            let c = validate::ValidCut::validate(cut, &self.stack)?;
            cuts[c.layer].push(c);
            // FIXME: cell validation should also check that this lies within our outline. probably do this earlier
        }
        // Validate all the cell's assignments, and arrange references by layer
        let mut bot_assns = vec![vec![]; layout.top_layer];
        let mut top_assns = vec![vec![]; layout.top_layer];
        let mut assignments = SlotMap::with_key();
        for assn in layout.assignments.iter() {
            let v = validate::ValidAssign::validate(assn, &self.stack)?;
            let bot = v.loc.bot.layer;
            let top = v.loc.top.layer;
            let k = assignments.insert(v);
            bot_assns[bot].push(k);
            top_assns[top].push(k);
        }
        // And create our (temporary) cell data!
        Ok(TempCell {
            cell: layout,
            lib: &self.lib,
            instances,
            assignments,
            top_assns,
            bot_assns,
            cuts,
        })
    }
    /// Convert a single row/col (period) on a single layer in a single Cell.
    fn convert_cell_layer_period(
        &self,
        temp_period: &TempPeriod,
    ) -> LayoutResult<Vec<raw::Element>> {
        let mut elems: Vec<raw::Element> = Vec::new();
        let layer = temp_period.layer.layer; // FIXME! Can't love this name.

        // Create the layer-period object we'll manipulate most of the way
        let mut layer_period = temp_period
            .layer
            .layer
            .spec
            .to_layer_period(temp_period.periodnum, temp_period.layer.span.0)?;
        // Insert blockages on each track
        for (n1, n2, inst) in temp_period.blockages.iter() {
            // Convert primitive-pitch-based blockages to db units
            let start = self.db_units(*n1);
            let stop = self.db_units(*n2);
            let res = layer_period.block(start, stop, inst);
            self.ok(
                res,
                format!(
                    "Could not insert blockage on Layer {:?}, period {} from {:?} to {:?}",
                    layer, temp_period.periodnum, start, stop
                ),
            )?;
        }
        // Place all relevant cuts
        let nsig = layer_period.signals.len();
        for cut in temp_period.cuts.iter() {
            // Cut the assigned track
            let track = &mut layer_period.signals[cut.track % nsig];
            let dist = cut.xy[layer.spec.dir];
            let res = track.cut(
                dist - layer.spec.cutsize / 2, // start
                dist + layer.spec.cutsize / 2, // stop
                cut.src,                       // src
            );
            self.ok(
                res,
                format!("Could not make track-cut {:?} in {:?}", cut, temp_period),
            )?;
        }
        // Handle Net Assignments
        // Start with those for which we're the lower of the two layers.
        // These will also be where we add vias
        let via_layer = &self.stack.vias[layer.index];

        for assn_id in temp_period.bot_assns.iter() {
            let assn = self.unwrap(
                temp_period.cell.assignments.get(*assn_id),
                "Internal error: invalid assignment",
            )?;
            self.assign_track(layer, &mut layer_period, assn, false)?;

            // Create the via element
            let e = raw::Element {
                net: None, // FIXME: add net labels to these vias
                layer: via_layer.raw.unwrap(),
                purpose: raw::LayerPurpose::Drawing,
                inner: raw::Shape::Rect {
                    p0: self.convert_point(
                        assn.loc.xy.x - via_layer.size.x / 2,
                        assn.loc.xy.y - via_layer.size.y / 2,
                    ),
                    p1: self.convert_point(
                        assn.loc.xy.x + via_layer.size.x / 2,
                        assn.loc.xy.y + via_layer.size.y / 2,
                    ),
                },
            };
            elems.push(e);
        }

        // Assign all the segments for which we're the top layer
        for assn_id in temp_period.top_assns.iter() {
            let assn = self.unwrap(
                temp_period.cell.assignments.get(*assn_id),
                "Internal error: invalid assignment",
            )?;
            self.assign_track(layer, &mut layer_period, assn, true)?;
        }

        // Convert all TrackSegments to raw Elements
        for t in layer_period.rails.iter() {
            elems.extend(self.convert_track(t, &layer)?);
        }
        for t in layer_period.signals.iter() {
            elems.extend(self.convert_track(t, &layer)?);
        }
        Ok(elems)
    }
    /// Set the net corresponding to `assn` on layer `layer`.
    ///
    /// The type signature, particularly lifetimes, aren't pretty.
    /// <'f> is the short "function lifetime" of the argument references
    pub fn assign_track<'f>(
        &self,
        layer: &'f validate::ValidMetalLayer<'lib>,
        layer_period: &'f mut LayerPeriod<'lib>,
        assn: &'f validate::ValidAssign<'lib>,
        top: bool, // Boolean indication of whether to assign `top` or `bot`. FIXME: not our favorite.
    ) -> LayoutResult<()> {
        // Grab a (mutable) reference to the assigned track
        let nsig = layer_period.signals.len();
        let track = if top {
            assn.loc.top.track
        } else {
            assn.loc.bot.track
        };
        let track = &mut layer_period.signals[track % nsig];
        // And set the net at the assignment's location
        match track.set_net(assn.loc.xy[layer.spec.dir], &assn.src) {
            Ok(()) => Ok(()),
            Err(TrackError::BlockageConflict(x)) => self.fail(format!(
                "Assignment {:?} conflicts with Instance Blockage for {:?}",
                assn.src, x
            )),
            Err(TrackError::CutConflict(x)) => self.fail(format!(
                "Assignment {:?} conflicts with Cut at {:?}",
                assn.src, x
            )),
            Err(TrackError::OutOfBounds(dist)) => self.fail(format!(
                "{:?} out-of-bounds at distance {:?}",
                assn.src, dist
            )),
            Err(TrackError::Overlap(d0, d1)) => self.fail(format!(
                "{:?} trying to cut across segments at ({:?}, {:?})",
                assn.src, d0, d1
            )),
        }?;
        Ok(())
    }
    /// Convert an [Abstract]
    /// FIXME: make these [raw::LayoutAbstract] instead
    pub fn convert_abstract(&self, abs: &abstrakt::LayoutAbstract) -> LayoutResult<raw::Cell> {
        let mut elems = Vec::with_capacity(abs.ports.len() + 1);
        // Create our [Outline]s boundary
        elems.push(self.convert_outline(&abs.outline)?);
        // Create shapes for each pin
        for port in abs.ports.iter() {
            // Add its shape
            use abstrakt::PortKind::{Edge, ZTopEdge, ZTopInner};

            let elem = match &port.kind {
                Edge {
                    layer: layer_index,
                    track,
                    side,
                } => {
                    let layer = &self.stack.metals[*layer_index].spec;
                    // First get the "infinite dimension" coordinate from the edge
                    let infdims: (DbUnits, DbUnits) = match side {
                        abstrakt::Side::BottomOrLeft => (DbUnits(0), DbUnits(100)),
                        abstrakt::Side::TopOrRight => {
                            // FIXME: this assumes rectangular outlines; will take some more work for polygons.
                            let outside = self.db_units(abs.outline.max(layer.dir));
                            (outside - DbUnits(100), outside)
                        }
                    };
                    // Now get the "periodic dimension" from our layer-center
                    let perdims: (DbUnits, DbUnits) = self.track_span(*layer_index, *track)?;
                    // Presuming we're horizontal, points are here:
                    let mut pts = [Xy::new(infdims.0, perdims.0), Xy::new(infdims.1, perdims.1)];
                    // And if vertical, just transpose them
                    if layer.dir == Dir::Vert {
                        pts[0] = pts[0].transpose();
                        pts[1] = pts[1].transpose();
                    }
                    raw::Element {
                        net: Some(port.name.clone()),
                        layer: self.stack.metals[*layer_index].raw.unwrap(),
                        purpose: raw::LayerPurpose::Drawing,
                        inner: raw::Shape::Rect {
                            p0: self.convert_xy(&pts[0]),
                            p1: self.convert_xy(&pts[1]),
                        },
                    }
                }
                ZTopEdge { track, side, into } => {
                    let layer = &self.stack.metals[abs.top_layer].spec;
                    let other_layer_index = match into.1 {
                        RelZ::Above => abs.top_layer + 1,
                        RelZ::Below => abs.top_layer - 1,
                    };
                    let other_layer = &self.stack.metals[other_layer_index];
                    let other_layer_center = other_layer.center(into.0)?;
                    // First get the "infinite dimension" coordinate from the edge
                    let infdims: (DbUnits, DbUnits) = match side {
                        abstrakt::Side::BottomOrLeft => (DbUnits(0), other_layer_center),
                        abstrakt::Side::TopOrRight => {
                            // FIXME: this assumes rectangular outlines; will take some more work for polygons.
                            let outside = self.db_units(abs.outline.max(layer.dir));
                            (other_layer_center, outside)
                        }
                    };
                    // Now get the "periodic dimension" from our layer-center
                    let perdims: (DbUnits, DbUnits) = self.track_span(abs.top_layer, *track)?;
                    // Presuming we're horizontal, points are here:
                    let mut pts = [Xy::new(infdims.0, perdims.0), Xy::new(infdims.1, perdims.1)];
                    // And if vertical, just transpose them
                    if layer.dir == Dir::Vert {
                        pts[0] = pts[0].transpose();
                        pts[1] = pts[1].transpose();
                    }
                    raw::Element {
                        net: Some(port.name.clone()),
                        layer: self.stack.metals[abs.top_layer].raw.unwrap(),
                        purpose: raw::LayerPurpose::Drawing,
                        inner: raw::Shape::Rect {
                            p0: self.convert_xy(&pts[0]),
                            p1: self.convert_xy(&pts[1]),
                        },
                    }
                }
                ZTopInner { .. } => todo!(),
            };
            elems.push(elem);
        }
        // And return a new [raw::LayoutAbstract]
        Ok(raw::Cell {
            name: abs.name.clone(),
            elems,
            ..Default::default()
        })
    }
    /// Get the positions spanning track number `track` on layer number `layer`
    fn track_span(
        &self,
        layer_index: usize,
        track_index: usize,
    ) -> LayoutResult<(DbUnits, DbUnits)> {
        let layer = &self.stack.metals[layer_index];
        layer.span(track_index)
    }
    /// Convert an [Outline] to a [raw::Element] polygon
    pub fn convert_outline(&self, outline: &Outline) -> LayoutResult<raw::Element> {
        // Create an array of Outline-Points
        let mut pts = vec![Point { x: 0, y: 0 }];
        let mut xp: isize;
        let mut yp: isize = 0;
        for i in 0..outline.x.len() {
            xp = self.db_units(outline.x[i]).raw();
            pts.push(Point::new(xp, yp));
            yp = self.db_units(outline.y[i]).raw();
            pts.push(Point::new(xp, yp));
        }
        // Add the final implied Point at (x, y[-1])
        pts.push(Point::new(0, yp));
        // Create the [raw::Element]
        Ok(raw::Element {
            net: None,
            layer: self.stack.boundary_layer.unwrap(),
            purpose: raw::LayerPurpose::Outline,
            inner: raw::Shape::Poly { pts },
        })
    }
    /// Convert a [Track]-full of [TrackSegment]s to a vector of [raw::Element] rectangles
    fn convert_track(
        &self,
        track: &Track,
        layer: &validate::ValidMetalLayer,
    ) -> LayoutResult<Vec<raw::Element>> {
        let mut elems = Vec::new();
        for seg in &track.segments {
            // Convert wires and rails, skip blockages and cuts
            use TrackSegmentType::*;
            let net: Option<String> = match seg.tp {
                Wire { src } => src.map(|src| src.net.clone()),
                Rail(rk) => Some(rk.to_string()),
                Cut { .. } | Blockage { .. } => continue,
            };
            // Convert the inner shape
            let inner = match track.dir {
                Dir::Horiz => raw::Shape::Rect {
                    p0: self.convert_point(seg.start, track.start),
                    p1: self.convert_point(seg.stop, track.start + track.width),
                },
                Dir::Vert => raw::Shape::Rect {
                    p0: self.convert_point(track.start, seg.start),
                    p1: self.convert_point(track.start + track.width, seg.stop),
                },
            };
            // And pack it up as a [raw::Element]
            let e = raw::Element {
                net,
                layer: self.stack.metals[layer.index].raw.unwrap(),
                purpose: raw::LayerPurpose::Drawing,
                inner,
            };
            elems.push(e);
        }
        Ok(elems)
    }
    /// Create a [TempCellLayer] for the intersection of `temp_cell` and `layer`
    fn temp_cell_layer<'a>(
        &self,
        temp_cell: &'a TempCell,
        layer: &'a validate::ValidMetalLayer,
    ) -> LayoutResult<TempCellLayer<'a>> {
        // Sort out which of the cell's [Instance]s come up to this layer
        let mut instances = Vec::with_capacity(temp_cell.instances.len());
        for inst in &temp_cell.instances {
            let cell = inst.cell.read()?;
            if cell.top_layer()? >= layer.index {
                instances.push(*inst);
            }
        }

        // Sort out which direction we're working across
        let cell = temp_cell.cell;
        // Convert to database units
        let x = self.db_units(cell.outline.x[0]); // FIXME: rectangles implied here
        let y = self.db_units(cell.outline.y[0]);
        let (span, breadth) = match layer.spec.dir {
            Dir::Horiz => (x, y),
            Dir::Vert => (y, x),
        };

        // FIXME: move to `validate` stage
        if (breadth % layer.pitch) != 0 {
            return self.fail(format!(
                "{:?} has invalid pitch {:?}, must be multiple of {:?}",
                layer, layer.pitch, breadth,
            ));
        }
        let nperiods = usize::try_from(breadth / layer.pitch).unwrap(); // FIXME: errors
        Ok(TempCellLayer {
            layer,
            cell: temp_cell,
            instances,
            nperiods,
            pitch: layer.pitch,
            span,
        })
    }
    /// Create the [TempPeriod] at the intersection of `temp_layer` and `periodnum`
    fn temp_cell_layer_period<'a>(
        &self,
        temp_layer: &'a TempCellLayer,
        periodnum: usize,
    ) -> LayoutResult<TempPeriod<'a>> {
        let cell = temp_layer.cell;
        let layer = temp_layer.layer;
        let dir = layer.spec.dir;

        // For each row, decide which instances intersect
        // Convert these into blockage-areas for the tracks
        let mut blockages = Vec::with_capacity(temp_layer.instances.len());
        for inst in &temp_layer.instances {
            if self.instance_intersects(inst, layer, periodnum)? {
                // Create the blockage
                let cell = inst.cell.read()?;
                let start = inst.loc[dir];
                let stop = start + cell.outline()?.max(dir);
                blockages.push((start, stop, *inst));
            }
        }

        // Grab indices of the relevant tracks for this period
        let nsig = temp_layer.layer.period.signals.len();
        let relevant_track_nums = (periodnum * nsig, (periodnum + 1) * nsig);
        // Filter cuts down to those in this period
        let cuts: Vec<&validate::ValidCut> = cell.cuts[temp_layer.layer.index]
            .iter()
            .filter(|cut| cut.track >= relevant_track_nums.0 && cut.track < relevant_track_nums.1)
            .collect();
        // Filter assignments down to those in this period
        let top_assns = cell.top_assns[temp_layer.layer.index]
            .iter()
            .filter(|id| {
                let assn = cell
                    .assignments
                    .get(**id)
                    .ok_or(LayoutError::from("Internal error: invalid assignment"))
                    .unwrap();
                assn.loc.top.track >= relevant_track_nums.0
                    && assn.loc.top.track < relevant_track_nums.1
            })
            .copied()
            .collect();
        let bot_assns = cell.bot_assns[temp_layer.layer.index]
            .iter()
            .filter(|id| {
                let assn = cell
                    .assignments
                    .get(**id)
                    .ok_or(LayoutError::from("Internal error: invalid assignment"))
                    .unwrap();
                assn.loc.bot.track >= relevant_track_nums.0
                    && assn.loc.bot.track < relevant_track_nums.1
            })
            .copied()
            .collect();

        Ok(TempPeriod {
            periodnum,
            cell,
            layer: temp_layer,
            blockages,
            cuts,
            top_assns,
            bot_assns,
        })
    }
    /// Boolean indication of whether `inst` intersects `layer` at `periodnum`
    fn instance_intersects(
        &self,
        inst: &Instance,
        layer: &validate::ValidMetalLayer,
        periodnum: usize,
    ) -> LayoutResult<bool> {
        // Grab the instance's [DbUnits] span, in the layer's periodic direction
        let dir = !layer.spec.dir;
        let inst_min = self.db_units(inst.loc[dir]);
        let cell = inst.cell.read()?;
        let inst_max = inst_min + self.db_units(cell.outline()?.max(dir));
        // And return the boolean intersection. "Touching" edge-to-edge is *not* considered an intersection.
        Ok(inst_max > layer.pitch * periodnum && inst_min < layer.pitch * (periodnum + 1))
    }
    /// Convert any [UnitSpeced]-convertible distances into [DbUnits]
    fn db_units(&self, pt: impl Into<UnitSpeced>) -> DbUnits {
        let pt: UnitSpeced = pt.into();
        match pt {
            UnitSpeced::DbUnits(u) => u, // Return as-is
            UnitSpeced::PrimPitches(p) => {
                // Multiply by the primitive pitch in `pt`s direction
                let pitch = self.stack.prim.pitches[p.dir];
                (p.num * pitch.raw()).into()
            }
            UnitSpeced::LayerPitches(_p) => {
                // LayerPitches are always in the layer's "periodic" dimension
                todo!()
            }
        }
    }
    /// Convert an [Xy] into a [raw::Point]
    fn convert_xy<T: HasUnits + Into<UnitSpeced>>(&self, xy: &Xy<T>) -> raw::Point {
        let x = self.db_units(xy.x);
        let y = self.db_units(xy.y);
        self.convert_point(x, y)
    }
    /// Convert a two-tuple of [DbUnits] into a [raw::Point]
    fn convert_point(&self, x: DbUnits, y: DbUnits) -> raw::Point {
        raw::Point::new(x.0, y.0)
    }
}
impl HasErrors for RawExporter<'_> {
    fn err(&self, msg: impl Into<String>) -> LayoutError {
        LayoutError::Export {
            message: msg.into(),
            stack: Vec::new(), // FIXME! get a stack already! self.ctx_stack.clone(),
        }
    }
}
