//! # Raw-Layout Conversion Module

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
use crate::outline::{HasOutline, Outline};
use crate::raw::{self, Dir, HasErrors, LayoutError, LayoutResult, Point};
use crate::stack::{LayerPeriod, RelZ, Stack, Track, TrackError, TrackSegmentType};
use crate::{abstrakt, cell, validate};

/// A short-lived set of references to an [Instance] and its cell-definition
#[derive(Debug, Clone)]
struct TempInstance<'lib> {
    inst: &'lib Instance,
    def: &'lib (dyn HasOutline + 'static),
}
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
    instances: Vec<TempInstance<'lib>>,
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
    instances: Vec<&'lib TempInstance<'lib>>,
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
    stack: Vec<cell::CellBagKey>,
    seen: HashSet<cell::CellBagKey>,
}
impl<'lib> DepOrder<'lib> {
    fn order(lib: &'lib Library) -> Vec<cell::CellBagKey> {
        let mut me = Self {
            lib,
            stack: Vec::new(),
            seen: HashSet::new(),
        };
        for key in me.lib.cells.keys() {
            me.push(key)
        }
        me.stack
    }
    fn push(&mut self, key: cell::CellBagKey) {
        if !self.seen.contains(&key) {
            let cell = &self.lib.cells[key];
            // If the cell has an implementation, visit its [Instance]s before inserting it
            if let Some(layout) = &cell.layout {
                for inst in &layout.instances {
                    self.push(inst.cell);
                }
            }
            // And insert the cell (key) itself
            self.seen.insert(key);
            self.stack.push(key);
        }
    }
}

/// # Converter from [Library] and constituent elements to [raw::Library]
pub struct RawConverter<'lib> {
    pub(crate) lib: Library,
    pub(crate) stack: validate::ValidStack<'lib>,
    pub(crate) rawlayers: raw::Layers,
    pub(crate) layerkeys: Vec<raw::LayerKey>,
    pub(crate) boundary_layer: raw::LayerKey,
    /// Mapping from our cell-definition keys to the raw-library keys
    pub(crate) rawcells: HashMap<cell::CellBagKey, raw::CellKey>,
}
impl<'lib> RawConverter<'lib> {
    /// Convert [Library] `lib` to a [raw::Library]
    /// Consumes `lib` in the process
    pub fn convert(lib: Library, stack: Stack) -> LayoutResult<raw::Library> {
        let stack = validate::StackValidator::validate(stack)?;
        let (mut rawlayers, layerkeys) = Self::convert_layers(&stack)?;
        // Conversion requires our [Stack] specify a `boundary_layer`. If not, fail.
        let boundary_layer = (stack.boundary_layer).as_ref().ok_or(LayoutError::msg(
            "Cannot Convert Abstract to Raw without Boundary Layer",
        ))?;
        let boundary_layer = rawlayers.add(boundary_layer.clone());
        Self {
            lib,
            stack,
            rawlayers,
            layerkeys,
            boundary_layer,
            rawcells: HashMap::new(),
        }
        .convert_lib()
    }
    /// Convert our [Stack] to [raw::Layers]
    fn convert_layers(
        stack: &validate::ValidStack,
    ) -> LayoutResult<(raw::Layers, Vec<raw::LayerKey>)> {
        let mut rawlayers = raw::Layers::default();
        let mut layerkeys = Vec::new();
        for layer in stack.metals.iter() {
            let rawlayer = match &layer.spec.raw {
                Some(r) => r.clone(), // FIXME: stop cloning
                None => return Err("Error exporting: no raw layer defined".into()),
            };
            let key = rawlayers.add(rawlayer);
            layerkeys.push(key);
        }
        Ok((rawlayers, layerkeys))
    }
    /// Convert everything in our [Library]
    fn convert_lib(&mut self) -> LayoutResult<raw::Library> {
        let mut rawlib = raw::Library::new(&self.lib.name, self.stack.units);
        rawlib.layers = self.rawlayers.clone();
        // Convert each defined [Cell] to a [raw::Cell]
        for srckey in DepOrder::order(&self.lib).iter() {
            let srccell = &self.lib.cells[*srckey];
            let rawcell = self.convert_cell(srccell)?;
            let rawkey = rawlib.cells.insert(rawcell);
            self.rawcells.insert(*srckey, rawkey);
        }
        Ok(rawlib)
    }
    /// Convert a [CellBag] to a [raw::Cell]
    /// In reality only one of the cell-views is converted,
    /// generally the "most specific" available view.
    fn convert_cell(&self, cell: &cell::CellBag) -> LayoutResult<raw::Cell> {
        if let Some(ref x) = cell.raw {
            Ok(x.clone())
        } else if let Some(ref x) = cell.layout {
            self.convert_layout_impl(x)
        } else if let Some(ref x) = cell.abstrakt {
            self.convert_abstract(x)
        } else {
            self.fail(format!(
                "No Abstract or Implementation found for cell {}",
                cell.name
            ))
        }
    }
    /// Convert to a raw layout cell
    fn convert_layout_impl(&self, cell: &LayoutImpl) -> LayoutResult<raw::Cell> {
        if cell.outline.x.len() > 1 {
            return Err(LayoutError::Str(
                "Non-rectangular outline; conversions not supported (yet)".into(),
            ));
        };
        let mut elems: Vec<raw::Element> = Vec::new();
        // Re-organize the cell into the format most helpful here
        let temp_cell = self.temp_cell(cell)?;
        // Convert a layer at a time, starting from bottom
        for layernum in 0..cell.top_layer {
            // Organize the cell/layer combo into temporary conversion format
            let temp_layer = self.temp_cell_layer(&temp_cell, &self.stack.metals[layernum]);
            // Convert each "layer period" one at a time
            for periodnum in 0..temp_layer.nperiods {
                // Again, re-organize into the relevant objects for this "layer period"
                let temp_period = self.temp_cell_layer_period(&temp_layer, periodnum);
                // And finally start doing stuff!
                elems.extend(self.convert_cell_layer_period(&temp_period)?);
            }
        }
        // Convert our [Outline] into a polygon
        elems.push(self.convert_outline(cell)?);
        // Convert our [Instance]s
        let insts = cell
            .instances
            .iter()
            .map(|inst| self.convert_instance(inst).unwrap()) // FIXME: error handling
            .collect();
        // Aaaand create our new [raw::Cell]
        Ok(raw::Cell {
            name: cell.name.clone(),
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
    fn temp_cell<'a>(&'a self, cell: &'a LayoutImpl) -> LayoutResult<TempCell<'a>> {
        // Create one of these for each of our instances
        let instances: Vec<TempInstance> = cell
            .instances
            .iter()
            .map(|inst| {
                let cell = self.lib.cells.get(inst.cell);
                let cell = self.unwrap(
                    cell,
                    format!("Undefined Cell for Instance {}", inst.inst_name),
                )?;
                // We take the "most abstract" view for the outline
                // (although if there are more than one, they better be the same...
                // FIXME: this should be a validation step.)
                if let Some(ref x) = cell.abstrakt {
                    Ok(TempInstance { inst, def: x })
                } else if let Some(ref x) = cell.layout {
                    Ok(TempInstance { inst, def: x })
                } else {
                    self.fail(format!("Undefined Cell for Instance {}", inst.inst_name))
                }
            })
            .collect::<Result<Vec<_>, _>>()?;
        // Validate `cuts`, and arrange them by layer
        let mut cuts: Vec<Vec<validate::ValidCut>> = vec![vec![]; cell.top_layer()];
        for cut in cell.cuts.iter() {
            let c = validate::ValidCut::validate(cut, &self.stack)?;
            cuts[c.layer].push(c);
            // FIXME: cell validation should also check that this lies within our outline. probably do this earlier
        }
        // Validate all the cell's assignments, and arrange references by layer
        let mut bot_assns = vec![vec![]; cell.top_layer()];
        let mut top_assns = vec![vec![]; cell.top_layer()];
        let mut assignments = SlotMap::with_key();
        for assn in cell.assignments.iter() {
            let v = validate::ValidAssign::validate(assn, &self.stack)?;
            let bot = v.loc.bot.layer;
            let top = v.loc.top.layer;
            let k = assignments.insert(v);
            bot_assns[bot].push(k);
            top_assns[top].push(k);
        }
        // And create our (temporary) cell data!
        Ok(TempCell {
            cell,
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
                net: None,
                layer: self.layerkeys[layer.index], // FIXME: via_layer?
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
        elems.push(self.convert_outline(abs)?);
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
                            let outside = self.db_units(abs.outline().max(layer.dir));
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
                        layer: self.layerkeys[*layer_index],
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
                            let outside = self.db_units(abs.outline().max(layer.dir));
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
                        layer: self.layerkeys[abs.top_layer],
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
    pub fn convert_outline(&self, cell: &impl HasOutline) -> LayoutResult<raw::Element> {
        let outline: &Outline = cell.outline();
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
            layer: self.boundary_layer,
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
                layer: self.layerkeys[layer.index],
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
    ) -> TempCellLayer<'a> {
        // Sort out which of the cell's [Instance]s come up to this layer
        let instances: Vec<&TempInstance> = temp_cell
            .instances
            .iter()
            .filter(|i| i.def.top_layer() >= layer.index)
            .collect();

        // Sort out which direction we're working across
        let cell = temp_cell.cell;
        // Convert to database units
        let x = self.db_units(cell.outline.x[0]); // FIXME: rectangles implied here
        let y = self.db_units(cell.outline.y[0]);
        let (span, breadth) = match layer.spec.dir {
            Dir::Horiz => (x, y),
            Dir::Vert => (y, x),
        };

        // FIXME: we probably want to detect bad Outline dimensions sooner than this
        if (breadth % layer.pitch) != 0 {
            panic!("HOWD WE GET THIS BAD LAYER?!?!");
        }
        let nperiods = usize::try_from(breadth / layer.pitch).unwrap(); // FIXME: errors

        TempCellLayer {
            layer,
            cell: temp_cell,
            instances,
            nperiods,
            pitch: layer.pitch,
            span,
        }
    }
    /// Create the [TempPeriod] at the intersection of `temp_layer` and `periodnum`
    fn temp_cell_layer_period<'a>(
        &self,
        temp_layer: &'a TempCellLayer,
        periodnum: usize,
    ) -> TempPeriod<'a> {
        let cell = temp_layer.cell;
        let layer = temp_layer.layer;
        let dir = layer.spec.dir;

        // For each row, decide which instances intersect
        // Convert these into blockage-areas for the tracks
        let blockages = temp_layer
            .instances
            .iter()
            .filter(|i| self.instance_intersects(i, layer, periodnum))
            .map(|i| {
                (
                    i.inst.loc[dir],                            // start
                    i.inst.loc[dir] + i.def.outline().max(dir), // stop
                    i.inst,                                     // inst
                )
            })
            .collect();

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

        TempPeriod {
            periodnum,
            cell,
            layer: temp_layer,
            blockages,
            cuts,
            top_assns,
            bot_assns,
        }
    }
    /// Boolean indication of whether `inst` intersects `layer` at `periodnum`
    fn instance_intersects(
        &self,
        inst: &TempInstance,
        layer: &validate::ValidMetalLayer,
        periodnum: usize,
    ) -> bool {
        // Grab the instance's [DbUnits] span, in the layer's periodic direction
        let dir = !layer.spec.dir;
        let inst_min = self.db_units(inst.inst.loc[dir]);
        let inst_max = inst_min + self.db_units(inst.def.outline().max(dir));
        // And return the boolean intersection. "Touching" edge-to-edge is *not* considered an intersection.
        return inst_max > layer.pitch * periodnum && inst_min < layer.pitch * (periodnum + 1);
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
impl HasErrors for RawConverter<'_> {
    fn err(&self, msg: impl Into<String>) -> LayoutError {
        LayoutError::Export {
            message: msg.into(),
            stack: Vec::new(), // FIXME! get a stack already! self.ctx_stack.clone(),
        }
    }
}
