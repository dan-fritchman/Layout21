//!
//! # GDSII Conversion Module
//!
//! Converts between Layout21's "raw" layout data-model and [gds21] structures.
//!

// Std-Lib
use std::collections::{HashMap, HashSet};
use std::convert::{TryFrom, TryInto};
use std::hash::Hash;

use gds21::GdsElement;
// Crates.io
use slotmap::{new_key_type, SlotMap};

// Local imports
use crate::{
    bbox::BoundBoxTrait,
    error::{LayoutError, LayoutResult},
    geom::{Path, Point, Polygon, Rect, Shape, ShapeTrait},
    utils::{ErrorContext, ErrorHelper, Ptr, Unwrapper},
    Abstract, AbstractPort, Cell, Dir, Element, Instance, Int, LayerKey, LayerPurpose, Layers,
    Layout, Library, TextElement, Units,
};
pub use gds21;

/// Additional [Library] methods for GDSII conversion
impl Library {
    /// Convert to a GDSII Library
    pub fn to_gds(&self) -> LayoutResult<gds21::GdsLibrary> {
        GdsExporter::export(&self)
    }
    /// Create from GDSII
    pub fn from_gds(
        gdslib: &gds21::GdsLibrary,
        layers: Option<Ptr<Layers>>,
    ) -> LayoutResult<Library> {
        GdsImporter::import(&gdslib, layers)
    }
}

new_key_type! {
    /// Keys for [Element] entries
    pub struct ElementKey;
}

impl From<gds21::GdsError> for LayoutError {
    fn from(e: gds21::GdsError) -> Self {
        Self::Boxed(Box::new(e))
    }
}

/// # Gds21 Exporter
/// Converts a [raw::Library] to a GDSII library ([gds21::GdsLibrary]).
/// The sole valid top-level entity for conversion is always a [Library].
#[derive(Debug)]
pub struct GdsExporter<'lib> {
    /// Source [Library]
    lib: &'lib Library,
    ctx: Vec<ErrorContext>,
}
impl<'lib> GdsExporter<'lib> {
    /// Export `lib` to a GDSII library.
    pub fn export(lib: &'lib Library) -> LayoutResult<gds21::GdsLibrary> {
        Self {
            lib,
            ctx: Vec::new(),
        }
        .export_lib()
    }
    /// Primary internal method for exporting [Library] `self.lib`.
    fn export_lib(&mut self) -> LayoutResult<gds21::GdsLibrary> {
        self.ctx.push(ErrorContext::Library(self.lib.name.clone()));
        // Create a new Gds Library
        let mut gdslib = gds21::GdsLibrary::new(&self.lib.name);
        // Set its distance units
        // In all cases the GDSII "user units" are set to 1µm.
        gdslib.units = match self.lib.units {
            Units::Micro => gds21::GdsUnits::new(1.0, 1e-6),
            Units::Nano => gds21::GdsUnits::new(1e-3, 1e-9),
            Units::Angstrom => gds21::GdsUnits::new(1e-4, 1e-10),
            Units::Pico => gds21::GdsUnits::new(1e-6, 1e-12),
        };
        // And convert each of our `cells` into its `structs`
        for cell in self.lib.cells.iter() {
            let cell = cell.read()?;
            if let Some(strukt) = self.export_cell(&*cell)? {
                gdslib.structs.push(strukt);
            }
        }
        self.ctx.pop();
        Ok(gdslib)
    }
    /// Convert a [Cell] to a [gds21::GdsStruct] cell-definition, if the cell has an implementation or abstract.
    ///
    /// Priorities for the exported content are:
    /// * If the Cell has a layout implementation, it is converted to a [gds21::GdsStruct]
    /// * If not, and it has a layout abstract, that abstract is converted to a [gds21::GdsStruct]
    /// * If the cell has neither an abstract nor implementation, `export_cell` returns `Ok(None)`, and no data is exported.
    fn export_cell(&mut self, cell: &Cell) -> LayoutResult<Option<gds21::GdsStruct>> {
        self.ctx.push(ErrorContext::Cell(cell.name.clone()));

        let strukt_option = if let Some(ref lay) = cell.layout {
            // If the cell has a layout implementation, export that
            Some(self.export_layout(lay)?)
        } else if let Some(ref a) = cell.abs {
            // Otherwise if the cell has an abstract, export that. Add a warning.
            println!(
                "No implementation for Cell {}, exporting abstract to GDSII",
                cell.name
            );
            Some(self.export_abstract(a)?)
        } else {
            // And if we have neither, return `None`
            println!("No abstract or implementation for cell {}", cell.name);
            None
        };

        self.ctx.pop();
        Ok(strukt_option)
    }
    /// Convert a [Abstract]
    fn export_abstract(&mut self, abs: &Abstract) -> LayoutResult<gds21::GdsStruct> {
        self.ctx.push(ErrorContext::Abstract);

        let mut elems = Vec::with_capacity(1 + abs.ports.len());

        // Flatten our points-vec, converting to 32-bit along the way
        let mut xy = abs
            .outline
            .points
            .iter()
            .map(|p| self.export_point(p))
            .collect::<Result<Vec<_>, _>>()?;
        // Add the origin a second time, to "close" the polygon
        xy.push(self.export_point(&abs.outline.points[0])?);
        let outline = GdsElement::GdsBoundary(gds21::GdsBoundary {
            layer: i16::MAX,
            datatype: i16::MAX,
            xy,
            ..Default::default()
        });
        // Blockages do not map to GDSII elements.
        // Conversion includes the abstract's name, outline and ports.
        elems.push(outline);

        // Convert each [AbstractPort]
        for port in abs.ports.iter() {
            elems.extend(self.export_abstract_port(&port)?);
        }

        // Create and return a [GdsStruct]
        let mut gds_struct = gds21::GdsStruct::new(&abs.name);
        gds_struct.elems = elems;
        self.ctx.pop();
        Ok(gds_struct)
    }
    /// Export an [AbstractPort]
    pub fn export_abstract_port(
        &mut self,
        port: &AbstractPort,
    ) -> LayoutResult<Vec<gds21::GdsElement>> {
        let mut elems = Vec::new();
        for (layerkey, shapes) in &port.shapes {
            // Export [LayerPurpose::Drawing] and [LayerPurpose::Pin] shapes for each
            let drawing_spec = self.export_layerspec(&layerkey, &LayerPurpose::Drawing)?;
            let pin_spec = self.export_layerspec(&layerkey, &LayerPurpose::Pin)?;
            let label_spec = self.export_layerspec(&layerkey, &LayerPurpose::Label)?;
            for shape in shapes.iter() {
                elems.push(self.export_shape(shape, &drawing_spec)?);
                elems.push(self.export_shape(shape, &pin_spec)?);
                elems.push(self.export_shape_label(&port.net, shape, &label_spec)?);
            }
        }
        Ok(elems)
    }
    /// Convert a [Layout] to a [gds21::GdsStruct] cell-definition
    fn export_layout(&mut self, cell: &Layout) -> LayoutResult<gds21::GdsStruct> {
        self.ctx.push(ErrorContext::Impl);
        let mut elems = Vec::with_capacity(cell.elems.len() + cell.insts.len());
        // Convert each [Instance]
        for inst in cell.insts.iter() {
            elems.push(self.export_instance(inst)?.into());
        }
        // Convert each [Element]
        // Note each can produce more than one [GdsElement]
        self.ctx.push(ErrorContext::Geometry);
        for elem in cell.elems.iter() {
            for gdselem in self.export_element(elem)?.into_iter() {
                elems.push(gdselem);
            }
        }
        self.ctx.pop();
        // Create and return a [GdsStruct]
        let mut strukt = gds21::GdsStruct::new(&cell.name);
        strukt.elems = elems;
        self.ctx.pop();
        Ok(strukt)
    }
    /// Convert an [Instance] to a GDS instance, AKA [gds21::GdsStructRef]
    fn export_instance(&mut self, inst: &Instance) -> LayoutResult<gds21::GdsStructRef> {
        self.ctx
            .push(ErrorContext::Instance(inst.inst_name.clone()));
        // Convert the orientation to a [gds21::GdsStrans] option
        let mut strans = None;
        if inst.reflect_vert || inst.angle.is_some() {
            let angle = inst.angle.map(|a| f64::from(a));
            strans = Some(gds21::GdsStrans {
                reflected: inst.reflect_vert,
                angle,
                ..Default::default()
            });
        }
        let cell = inst.cell.read()?;
        let gdsinst = gds21::GdsStructRef {
            name: cell.name.clone(),
            xy: self.export_point(&inst.loc)?,
            strans,
            ..Default::default()
        };
        self.ctx.pop();
        Ok(gdsinst)
    }
    /// Convert a (LayerKey, LayerPurpose) combination to a [gds21::GdsLayerSpec]
    pub fn export_layerspec(
        &mut self,
        layer: &LayerKey,
        purpose: &LayerPurpose,
    ) -> LayoutResult<gds21::GdsLayerSpec> {
        let layers = self.lib.layers.read()?;
        let layer = layers.get(*layer).unwrapper(
            self,
            format!("Layer {:?} Not Defined in Library {}", layer, self.lib.name),
        )?;
        let xtype = layer
            .num(purpose)
            .unwrapper(
                self,
                format!("LayerPurpose Not Defined for {:?}, {:?}", layer, purpose),
            )?
            .clone();
        let layer = layer.layernum;
        Ok(gds21::GdsLayerSpec { layer, xtype })
    }
    /// Convert an [Element] into one or more [gds21::GdsElement]s.
    ///
    /// Our [Element]s often correspond to more than one GDSII element,
    /// notably in the case in which a polygon is annotated with a net-name.
    /// Here, the net-name is an attribute of the polygon [Element].
    /// In GDSII, text is "free floating" as a separate element.
    ///
    pub fn export_element(&mut self, elem: &Element) -> LayoutResult<Vec<gds21::GdsElement>> {
        // Get the element's layer-numbers pair
        let layerspec = self.export_layerspec(&elem.layer, &elem.purpose)?;
        // Convert its core inner [Shape]
        let mut gds_elems = vec![self.export_shape(&elem.inner, &layerspec)?];
        // If there's an assigned net, create a corresponding text-element
        if let Some(name) = &elem.net {
            // Get the element's layer-numbers pair
            let layerspec = self.export_layerspec(&elem.layer, &LayerPurpose::Label)?;
            gds_elems.push(self.export_shape_label(name, &elem.inner, &layerspec)?);
        }
        Ok(gds_elems)
    }
    /// Convert a [Shape] to a [gds21::GdsElement]
    /// Layer and datatype must be previously converted to gds21's [gds21::GdsLayerSpec] format.
    ///
    /// GDS shapes include an explicit repetition of their origin for closure.
    /// So an N-sided polygon is described by a (N+1)-point vector.
    ///
    pub fn export_shape(
        &mut self,
        shape: &Shape,
        layerspec: &gds21::GdsLayerSpec,
    ) -> LayoutResult<gds21::GdsElement> {
        let elem = match shape {
            Shape::Rect(r) => {
                let (p0, p1) = (&r.p0, &r.p1);
                let x0 = p0.x.try_into()?;
                let y0 = p0.y.try_into()?;
                let x1 = p1.x.try_into()?;
                let y1 = p1.y.try_into()?;
                let xy = gds21::GdsPoint::vec(&[(x0, y0), (x1, y0), (x1, y1), (x0, y1), (x0, y0)]);
                // Both rect and polygon map to [GdsBoundary], although [GdsBox] is also suitable here.
                gds21::GdsBoundary {
                    layer: layerspec.layer,
                    datatype: layerspec.xtype,
                    xy,
                    ..Default::default()
                }
                .into()
            }
            Shape::Polygon(poly) => {
                // Flatten our points-vec, converting to 32-bit along the way
                let mut xy = poly
                    .points
                    .iter()
                    .map(|p| self.export_point(p))
                    .collect::<Result<Vec<_>, _>>()?;
                // Add the origin a second time, to "close" the polygon
                xy.push(self.export_point(&poly.points[0])?);
                gds21::GdsBoundary {
                    layer: layerspec.layer,
                    datatype: layerspec.xtype,
                    xy,
                    ..Default::default()
                }
                .into()
            }
            Shape::Path(path) => {
                // Flatten our points-vec, converting to 32-bit along the way
                let mut xy = Vec::new();
                for p in path.points.iter() {
                    xy.push(self.export_point(p)?);
                }
                // Add the origin a second time, to "close" the polygon
                xy.push(self.export_point(&path.points[0])?);
                gds21::GdsPath {
                    layer: layerspec.layer,
                    datatype: layerspec.xtype,
                    width: Some(i32::try_from(path.width)?),
                    xy,
                    ..Default::default()
                }
                .into()
            }
        };
        Ok(elem)
    }
    /// Create a labeling [gds21::GdsElement] for [Shape] `shape`
    pub fn export_shape_label(
        &mut self,
        net: &str,
        shape: &Shape,
        layerspec: &gds21::GdsLayerSpec,
    ) -> LayoutResult<gds21::GdsElement> {
        // Sort out a location to place the text
        let loc = shape.label_location()?;

        // Rotate that text 90 degrees for mostly-vertical shapes
        let strans = match shape.orientation() {
            Dir::Horiz => None,
            Dir::Vert => Some(gds21::GdsStrans {
                angle: Some(90.0),
                ..Default::default()
            }),
        };
        // And return a converted [GdsTextElem]
        Ok(gds21::GdsTextElem {
            string: net.into(),
            layer: layerspec.layer,
            texttype: layerspec.xtype,
            xy: self.export_point(&loc)?,
            strans,
            ..Default::default()
        }
        .into())
    }
    /// Convert a [Point] to a GDS21 [gds21::GdsPoint]
    pub fn export_point(&mut self, pt: &Point) -> LayoutResult<gds21::GdsPoint> {
        let x = pt.x.try_into()?;
        let y = pt.y.try_into()?;
        Ok(gds21::GdsPoint::new(x, y))
    }
}
impl ErrorHelper for GdsExporter<'_> {
    type Error = LayoutError;
    fn err(&self, msg: impl Into<String>) -> LayoutError {
        LayoutError::Export {
            message: msg.into(),
            stack: self.ctx.clone(),
        }
    }
}

/// # PlaceLabels
///
/// Trait for calculating the location of text-labels, generally per [Shape].
///
/// Sole function `label_location` calculates an appropriate location,
/// or returns a [LayoutError] if one cannot be found.
///
/// While Layout21 formats do not include "placed text", GDSII relies on it for connectivity annotations.
/// How to place these labels varies by shape type.
///
trait PlaceLabels {
    fn label_location(&self) -> LayoutResult<Point>;
}
impl PlaceLabels for Shape {
    fn label_location(&self) -> LayoutResult<Point> {
        // Dispatch based on shape-type
        match self {
            Shape::Rect(ref r) => r.label_location(),
            Shape::Polygon(ref p) => p.label_location(),
            Shape::Path(ref p) => p.label_location(),
        }
    }
}
impl PlaceLabels for Rect {
    fn label_location(&self) -> LayoutResult<Point> {
        // Place rectangle-labels in the center of the rectangle
        Ok(self.center())
    }
}
impl PlaceLabels for Path {
    fn label_location(&self) -> LayoutResult<Point> {
        // Place on the center of the first segment
        let p0 = &self.points[0];
        let p1 = &self.points[1];
        Ok(Point::new((p0.x + p1.x) / 2, (p0.y + p1.y) / 2))
    }
}
impl PlaceLabels for Polygon {
    fn label_location(&self) -> LayoutResult<Point> {
        // Where, oh where, to place a label on an arbitrary polygon? Let us count the ways.

        // Priority 1: if the center of our bounding box lies within the polygon, use that.
        // In simple-polygon cases, this is most likely our best choice.
        // In many other cases, e.g. for "U-shaped" polygons, this will fall outside the polygon and be invalid.
        let bbox_center = self.points.bbox().center();
        if self.contains(&bbox_center) {
            return Ok(bbox_center);
        }

        // Priority 2: try the four coordinates immediately above, below, left, and right of the polygon's first point.
        // At least one must lie within the polygon for it to be a valid layout shape.
        // If none are, fail.
        let pt0 = self.point0();
        let candidates = vec![
            Point::new(pt0.x, pt0.y - 1),
            Point::new(pt0.x - 1, pt0.y),
            Point::new(pt0.x, pt0.y + 1),
            Point::new(pt0.x + 1, pt0.y),
        ];
        for pt in candidates {
            if self.contains(&pt) {
                return Ok(pt);
            }
        }
        Err(LayoutError::msg(format!(
            "No valid label location found for polygon {:?}",
            self,
        )))
    }
}

/// # Gds Dependency-Order
///
/// Creates a vector of references Gds structs, ordered by their instance dependencies.
/// Each item in the ordered return value is guaranteed *not* to instantiate any item which comes later.
/// Intended usage: `for s in GdsDepOrder::order(&gds) { /* do stuff */ }`
/// Note this *does not* use the `utils` [DepOrder] trait, as it requires tracking of a separete
/// hash-map of structs by (string) name.
///
#[derive(Debug)]
pub struct GdsDepOrder<'a> {
    strukts: HashMap<String, &'a gds21::GdsStruct>,
    stack: Vec<&'a gds21::GdsStruct>,
    seen: HashSet<String>,
}
impl<'a> GdsDepOrder<'a> {
    fn order(gdslib: &'a gds21::GdsLibrary) -> Vec<&'a gds21::GdsStruct> {
        // First create a map from names to structs
        let mut strukts = HashMap::new();
        for s in &gdslib.structs {
            strukts.insert(s.name.clone(), s);
        }
        let mut me = Self {
            strukts,
            stack: Vec::new(),
            seen: HashSet::new(),
        };
        for s in &gdslib.structs {
            me.push(s)
        }
        me.stack
    }
    /// Add all of `strukt`'s dependencies, and then `strukt` itself, to the stack
    fn push(&mut self, strukt: &'a gds21::GdsStruct) {
        if !self.seen.contains(&strukt.name) {
            for elem in &strukt.elems {
                use gds21::GdsElement::*;
                match elem {
                    GdsStructRef(ref x) => self.push(self.strukts.get(&x.name).unwrap()),
                    GdsArrayRef(ref x) => self.push(self.strukts.get(&x.name).unwrap()),
                    _ => (),
                };
            }
            self.seen.insert(strukt.name.clone());
            self.stack.push(strukt);
        }
    }
}
/// # GDSII Importer
#[derive(Debug, Default)]
pub struct GdsImporter {
    pub layers: Ptr<Layers>,
    ctx: Vec<ErrorContext>,
    unsupported: Vec<gds21::GdsElement>,
    cell_map: HashMap<String, Ptr<Cell>>,
    lib: Library,
}
impl GdsImporter {
    /// Import a [gds21::GdsLibrary] into a [Library]
    pub fn import(
        gdslib: &gds21::GdsLibrary,
        layers: Option<Ptr<Layers>>,
    ) -> LayoutResult<Library> {
        // Create a default [Layers] if none were provided
        let layers = match layers {
            Some(l) => l,
            None => Ptr::new(Layers::default()),
        };
        // Create the importer
        let mut importer = Self {
            layers,
            ..Default::default()
        };
        // Run the main import method
        importer.import_lib(&gdslib)?;
        // And destructure the result from our importer
        let Self {
            mut lib,
            layers,
            unsupported,
            ..
        } = importer;
        if unsupported.len() > 0 {
            println!(
                "Read {} Unsupported GDS Elements: {:?}",
                unsupported.len(),
                unsupported
            );
        }
        lib.layers = layers;
        Ok(lib)
    }
    /// Internal implementation method. Convert all, starting from our top-level [gds21::GdsLibrary].
    fn import_lib(&mut self, gdslib: &gds21::GdsLibrary) -> LayoutResult<()> {
        self.ctx.push(ErrorContext::Library(gdslib.name.clone()));
        // Unsupported GDSII features, if ever added, shall be imported here:
        // if gdslib.libdirsize.is_some()
        //     || gdslib.srfname.is_some()
        //     || gdslib.libsecur.is_some()
        //     || gdslib.reflibs.is_some()
        //     || gdslib.fonts.is_some()
        //     || gdslib.attrtable.is_some()
        //     || gdslib.generations.is_some()
        //     || gdslib.format_type.is_some()
        // {
        //     return self.fail("Unsupported GDSII Feature");
        // }
        // Give our library the same name as the GDS
        self.lib.name = gdslib.name.clone();
        // Set its distance units
        self.lib.units = self.import_units(&gdslib.units)?;
        // And convert each of its `structs` into our `cells`
        for strukt in &GdsDepOrder::order(&gdslib) {
            self.import_and_add(strukt)?
        }
        Ok(())
    }
    /// Import our [Units]
    fn import_units(&mut self, units: &gds21::GdsUnits) -> LayoutResult<Units> {
        self.ctx.push(ErrorContext::Units);
        // Peel out the GDS "database unit", the one of its numbers that really matters
        let gdsunit = units.db_unit();
        // FIXME: intermediate/ calculated units. Only our enumerated values are thus far supported
        // Note: sadly many real-life GDSII files set, for example "1nm" units,
        // but do so with the floating-point number *next to* 1e-9.
        // These files presumably rely on other software "converging" to 1nm, as we do here.
        let rv = if (gdsunit - 1e-10).abs() < 1e-13 {
            Units::Angstrom
        } else if (gdsunit - 1e-9).abs() < 1e-12 {
            Units::Nano
        } else if (gdsunit - 1e-6).abs() < 1e-9 {
            Units::Micro
        } else {
            return self.fail(format!("Unsupported GDSII Units {:10.3e}", gdsunit));
        };
        self.ctx.pop();
        Ok(rv)
    }
    /// Import and add a cell, if not already defined
    fn import_and_add(&mut self, strukt: &gds21::GdsStruct) -> LayoutResult<()> {
        let name = &strukt.name;
        // Check whether we're already defined, and bail if so
        match self.cell_map.get(name) {
            Some(_) => return Ok(()), // Already done
            None => (),               // Not yet defined, run the code below
        }
        // Do the real work
        let cell = self.import_cell(strukt)?;
        // Add it to our library
        let key = self.lib.cells.insert(cell);
        // And add the key to our name-map
        self.cell_map.insert(name.to_string(), key);
        Ok(())
    }
    /// Import a GDS Cell ([gds21::GdsStruct]) into a [Cell]
    fn import_cell(&mut self, strukt: &gds21::GdsStruct) -> LayoutResult<Cell> {
        self.ctx.push(ErrorContext::Cell(strukt.name.clone()));
        let cell = self.import_layout(strukt)?.into();
        self.ctx.pop();
        Ok(cell)
    }
    /// Import a GDS Cell ([gds21::GdsStruct]) into a [Layout]
    fn import_layout(&mut self, strukt: &gds21::GdsStruct) -> LayoutResult<Layout> {
        let mut layout = Layout::default();
        let name = strukt.name.clone();
        layout.name = name.clone();
        self.ctx.push(ErrorContext::Impl);

        // Importing each layout requires at least two passes over its elements.
        // In the first pass we add each [Instance] and geometric element,
        // And keep a list of [gds21::GdsTextElem] on the side.
        let mut texts: Vec<&gds21::GdsTextElem> = Vec::new();
        let mut elems: SlotMap<ElementKey, Element> = SlotMap::with_key();
        // Also keep a hash of by-layer elements, to aid in text-assignment in our second pass
        let mut layers: HashMap<i16, Vec<ElementKey>> = HashMap::new();
        for elem in &strukt.elems {
            /// A quick local enum, indicating whether each GDS element causes us
            /// to add a new [Element]. If so, more stuff is to be done.
            enum AddingAnElement {
                Yes(Element),
                No(()),
            }
            use gds21::GdsElement::*;
            use AddingAnElement::{No, Yes};
            let e = match elem {
                GdsBoundary(ref x) => Yes(self.import_boundary(x)?),
                GdsPath(ref x) => Yes(self.import_path(x)?),
                GdsBox(ref x) => Yes(self.import_box(x)?),
                GdsArrayRef(ref x) => {
                    let array = self.import_instance_array(x)?;
                    if let Some(insts) = array {
                        layout.insts.extend(insts);
                    }
                    No(())
                },
                GdsStructRef(ref x) => No(layout.insts.push(self.import_instance(x)?)),
                GdsTextElem(ref x) => No(texts.push(x)),
                // GDSII "Node" elements are fairly rare, and are not supported.
                // (Maybe some day we'll even learn what they are.)
                GdsNode(ref x) => No(self.unsupported.push(x.clone().into())),
            };
            // If we got a new element, add it to our per-layer hash
            if let Yes(e) = e {
                let selflayers = self.layers.read()?;
                let layernum = match selflayers.get(e.layer) {
                    Some(l) => l.layernum,
                    None => return self.fail("Internal error: element added to invalid layer"),
                };
                let ekey = elems.insert(e);
                if let Some(ref mut bucket) = layers.get_mut(&layernum) {
                    bucket.push(ekey);
                } else {
                    layers.insert(layernum, vec![ekey]);
                }
            }
        }
        // Pass two: sort out whether each [gds21::GdsTextElem] is a net-label,
        // And if so, assign it as a net-name on each intersecting [Element].
        // Text elements which do not overlap a geometric element on the same layer
        // are converted to annotations.
        for textelem in &texts {
            let loc = self.import_point(&textelem.xy)?;
            if let Some(layer) = layers.get(&textelem.layer) {
                // Layer exists in geometry; see which elements intersect with this text
                let mut hit = false;
                for ekey in layer.iter() {
                    let elem = elems.get_mut(*ekey).unwrap();
                    if elem.inner.contains(&loc) {
                        // Label lands inside this element.
                        // Check whether we have an existing label.
                        // If so, it better be the same net name!
                        // FIXME: casing, as usual with all EDA crap.
                        // Here we support case *insensitive* GDSes, and lower-case everything.
                        // Many GDS seem to mix and match upper and lower case,
                        // essentially using the case-insensitivity for connections (bleh).
                        let lower_case_name = textelem.string.to_lowercase();
                        if let Some(pname) = &elem.net {
                            if *pname != lower_case_name {
                                println!(
                                    "Warning: GDSII labels shorting nets {} and {} on layer {}",
                                    pname,
                                    textelem.string.clone(),
                                    textelem.layer
                                );
                                // return self.fail(format!(
                                //     "GDSII labels shorting nets {} and {} on layer {}",
                                //     pname,
                                //     textelem.string.clone(),
                                //     textelem.layer
                                // ));
                            }
                        } else {
                            elem.net = Some(lower_case_name);
                        }
                        hit = true;
                    }
                }
                // If we've hit at least one, carry onto the next TextElement
                if hit {
                    continue;
                }
            }
            // No hits (or a no-shape Layer). Create an annotation instead.
            layout.annotations.push(TextElement {
                string: textelem.string.clone(),
                loc,
            });
        }
        // Pull the elements out of the local slot-map, into the vector that [Layout] wants
        layout.elems = elems.drain().map(|(_k, v)| v).collect();
        self.ctx.pop();
        Ok(layout)
    }
    /// Import a [gds21::GdsBoundary] into an [Element]
    fn import_boundary(&mut self, x: &gds21::GdsBoundary) -> LayoutResult<Element> {
        self.ctx.push(ErrorContext::Geometry);
        let mut pts: Vec<Point> = self.import_point_vec(&x.xy)?;
        if pts[0] != *pts.last().unwrap() {
            return self.fail("GDS Boundary must start and end at the same point");
        }
        // Pop the redundant last entry
        pts.pop();
        // Check for Rectangles; they help
        let inner = if pts.len() == 4
            && ((pts[0].x == pts[1].x // Clockwise
                && pts[1].y == pts[2].y
                && pts[2].x == pts[3].x
                && pts[3].y == pts[0].y)
                || (pts[0].y == pts[1].y // Counter-clockwise
                    && pts[1].x == pts[2].x
                    && pts[2].y == pts[3].y
                    && pts[3].x == pts[0].x))
        {
            // That makes this a Rectangle.
            Shape::Rect(Rect {
                p0: pts[0].clone(),
                p1: pts[2].clone(),
            })
        } else {
            // Otherwise, it's a polygon
            Shape::Polygon(Polygon { points: pts })
        };

        // Grab (or create) its [Layer]
        let (layer, purpose) = self.import_element_layer(x)?;
        // Create the Element, and insert it in our slotmap
        let e = Element {
            net: None,
            layer,
            purpose,
            inner,
        };
        self.ctx.pop();
        Ok(e)
    }
    /// Import a [gds21::GdsBox] into an [Element]
    fn import_box(&mut self, x: &gds21::GdsBox) -> LayoutResult<Element> {
        self.ctx.push(ErrorContext::Geometry);

        // GDS stores *five* coordinates per box (for whatever reason).
        // This does not check fox "box validity", and imports the
        // first and third of those five coordinates,
        // which are by necessity for a valid [GdsBox] located at opposite corners.
        let inner = Shape::Rect(Rect {
            p0: self.import_point(&x.xy[0])?,
            p1: self.import_point(&x.xy[2])?,
        });

        // Grab (or create) its [Layer]
        let (layer, purpose) = self.import_element_layer(x)?;
        // Create the Element, and insert it in our slotmap
        let e = Element {
            net: None,
            layer,
            purpose,
            inner,
        };
        self.ctx.pop();
        Ok(e)
    }
    /// Import a [gds21::GdsPath] into an [Element]
    fn import_path(&mut self, x: &gds21::GdsPath) -> LayoutResult<Element> {
        self.ctx.push(ErrorContext::Geometry);

        let pts = self.import_point_vec(&x.xy)?;
        let width = if let Some(w) = x.width {
            w as usize
        } else {
            return self.fail("Invalid nonspecifed GDS Path width ");
        };
        // Create the shape
        let inner = Shape::Path(Path { width, points: pts });

        // Grab (or create) its [Layer]
        let (layer, purpose) = self.import_element_layer(x)?;
        // Create the Element, and insert it in our slotmap
        let e = Element {
            net: None,
            layer,
            purpose,
            inner,
        };
        self.ctx.pop();
        Ok(e)
    }
    /// Import a [gds21::GdsStructRef] cell/struct-instance into an [Instance]
    fn import_instance(&mut self, sref: &gds21::GdsStructRef) -> LayoutResult<Instance> {
        let cname = sref.name.clone();
        self.ctx.push(ErrorContext::Instance(cname.clone()));
        // Look up the cell-key, which must be imported by now
        let cell = self
            .cell_map
            .get(&sref.name)
            .unwrapper(self, format!("Instance of invalid cell {}", cname))?;

        let cell = Ptr::clone(cell);
        // Convert its location
        let loc = self.import_point(&sref.xy)?;
        let inst_name = "".into(); // FIXME: should we create imported instance names?
        let mut inst = Instance {
            // Already-converted fields
            inst_name,
            cell,
            loc,
            // Initial default values for orientation
            reflect_vert: false,
            angle: None,
        };
        // If defined, convert orientation settings
        if let Some(strans) = &sref.strans {
            if strans.abs_mag || strans.abs_angle {
                return self.fail("Unsupported GDSII Instance Feature: Absolute Magnitude/ Angle");
            }
            // The [GdsStrans] `reflect` field indicates reflection "about the x-axis",
            // i.e. the [Instance] `reflect_vert` field.
            inst.reflect_vert = strans.reflected;
            inst.angle = strans.angle;
        }
        self.ctx.pop();
        Ok(inst)
    }
    /// Import a (two-dimensional) [gds21::GdsArrayRef] into [Instance]s
    ///
    /// Returns the newly-created [Instance]s as a vector.
    /// Instance names are of the form `{array.name}[{col}][{row}]`.
    ///
    /// GDSII arrays are described by three spatial points:
    /// The origin, extent in "rows", and extent in "columns".
    /// In principle these need not be the same as "x" and "y" spacing,
    /// i.e. there might be "diamond-shaped" array specifications.
    ///
    /// Here, arrays are supported if they are "specified rectangular",
    /// i.e. that (a) the first two points align in `y`, and (b) the second two points align in `x`.
    ///
    /// Further support for such "non-rectangular-specified" arrays may (or may not) become a future addition,
    /// based on observed GDSII usage.
    ///
    fn import_instance_array(&mut self, aref: &gds21::GdsArrayRef) -> LayoutResult<Option<Vec<Instance>>> {
        let cname = aref.name.clone();
        self.ctx.push(ErrorContext::Array(cname.clone()));

        // Look up the cell, which must be imported by now
        let cell = self
            .cell_map
            .get(&aref.name)
            .unwrapper(self, format!("Instance Array of invalid cell {}", cname))?;
        let cell = Ptr::clone(cell);

        // Convert its three (x,y) coordinates
        let p0 = self.import_point(&aref.xy[0])?;
        let p1 = self.import_point(&aref.xy[1])?;
        let p2 = self.import_point(&aref.xy[2])?;
        // Check for (thus far) unsupported non-rectangular arrays
        if p0.y != p1.y || p0.x != p2.x {
            //self.fail("Unsupported Non-Rectangular GDS Array")?;
            return Ok(None);
        }
        // Sort out the inter-element spacing
        let mut xstep = (p1.x - p0.x) / Int::from(aref.cols);
        let mut ystep = (p2.y - p0.y) / Int::from(aref.rows);

        // Incorporate the reflection/ rotation settings
        let mut angle = None;
        let mut reflect_vert = false;
        if let Some(strans) = &aref.strans {
            if strans.abs_mag || strans.abs_angle {
                self.fail("Unsupported GDSII Array Setting: Absolute Magnitude/ Angle")?;
            }
            if strans.mag.is_some() {
                self.fail("Unsupported GDSII Array Setting: Magnitude")?;
            }
            if let Some(a) = strans.angle {
                // The angle-setting rotates the *entire* array lattice together.
                // Update the (x,y) steps via a rotation-matrix multiplication:
                // x = x * cos(a) - y * sin(a)
                // y = x * sin(a) + y * cos(a)
                let prev_xy = (i32::try_from(xstep)?, i32::try_from(ystep)?);
                let prev_xy = (f64::from(prev_xy.0), f64::from(prev_xy.1));
                let a = a.to_radians(); // Rust `sin` and `cos` take radians, convert first
                xstep = (prev_xy.0 * a.cos() - prev_xy.1 * a.sin()) as Int;
                ystep = (prev_xy.0 * a.sin() + prev_xy.1 * a.cos()) as Int;

                // Set the same angle to each generated Instance
                angle = Some(a);
            }
            // Apply the reflection setting to each generated Instance
            reflect_vert = strans.reflected;
        }
        // Create the Instances
        let mut insts = Vec::with_capacity((aref.rows * aref.cols) as usize);
        for ix in 0..Int::from(aref.cols) {
            let x = p0.x + ix * xstep;
            for iy in 0..Int::from(aref.rows) {
                let y = p0.y + iy * ystep;
                insts.push(Instance {
                    inst_name: format!("{}[{}][{}]", cname, ix, iy), // `{array.name}[{col}][{row}]`
                    cell: cell.clone(),
                    loc: Point::new(x, y),
                    reflect_vert,
                    angle,
                });
            }
        }
        self.ctx.pop();
        Ok(Some(insts))
    }
    /// Import a [Point]
    fn import_point(&mut self, pt: &gds21::GdsPoint) -> LayoutResult<Point> {
        let x = pt.x.try_into()?;
        let y = pt.y.try_into()?;
        Ok(Point::new(x, y))
    }
    /// Import a vector of [Point]s
    fn import_point_vec(&mut self, pts: &Vec<gds21::GdsPoint>) -> LayoutResult<Vec<Point>> {
        pts.iter()
            .map(|p| self.import_point(p))
            .collect::<Result<Vec<_>, _>>()
    }
    /// Get the ([LayerKey], [LayerPurpose]) pair for a GDS element implementing its [gds21::HasLayer] trait.
    /// Layers are created if they do not already exist,
    /// although this may eventually be a per-importer setting.
    fn import_element_layer(
        &mut self,
        elem: &impl gds21::HasLayer,
    ) -> LayoutResult<(LayerKey, LayerPurpose)> {
        let spec = elem.layerspec();
        let mut layers = self.layers.write()?;
        layers.get_or_insert(spec.layer, spec.xtype)
    }
}
impl ErrorHelper for GdsImporter {
    type Error = LayoutError;
    fn err(&self, msg: impl Into<String>) -> LayoutError {
        LayoutError::Import {
            message: msg.into(),
            stack: self.ctx.clone(),
        }
    }
}

/// Import a GDS Cell with two polygons:
/// One assigned to a net, and the other not.
#[cfg(all(test, feature = "gds"))]
#[test]
fn gds_import1() -> LayoutResult<()> {
    use gds21::*;
    let gds = GdsLibrary {
        name: "lib1".into(),
        structs: vec![GdsStruct {
            name: "cell1".into(),
            elems: vec![
                GdsElement::GdsBoundary(GdsBoundary {
                    layer: 11,
                    datatype: 22,
                    xy: GdsPoint::vec(&[(0, 0), (2, 0), (2, 2), (0, 2), (0, 0)]),
                    ..Default::default()
                }),
                GdsElement::GdsTextElem(GdsTextElem {
                    string: "net1".into(),
                    layer: 11,    // Same layer as the boundary
                    texttype: 66, // Could be anything, for now
                    xy: GdsPoint::new(1, 1),
                    ..Default::default()
                }),
                GdsElement::GdsBoundary(GdsBoundary {
                    layer: 33,
                    datatype: 44,
                    xy: GdsPoint::vec(&[(10, 10), (12, 10), (12, 12), (10, 12), (10, 10)]),
                    ..Default::default()
                }),
                GdsElement::GdsTextElem(GdsTextElem {
                    string: "net1".into(),
                    layer: 44, // *Not* Same layer as the boundary
                    texttype: 66,
                    xy: GdsPoint::new(11, 11), // Intersects with the boundary
                    ..Default::default()
                }),
            ],
            ..Default::default()
        }],
        ..Default::default()
    };
    let lib = GdsImporter::import(&gds, None)?;
    assert_eq!(lib.name, "lib1");
    assert_eq!(lib.cells.len(), 1);
    let cell = lib.cells.first().unwrap().clone();
    let cell = cell.read()?;
    let layout = cell.layout.as_ref().unwrap();
    assert_eq!(layout.name, "cell1");
    let elem = &layout.elems[0];
    assert_eq!(elem.net, Some("net1".to_string()));
    let elem = &layout.elems[1];
    assert_eq!(elem.net, None);

    Ok(())
}
