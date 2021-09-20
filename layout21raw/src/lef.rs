//!
//! # Lef Conversion Module
//!
//! Converts between Layout21's "raw" layout [Abstract]s and [lef21] structures.
//!

// Std-Lib
use std::collections::hash_map::Entry;
use std::convert::{TryFrom, TryInto};

// Local imports
use crate::utils::Ptr;
use crate::utils::{ErrorContext, ErrorHelper};
use crate::{
    AbstractPort, CellBag, DepOrder, Element, Instance, Layer, LayerKey, LayerPurpose, Layers,
    LayoutAbstract, LayoutError, LayoutImpl, LayoutResult, Library, Point, Shape, TextElement,
    Units,
};
use lef21;

/// # Lef Exporter
#[derive(Debug)]
pub struct LefExporter<'lib> {
    lib: &'lib Library,
    ctx: Vec<ErrorContext>,
}
impl<'lib> LefExporter<'lib> {
    pub fn export(lib: &'lib Library) -> LayoutResult<lef21::LefLibrary> {
        Self {
            lib,
            ctx: Vec::new(),
        }
        .export_lib()
    }
    /// Internal implementation method. Convert all, starting from our top-level [Library].
    fn export_lib(&mut self) -> LayoutResult<lef21::LefLibrary> {
        self.ctx.push(ErrorContext::Library(self.lib.name.clone()));
        // Create a new [lef21::LefLibrary]
        let mut lib = lef21::LefLibrary::default();
        // Export its Units
        lib.units = self.export_units(&self.lib.units)?.into();
        // And convert each of our cells
        for cell in self.lib.cells.iter() {
            let cell = cell.read()?;
            // Export cells that have abstracts
            if let Some(ref a) = cell.abstrakt {
                lib.macros.push(self.export_abstract(a)?);
            }
        }
        Ok(lib)
    }
    /// Export the [Units] distance definition
    /// Lef sets this as a "databuse units per micron" numeric field, although only a few values are accepted.
    fn export_units(&mut self, units: &Units) -> LayoutResult<lef21::LefUnits> {
        let scale = match units {
            Units::Micro => 1,
            Units::Nano => 1000,
            Units::Angstrom => 10_000,
        };
        let dbu = match lef21::LefDbuPerMicron::try_new(lef21::LefDecimal::new(scale, 0)) {
            Ok(dbu) => Ok(dbu),
            Err(_) => self.fail("Internal error: invalid units for LEF export"),
        }?;
        let database_microns = Some(dbu);
        Ok(lef21::LefUnits {
            database_microns,
            ..Default::default()
        })
    }
    /// Convert a [Cell] to a [lef21::LefMacro] cell-definition
    fn export_abstract(&mut self, abstrakt: &LayoutAbstract) -> LayoutResult<lef21::LefMacro> {
        self.ctx.push(ErrorContext::Cell(abstrakt.name.clone()));
        // Create the empty/default [lef21::LefMacro]
        let mut lefmac = lef21::LefMacro::default();
        // Convert our name
        lefmac.name = abstrakt.name.clone();
        // Convert ports
        for port in &abstrakt.ports {
            lefmac.pins.push(self.export_port(port)?);
        }
        // Convert blockages
        for (layerkey, blockage) in &abstrakt.blockages {
            lefmac
                .obs
                .push(self.export_layer_shapes(*layerkey, blockage)?);
        }
        self.ctx.pop();
        Ok(lefmac)
    }
    /// Export an [AbstractPort] to a [lef21::LefPin]
    fn export_port(&mut self, port: &AbstractPort) -> LayoutResult<lef21::LefPin> {
        let mut pin = lef21::LefPin::default();
        pin.name = port.net.clone();
        // FIXME: export direction
        pin.direction = None;
        // While Lef has a concept of "multiple ports per pin", we do not.
        // Genarated [LefPin]s always have one [LefPort]
        let mut lefport = lef21::LefPort::default();
        for (layerkey, shape) in &port.shapes {
            lefport
                .layers
                .push(self.export_layer_shapes(*layerkey, shape)?);
        }
        pin.ports = vec![lefport];
        Ok(pin)
    }
    /// Export a set of [Shapes] on `layer` to a [LefLayerGeometries]
    fn export_layer_shapes(
        &mut self,
        layerkey: LayerKey,
        shapes: &Vec<Shape>,
    ) -> LayoutResult<lef21::LefLayerGeometries> {
        self.ctx.push(ErrorContext::Geometry);
        let mut layer_geom = lef21::LefLayerGeometries::default();
        // Set its layer (name)
        layer_geom.layer_name = self.export_layer(layerkey)?;
        // Export each shape
        for shape in shapes {
            layer_geom.geometries.push(self.export_shape(shape)?);
        }
        self.ctx.pop();
        Ok(layer_geom)
    }
    /// Export a [Layer] from its [LayerKey].
    /// Lef layers are just string identifiers, returned here as-is.
    fn export_layer(&self, layerkey: LayerKey) -> LayoutResult<String> {
        let layers = self.lib.layers.read()?;
        let name = self.unwrap(
            layers.get_name(layerkey),
            format!("Invalid un-named layer for LEF export"),
        )?;
        Ok(name.to_string())
    }
    /// Export a [Shape] to a [lef21::LefGeometry]
    fn export_shape(&self, shape: &Shape) -> LayoutResult<lef21::LefGeometry> {
        // Conver to a [LefShape]
        let inner: lef21::LefShape = match shape {
            Shape::Rect { ref p0, ref p1 } => {
                lef21::LefShape::Rect(self.export_point(p0)?, self.export_point(p1)?)
            }
            Shape::Poly { ref pts } => {
                let points = pts
                    .iter()
                    .map(|p| self.export_point(p))
                    .collect::<Result<Vec<_>, _>>()?;
                lef21::LefShape::Polygon(points)
            }
            Shape::Path { .. } => {
                todo!();
            }
        };
        // Wrap it in the [LefGeometry] enum (which also includes repetitions) and return it
        Ok(lef21::LefGeometry::Shape(inner))
    }
    /// Export a [Point]
    fn export_point(&self, point: &Point) -> LayoutResult<lef21::LefPoint> {
        Ok(lef21::LefPoint::new(
            lef21::LefDecimal::from(point.x),
            lef21::LefDecimal::from(point.y),
        ))
    }
}

impl ErrorHelper for LefExporter<'_> {
    type Error = LayoutError;
    fn err(&self, msg: impl Into<String>) -> LayoutError {
        LayoutError::Export {
            message: msg.into(),
            stack: self.ctx.clone(),
        }
    }
}

/// # Lef Importer
#[derive(Debug, Default)]
pub struct LefImporter {
    layers: Ptr<Layers>,
    ctx: Vec<ErrorContext>,
    lib: Library,
}
impl LefImporter {
    pub fn import(plib: &lef21::LefLibrary, layers: Option<Ptr<Layers>>) -> LayoutResult<Library> {
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
        importer.import_lib(&plib)?;
        // And destructure the result from our importer
        let Self {
            mut lib, layers, ..
        } = importer;
        lib.layers = layers;
        Ok(lib)
    }
    /// Internal implementation method. Convert the top-level library.
    fn import_lib(&mut self, leflib: &lef21::LefLibrary) -> LayoutResult<()> {
        // Give our library its name
        let name = "".to_string(); // FIXME: naming? from file maybe?
        self.ctx.push(ErrorContext::Library(name.clone()));
        self.lib.name = name;
        // Check for unsupported features
        if leflib.sites.len() > 0 {
            self.fail("Unsupported LEF feature: sites")?;
        }
        if leflib.vias.is_some() {
            self.fail("Unsupported LEF feature: vias")?;
        }
        if leflib.extensions.is_some() {
            self.fail("Unsupported LEF feature: extensions")?;
        }
        if leflib.version.is_some()
            || leflib.names_case_sensitive.is_some()
            || leflib.no_wire_extension_at_pin.is_some()
            || leflib.bus_bit_chars.is_some()
            || leflib.divider_char.is_some()
        {
            self.fail("Unsupported LEF feature")?;
        }
        // Set its distance units
        self.lib.units = self.import_units(&leflib.units)?;
        // And convert each of its `macros`
        for lefmacro in &leflib.macros {
            let cell = self.import_cell(lefmacro)?;
            self.lib.cells.insert(cell);
        }
        Ok(())
    }
    /// Import our [Units]. (Yes, proto-code keeps them as an integer. At least it has some helpers.)
    fn import_units(&mut self, lefunits: &Option<lef21::LefUnits>) -> LayoutResult<Units> {
        self.ctx.push(ErrorContext::Units);
        if lefunits.is_none() {
            return Ok(Units::default()); // FIXME! Needs the *LEF default* 10nm
        }
        let lefunits = lefunits.as_ref().unwrap();
        // Check for unsupported units: all types but distance
        if lefunits.time_ns.is_some()
            || lefunits.capacitance_pf.is_some()
            || lefunits.resistance_ohms.is_some()
            || lefunits.power_mw.is_some()
            || lefunits.current_ma.is_some()
            || lefunits.voltage_volts.is_some()
            || lefunits.frequency_mhz.is_some()
        {
            self.fail("Unsupported LEF units")?;
        }
        let ours: LayoutResult<Units> = match lefunits.database_microns {
            None => Ok(Units::default()), // FIXME! Needs the *LEF default* 10nm
            Some(ref dbu) => match dbu.value() {
                1000 => Ok(Units::Nano),
                10_000 => Ok(Units::Angstrom),
                100 | 200 | 400 | 800 | 2000 | 4000 | 8000 | 20_000 => {
                    self.fail(format!("Unsupported LEF DBU per Micron {}", dbu.value()))?
                }
                _ => self.fail(format!("Invalid LEF DBU Per Micron: {}", dbu.value()))?,
            },
        };
        let ours: Units = ours?;
        self.ctx.pop();
        Ok(ours)
    }
    /// Import a [CellBag]
    fn import_cell(&mut self, lefmacro: &lef21::LefMacro) -> LayoutResult<CellBag> {
        self.ctx.push(ErrorContext::Cell(lefmacro.name.clone()));
        let abstrakt = self.import_abstract(&lefmacro)?;
        let cell = CellBag::from(abstrakt);
        self.ctx.pop();
        Ok(cell)
    }
    /// Import a [LayoutAbstract]
    fn import_abstract(&mut self, lefmacro: &lef21::LefMacro) -> LayoutResult<LayoutAbstract> {
        self.ctx.push(ErrorContext::Abstract);
        // Get an [Outline] from the LEF `size` field
        if lefmacro.size.is_none() {
            self.fail("Missing LEF size")?;
        }
        // FIXME: faking out the `outline` field for now; policy on these TBD.
        // let lefsize = lefmacro.size.as_ref().unwrap();
        let fake_outline = {
            let layers = self.layers.write()?;
            Element {
                net: None,
                layer: layers.keynum(0).unwrap(), // FIXME: gonna fail
                purpose: LayerPurpose::Outline,
                inner: Shape::Rect {
                    p0: Point::new(0, 0),
                    p1: Point::new(0, 0),
                },
            }
        };
        // Create the [LayoutAbstract] to be returned
        let mut abstrakt = LayoutAbstract::new(&lefmacro.name, fake_outline);
        // Import all pins
        for lefpin in &lefmacro.pins {
            let abstrakt_port = self.import_pin(lefpin)?;
            abstrakt.ports.push(abstrakt_port);
        }
        // Import all obstructions
        for lefobs in &lefmacro.obs {
            // Import the set of shapes
            let (layerkey, shapes) = self.import_layer_geometries(lefobs)?;

            // Extend any existing entry on this layer, or create a new one
            match abstrakt.blockages.entry(layerkey) {
                Entry::Occupied(mut e) => e.get_mut().extend(shapes),
                Entry::Vacant(e) => {
                    e.insert(shapes);
                }
            }
        }
        self.ctx.pop();
        Ok(abstrakt)
    }
    /// Import a [LefPin]
    fn import_pin(&mut self, lefpin: &lef21::LefPin) -> LayoutResult<AbstractPort> {
        let mut abstrakt_port = AbstractPort::new(&lefpin.name);
        // The LEF "pin vs port" distinction is not a thing here.
        // Only single-port pins can be imported.
        if lefpin.ports.len() != 1 {
            self.fail("LEF pin has multiple ports")?;
        }
        let port = &lefpin.ports[0];
        // FIXME: check more unsupported [LefPin] fields
        // Import the shapes on each layer
        for lef_layer_geom in &port.layers {
            let (layerkey, shapes) = self.import_layer_geometries(lef_layer_geom)?;
            match abstrakt_port.shapes.entry(layerkey) {
                Entry::Occupied(mut e) => e.get_mut().extend(shapes),
                Entry::Vacant(e) => {
                    e.insert(shapes);
                }
            }
        }
        Ok(abstrakt_port)
    }
    /// Import a layer's-worth of shapes.
    /// Returns a tuple of (layerkey, shapes)
    fn import_layer_geometries(
        &mut self,
        geoms: &lef21::LefLayerGeometries,
    ) -> LayoutResult<(LayerKey, Vec<Shape>)> {
        self.ctx.push(ErrorContext::Geometry);
        // Import the layer
        let layerkey = self.import_layer(&geoms.layer_name)?;
        if geoms.except_pg_net.is_some()
            || geoms.spacing.is_some()
            || geoms.width.is_some()
            || geoms.vias.len() > 0
        {
            self.fail("Unsupported LEF Feature")?;
        }
        // Import all the shapes
        let mut shapes = Vec::new();
        for geom in &geoms.geometries {
            let shape = self.import_geometry(geom, &geoms)?;
            shapes.push(shape);
        }
        self.ctx.pop();
        Ok((layerkey, shapes))
    }
    /// Import a [LefGeometry]
    fn import_geometry(
        &mut self,
        geom: &lef21::LefGeometry,
        layer: &lef21::LefLayerGeometries,
    ) -> LayoutResult<Shape> {
        match geom {
            lef21::LefGeometry::Shape(s) => self.import_shape(s, layer),
            lef21::LefGeometry::Iterate { .. } => self.fail("Unsupported LEF Feature: Iterate"),
        }
    }
    /// Import a [lef21::LefShape] to a [Shape]
    fn import_shape(
        &mut self,
        lefshape: &lef21::LefShape,
        layer: &lef21::LefLayerGeometries,
    ) -> LayoutResult<Shape> {
        use lef21::LefShape::{Path, Polygon, Rect};
        match lefshape {
            Rect(ref p0, ref p1) => self.import_rect((p0, p1)),
            Polygon(ref pts) => self.import_polygon(pts),
            Path(ref pts) => self.import_path(pts, layer),
        }
    }
    /// Import a [Shape::Poly]
    fn import_polygon(&mut self, lefpoints: &Vec<lef21::LefPoint>) -> LayoutResult<Shape> {
        let pts: Vec<Point> = self.import_point_vec(lefpoints)?;
        Ok(Shape::Poly { pts })
    }
    /// Import a [Shape::Rect]
    fn import_rect(
        &mut self,
        lefpoints: (&lef21::LefPoint, &lef21::LefPoint),
    ) -> LayoutResult<Shape> {
        let p0 = self.import_point(lefpoints.0)?;
        let p1 = self.import_point(lefpoints.1)?;
        Ok(Shape::Rect { p0, p1 })
    }
    /// Import a [Shape::Path]
    fn import_path(
        &mut self,
        pts: &Vec<lef21::LefPoint>,
        layer: &lef21::LefLayerGeometries,
    ) -> LayoutResult<Shape> {
        // Paths require that `layer` have a `width` attribute.
        // And *our* paths are integer-width'ed, so they better have a zero-valued fractional part.
        // FIXME: real width conversion into `self.units`
        let width = self.unwrap(layer.width, "Invalid LEF Path with no Width")?;
        if !width.fract().is_zero() {
            self.fail("LEF Path has non-zero fractional width")?;
        }
        let width = usize::try_from(width.mantissa())?;
        // Convert each of the Points
        let pts = self.import_point_vec(pts)?;
        // And return the path
        Ok(Shape::Path { width, pts })
    }
    /// Add the finishing touches to convert a [Shape] to an [Element]
    fn convert_shape(
        &mut self,
        inner: Shape,
        layer: LayerKey,
        purpose: LayerPurpose,
        net: &str,
    ) -> LayoutResult<Element> {
        let net = if net.is_empty() {
            None
        } else {
            Some(net.to_string())
        };
        // Create the Element. Note the layer fields are thus far left default.
        Ok(Element {
            net,
            inner,
            layer,
            purpose,
        })
    }
    /// Import a vector of [Point]s
    fn import_point_vec(&mut self, pts: &Vec<lef21::LefPoint>) -> LayoutResult<Vec<Point>> {
        pts.iter()
            .map(|p| self.import_point(p))
            .collect::<Result<Vec<_>, _>>()
    }
    /// Import a [Point]
    fn import_point(&mut self, pt: &lef21::LefPoint) -> LayoutResult<Point> {
        Ok(Point::new(
            self.import_dist(&pt.x)?,
            self.import_dist(&pt.x)?,
        ))
    }
    /// Import a distance coordinate, converting between units
    fn import_dist(&mut self, lefdec: &lef21::LefDecimal) -> LayoutResult<isize> {
        if !lefdec.fract().is_zero() {
            self.fail("LEF Decimal has non-zero fractional part")?;
        }
        // Since only matching units are (thus far) supported,
        // no math necessary here yet, just type conversion.
        Ok(lefdec.mantissa().try_into()?)
    }
    /// Get the ([LayerKey], [LayerPurpose]) pair for layer-name `leflayer`.
    /// Layers are created if they do not already exist, although this may eventually be a per-importer setting.
    fn import_layer(&mut self, leflayer: &str) -> LayoutResult<LayerKey> {
        // Unlock the `layers` shared-pointer
        let mut layers = self.layers.write()?;
        // Grab the layer-key, creating it if it does not exist
        let layerkey = match layers.keyname(leflayer) {
            Some(layer) => layer,
            None => {
                // Create a new layer, add it and return the key
                let num = layers.nextnum()?; // FIXME: support no-number [Layer]s
                let newlayer = Layer::new(num, leflayer);
                layers.add(newlayer)
            }
        };
        Ok(layerkey)
    }
}
impl ErrorHelper for LefImporter {
    type Error = LayoutError;
    fn err(&self, msg: impl Into<String>) -> LayoutError {
        LayoutError::Import {
            message: msg.into(),
            stack: self.ctx.clone(),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::{Element, LayerPurpose};

    #[test]
    fn test_lef1() -> LayoutResult<()> {
        use crate::utils::{Ptr, PtrList};
        let layers = crate::tests::layers()?;
        let a = LayoutAbstract {
            name: "to_lef1".into(),
            outline: Element {
                net: None,
                layer: layers.keyname("boundary").unwrap(),
                purpose: LayerPurpose::Outline,
                inner: Shape::Rect {
                    p0: Point::new(0, 0),
                    p1: Point::new(11, 11),
                },
            },
            ports: vec![AbstractPort {
                net: "port1".into(),
                // Collect a hashmap of shapes from (LayerKey, Vec<Shape>) pairs
                shapes: vec![(
                    layers.keyname("met1").unwrap(),
                    vec![Shape::Rect {
                        p0: Point::new(1, 1),
                        p1: Point::new(2, 2),
                    }],
                )]
                .into_iter()
                .collect(),
            }],
            // Collect a hashmap of shapes from (LayerKey, Vec<Shape>) pairs
            blockages: vec![(
                layers.keyname("met1").unwrap(),
                vec![Shape::Rect {
                    p0: Point::new(0, 0),
                    p1: Point::new(10, 10),
                }],
            )]
            .into_iter()
            .collect(),
        };
        let cells = PtrList::from_owned(vec![a.into()]);
        let lib = Library {
            name: "to_lef_lib1".into(),
            layers: Ptr::new(layers),
            cells,
            ..Default::default()
        };
        let leflib = LefExporter::export(&lib)?;
        assert_eq!(leflib.macros.len(), 1);
        let lefmac = &leflib.macros[0];
        assert_eq!(lefmac.name, "to_lef1");
        assert_eq!(lefmac.pins.len(), 1);
        let lefpin = &lefmac.pins[0];
        assert_eq!(lefpin.name, "port1");
        assert_eq!(lefpin.ports.len(), 1);
        let lefport = &lefpin.ports[0];
        assert_eq!(lefport.layers.len(), 1);

        Ok(())
    }
}
