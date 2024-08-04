//!
//! # Lef Conversion Module
//!
//! Converts between Layout21's "raw" layout [Abstract]s and [lef21] structures.
//!

// Std-Lib
use std::collections::hash_map::Entry;
use std::convert::{TryFrom, TryInto};

// Local imports
use crate::utils::{ErrorContext, ErrorHelper, Ptr, Unwrapper};
use crate::{
    Abstract, AbstractPort, Cell, Int, Layer, LayerKey, Layers, LayoutError, LayoutResult, Library,
    Path, Point, Polygon, Rect, Shape, Units,
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
            if let Some(ref a) = cell.abs {
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
            Units::Pico => 1_000_000,
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
    fn export_abstract(&mut self, abs: &Abstract) -> LayoutResult<lef21::LefMacro> {
        self.ctx.push(ErrorContext::Cell(abs.name.clone()));
        // Create the empty/default [lef21::LefMacro]
        let mut lefmac = lef21::LefMacro::default();
        // Convert our name
        lefmac.name = abs.name.clone();
        // Convert ports
        for port in &abs.ports {
            lefmac.pins.push(self.export_port(port)?);
        }
        // Convert blockages
        for (layerkey, blockage) in &abs.blockages {
            let obs = self.export_layer_shapes(*layerkey, blockage)?;
            lefmac.obs.push(obs);
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
        let name = layers
            .get_name(layerkey)
            .unwrapper(self, format!("Invalid un-named layer for LEF export"))?;
        Ok(name.to_string())
    }
    /// Export a [Shape] to a [lef21::LefGeometry]
    fn export_shape(&self, shape: &Shape) -> LayoutResult<lef21::LefGeometry> {
        // Conver to a [LefShape]
        let inner: lef21::LefShape = match shape {
            Shape::Rect(ref r) => {
                lef21::LefShape::Rect(None, self.export_point(&r.p0)?, self.export_point(&r.p1)?)
            }
            Shape::Polygon(ref poly) => {
                let points = poly
                    .points
                    .iter()
                    .map(|p| self.export_point(p))
                    .collect::<Result<Vec<_>, _>>()?;
                lef21::LefShape::Polygon(None, points)
            }
            Shape::Path { .. } => {
                unimplemented!("LefExporter::PATH");
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
    /// Distance scaling factor
    dist_scale: u32,
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
            self.warn("Ignored LEF feature: sites");
        }
        if let Some(sens) = &leflib.names_case_sensitive {
            if *sens == lef21::LefOnOff::Off {
                self.fail("Unsupported LEF feature: case-insensitive naming")?;
            }
        }
        // Fields that are *there*, but not really used for now.
        // || leflib.version.is_some()
        // || leflib.bus_bit_chars.is_some()
        // || leflib.divider_char.is_some()
        // || leflib.no_wire_extension_at_pin.is_some()

        // Set its distance units
        self.lib.units = self.import_units(&leflib.units)?;
        // And convert each of its `macros`
        for lefmacro in &leflib.macros {
            let cell = self.import_cell(lefmacro)?;
            self.lib.cells.insert(cell);
        }
        Ok(())
    }
    /// Import our [Units].  
    fn import_units(&mut self, _lefunits: &Option<lef21::LefUnits>) -> LayoutResult<Units> {
        // Despite having a big structure for all kinds of units, LEF distance data is always in microns.
        // Distance values are commonly stored to 2-4 decimal places, which we'll move to angstroms.
        self.ctx.push(ErrorContext::Units);
        self.dist_scale = 10_000;
        self.ctx.pop();
        Ok(Units::Angstrom)
    }
    /// Import a [Cell]
    fn import_cell(&mut self, lefmacro: &lef21::LefMacro) -> LayoutResult<Cell> {
        self.ctx.push(ErrorContext::Cell(lefmacro.name.clone()));
        let abs = self.import_abstract(&lefmacro)?;
        let cell = Cell::from(abs);
        self.ctx.pop();
        Ok(cell)
    }
    /// Import a [Abstract]
    fn import_abstract(&mut self, lefmacro: &lef21::LefMacro) -> LayoutResult<Abstract> {
        self.ctx.push(ErrorContext::Abstract);
        // Create an outline-rectangle.
        let outline = {
            // Grab a [Point] from the `size` field
            let lefsize = lefmacro.size.as_ref().unwrapper(self, "Missing LEF size")?;
            let lefsize = lef21::LefPoint::new(lefsize.0, lefsize.1);
            let Point { x, y } = self.import_point(&lefsize)?;

            // FIXME: what layer this goes on.
            // Grab one named `boundary`.
            let _layer = {
                let mut layers = self.layers.write()?;
                match layers.names.get("boundary") {
                    Some(key) => key.clone(),
                    None => {
                        let layernum = layers.nextnum()?; // FIXME: support no-number [Layer]s
                        let newlayer = Layer::new(layernum, "boundary");
                        layers.add(newlayer)
                    }
                }
            };

            Polygon {
                points: vec![
                    Point::new(0, 0),
                    Point::new(x, 0),
                    Point::new(x, y),
                    Point::new(0, y),
                ],
            }
        };
        // Create the [Abstract] to be returned
        let mut abs = Abstract::new(&lefmacro.name, outline);
        // Import all pins
        for lefpin in &lefmacro.pins {
            let abs_port = self.import_pin(lefpin)?;
            abs.ports.push(abs_port);
        }
        // Import all obstructions
        for lefobs in &lefmacro.obs {
            // Import the set of shapes
            let (layerkey, shapes) = self.import_layer_geometries(lefobs)?;

            // Extend any existing entry on this layer, or create a new one
            match abs.blockages.entry(layerkey) {
                Entry::Occupied(mut e) => e.get_mut().extend(shapes),
                Entry::Vacant(e) => {
                    e.insert(shapes);
                }
            }
        }
        self.ctx.pop();
        Ok(abs)
    }
    /// Import a [LefPin]
    fn import_pin(&mut self, lefpin: &lef21::LefPin) -> LayoutResult<AbstractPort> {
        let mut abs_port = AbstractPort::new(&lefpin.name);
        // The LEF "pin vs port" distinction is not a thing here.
        // Only single-port pins can be imported.
        if lefpin.ports.len() != 1 {
            self.warn(format!(
                "Warning multiple `LefPort`s on `LefPin` {} will be merged",
                lefpin.name
            ));
        }

        // FIXME: check more unsupported [LefPin] fields

        // Import the shapes on each layer
        for port in &lefpin.ports {
            for lef_layer_geom in &port.layers {
                let (layerkey, shapes) = self.import_layer_geometries(lef_layer_geom)?;
                match abs_port.shapes.entry(layerkey) {
                    Entry::Occupied(mut e) => e.get_mut().extend(shapes),
                    Entry::Vacant(e) => {
                        e.insert(shapes);
                    }
                }
            }
        }
        Ok(abs_port)
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
        if geoms.except_pg_net.is_some() {
            self.fail("Unsupported LEF Feature: except_pg_net")?;
        }
        if let Some(spacing) = &geoms.spacing {
            if *spacing != lef21::LefLayerSpacing::Spacing(lef21::LefDecimal::ZERO) {
                self.fail("Unsupported LEF Feature: nonzero spacing")?;
            }
        }
        if geoms.vias.len() > 0 {
            self.warn("Ignored LEF Feature: vias");
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
            Rect(_, ref p0, ref p1) => self.import_rect((p0, p1)),
            Polygon(_, ref pts) => self.import_polygon(pts),
            Path(_, ref pts) => self.import_path(pts, layer),
        }
    }
    /// Import a [Shape::Poly]
    fn import_polygon(&mut self, lefpoints: &Vec<lef21::LefPoint>) -> LayoutResult<Shape> {
        let pts: Vec<Point> = self.import_point_vec(lefpoints)?;
        Ok(Shape::Polygon(Polygon { points: pts }))
    }
    /// Import a [Shape::Rect]
    fn import_rect(
        &mut self,
        lefpoints: (&lef21::LefPoint, &lef21::LefPoint),
    ) -> LayoutResult<Shape> {
        let p0 = self.import_point(lefpoints.0)?;
        let p1 = self.import_point(lefpoints.1)?;
        Ok(Shape::Rect(Rect { p0, p1 }))
    }
    /// Import a [Shape::Path]
    fn import_path(
        &mut self,
        pts: &Vec<lef21::LefPoint>,
        layer: &lef21::LefLayerGeometries,
    ) -> LayoutResult<Shape> {
        // Paths require that `layer` have a `width` attribute.
        // And *our* paths are integer-width'ed, so they better have a zero-valued fractional part.
        let width = layer
            .width
            .unwrapper(self, "Invalid LEF Path with no Width")?;
        let width = self.import_dist(&width)?;
        let width = usize::try_from(width)?;
        // Convert each of the Points
        let pts = self.import_point_vec(pts)?;
        // And return the path
        Ok(Shape::Path(Path { width, points: pts }))
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
    fn import_dist(&mut self, lefdec: &lef21::LefDecimal) -> LayoutResult<Int> {
        let scaled = lefdec * lef21::LefDecimal::from(self.dist_scale);
        if !scaled.fract().is_zero() {
            self.fail(format!(
                "LEF Decimal {} has non-zero fractional part when scaled to internal units as {}",
                lefdec, scaled
            ))?;
        }
        Ok(scaled.mantissa().try_into()?)
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
    /// Generate a warning.
    /// Thus far, prints to the console.
    fn warn(&self, msg: impl Into<String>) {
        eprintln!("Warning: {}", msg.into());
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

    #[test]
    fn test_lef1() -> LayoutResult<()> {
        use crate::utils::{Ptr, PtrList};
        let layers = crate::tests::layers()?;
        let a = Abstract {
            name: "to_lef1".into(),
            outline: Polygon {
                points: vec![
                    Point::new(0, 0),
                    Point::new(11, 0),
                    Point::new(11, 11),
                    Point::new(0, 11),
                ],
            },
            ports: vec![AbstractPort {
                net: "port1".into(),
                // Collect a hashmap of shapes from (LayerKey, Vec<Shape>) pairs
                shapes: vec![(
                    layers.keyname("met1").unwrap(),
                    vec![Shape::Rect(Rect {
                        p0: Point::new(1, 1),
                        p1: Point::new(2, 2),
                    })],
                )]
                .into_iter()
                .collect(),
            }],
            // Collect a hashmap of shapes from (LayerKey, Vec<Shape>) pairs
            blockages: vec![(
                layers.keyname("met1").unwrap(),
                vec![Shape::Rect(Rect {
                    p0: Point::new(0, 0),
                    p1: Point::new(10, 10),
                })],
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
