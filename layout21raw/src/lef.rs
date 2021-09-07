//!
//! # Lef Conversion Module
//!
//! Converts between Layout21's "raw" layout [Abstract]s and [lef21] structures.
//!

// Local imports
use crate::{
    AbstractPort, ErrorContext, HasErrors, LayerKey, LayoutAbstract, LayoutError, LayoutResult,
    Library, Point, Shape, Units,
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

impl HasErrors for LefExporter<'_> {
    fn err(&self, msg: impl Into<String>) -> LayoutError {
        LayoutError::Export {
            message: msg.into(),
            stack: self.ctx.clone(),
        }
    }
}

/// FIXME: coming soon!
pub struct LefImporter;

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
                layer: *layers.keyname("boundary").unwrap(),
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
                    *layers.keyname("met1").unwrap(),
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
                *layers.keyname("met1").unwrap(),
                vec![Shape::Rect {
                    p0: Point::new(0, 0),
                    p1: Point::new(10, 10),
                }],
            )]
            .into_iter()
            .collect(),
        };
        let cells = PtrList::new(vec![Ptr::new(a.into())]);
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
