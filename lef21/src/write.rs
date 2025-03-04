//!
//! # Lef Writer Module
//!

// Standard Lib Imports
use std::io::Write;
use std::ops::{AddAssign, SubAssign};
use std::path::Path;

// Layout21 Imports
use layout21utils as utils;
// pub use utils::{EnumStr, SerdeFile, SerializationFormat};
pub use utils::EnumStr;

// Local imports
use super::data::*;

/// Write a [LefLibrary] to file `fname`.  
/// Fields are written in the LEF-recommended order.  
pub fn save(lib: &LefLibrary, fname: impl AsRef<Path>) -> LefResult<()> {
    let f = std::fs::File::create(fname)?;
    LefWriter::new(f).write_lib(lib)
}
/// Write a [LefLibrary] to LEF-format [String].  
/// Fields are written in the LEF-recommended order.  
pub fn to_string(lib: &LefLibrary) -> LefResult<String> {
    let mut buf = Vec::new();
    LefWriter::new(&mut buf).write_lib(lib)?;
    let rv = std::str::from_utf8(buf.as_slice()).unwrap().to_string();
    Ok(rv)
}

/// # Lef Writing Helper
pub struct LefWriter<'wr> {
    /// Write Destination
    dest: Box<dyn Write + 'wr>,
    /// Indentation Helper
    indent: Indent,
    /// Session State
    session: LefWriterSession,
}
impl<'wr> LefWriter<'wr> {
    /// Create a new [LefWriter] to destination `dest`.
    /// Destination is boxed internally.
    fn new(dest: impl Write + 'wr) -> Self {
        Self {
            dest: Box::new(dest),
            indent: Indent::new("    "), // Always uses four spaces. Potentially make this an option.
            session: LefWriterSession::default(),
        }
    }
    /// Write a [LefLibrary] to the destination
    /// Fields are written in the LEF-recommended order
    fn write_lib(&mut self, lib: &LefLibrary) -> LefResult<()> {
        use LefKey::{
            BusBitChars, ClearanceMeasure, DividerChar, End, FixedMask, Library, ManufacturingGrid,
            NamesCaseSensitive, NoWireExtensionAtPin, Obs, PropertyDefinitions, UseMinSpacing,
            Version,
        };
        if let Some(ref v) = lib.version {
            // Save a copy in our session-state
            self.session.lef_version = v.clone();
            self.write_line(format_args_f!("{Version} {v} ; "))?;
        }
        if let Some(ref v) = lib.names_case_sensitive {
            // Valid for versions <= 5.4
            if self.session.lef_version > *V5P4 {
                let v = self.session.lef_version;
                let msg = format_f!("Invalid: {NamesCaseSensitive} in Version: {v}");
                return Err(LefError::Str(msg));
            }
            self.write_line(format_args_f!("{NamesCaseSensitive} {v} ; "))?;
        }
        if let Some(ref v) = lib.no_wire_extension_at_pin {
            // Valid for versions <= 5.4
            if self.session.lef_version > *V5P4 {
                let v = self.session.lef_version;
                let msg = format_f!("Invalid: {NoWireExtensionAtPin} in Version: {v}");
                return Err(LefError::Str(msg));
            }
            self.write_line(format_args_f!("{NoWireExtensionAtPin} {} ; ", v))?;
        }
        if let Some(ref v) = lib.bus_bit_chars {
            self.write_line(format_args_f!("{BusBitChars} \"{v.0}{v.1}\" ; "))?;
            //, v.0, v.1))?;
        }
        if let Some(ref v) = lib.divider_char {
            self.write_line(format_args_f!("{DividerChar} \"{}\" ; ", v))?;
        }
        if let Some(ref v) = lib.units {
            self.write_units(v)?
        }
        // MANUFACTURINGGRID
        if let Some(ref v) = lib.manufacturing_grid {
            self.write_line(format_args_f!("{ManufacturingGrid} {} ; ", v))?;
        }
        // USEMINSPACING
        if let Some(ref v) = lib.use_min_spacing {
            self.write_line(format_args_f!("{UseMinSpacing} {Obs} {} ; ", v))?;
        }
        // CLEARANCEMEASURE
        if let Some(ref v) = lib.clearance_measure {
            self.write_line(format_args_f!("{ClearanceMeasure} {} ; ", v))?;
        }
        // PROPERTYDEFINITIONS
        if lib.property_definitions.len() > 0 {
            self.write_line(format_args_f!("{PropertyDefinitions} "))?;
            self.indent += 1;
            for propdef in lib.property_definitions.iter() {
                let propdef_str: String = match propdef {
                    LefPropertyDefinition::LefString(objtype, name, None) => {
                        format!("{objtype} {name} {}", LefKey::String)
                    }
                    LefPropertyDefinition::LefString(objtype, name, Some(val)) => {
                        format!("{objtype} {name} {} {}", LefKey::String, val)
                    }
                    LefPropertyDefinition::LefReal(objtype, name, value, range) => {
                        self.format_numeric_prop_def(objtype, name, LefKey::Real, value, range)?
                    }
                    LefPropertyDefinition::LefInteger(objtype, name, value, range) => {
                        self.format_numeric_prop_def(objtype, name, LefKey::Integer, value, range)?
                    }
                };
                self.write_line(format_args_f!("{} ; ", propdef_str))?;
            }
            self.indent -= 1;
            self.write_line(format_args_f!("{End} {PropertyDefinitions} "))?;
        }
        // FIXEDMASK
        if lib.fixed_mask {
            self.write_line(format_args_f!("{FixedMask} ;"))?;
        }

        // TODO: LAYER
        // TODO: MAXVIASTACK

        // Write each via definition
        for via in lib.vias.iter() {
            self.write_via(via)?;
        }

        // TODO: VIARULE [GENERATE]
        // TODO: NONDEFAULTRULE

        // Write each SITE definition
        for site in lib.sites.iter() {
            self.write_site(site)?;
        }
        // Write each MACRO definition
        for mac in lib.macros.iter() {
            self.write_macro(mac)?;
        }

        for ext in lib.extensions.iter() {
            use LefKey::{BeginExtension, EndExtension};
            self.write_line(format_args_f!(
                "{BeginExtension} {ext.name} {ext.data} {EndExtension}"
            ))?;
        }

        self.write_line(format_args_f!("{End} {Library} \n"))?;
        self.dest.flush()?;
        Ok(())
    }

    /// Write a [LefViaDef].
    fn write_via(&mut self, via: &LefViaDef) -> LefResult<()> {
        use LefKey::{
            CutSize, CutSpacing, Default, Enclosure, End, Layers, Offset, Origin, Resistance,
            RowCol, Via, ViaRule,
        };

        if via.default {
            self.write_line(format_args_f!("{Via} {via.name} {Default}"))?;
        } else {
            self.write_line(format_args_f!("{Via} {via.name}"))?;
        }
        self.indent += 1;

        match &via.data {
            LefViaDefData::Fixed(via) => {
                if let Some(r) = via.resistance_ohms {
                    self.write_line(format_args_f!("{Resistance} {r} ; "))?;
                }
                for layer in via.layers.iter() {
                    self.write_via_layer_geom(layer)?;
                }
            }
            LefViaDefData::Generated(via) => {
                self.write_line(format_args_f!("{ViaRule} {via.via_rule_name} ; "))?;
                self.write_line(format_args_f!(
                    "{CutSize} {via.cut_size_x} {via.cut_size_y} ; "
                ))?;
                self.write_line(format_args_f!(
                    "{Layers} {via.bot_metal_layer} {via.cut_layer} {via.top_metal_layer} ; "
                ))?;
                self.write_line(format_args_f!(
                    "{CutSpacing} {via.cut_spacing_x} {via.cut_spacing_y} ; "
                ))?;
                self.write_line(format_args_f!(
                    "{Enclosure} {via.bot_enc_x} {via.bot_enc_y} {via.top_enc_x} {via.top_enc_y} ; "
                ))?;
                if let Some(ref rowcol) = via.rowcol {
                    self.write_line(format_args_f!("{RowCol} {rowcol.rows} {rowcol.cols} ; "))?;
                }
                if let Some(ref origin) = via.origin {
                    self.write_line(format_args_f!("{Origin} {origin.x} {origin.y} ; "))?;
                }
                if let Some(ref offset) = via.offset {
                    self.write_line(format_args_f!(
                        "{Offset} {offset.bot_x} {offset.bot_y} {offset.top_x} {offset.top_y} ; "
                    ))?;
                }
                // PATTERN would go here
            }
        }

        // PROPERTIES would go here

        self.indent -= 1;
        self.write_line(format_args_f!("{End} {} ", via.name))?;
        Ok(())
    }

    // helper function to format numeric PROPERTYDEFINITION entries
    //   for "objType propName [RANGE begin end] [value]"
    fn format_numeric_prop_def(
        &mut self,
        objtype: &LefPropertyDefinitionObjectType,
        name: &String,
        key: LefKey,
        value: &Option<LefDecimal>,
        range: &Option<LefPropertyRange>,
    ) -> LefResult<String> {
        use LefKey::Range;
        let mut string_list: Vec<String> = Vec::new();
        string_list.push(objtype.to_string());
        string_list.push(name.to_string());
        string_list.push(key.to_string());
        match range {
            Some(r) => string_list.push(format!("{Range} {} {}", r.begin, r.end)),
            None => (),
        }
        match value {
            Some(v) => string_list.push(v.to_string()),
            None => (),
        }
        Ok(string_list.join(" "))
    }

    /// Write a [LefSite] definition
    fn write_site(&mut self, site: &LefSite) -> LefResult<()> {
        use LefKey::{By, Class, End, Site, Size};
        self.write_line(format_args_f!("{Site} {site.name} ; "))?;
        self.indent += 1;
        self.write_line(format_args_f!("{Class} {site.class};"))?;
        if let Some(ref v) = site.symmetry {
            self.write_symmetries(v)?;
        }
        // ROWPATTERN would be written here
        // if site.row_pattern.is_some() { }
        self.write_line(format_args_f!("{Size} {site.size.0} {By} {site.size.1} ;"))?;
        self.indent -= 1;
        self.write_line(format_args_f!("{End} {site.name} ; "))?;
        Ok(())
    }

    /// Write a [LefUnits] block.
    fn write_units(&mut self, units: &LefUnits) -> LefResult<()> {
        use LefKey::{
            Capacitance, Current, Database, End, Frequency, Megahertz, Microns, Milliamps,
            Milliwatts, Nanoseconds, Ohms, Picofarads, Power, Resistance, Time, Units, Voltage,
            Volts,
        };
        self.write_line(format_args_f!("{Units} "))?;
        self.indent += 1;
        if let Some(ref val) = units.time_ns {
            self.write_line(format_args_f!("{Time} {Nanoseconds} {} ; ", val))?;
        }
        if let Some(ref val) = units.capacitance_pf {
            self.write_line(format_args_f!("{Capacitance} {Picofarads} {} ; ", val))?;
        }
        if let Some(ref val) = units.resistance_ohms {
            self.write_line(format_args_f!("{Resistance} {Ohms} {} ; ", val))?;
        }
        if let Some(ref val) = units.power_mw {
            self.write_line(format_args_f!("{Power} {Milliwatts} {} ; ", val))?;
        }
        if let Some(ref val) = units.current_ma {
            self.write_line(format_args_f!("{Current} {Milliamps} {} ; ", val))?;
        }
        if let Some(ref val) = units.voltage_volts {
            self.write_line(format_args_f!("{Voltage} {Volts} {} ; ", val))?;
        }
        if let Some(ref db) = units.database_microns {
            self.write_line(format_args_f!("{Database} {Microns} {} ; ", db.0))?;
        }
        if let Some(ref val) = units.frequency_mhz {
            self.write_line(format_args_f!("{Frequency} {Megahertz} {} ; ", val))?;
        }
        self.indent -= 1;
        self.write_line(format_args_f!("{End} {Units} "))?;
        Ok(())
    }

    /// Write a [LefMacro], in recommended order of fields.
    fn write_macro(&mut self, mac: &LefMacro) -> LefResult<()> {
        use LefKey::{By, Eeq, End, FixedMask, Foreign, Macro, Obs, Origin, Site, Size, Source};
        self.write_line(format_args_f!("{Macro} {mac.name}"))?;
        self.indent += 1;

        if let Some(ref v) = mac.class {
            self.write_macro_class(v)?;
        }
        if mac.fixed_mask {
            self.write_line(format_args_f!("{FixedMask} ;"))?;
        }
        if let Some(ref v) = mac.foreign {
            let pt = match v.pt {
                Some(ref p) => p.to_string(),
                None => "".into(),
            };
            let orient = match v.orient {
                Some(ref o) => o.to_string(),
                None => "".into(),
            };
            self.write_line(format_args_f!("{Foreign} {v.cell_name} {pt} {orient} ;"))?;
        }
        if let Some(ref v) = mac.origin {
            self.write_line(format_args_f!("{Origin} {v} ;"))?;
        }
        if let Some(ref v) = mac.source {
            // Valid for versions <= 5.4
            if self.session.lef_version > *V5P4 {
                let v = self.session.lef_version;
                let msg = format_f!("Invalid VERSION for MACRO SOURCE: {v}");
                return Err(LefError::Str(msg));
            }
            self.write_line(format_args_f!("{Source} {v} ;"))?;
        }

        if let Some(ref cell) = mac.eeq {
            self.write_line(format_args_f!("{Eeq} {cell} ;"))?;
        }
        if let Some(ref v) = mac.size {
            self.write_line(format_args_f!("{Size} {v.0} {By} {v.1} ;"))?;
        }
        if let Some(ref v) = mac.symmetry {
            self.write_symmetries(v)?;
        }
        if let Some(ref v) = mac.site {
            self.write_line(format_args_f!("{Site} {v} ;"))?;
        }
        for pin in mac.pins.iter() {
            self.write_pin(pin)?;
        }
        if !mac.obs.is_empty() {
            self.write_line(format_args_f!("{Obs} "))?;
            self.indent += 1;
            for layer in mac.obs.iter() {
                self.write_layer_geom(layer)?;
            }
            self.indent -= 1;
            self.write_line(format_args_f!("{End} "))?;
        }
        for prop in mac.properties.iter() {
            self.write_property(prop)?;
        }
        if let Some(ref v) = mac.density {
            self.write_density(v)?;
        }

        self.indent -= 1;
        self.write_line(format_args_f!("{End} {} ", mac.name))?;
        Ok(())
    }

    fn write_property(&mut self, prop: &LefProperty) -> LefResult<()> {
        use LefKey::Property;
        self.write_line(format_args_f!("{Property} {} {}", prop.name, prop.value))?;
        Ok(())
    }

    /// Write a [LefPin] definition
    fn write_pin(&mut self, pin: &LefPin) -> LefResult<()> {
        use LefKey::{
            AntennaModel, Direction, End, GroundSensitivity, Layer, MustJoin, NetExpr, Pin, Shape,
            SupplySensitivity, TaperRule, Use,
        };
        self.write_line(format_args_f!("{Pin} {pin.name} "))?;
        self.indent += 1;
        if let Some(ref v) = pin.direction {
            self.write_line(format_args_f!("{Direction} {v} ; "))?;
        }
        if let Some(ref v) = pin.use_ {
            self.write_line(format_args_f!("{Use} {v} ; "))?;
        }
        if let Some(ref v) = pin.shape {
            self.write_line(format_args_f!("{Shape} {v} ; "))?;
        }
        if let Some(ref v) = pin.antenna_model {
            self.write_line(format_args_f!("{AntennaModel} {v} ; "))?;
        }
        for attr in pin.antenna_attrs.iter() {
            let layer = if let Some(ref lname) = attr.layer {
                format_f!("{Layer} {lname}")
            } else {
                String::new()
            };
            self.write_line(format_args_f!("{attr.key} {attr.val} {layer} ;"))?;
        }
        if let Some(ref v) = pin.taper_rule {
            self.write_line(format_args_f!("{TaperRule} {v} ; "))?;
        }
        if let Some(ref v) = pin.supply_sensitivity {
            self.write_line(format_args_f!("{SupplySensitivity} {v} ; "))?;
        }
        if let Some(ref v) = pin.ground_sensitivity {
            self.write_line(format_args_f!("{GroundSensitivity} {v} ; "))?;
        }
        if let Some(ref v) = pin.must_join {
            self.write_line(format_args_f!("{MustJoin} {v} ; "))?;
        }
        if let Some(ref v) = pin.net_expr {
            self.write_line(format_args_f!("{NetExpr} {v} ; "))?;
        }
        for prop in pin.properties.iter() {
            self.write_property(prop)?;
        }

        // Most unsupported PINS features *would* go here.
        // if pin.properties.is_some() {
        //     return Err(LefError::Str("Unsupported LefPin Attr".into()));
        // }

        // Write each PORT
        for port in pin.ports.iter() {
            self.write_port(port)?;
        }
        self.indent -= 1;
        self.write_line(format_args_f!("{End} {} ", pin.name))?;
        Ok(())
    }
    /// Write a [LefPort] definition
    fn write_port(&mut self, port: &LefPort) -> LefResult<()> {
        use LefKey::{Class, End, Port};
        self.write_line(format_args_f!("{Port} "))?;
        self.indent += 1;
        if let Some(ref v) = port.class {
            self.write_line(format_args_f!("{Class} {v} ; "))?;
        }
        for layer in port.layers.iter() {
            self.write_layer_geom(layer)?;
        }
        self.indent -= 1;
        self.write_line(format_args_f!("{End} "))?;
        Ok(())
    }
    /// Write the [LefLayerGeometries], common to both ports and obstructions
    fn write_layer_geom(&mut self, layer: &LefLayerGeometries) -> LefResult<()> {
        use LefKey::{DesignRuleWidth, ExceptPgNet, Layer, Spacing, Via, Width};
        // The "LAYER" statement has a few inline options. Extract them here.
        let pg = match layer.except_pg_net {
            Some(ref pg) if *pg => format_f!("{ExceptPgNet} "),
            Some(_) | None => String::new(),
        };
        let sps = match layer.spacing {
            Some(LefLayerSpacing::DesignRuleWidth(ref s)) => format_f!("{DesignRuleWidth} {s} "),
            Some(LefLayerSpacing::Spacing(ref s)) => format_f!("{Spacing} {s} "),
            None => "".into(),
        };
        self.write_line(format_args_f!("{Layer} {layer.layer_name} {pg}{sps};"))?;
        self.indent += 1;

        if let Some(ref v) = layer.width {
            self.write_line(format_args_f!("{Width} {v} ; "))?;
        }
        for geom in layer.geometries.iter() {
            self.write_geom(geom)?;
        }
        for via in layer.vias.iter() {
            self.write_line(format_args_f!("{Via} {via.pt} {via.via_name} ;"))?;
        }
        self.indent -= 1;
        Ok(()) // Note [LefLayerGeometries] have no "END" or other closing delimeter.
    }
    /// Write [LefViaLayerGeometries].
    fn write_via_layer_geom(&mut self, layer: &LefViaLayerGeometries) -> LefResult<()> {
        use LefKey::Layer;
        self.write_line(format_args_f!("{Layer} {layer.layer_name} ;"))?;
        self.indent += 1;

        for shape in layer.shapes.iter() {
            self.write_via_shape(shape)?;
        }
        self.indent -= 1;
        Ok(()) // No END token.
    }
    /// Writes a [`LefViaShape`].
    fn write_via_shape(&mut self, shape: &LefViaShape) -> LefResult<()> {
        use LefKey::{Mask, Polygon, Rect};
        match shape {
            LefViaShape::Rect(mask, p0, p1) => {
                let mut line = format!("{Rect} ");
                if let Some(mask) = mask {
                    line.push_str(&format!("{Mask} {mask} "));
                }
                self.write_line(format_args_f!("{line}{p0} {p1} ; "))?;
            }
            LefViaShape::Polygon(mask, pts) => {
                let mut line = format!("{Polygon} ");
                if let Some(mask) = mask {
                    line.push_str(&format!("{Mask} {mask} "));
                }
                let ptstr = pts
                    .iter()
                    .map(|x| x.to_string())
                    .collect::<Vec<String>>()
                    .join(" ");
                self.write_line(format_args_f!("{line}{ptstr} ;"))?;
            }
        };
        Ok(())
    }
    fn write_geom(&mut self, geom: &LefGeometry) -> LefResult<()> {
        let mut wordlist: Vec<String> = Vec::new();
        match geom {
            LefGeometry::Iterate { shape, pattern } => {
                wordlist.extend(self.format_geom(shape, Some(pattern)))
            }
            LefGeometry::Shape(ref shape) => wordlist.extend(self.format_geom(shape, None)),
        };
        let linestr = wordlist.join(" ");
        self.write_line(format_args_f!("{linestr} ;"))?;
        Ok(())
    }

    fn format_geom(
        &mut self,
        shape: &LefShape,
        step_pattern: Option<&LefStepPattern>,
    ) -> Vec<String> {
        let mut wordlist: Vec<String> = Vec::new();
        match shape {
            LefShape::Rect(mask, p0, p1) => {
                wordlist.push(LefKey::Rect.to_string());
                wordlist.extend(self.format_mask(mask));
                if step_pattern != None {
                    wordlist.push(LefKey::Iterate.to_string());
                }
                wordlist.push(p0.to_string());
                wordlist.push(p1.to_string());
            }
            LefShape::Polygon(mask, pts) => {
                wordlist.push(LefKey::Polygon.to_string());
                wordlist.extend(self.format_mask(mask));
                if step_pattern != None {
                    wordlist.push(LefKey::Iterate.to_string());
                }
                wordlist.extend(pts.iter().map(|x| x.to_string()));
            }
            LefShape::Path(mask, pts) => {
                wordlist.push(LefKey::Path.to_string());
                wordlist.extend(self.format_mask(mask));
                if step_pattern != None {
                    wordlist.push(LefKey::Iterate.to_string());
                }
                wordlist.extend(pts.iter().map(|x| x.to_string()));
            }
        }
        match step_pattern {
            Some(pattern) => {
                wordlist.push(LefKey::Do.to_string());
                wordlist.push(pattern.numx.to_string());
                wordlist.push(LefKey::By.to_string());
                wordlist.push(pattern.numy.to_string());
                wordlist.push(LefKey::Step.to_string());
                wordlist.push(pattern.spacex.to_string());
                wordlist.push(pattern.spacey.to_string());
            }
            _ => {}
        }

        wordlist
    }
    /// Format mask
    fn format_mask(&mut self, mask: &Option<LefMask>) -> Vec<String> {
        match mask {
            Some(mask) => vec![LefKey::Mask.to_string(), mask.to_string()],
            None => <Vec<String>>::new(),
        }
    }

    /// Write a vector of [LefSymmetry] to the SYMMETRY statement
    fn write_symmetries(&mut self, symms: &Vec<LefSymmetry>) -> LefResult<()> {
        use LefKey::Symmetry;
        let symmstr = symms
            .iter()
            .map(|x| x.to_string())
            .collect::<Vec<String>>()
            .join(" ");
        self.write_line(format_args_f!("{Symmetry} {symmstr} ;"))?;
        Ok(())
    }

    /// Write the DENSITY construct which includes LAYER and density RECT statements.
    fn write_density(&mut self, dens_geoms: &Vec<LefDensityGeometries>) -> LefResult<()> {
        use LefKey::{Density, End, Layer, Rect};
        self.write_line(format_args_f!("{Density} "))?;
        self.indent += 1;
        for layer_geom_set in dens_geoms.iter() {
            self.write_line(format_args_f!("{Layer} {layer_geom_set.layer_name} ; "))?;
            for dens_rect in layer_geom_set.geometries.iter() {
                self.write_line(format_args_f!(
                    "{Rect} {dens_rect.pt1} {dens_rect.pt2} {dens_rect.density_value} ; "
                ))?;
            }
        }
        self.indent -= 1;
        self.write_line(format_args_f!("{End} "))?;
        Ok(())
    }

    /// Write the [LefMacroClass] enumerations.
    /// Note most sub-types use their macro-generated [Display] implementations.
    fn write_macro_class(&mut self, class: &LefMacroClass) -> LefResult<()> {
        use LefKey::Class;
        use LefMacroClassName::{Block, Core, Cover, EndCap, Pad, Ring};
        match class {
            LefMacroClass::Cover { bump: ref b } => {
                let tp = if *b { LefKey::Bump.to_str() } else { "" };
                self.write_line(format_args_f!("{Class} {Cover} {tp} ;"))?;
            }
            LefMacroClass::Ring => self.write_line(format_args_f!("{Class} {Ring} ;"))?,
            LefMacroClass::Block { tp: ref t } => {
                self.write_line(format_args_f!("{Class} {Block} {} ;", display_option(t)))?;
            }
            LefMacroClass::Pad { tp: ref t } => {
                self.write_line(format_args_f!("{Class} {Pad} {} ;", display_option(t)))?;
            }
            LefMacroClass::Core { tp: ref t } => {
                self.write_line(format_args_f!("{Class} {Core} {} ;", display_option(t)))?;
            }
            LefMacroClass::EndCap { tp: ref t } => {
                self.write_line(format_args_f!("{Class} {EndCap} {t} ;"))?;
            }
        };
        Ok(())
    }
    /// Helper function writing a single line at the current indentation level.
    /// Note while the newline character is added, any trailing semicolons are not.
    fn write_line(&mut self, args: std::fmt::Arguments) -> std::io::Result<()> {
        writeln!(self.dest, "{}{}", self.indent.state, args)
    }
    /*
    /// Failure Function
    /// Wraps error-message `msg` in a [LefError::Str].
    fn fail(&mut self, msg: &str) -> LefResult<()> {
        Err(LefError::Str(msg.to_string()))
    }
    */
}
/// Helper function to call `T`'s [Display] method if `opt` is Some, or return an empty string if `opt` is None.
fn display_option<T: std::fmt::Display>(opt: &Option<T>) -> String {
    if let Some(s) = opt {
        s.to_string()
    } else {
        String::new()
    }
}

/// State kept over the course of a write
struct LefWriterSession {
    lef_version: LefDecimal,
}
impl Default for LefWriterSession {
    /// Default version is v5.8
    fn default() -> Self {
        Self {
            lef_version: V5P8.clone(),
        }
    }
}

/// Indentation Helper
struct Indent {
    unit: String,
    level: usize,
    state: String,
}
impl Indent {
    /// Create a new [Indent], initially at level 0
    fn new(unit: impl Into<String>) -> Self {
        Self {
            unit: unit.into(),
            level: 0,
            state: String::new(),
        }
    }
}
impl AddAssign<usize> for Indent {
    fn add_assign(&mut self, rhs: usize) {
        self.level += rhs;
        self.state = self.unit.repeat(self.level);
    }
}
impl SubAssign<usize> for Indent {
    fn sub_assign(&mut self, rhs: usize) {
        if rhs > self.level {
            panic!("Indentation cannot go below 0");
        }
        self.level -= rhs;
        self.state = self.unit.repeat(self.level);
    }
}
