//!
//! # Lef Writer Module
//!

// Standard Lib Imports
use std::io::Write;
use std::ops::{AddAssign, SubAssign};
use std::path::Path;

// Layout21 Imports
use layout21utils as utils;
pub use utils::{EnumStr, SerdeFile, SerializationFormat};

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
            BusBitChars, DividerChar, End, Library, NamesCaseSensitive, NoWireExtensionAtPin,
            Units, Version,
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
            self.write_line(format_args_f!("{Units} "))?;
            self.indent += 1;
            if let Some(ref db) = v.database_microns {
                self.write_line(format_args_f!("DATABASE MICRONS {} ; ", db.0))?;
            }
            // Other {Units} would be written here
            // if v.time_ns.is_some()
            //     || v.capacitance_pf.is_some()
            //     || v.resistance_ohms.is_some()
            //     || v.power_mw.is_some()
            //     || v.current_ma.is_some()
            //     || v.voltage_volts.is_some()
            //     || v.frequency_mhz.is_some()
            // { }
            self.indent -= 1;
            self.write_line(format_args_f!("{End} {Units} "))?;
        }

        // VIAS would be written here
        // if let Some(ref v) = lib.vias { }

        // Write each site definition
        for site in lib.sites.iter() {
            self.write_site(site)?;
        }
        // Write each macro definition
        for mac in lib.macros.iter() {
            self.write_macro(mac)?;
        }

        // EXTENSIONS would be written here
        // if let Some(ref v) = lib.extensions { }

        self.write_line(format_args_f!("{End} {Library} \n"))?;
        self.dest.flush()?;
        Ok(())
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
    /// Write a [LefMacro], in recommended order of fields.
    fn write_macro(&mut self, mac: &LefMacro) -> LefResult<()> {
        use LefKey::{By, End, Foreign, Macro, Obs, Origin, Site, Size, Source};
        self.write_line(format_args_f!("{Macro} {mac.name} ; "))?;
        self.indent += 1;

        if let Some(ref v) = mac.class {
            self.write_macro_class(v)?;
        }
        // FIXEDMASK would be written here
        // if mac.fixed_mask.is_some() { }
        if let Some(ref v) = mac.foreign {
            let pt = match v.pt {
                Some(ref p) => p.to_string(),
                None => "".into(),
            };
            self.write_line(format_args_f!("{Foreign} {v.cell_name} {pt} ;"))?;
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
        // EEQ would be written here
        // if mac.eeq.is_some() { }
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

        // DENSTITY and PROPERTIES would go here
        // if mac.density.is_some() { }
        // if mac.properties.is_some() { }

        self.indent -= 1;
        self.write_line(format_args_f!("{End} {} ", mac.name))?;
        Ok(())
    }
    /// Write a [LefPin] definition
    fn write_pin(&mut self, pin: &LefPin) -> LefResult<()> {
        use LefKey::{AntennaModel, Direction, End, Layer, Pin, Shape, Use};
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

        // Most unsupported PINS features *would* go here.
        // if pin.taper_rule.is_some()
        //     || pin.net_expr.is_some()
        //     || pin.supply_sensitivity.is_some()
        //     || pin.ground_sensitivity.is_some()
        //     || pin.must_join.is_some()
        //     || pin.properties.is_some()
        // {
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
    fn write_geom(&mut self, geom: &LefGeometry) -> LefResult<()> {
        use LefKey::{Polygon, Rect};
        match geom {
            LefGeometry::Iterate { .. } => unimplemented!(),
            LefGeometry::Shape(ref shape) => match shape {
                LefShape::Rect(mask, p0, p1) => {
                    let mut line = format!("{Rect} ");
                    match mask {
                        Some(mask) => line.push_str(&format!("MASK {mask} ")),
                        None => (),
                    };
                    self.write_line(format_args_f!("{line}{p0} {p1} ; "))?;
                }
                LefShape::Polygon(pts) => {
                    let ptstr = pts
                        .iter()
                        .map(|x| x.to_string())
                        .collect::<Vec<String>>()
                        .join(" ");
                    self.write_line(format_args_f!("{Polygon} {ptstr} ;"))?;
                }
                LefShape::Path(_) => {
                    self.fail(&format_f!("Unsupported Write: LefShape::Path"))?;
                }
            },
        };
        Ok(())
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
    /// Failure Function
    /// Wraps error-message `msg` in a [LefError::Str].
    fn fail(&mut self, msg: &str) -> LefResult<()> {
        Err(LefError::Str(msg.to_string()))
    }
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
