//!
//! # Lef Writer Module
//!

use super::*;

/// Write a [LefLibrary] to file `fname`
/// Fields are written in the LEF-recommended order
pub fn save(lib: &LefLibrary, fname: &str) -> LefResult<()> {
    let f = std::fs::File::create(fname)?;
    write::LefWriter::new(f).write_lib(lib)
}

/// # Lef Writing Helper
pub struct LefWriter<'wr> {
    /// Write Destination
    dest: Box<dyn Write + 'wr>,
    /// Indentation Level
    indent_level: usize,
    /// Indentation String. Usually a series of spaces or tabs.
    indent_str: String,
}
impl<'wr> LefWriter<'wr> {
    fn new(dest: impl Write + 'wr) -> Self {
        Self {
            dest: Box::new(dest),
            indent_level: 0,
            indent_str: "    ".into(),
        }
    }
    /// Write a [LefLibrary] to the destination
    /// Fields are written in the LEF-recommended order
    fn write_lib(&mut self, lib: &LefLibrary) -> LefResult<()> {
        if let Some(ref v) = lib.version {
            self.write_line(format_args!("VERSION {} ; ", v))?;
        }
        if let Some(ref v) = lib.bus_bit_chars {
            self.write_line(format_args!("BUSBITCHARS \"{}{}\" ; ", v.0, v.1))?;
        }
        if let Some(ref v) = lib.divider_char {
            self.write_line(format_args!("DIVIDERCHAR \"{}\" ; ", v))?;
        }
        if let Some(ref v) = lib.units {
            self.write_line(format_args!("UNITS "))?;
            self.indent_level += 1;
            if let Some(ref db) = v.database_microns {
                self.write_line(format_args!("DATABASE MICRONS {} ; ", db.0))?;
            }
            if v.time_ns.is_some()
                || v.capacitance_pf.is_some()
                || v.resistance_ohms.is_some()
                || v.power_mw.is_some()
                || v.current_ma.is_some()
                || v.voltage_volts.is_some()
                || v.frequency_mhz.is_some()
            {
                return Err(LefError::Str("Unsupported UNITS field".into()));
            }
            self.indent_level -= 1;
            self.write_line(format_args!("END UNITS "))?;
        }
        if let Some(ref _v) = lib.vias {
            return Err(LefError::Str(
                "Unsupported Attribute: LefLibrary.vias".into(),
            ));
        }
        // Write each site definition
        for site in lib.sites.iter() {
            self.write_site(site)?;
        }
        // Write each macro definition
        for mac in lib.macros.iter() {
            self.write_macro(mac)?;
        }
        if let Some(ref _v) = lib.extensions {
            return Err(LefError::Str(
                "Unsupported Attribute: LefLibrary.extensions".into(),
            ));
        }
        self.write_line(format_args!("END LIBRARY \n"))?;
        self.dest.flush()?;
        Ok(())
    }
    fn write_site(&mut self, site: &LefSite) -> LefResult<()> {
        self.write_line(format_args!("SITE {} ; ", site.name))?;
        self.indent_level += 1;
        self.write_line(format_args!("CLASS {};", site.class))?;
        if let Some(ref v) = site.symmetry {
            self.write_symmetries(v)?;
        }
        if site.row_pattern.is_some() {
            return Err(LefError::Str(
                "Unsupported Attribute: LefSite.row_pattern".into(),
            ));
        }
        self.write_line(format_args!("SIZE {} BY {} ;", site.size.0, site.size.1))?;
        self.indent_level -= 1;
        self.write_line(format_args!("END {} ; ", site.name))?;
        Ok(())
    }
    fn write_macro(&mut self, mac: &LefMacro) -> LefResult<()> {
        self.write_line(format_args!("MACRO {} ; ", mac.name))?;
        self.indent_level += 1;

        if let Some(ref v) = mac.class {
            self.write_macro_class(v)?;
        }
        if mac.fixed_mask.is_some() {
            return Err(LefError::Str(
                "Unsupported Attribute: LefMacro.fixed_mask".into(),
            ));
        }
        if let Some(ref v) = mac.foreign {
            let pt = match v.pt {
                Some(ref p) => p.to_string(),
                None => "".into(),
            };
            self.write_line(format_args!("FOREIGN {} {} ;", v.cell_name, pt))?;
        }
        if let Some(ref v) = mac.origin {
            self.write_line(format_args!("ORIGIN {} ;", v))?;
        }
        if let Some(ref v) = mac.source {
            // FIXME: only supported in some LEF versions
            self.write_line(format_args!("SOURCE {} ;", v))?;
        }
        if mac.eeq.is_some() {
            return Err(LefError::Str("Unsupported Attribute: LefMacro.eeq".into()));
        }
        if let Some(ref v) = mac.size {
            self.write_line(format_args!("SIZE {} BY {} ;", v.0, v.1))?;
        }
        if let Some(ref v) = mac.symmetry {
            self.write_symmetries(v)?;
        }
        if let Some(ref v) = mac.site {
            self.write_line(format_args!("SITE {} ;", v))?;
        }
        for pin in mac.pins.iter() {
            self.write_pin(pin)?;
        }
        if !mac.obs.is_empty() {
            self.write_line(format_args!("OBS "))?;
            self.indent_level += 1;
            for layer in mac.obs.iter() {
                self.write_layer_geom(layer)?;
            }
            self.indent_level -= 1;
            self.write_line(format_args!("END "))?;
        }
        if mac.density.is_some() {
            return Err(LefError::Str(
                "Unsupported Attribute: LefMacro.density".into(),
            ));
        }
        if mac.properties.is_some() {
            return Err(LefError::Str(
                "Unsupported Attribute: LefMacro.properties".into(),
            ));
        }
        self.indent_level -= 1;
        self.write_line(format_args!("END {} ", mac.name))?;
        Ok(())
    }
    /// Write a [LefPin] definition
    fn write_pin(&mut self, pin: &LefPin) -> LefResult<()> {
        self.write_line(format_args!("PIN {} ", pin.name))?;
        self.indent_level += 1;
        if let Some(ref v) = pin.direction {
            self.write_line(format_args!("DIRECTION {} ; ", v))?;
        }
        if let Some(ref v) = pin.use_ {
            self.write_line(format_args!("USE {} ; ", v))?;
        }
        if let Some(ref v) = pin.shape {
            self.write_line(format_args!("SHAPE {} ; ", v))?;
        }
        if let Some(ref v) = pin.antenna_model {
            self.write_line(format_args!("ANTENNAMODEL {} ; ", v))?;
        }
        for attr in pin.antenna_attrs.iter() {
            let layer = if let Some(ref lname) = attr.layer {
                format!("LAYER {}", lname)
            } else {
                String::new()
            };
            self.write_line(format_args!("{} {} {} ;", attr.key, attr.val, layer))?;
        }
        if pin.taper_rule.is_some()
            || pin.net_expr.is_some()
            || pin.supply_sensitivity.is_some()
            || pin.ground_sensitivity.is_some()
            || pin.must_join.is_some()
            || pin.properties.is_some()
        {
            return Err(LefError::Str("Unsupported LefPin Attr".into()));
        }
        for port in pin.ports.iter() {
            self.write_port(port)?;
        }
        self.indent_level -= 1;
        self.write_line(format_args!("END {} ", pin.name))?;
        Ok(())
    }
    /// Write a [LefPort] definition
    fn write_port(&mut self, port: &LefPort) -> LefResult<()> {
        self.write_line(format_args!("PORT "))?;
        self.indent_level += 1;
        if let Some(ref v) = port.class {
            self.write_line(format_args!("CLASS {} ; ", v))?;
        }
        for layer in port.layers.iter() {
            self.write_layer_geom(layer)?;
        }
        self.indent_level -= 1;
        self.write_line(format_args!("END "))?;
        Ok(())
    }
    /// Write the [LefLayerGeometries], common to both ports and obstructions
    fn write_layer_geom(&mut self, layer: &LefLayerGeometries) -> LefResult<()> {
        // The "LAYER" statement has a few inline options. Extract them here.
        let pg = match layer.except_pg_net {
            Some(ref pg) if *pg => "EXCEPTPGNET ",
            Some(_) | None => "",
        };
        let sps = match layer.spacing {
            Some(LefLayerSpacing::DesignRuleWidth(ref s)) => format!("DESIGNRULEWIDTH {} ", s),
            Some(LefLayerSpacing::Spacing(ref s)) => format!("SPACING {} ", s),
            None => "".into(),
        };
        self.write_line(format_args!("LAYER {} {}{};", layer.layer_name, pg, sps))?;
        self.indent_level += 1;

        if let Some(ref v) = layer.width {
            self.write_line(format_args!("WIDTH {} ; ", v))?;
        }
        for geom in layer.geometries.iter() {
            self.write_geom(geom)?;
        }
        for via in layer.vias.iter() {
            self.write_line(format_args!("VIA {} {} ;", via.pt, via.via_name,))?;
        }
        self.indent_level -= 1;
        Ok(()) // Note [LefLayerGeometries] have no "END" or other closing delimeter.
    }
    fn write_geom(&mut self, geom: &LefGeometry) -> LefResult<()> {
        match geom {
            LefGeometry::Iterate { .. } => unimplemented!(),
            LefGeometry::Shape(ref shape) => match shape {
                LefShape::Rect(p0, p1) => {
                    self.write_line(format_args!("RECT {} {} ; ", p0, p1))?;
                }
                LefShape::Polygon(pts) => {
                    let ptstr = pts
                        .iter()
                        .map(|x| x.to_string())
                        .collect::<Vec<String>>()
                        .join(" ");
                    self.write_line(format_args!("POLYGON {} ;", ptstr))?;
                }
                LefShape::Path(_) => {
                    unimplemented!();
                }
            },
        };
        Ok(())
    }
    /// Write a vector of [LefSymmetry] to the SYMMETRY statement
    fn write_symmetries(&mut self, symms: &Vec<LefSymmetry>) -> LefResult<()> {
        let symmstr = symms
            .iter()
            .map(|x| x.to_string())
            .collect::<Vec<String>>()
            .join(" ");
        self.write_line(format_args!("SYMMETRY {} ;", symmstr))?;
        Ok(())
    }
    /// Write the [LefMacroClass] enumerations.
    /// Note most sub-types use their macro-generated [Display] implementations.
    fn write_macro_class(&mut self, class: &LefMacroClass) -> LefResult<()> {
        match class {
            LefMacroClass::Cover { bump: ref b } => {
                let tp = if *b { "BUMP" } else { "" };
                self.write_line(format_args!("CLASS COVER {} ;", tp))?;
            }
            LefMacroClass::Ring => self.write_line(format_args!("CLASS RING ;"))?,
            LefMacroClass::Block { tp: ref t } => {
                self.write_line(format_args!("CLASS BLOCK {} ;", display_option(t)))?;
            }
            LefMacroClass::Pad { tp: ref t } => {
                self.write_line(format_args!("CLASS PAD {} ;", display_option(t)))?;
            }
            LefMacroClass::Core { tp: ref t } => {
                self.write_line(format_args!("CLASS CORE {} ;", display_option(t)))?;
            }
            LefMacroClass::EndCap { tp: ref t } => {
                self.write_line(format_args!("CLASS ENDCAP {} ;", t))?;
            }
        };
        Ok(())
    }
    /// Helper function writing a single line at the current indentation level.
    /// Note while the newline character is added, any trailing semicolons are not.
    fn write_line(&mut self, args: std::fmt::Arguments) -> std::io::Result<()> {
        let indent = self.indent_str.repeat(self.indent_level);
        writeln!(self.dest, "{}{}", indent, args)
    }
}
/// Helper function to call `T`'s [Display] method if `opt` is Some,
/// or return and empty string if `opt` is None.
fn display_option<T: std::fmt::Display>(opt: &Option<T>) -> String {
    if let Some(s) = opt {
        s.to_string()
    } else {
        String::new()
    }
}
