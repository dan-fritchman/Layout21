//!
//! # Gds21 Byte-Encoding and Writing
//!

// Local imports
use super::*;

/// Gds Writing Helper
pub struct GdsWriter<'wr> {
    /// Write Destination
    dest: Box<dyn Write + 'wr>,
}
impl<'wr> GdsWriter<'wr> {
    /// Create new [GdsWriter] with destination file `fname`
    pub fn open(fname: &str) -> GdsResult<Self> {
        let file = BufWriter::new(File::create(fname)?);
        Ok(Self::new(file))
    }
    /// Create a new [GdsWriter] to destination `dest`
    pub fn new(dest: impl Write + 'wr) -> Self {
        Self {
            dest: Box::new(dest),
        }
    }
    /// Write a [GdsLibrary] to the destination
    /// Fields are written in the Gds-recommended order
    pub fn write_lib(&mut self, lib: &GdsLibrary) -> GdsResult<()> {
        // Check our in-memory self doesn't include any unsupported features
        if lib.libdirsize.is_some()
            || lib.srfname.is_some()
            || lib.libsecur.is_some()
            || lib.reflibs.is_some()
            || lib.fonts.is_some()
            || lib.attrtable.is_some()
            || lib.generations.is_some()
            || lib.format_type.is_some()
        {
            return Err(GdsError::Unsupported(None, Some(GdsContext::Library)));
        }
        // Write our header content
        GdsRecord::Header {
            version: lib.version,
        }
        .encode(self)?;
        // Write our modification-date info
        GdsRecord::BgnLib {
            dates: lib.dates.encode(),
        }
        .encode(self)?;
        // Write our library name
        GdsRecord::LibName(lib.name.clone()).encode(self)?;
        // Write our units
        GdsRecord::Units(lib.units.0, lib.units.1).encode(self)?;
        // Write all of our Structs/Cells
        for strukt in lib.structs.iter() {
            self.write_struct(strukt)?;
        }
        // And finally, the library terminator
        GdsRecord::EndLib.encode(self)?;
        Ok(())
    }
    /// Write [GdsStruct] `strukt` to the destination
    pub fn write_struct(&mut self, strukt: &GdsStruct) -> GdsResult<()> {
        // Write the header content
        self.write_records(&[
            &GdsRecord::BgnStruct {
                dates: strukt.dates.encode(),
            },
            &GdsRecord::StructName(strukt.name.clone()),
        ])?;
        // Write each of our elements
        for elem in strukt.elems.iter() {
            // FIXME: do the work of `to_records` here, rather than creating all these vectors
            for record in elem.to_records().iter() {
                record.encode(self)?;
            }
        }
        // And its terminator
        self.write_record(&GdsRecord::EndStruct)?;
        Ok(())
    }
    /// Helper to write a sequence of [GdsRecord] references
    fn write_records(&mut self, records: &[&GdsRecord]) -> GdsResult<()> {
        for r in records {
            self.write_record(r)?;
        }
        Ok(())
    }
    /// Encode into bytes and write onto `dest`
    /// FIXME: make private (after refactor)
    pub fn write_record(&mut self, record: &GdsRecord) -> GdsResult<()> {
        // This is split in two parts - header and data -
        // largely to ease handling the variety of datatypes

        // A quick closure for GDS's "even-lengths-only allowed" strings
        let gds_strlen = |s: &str| -> usize { s.len() + s.len() % 2 };
        // First grab the header info: RecordType, DataType, and length
        use GdsDataType::{BitArray, NoData, Str, F64, I16, I32};
        let (rtype, dtype, len) = match record {
            // Library-Level Records
            GdsRecord::Header { .. } => (GdsRecordType::Header, I16, 2),
            GdsRecord::BgnLib { .. } => (GdsRecordType::BgnLib, I16, 24),
            GdsRecord::LibName(s) => (GdsRecordType::LibName, Str, gds_strlen(s)),
            GdsRecord::Units(_, _) => (GdsRecordType::Units, F64, 16),
            GdsRecord::EndLib => (GdsRecordType::EndLib, NoData, 0),

            // Structure (Cell) Level Records
            GdsRecord::BgnStruct { .. } => (GdsRecordType::BgnStruct, I16, 24),
            GdsRecord::StructName(s) => (GdsRecordType::StructName, Str, gds_strlen(s)),
            GdsRecord::StructRefName(s) => (GdsRecordType::StructRefName, Str, gds_strlen(s)),
            GdsRecord::EndStruct => (GdsRecordType::EndStruct, NoData, 0),

            // Element-Level Records
            GdsRecord::Boundary => (GdsRecordType::Boundary, NoData, 0),
            GdsRecord::Path => (GdsRecordType::Path, NoData, 0),
            GdsRecord::StructRef => (GdsRecordType::StructRef, NoData, 0),
            GdsRecord::ArrayRef => (GdsRecordType::ArrayRef, NoData, 0),
            GdsRecord::Text => (GdsRecordType::Text, NoData, 0),
            GdsRecord::Layer(_) => (GdsRecordType::Layer, I16, 2),
            GdsRecord::DataType(_) => (GdsRecordType::DataType, I16, 2),
            GdsRecord::Width(_) => (GdsRecordType::Width, I32, 4),
            GdsRecord::Xy(d) => (GdsRecordType::Xy, I32, 4 * d.len()),
            GdsRecord::EndElement => (GdsRecordType::EndElement, NoData, 0),

            // More (less well-categorized here) record-types
            GdsRecord::ColRow { .. } => (GdsRecordType::ColRow, I16, 4),
            GdsRecord::Node => (GdsRecordType::Node, NoData, 0),
            GdsRecord::TextType(_) => (GdsRecordType::TextType, I16, 2),
            GdsRecord::Presentation(_, _) => (GdsRecordType::Presentation, BitArray, 2),
            GdsRecord::String(s) => (GdsRecordType::String, Str, gds_strlen(s)),
            GdsRecord::Strans(_, _) => (GdsRecordType::Strans, BitArray, 2),
            GdsRecord::Mag(_) => (GdsRecordType::Mag, F64, 8),
            GdsRecord::Angle(_) => (GdsRecordType::Angle, F64, 8),
            GdsRecord::RefLibs(s) => (GdsRecordType::RefLibs, Str, gds_strlen(s)),
            GdsRecord::Fonts(s) => (GdsRecordType::Fonts, Str, gds_strlen(s)),
            GdsRecord::PathType(_) => (GdsRecordType::PathType, I16, 2),
            GdsRecord::Generations(_) => (GdsRecordType::Generations, I16, 2),
            GdsRecord::AttrTable(s) => (GdsRecordType::AttrTable, Str, gds_strlen(s)),
            GdsRecord::ElemFlags(_, _) => (GdsRecordType::ElemFlags, BitArray, 2),
            GdsRecord::Nodetype(_) => (GdsRecordType::Nodetype, I16, 2),
            GdsRecord::PropAttr(_) => (GdsRecordType::PropAttr, I16, 2),
            GdsRecord::PropValue(s) => (GdsRecordType::PropValue, Str, gds_strlen(s)),
            GdsRecord::Box => (GdsRecordType::Box, NoData, 0),
            GdsRecord::BoxType(_) => (GdsRecordType::BoxType, I16, 2),
            GdsRecord::Plex(_) => (GdsRecordType::Plex, I32, 4),
            GdsRecord::BeginExtn(_) => (GdsRecordType::BeginExtn, I32, 4),
            GdsRecord::EndExtn(_) => (GdsRecordType::EndExtn, I32, 4),
            GdsRecord::TapeNum(_) => (GdsRecordType::TapeNum, I16, 2),
            GdsRecord::TapeCode(_) => (GdsRecordType::TapeCode, I16, 12),
            GdsRecord::Format(_) => (GdsRecordType::Format, I16, 2),
            GdsRecord::Mask(s) => (GdsRecordType::Mask, Str, gds_strlen(s)),
            GdsRecord::EndMasks => (GdsRecordType::EndMasks, NoData, 0),
            GdsRecord::LibDirSize(_) => (GdsRecordType::LibDirSize, I16, 2),
            GdsRecord::SrfName(s) => (GdsRecordType::SrfName, Str, gds_strlen(s)),
            GdsRecord::LibSecur(_) => (GdsRecordType::LibSecur, I16, 2),
        };
        // Send those header-bytes to the writer.
        // Include the four header bytes in total-length.
        match u16::try_from(len + 4) {
            Ok(val) => self.dest.write_u16::<BigEndian>(val)?,
            Err(_) => return Err(GdsError::RecordLen(len)),
        };
        self.dest.write_u8(rtype as u8)?;
        self.dest.write_u8(dtype as u8)?;

        // Now write the data portion
        // This section is generally organized by DataType
        match record {
            // NoData
            GdsRecord::EndLib
            | GdsRecord::EndStruct
            | GdsRecord::Boundary
            | GdsRecord::Path
            | GdsRecord::StructRef
            | GdsRecord::ArrayRef
            | GdsRecord::Text
            | GdsRecord::EndElement
            | GdsRecord::Node
            | GdsRecord::Box
            | GdsRecord::EndMasks => (),

            // BitArrays
            GdsRecord::Presentation(d0, d1)
            | GdsRecord::Strans(d0, d1)
            | GdsRecord::ElemFlags(d0, d1) => {
                self.dest.write_u8(*d0)?;
                self.dest.write_u8(*d1)?;
            }
            // Single I16s
            GdsRecord::Header { version: d }
            | GdsRecord::Layer(d)
            | GdsRecord::DataType(d)
            | GdsRecord::TextType(d)
            | GdsRecord::PathType(d)
            | GdsRecord::Generations(d)
            | GdsRecord::Nodetype(d)
            | GdsRecord::PropAttr(d)
            | GdsRecord::BoxType(d)
            | GdsRecord::TapeNum(d)
            | GdsRecord::Format(d)
            | GdsRecord::LibDirSize(d)
            | GdsRecord::LibSecur(d) => self.dest.write_i16::<BigEndian>(*d)?,

            // Single I32s
            GdsRecord::Width(d)
            | GdsRecord::Plex(d)
            | GdsRecord::BeginExtn(d)
            | GdsRecord::EndExtn(d) => self.dest.write_i32::<BigEndian>(*d)?,
            // Single F64s
            GdsRecord::Mag(d) | GdsRecord::Angle(d) => {
                self.dest.write_u64::<BigEndian>(GdsFloat64::encode(*d))?
            }
            // "Structs"
            GdsRecord::Units(d0, d1) => {
                self.dest.write_u64::<BigEndian>(GdsFloat64::encode(*d0))?;
                self.dest.write_u64::<BigEndian>(GdsFloat64::encode(*d1))?;
            }
            GdsRecord::ColRow { cols, rows } => {
                self.dest.write_i16::<BigEndian>(*cols)?;
                self.dest.write_i16::<BigEndian>(*rows)?;
            }
            // Vectors
            GdsRecord::TapeCode(d)
            | GdsRecord::BgnLib { dates: d }
            | GdsRecord::BgnStruct { dates: d } => {
                for val in d.iter() {
                    self.dest.write_i16::<BigEndian>(*val)?;
                }
            }
            GdsRecord::Xy(d) => {
                for val in d.iter() {
                    self.dest.write_i32::<BigEndian>(*val)?;
                }
            }
            // Strings
            GdsRecord::LibName(s)
            | GdsRecord::StructName(s)
            | GdsRecord::StructRefName(s)
            | GdsRecord::String(s)
            | GdsRecord::RefLibs(s)
            | GdsRecord::Fonts(s)
            | GdsRecord::AttrTable(s)
            | GdsRecord::PropValue(s)
            | GdsRecord::Mask(s)
            | GdsRecord::SrfName(s) => {
                for b in s.as_bytes() {
                    self.dest.write_u8(*b)?;
                }
                if s.len() % 2 != 0 {
                    // Pad odd-length strings with a zero-valued byte
                    self.dest.write_u8(0x00)?;
                }
            }
        };
        Ok(())
    }
}

impl ToRecords for GdsStrans {
    /// Convert to a Vector of [GdsRecord], ordered as dictated by the GDSII spec BNF.
    fn to_records(&self) -> Vec<GdsRecord> {
        let mut records = vec![GdsRecord::Strans(
            (self.reflected as u8) << 7,
            (self.abs_mag as u8) << 2 | (self.abs_angle as u8) << 1,
        )];
        if let Some(ref e) = self.mag {
            records.push(GdsRecord::Mag(*e));
        }
        if let Some(ref e) = self.angle {
            records.push(GdsRecord::Angle(*e));
        }
        records
    }
}

impl ToRecords for GdsPath {
    /// Convert to a Vector of [GdsRecord], ordered as dictated by the GDSII spec BNF.
    fn to_records(&self) -> Vec<GdsRecord> {
        let mut records = vec![GdsRecord::Path];
        if let Some(ref e) = self.elflags {
            records.push(GdsRecord::ElemFlags(e.0, e.1));
        }
        if let Some(ref e) = self.plex {
            records.push(GdsRecord::Plex(e.0));
        }
        records.push(GdsRecord::Layer(self.layer));
        records.push(GdsRecord::DataType(self.datatype));
        if let Some(ref e) = self.path_type {
            records.push(GdsRecord::PathType(*e));
        }
        if let Some(ref e) = self.width {
            records.push(GdsRecord::Width(*e));
        }
        if let Some(ref e) = self.begin_extn {
            records.push(GdsRecord::BeginExtn(*e));
        }
        if let Some(ref e) = self.end_extn {
            records.push(GdsRecord::EndExtn(*e));
        }
        records.push(GdsRecord::Xy(GdsPoint::flatten_vec(&self.xy)));
        for prop in self.properties.iter() {
            records.push(GdsRecord::PropAttr(prop.attr));
            records.push(GdsRecord::PropValue(prop.value.clone()));
        }
        records.push(GdsRecord::EndElement);
        records
    }
}

impl ToRecords for GdsBoundary {
    /// Convert to a Vector of [GdsRecord], ordered as dictated by the GDSII spec BNF.
    fn to_records(&self) -> Vec<GdsRecord> {
        let mut records = vec![GdsRecord::Boundary];
        if let Some(ref e) = self.elflags {
            records.push(GdsRecord::ElemFlags(e.0, e.1));
        }
        if let Some(ref e) = self.plex {
            records.push(GdsRecord::Plex(e.0));
        }
        records.push(GdsRecord::Layer(self.layer));
        records.push(GdsRecord::DataType(self.datatype));
        records.push(GdsRecord::Xy(GdsPoint::flatten_vec(&self.xy)));
        for prop in self.properties.iter() {
            records.push(GdsRecord::PropAttr(prop.attr));
            records.push(GdsRecord::PropValue(prop.value.clone()));
        }
        records.push(GdsRecord::EndElement);
        records
    }
}

impl ToRecords for GdsStructRef {
    /// Convert to a Vector of [GdsRecord], ordered as dictated by the GDSII spec BNF.
    fn to_records(&self) -> Vec<GdsRecord> {
        let mut records = vec![GdsRecord::StructRef];
        if let Some(ref e) = self.elflags {
            records.push(GdsRecord::ElemFlags(e.0, e.1));
        }
        if let Some(ref e) = self.plex {
            records.push(GdsRecord::Plex(e.0));
        }
        records.push(GdsRecord::StructRefName(self.name.clone()));
        if let Some(ref e) = self.strans {
            let rs = e.to_records();
            records.extend(rs);
        }
        records.push(GdsRecord::Xy(GdsPoint::flatten(&self.xy)));
        for prop in self.properties.iter() {
            records.push(GdsRecord::PropAttr(prop.attr));
            records.push(GdsRecord::PropValue(prop.value.clone()));
        }
        records.push(GdsRecord::EndElement);
        records
    }
}

impl ToRecords for GdsArrayRef {
    /// Convert to a Vector of [GdsRecord], ordered as dictated by the GDSII spec BNF.
    fn to_records(&self) -> Vec<GdsRecord> {
        let mut records = vec![GdsRecord::ArrayRef];
        if let Some(ref e) = self.elflags {
            records.push(GdsRecord::ElemFlags(e.0, e.1));
        }
        if let Some(ref e) = self.plex {
            records.push(GdsRecord::Plex(e.0));
        }
        records.push(GdsRecord::StructRefName(self.name.clone()));
        if let Some(ref e) = self.strans {
            let rs = e.to_records();
            records.extend(rs);
        }
        records.push(GdsRecord::ColRow {
            cols: self.cols,
            rows: self.rows,
        });
        let mut xy = GdsPoint::flatten(&self.xy[0]);
        xy.extend(GdsPoint::flatten(&self.xy[1]));
        xy.extend(GdsPoint::flatten(&self.xy[2]));
        records.push(GdsRecord::Xy(xy));
        for prop in self.properties.iter() {
            records.push(GdsRecord::PropAttr(prop.attr));
            records.push(GdsRecord::PropValue(prop.value.clone()));
        }
        records.push(GdsRecord::EndElement);
        records
    }
}

impl ToRecords for GdsTextElem {
    /// Convert to a Vector of [GdsRecord], ordered as dictated by the GDSII spec BNF.
    fn to_records(&self) -> Vec<GdsRecord> {
        let mut records = vec![GdsRecord::Text];
        if let Some(ref e) = self.elflags {
            records.push(GdsRecord::ElemFlags(e.0, e.1));
        }
        if let Some(ref e) = self.plex {
            records.push(GdsRecord::Plex(e.0));
        }
        records.push(GdsRecord::Layer(self.layer));
        records.push(GdsRecord::TextType(self.texttype));
        if let Some(ref e) = self.presentation {
            records.push(GdsRecord::Presentation(e.0, e.1));
        }
        if let Some(ref e) = self.path_type {
            records.push(GdsRecord::PathType(*e));
        }
        if let Some(ref e) = self.width {
            records.push(GdsRecord::Width(*e));
        }
        if let Some(ref e) = self.strans {
            let rs = e.to_records();
            records.extend(rs);
        }
        records.push(GdsRecord::Xy(GdsPoint::flatten(&self.xy)));
        records.push(GdsRecord::String(self.string.clone()));
        for prop in self.properties.iter() {
            records.push(GdsRecord::PropAttr(prop.attr));
            records.push(GdsRecord::PropValue(prop.value.clone()));
        }
        records.push(GdsRecord::EndElement);
        records
    }
}

impl ToRecords for GdsNode {
    /// Convert to a Vector of [GdsRecord], ordered as dictated by the GDSII spec BNF.
    fn to_records(&self) -> Vec<GdsRecord> {
        let mut records = vec![GdsRecord::Node];
        if let Some(ref e) = self.elflags {
            records.push(GdsRecord::ElemFlags(e.0, e.1));
        }
        if let Some(ref e) = self.plex {
            records.push(GdsRecord::Plex(e.0));
        }
        records.push(GdsRecord::Layer(self.layer));
        records.push(GdsRecord::Nodetype(self.nodetype));
        records.push(GdsRecord::Xy(GdsPoint::flatten_vec(&self.xy)));
        for prop in self.properties.iter() {
            records.push(GdsRecord::PropAttr(prop.attr));
            records.push(GdsRecord::PropValue(prop.value.clone()));
        }
        records.push(GdsRecord::EndElement);
        records
    }
}

impl ToRecords for GdsBox {
    /// Convert to a Vector of [GdsRecord], ordered as dictated by the GDSII spec BNF.
    fn to_records(&self) -> Vec<GdsRecord> {
        let mut records = vec![GdsRecord::Box];
        if let Some(ref e) = self.elflags {
            records.push(GdsRecord::ElemFlags(e.0, e.1));
        }
        if let Some(ref e) = self.plex {
            records.push(GdsRecord::Plex(e.0));
        }
        records.push(GdsRecord::Layer(self.layer));
        records.push(GdsRecord::BoxType(self.boxtype));
        records.push(GdsRecord::Xy(GdsPoint::flatten_vec(&self.xy.to_vec())));
        for prop in self.properties.iter() {
            records.push(GdsRecord::PropAttr(prop.attr));
            records.push(GdsRecord::PropValue(prop.value.clone()));
        }
        records.push(GdsRecord::EndElement);
        records
    }
}

impl GdsDateTimes {
    /// Encode in GDSII's vector of i16's format
    pub fn encode(&self) -> Vec<i16> {
        vec![
            self.modified.date().year() as i16,
            self.modified.date().month() as i16,
            self.modified.date().day() as i16,
            self.modified.time().hour() as i16,
            self.modified.time().minute() as i16,
            self.modified.time().second() as i16,
            self.accessed.date().year() as i16,
            self.accessed.date().month() as i16,
            self.accessed.date().day() as i16,
            self.accessed.time().hour() as i16,
            self.accessed.time().minute() as i16,
            self.accessed.time().second() as i16,
        ]
    }
}
