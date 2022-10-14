//!
//! # Gds21 Byte-Encoding and Writing
//!

// Std-Lib Imports
#[allow(unused_imports)]
use std::io::prelude::*;

use std::convert::TryFrom;
use std::fs::File;
use std::io::{BufWriter, Write};
use std::path::Path;
use std::str;

// Crates.io
use byteorder::{BigEndian, WriteBytesExt};
use chrono::prelude::*;
use chrono::{Datelike, };
use serde::{Deserialize, Serialize};

// Local imports
use crate::data::*;

/// Gds Writing Helper
pub struct GdsWriter<'wr> {
    /// Write Destination
    dest: Box<dyn Write + 'wr>,
}
impl<'wr> GdsWriter<'wr> {
    /// Create new [GdsWriter] with destination file `fname`
    pub fn open(fname: impl AsRef<Path>) -> GdsResult<Self> {
        let file = BufWriter::new(File::create(fname)?);
        Ok(Self::new(file))
    }
    /// Create a new [GdsWriter] to destination `dest`
    pub fn new(dest: impl Write + 'wr) -> Self {
        Self {
            dest: Box::new(dest),
        }
    }
    /// Write [GdsLibrary] `lib` to our destination
    pub fn write_lib(&mut self, lib: &GdsLibrary) -> GdsResult<()> {
        // `write_lib` is our typicaly entry point when writing to file.
        // It quickly dispatches most behavior off to our implementation of the [Encode] trait.
        self.encode_lib(lib)
    }
    /// Helper to write a sequence of [GdsRecord] references
    fn write_records(&mut self, records: &[GdsRecord]) -> GdsResult<()> {
        for r in records {
            self.write_record(&r)?;
        }
        Ok(())
    }
    /// Encode into bytes and write onto `dest`
    fn write_record(&mut self, record: &GdsRecord) -> GdsResult<()> {
        // This is split in two parts - header and data - largely to ease handling the variety of datatypes
        self.write_record_header(record)?;
        self.write_record_content(record)?;
        Ok(())
    }
    fn write_record_header(&mut self, record: &GdsRecord) -> GdsResult<()> {
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
        Ok(())
    }
    fn write_record_content(&mut self, record: &GdsRecord) -> GdsResult<()> {
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
            // Fixed-Length Arrays
            GdsRecord::BgnLib { dates: d } | GdsRecord::BgnStruct { dates: d } => {
                for val in d.iter() {
                    self.dest.write_i16::<BigEndian>(*val)?;
                }
            }
            // Vectors
            GdsRecord::TapeCode(d) => {
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

/// [Encode] implementation for [GdsWriter]
/// Dispatches record-level calls back to the `write_record(s)` methods.
impl Encode for GdsWriter<'_> {
    fn encode_record(&mut self, record: GdsRecord) -> GdsResult<()> {
        self.write_record(&record)
    }
    fn encode_records(&mut self, records: &[GdsRecord]) -> GdsResult<()> {
        self.write_records(records)
    }
}

/// # Gds Encoding Trait
///
/// Performs conversion of each element in the [GdsLibrary] tree to [GdsRecord]s,
/// each passed to its `encode_record` (singular) or `encode_records` (plural) methods.
/// Each type is encoded in the order recommended by the GDSII spec.
///
/// Most of the behavior required of [GdsWriter] is implemented in this trait.
/// It is broken out into a trait to enable alternate "destinations" for the encoded records,
/// such as collecting them in a [Vec] as done by [GdsRecordList].
///
trait Encode {
    // Virtual / Required Methods
    /// Encode a single [GdsRecord]
    fn encode_record(&mut self, record: GdsRecord) -> GdsResult<()>;
    /// Encode an array of [GdsRecord]s
    fn encode_records(&mut self, records: &[GdsRecord]) -> GdsResult<()>;

    // Default Methods
    /// Encode a [GdsLibrary]
    fn encode_lib(&mut self, lib: &GdsLibrary) -> GdsResult<()> {
        // Write our header content
        let dates = self.encode_datetimes(&lib.dates);
        self.encode_records(&[
            GdsRecord::Header {
                version: lib.version,
            },
            GdsRecord::BgnLib { dates },
            GdsRecord::LibName(lib.name.clone()),
            GdsRecord::Units(lib.units.0, lib.units.1),
        ])?;
        // Write all of our Structs/Cells
        for strukt in lib.structs.iter() {
            self.encode_struct(strukt)?;
        }
        // And finally, the library terminator
        self.encode_record(GdsRecord::EndLib)?;
        Ok(())
    }
    /// Encode a [GdsStruct]
    fn encode_struct(&mut self, strukt: &GdsStruct) -> GdsResult<()> {
        // Write the header content
        let dates = self.encode_datetimes(&strukt.dates);
        self.encode_records(&[
            GdsRecord::BgnStruct { dates },
            GdsRecord::StructName(strukt.name.clone()),
        ])?;
        // Write each of our elements
        for elem in strukt.elems.iter() {
            self.encode_element(elem)?;
        }
        // And its terminator
        self.encode_record(GdsRecord::EndStruct)?;
        Ok(())
    }
    /// Encode a [GdsElement], dispatching across its variants
    fn encode_element(&mut self, elem: &GdsElement) -> GdsResult<()> {
        use GdsElement::*;
        match elem {
            GdsBoundary(e) => self.encode_boundary(e)?,
            GdsPath(e) => self.encode_path(e)?,
            GdsStructRef(e) => self.encode_struct_ref(e)?,
            GdsArrayRef(e) => self.encode_array_ref(e)?,
            GdsTextElem(e) => self.encode_text_elem(e)?,
            GdsNode(e) => self.encode_node(e)?,
            GdsBox(e) => self.encode_box(e)?,
        };
        Ok(())
    }
    /// Encode a [GdsPath]
    fn encode_path(&mut self, path: &GdsPath) -> GdsResult<()> {
        self.encode_record(GdsRecord::Path)?;
        if let Some(ref e) = path.elflags {
            self.encode_record(GdsRecord::ElemFlags(e.0, e.1))?;
        }
        if let Some(ref e) = path.plex {
            self.encode_record(GdsRecord::Plex(e.0))?;
        }
        self.encode_record(GdsRecord::Layer(path.layer))?;
        self.encode_record(GdsRecord::DataType(path.datatype))?;
        if let Some(ref e) = path.path_type {
            self.encode_record(GdsRecord::PathType(*e))?;
        }
        if let Some(ref e) = path.width {
            self.encode_record(GdsRecord::Width(*e))?;
        }
        if let Some(ref e) = path.begin_extn {
            self.encode_record(GdsRecord::BeginExtn(*e))?;
        }
        if let Some(ref e) = path.end_extn {
            self.encode_record(GdsRecord::EndExtn(*e))?;
        }
        self.encode_record(GdsRecord::Xy(GdsPoint::flatten_vec(&path.xy)))?;
        for prop in path.properties.iter() {
            self.encode_record(GdsRecord::PropAttr(prop.attr))?;
            self.encode_record(GdsRecord::PropValue(prop.value.clone()))?;
        }
        self.encode_record(GdsRecord::EndElement)?;
        Ok(())
    }
    /// Encode a [GdsBoundary]
    fn encode_boundary(&mut self, boundary: &GdsBoundary) -> GdsResult<()> {
        self.encode_record(GdsRecord::Boundary)?;
        if let Some(ref e) = boundary.elflags {
            self.encode_record(GdsRecord::ElemFlags(e.0, e.1))?;
        }
        if let Some(ref e) = boundary.plex {
            self.encode_record(GdsRecord::Plex(e.0))?;
        }
        self.encode_record(GdsRecord::Layer(boundary.layer))?;
        self.encode_record(GdsRecord::DataType(boundary.datatype))?;
        self.encode_record(GdsRecord::Xy(GdsPoint::flatten_vec(&boundary.xy)))?;
        for prop in boundary.properties.iter() {
            self.encode_record(GdsRecord::PropAttr(prop.attr))?;
            self.encode_record(GdsRecord::PropValue(prop.value.clone()))?;
        }
        self.encode_record(GdsRecord::EndElement)?;
        Ok(())
    }
    /// Encode a [GdsStructRef]
    fn encode_struct_ref(&mut self, sref: &GdsStructRef) -> GdsResult<()> {
        self.encode_record(GdsRecord::StructRef)?;
        if let Some(ref e) = sref.elflags {
            self.encode_record(GdsRecord::ElemFlags(e.0, e.1))?;
        }
        if let Some(ref e) = sref.plex {
            self.encode_record(GdsRecord::Plex(e.0))?;
        }
        self.encode_record(GdsRecord::StructRefName(sref.name.clone()))?;
        if let Some(ref e) = sref.strans {
            self.encode_strans(e)?;
        }
        self.encode_record(GdsRecord::Xy(GdsPoint::flatten(&sref.xy)))?;
        for prop in sref.properties.iter() {
            self.encode_record(GdsRecord::PropAttr(prop.attr))?;
            self.encode_record(GdsRecord::PropValue(prop.value.clone()))?;
        }
        self.encode_record(GdsRecord::EndElement)?;
        Ok(())
    }
    /// Encode a [GdsArrayRef]
    fn encode_array_ref(&mut self, aref: &GdsArrayRef) -> GdsResult<()> {
        self.encode_record(GdsRecord::ArrayRef)?;
        if let Some(ref e) = aref.elflags {
            self.encode_record(GdsRecord::ElemFlags(e.0, e.1))?;
        }
        if let Some(ref e) = aref.plex {
            self.encode_record(GdsRecord::Plex(e.0))?;
        }
        self.encode_record(GdsRecord::StructRefName(aref.name.clone()))?;
        if let Some(ref e) = aref.strans {
            self.encode_strans(e)?;
        }
        self.encode_record(GdsRecord::ColRow {
            cols: aref.cols,
            rows: aref.rows,
        })?;
        let mut xy = GdsPoint::flatten(&aref.xy[0]);
        xy.extend(GdsPoint::flatten(&aref.xy[1]));
        xy.extend(GdsPoint::flatten(&aref.xy[2]));
        self.encode_record(GdsRecord::Xy(xy))?;
        for prop in aref.properties.iter() {
            self.encode_record(GdsRecord::PropAttr(prop.attr))?;
            self.encode_record(GdsRecord::PropValue(prop.value.clone()))?;
        }
        self.encode_record(GdsRecord::EndElement)?;
        Ok(())
    }
    /// Encode a [GdsTextElem]
    fn encode_text_elem(&mut self, text: &GdsTextElem) -> GdsResult<()> {
        self.encode_record(GdsRecord::Text)?;
        if let Some(ref e) = text.elflags {
            self.encode_record(GdsRecord::ElemFlags(e.0, e.1))?;
        }
        if let Some(ref e) = text.plex {
            self.encode_record(GdsRecord::Plex(e.0))?;
        }
        self.encode_record(GdsRecord::Layer(text.layer))?;
        self.encode_record(GdsRecord::TextType(text.texttype))?;
        if let Some(ref e) = text.presentation {
            self.encode_record(GdsRecord::Presentation(e.0, e.1))?;
        }
        if let Some(ref e) = text.path_type {
            self.encode_record(GdsRecord::PathType(*e))?;
        }
        if let Some(ref e) = text.width {
            self.encode_record(GdsRecord::Width(*e))?;
        }
        if let Some(ref e) = text.strans {
            self.encode_strans(e)?;
        }
        self.encode_record(GdsRecord::Xy(GdsPoint::flatten(&text.xy)))?;
        self.encode_record(GdsRecord::String(text.string.clone()))?;
        for prop in text.properties.iter() {
            self.encode_record(GdsRecord::PropAttr(prop.attr))?;
            self.encode_record(GdsRecord::PropValue(prop.value.clone()))?;
        }
        self.encode_record(GdsRecord::EndElement)?;
        Ok(())
    }
    /// Encode a [GdsNode]
    fn encode_node(&mut self, node: &GdsNode) -> GdsResult<()> {
        self.encode_record(GdsRecord::Node)?;
        if let Some(ref e) = node.elflags {
            self.encode_record(GdsRecord::ElemFlags(e.0, e.1))?;
        }
        if let Some(ref e) = node.plex {
            self.encode_record(GdsRecord::Plex(e.0))?;
        }
        self.encode_record(GdsRecord::Layer(node.layer))?;
        self.encode_record(GdsRecord::Nodetype(node.nodetype))?;
        self.encode_record(GdsRecord::Xy(GdsPoint::flatten_vec(&node.xy)))?;
        for prop in node.properties.iter() {
            self.encode_record(GdsRecord::PropAttr(prop.attr))?;
            self.encode_record(GdsRecord::PropValue(prop.value.clone()))?;
        }
        self.encode_record(GdsRecord::EndElement)?;
        Ok(())
    }
    /// Encode a [GdsBox]
    fn encode_box(&mut self, box_: &GdsBox) -> GdsResult<()> {
        self.encode_record(GdsRecord::Box)?;
        if let Some(ref e) = box_.elflags {
            self.encode_record(GdsRecord::ElemFlags(e.0, e.1))?;
        }
        if let Some(ref e) = box_.plex {
            self.encode_record(GdsRecord::Plex(e.0))?;
        }
        self.encode_record(GdsRecord::Layer(box_.layer))?;
        self.encode_record(GdsRecord::BoxType(box_.boxtype))?;
        self.encode_record(GdsRecord::Xy(GdsPoint::flatten_vec(&box_.xy.to_vec())))?;
        for prop in box_.properties.iter() {
            self.encode_record(GdsRecord::PropAttr(prop.attr))?;
            self.encode_record(GdsRecord::PropValue(prop.value.clone()))?;
        }
        self.encode_record(GdsRecord::EndElement)?;
        Ok(())
    }
    /// Encode a [GdsStrans]
    fn encode_strans(&mut self, strans: &GdsStrans) -> GdsResult<()> {
        self.encode_record(GdsRecord::Strans(
            (strans.reflected as u8) << 7,
            (strans.abs_mag as u8) << 2 | (strans.abs_angle as u8) << 1,
        ))?;
        if let Some(ref e) = strans.mag {
            self.encode_record(GdsRecord::Mag(*e))?;
        }
        if let Some(ref e) = strans.angle {
            self.encode_record(GdsRecord::Angle(*e))?;
        }
        Ok(())
    }
    /// Encode a [`GdsDateTime`] in GDSII's vector of i16's format
    fn encode_datetime(&self, dt: &GdsDateTime, dest: &mut [i16]) {
        match dt {
            GdsDateTime::Bytes(ref bytes) => dest.copy_from_slice(bytes),
            GdsDateTime::DateTime(dt) => {
                let bytes = [
                    dt.year() as i16 - 1900, // GDSII uses 1900 as the base year
                    dt.month() as i16,
                    dt.day() as i16,
                    dt.hour() as i16,
                    dt.minute() as i16,
                    dt.second() as i16,
                ];
                dest.copy_from_slice(&bytes)
            }
        }
    }
    /// Encode [`GdsDateTimes`] in GDSII's vector of i16's format
    fn encode_datetimes(&self, dts: &GdsDateTimes) -> [i16; 12] {
        let mut rv = [0; 12];
        self.encode_datetime(&dts.modified, &mut rv[0..6]);
        self.encode_datetime(&dts.accessed, &mut rv[6..12]);
        return rv;
    }
}

/// # GdsRecordList
/// A largely for-testing implementer of the [Encode] trait,
/// which collects the generated records into a vector.
#[derive(Default, Debug, Deserialize, Serialize)]
pub struct GdsRecordList {
    pub records: Vec<GdsRecord>,
}
impl Encode for GdsRecordList {
    /// Add a [GdsRecord] to the list
    fn encode_record(&mut self, record: GdsRecord) -> GdsResult<()> {
        Ok(self.records.push(record))
    }
    /// Add an array of [GdsRecord]s to the list
    fn encode_records(&mut self, records: &[GdsRecord]) -> GdsResult<()> {
        Ok(self.records.extend(records.to_vec()))
    }
}
