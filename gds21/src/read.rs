//!
//! # Gds21 Reading & Scanning
//!

// Std-Lib Imports
use std::convert::TryInto;
use std::io::{Cursor, SeekFrom};
use std::path::Path;

// Crates.io
use byteorder::{BigEndian, ReadBytesExt};
use num_traits::FromPrimitive;

// Local imports
use crate::data::*;

/// Size (in bytes) of the read/decode buffer array
const READER_BUFSIZE: usize = 65537;

/// # GdsReader
/// Helper for parsing and scanning GDS coming from files and similar sources.
pub struct GdsReader<R> {
    /// Read/conversion buffer
    buf: [u8; READER_BUFSIZE],
    /// Source being read
    source: R,
}

impl GdsReader<std::fs::File> {
    /// Create a [GdsReader], opening [File] at path `fname`
    pub fn open(fname: impl AsRef<Path>) -> GdsResult<GdsReader<std::fs::File>> {
        let file = std::fs::File::open(fname)?;
        Ok(GdsReader::new(file))
    }
}

impl<'a> GdsReader<Cursor<&'a [u8]>> {
    /// Create a [GdsReader] of `bytes`
    pub fn from_bytes(bytes: &'a [u8]) -> Self {
        GdsReader::new(Cursor::new(bytes))
    }
}

impl<R> GdsReader<R>
where
    R: std::io::Read + std::io::Seek,
{
    /// Create a [GdsReader] of `file`
    pub fn new(source: R) -> Self {
        let buf = [0; READER_BUFSIZE];
        Self { source, buf }
    }
    /// Read the next record-header from our file.
    /// Returns a [GdsRecordHeader] if successful.
    fn read_record_header(&mut self) -> GdsResult<GdsRecordHeader> {
        // Read the 16-bit record-size. (In bytes, including the four header bytes.)
        let len = match self.source.read_u16::<BigEndian>() {
            Err(e) => return Err(GdsError::Boxed(Box::new(e))), // Reading error; raise it.
            Ok(num) if num < 4 => return Err(GdsError::RecordLen(num.into())), // Invalid (too short) length; throw Error.
            Ok(num) if num % 2 != 0 => return Err(GdsError::RecordLen(num.into())), // Invalid (odd) length; throw Error.
            Ok(num) => num, // The normal case
        };
        let len = len - 4; // Strip out the four header-bytes
                           // Read and decode its RecordType
        let record_type = self.source.read_u8()?;
        let record_type: GdsRecordType =
            FromPrimitive::from_u8(record_type).ok_or(GdsError::InvalidRecordType(record_type))?;
        if !record_type.valid() {
            return Err(GdsError::InvalidRecordType(record_type as u8));
        }
        // Read and decode its DataType
        let data_type = self.source.read_u8()?;
        let data_type =
            FromPrimitive::from_u8(data_type).ok_or(GdsError::InvalidDataType(data_type))?;
        Ok(GdsRecordHeader {
            rtype: record_type,
            dtype: data_type,
            len,
        })
    }
    /// Read the next binary-encoded [GdsRecord].
    /// Returns a [GdsError] if `file` cursor is not on a record-boundary,
    /// or if binary decoding otherwise fails.
    fn read_record(&mut self) -> GdsResult<GdsRecord> {
        // Read the record header (types and length)
        let header = self.read_record_header()?;
        // And read the content
        self.read_record_content(&header)
    }
    fn read_record_content(&mut self, header: &GdsRecordHeader) -> GdsResult<GdsRecord> {
        // Based on that header-data, decode to a [GdsRecord]
        use GdsDataType::{BitArray, NoData, Str, F64, I16, I32};
        let len = header.len;
        let record: GdsRecord = match (header.rtype, header.dtype, len) {
            // Library-Level Records
            (GdsRecordType::Header, I16, 2) => GdsRecord::Header {
                version: self.read_i16(len)?[0],
            },
            (GdsRecordType::BgnLib, I16, 24) => GdsRecord::BgnLib {
                // Note the `try/unwrap` conversion here is infallible, since we know the length is 24 bytes / 12 i16s.
                dates: self.read_i16(24)?.try_into().unwrap(),
            },
            (GdsRecordType::LibName, Str, _) => GdsRecord::LibName(self.read_str(len)?),
            (GdsRecordType::Units, F64, 16) => {
                let v = self.read_f64(len)?;
                GdsRecord::Units(v[0], v[1])
            }
            (GdsRecordType::EndLib, NoData, 0) => GdsRecord::EndLib,

            // Structure (Cell) Level Records
            (GdsRecordType::BgnStruct, I16, 24) => GdsRecord::BgnStruct {
                // Note the `try/unwrap` conversion here is infallible, since we know the length is 24 bytes / 12 i16s.
                dates: self.read_i16(24)?.try_into().unwrap(),
            },
            (GdsRecordType::StructName, Str, _) => GdsRecord::StructName(self.read_str(len)?),
            (GdsRecordType::StructRefName, Str, _) => GdsRecord::StructRefName(self.read_str(len)?),
            (GdsRecordType::EndStruct, NoData, 0) => GdsRecord::EndStruct,

            // Element-Level Records
            (GdsRecordType::Boundary, NoData, 0) => GdsRecord::Boundary,
            (GdsRecordType::Path, NoData, 0) => GdsRecord::Path,
            (GdsRecordType::StructRef, NoData, 0) => GdsRecord::StructRef,
            (GdsRecordType::ArrayRef, NoData, 0) => GdsRecord::ArrayRef,
            (GdsRecordType::Text, NoData, 0) => GdsRecord::Text,
            (GdsRecordType::Layer, I16, 2) => GdsRecord::Layer(self.read_i16(len)?[0]),
            (GdsRecordType::DataType, I16, 2) => GdsRecord::DataType(self.read_i16(len)?[0]),
            (GdsRecordType::Width, I32, 4) => GdsRecord::Width(self.read_i32(len)?[0]),
            (GdsRecordType::Xy, I32, _) => GdsRecord::Xy(self.read_i32(len)?),
            (GdsRecordType::EndElement, NoData, 0) => GdsRecord::EndElement,

            // More (less well-categorized here) record-types
            (GdsRecordType::ColRow, I16, 4) => {
                let d = self.read_i16(len)?;
                GdsRecord::ColRow {
                    cols: d[0],
                    rows: d[1],
                }
            }
            (GdsRecordType::Node, NoData, 0) => GdsRecord::Node,
            (GdsRecordType::TextType, I16, 2) => GdsRecord::TextType(self.read_i16(len)?[0]),
            (GdsRecordType::Presentation, BitArray, 2) => {
                let bytes = self.read_bytes(len)?;
                GdsRecord::Presentation(bytes[0], bytes[1])
            }
            (GdsRecordType::String, Str, _) => GdsRecord::String(self.read_str(len)?),
            (GdsRecordType::Strans, BitArray, 2) => {
                let bytes = self.read_bytes(len)?;
                GdsRecord::Strans(bytes[0], bytes[1])
            }
            (GdsRecordType::Mag, F64, 8) => GdsRecord::Mag(self.read_f64(len)?[0]),
            (GdsRecordType::Angle, F64, 8) => GdsRecord::Angle(self.read_f64(len)?[0]),
            (GdsRecordType::RefLibs, Str, _) => GdsRecord::RefLibs(self.read_str(len)?),
            (GdsRecordType::Fonts, Str, _) => GdsRecord::Fonts(self.read_str(len)?),
            (GdsRecordType::PathType, I16, 2) => GdsRecord::PathType(self.read_i16(len)?[0]),
            (GdsRecordType::Generations, I16, 2) => GdsRecord::Generations(self.read_i16(len)?[0]),
            (GdsRecordType::AttrTable, Str, _) => GdsRecord::AttrTable(self.read_str(len)?),
            (GdsRecordType::ElemFlags, BitArray, 2) => {
                let bytes = self.read_bytes(len)?;
                GdsRecord::ElemFlags(bytes[0], bytes[1])
            }
            (GdsRecordType::Nodetype, I16, 2) => GdsRecord::Nodetype(self.read_i16(len)?[0]),
            (GdsRecordType::PropAttr, I16, 2) => GdsRecord::PropAttr(self.read_i16(len)?[0]),
            (GdsRecordType::PropValue, Str, _) => GdsRecord::PropValue(self.read_str(len)?),
            (GdsRecordType::Box, NoData, 0) => GdsRecord::Box,
            (GdsRecordType::BoxType, I16, 2) => GdsRecord::BoxType(self.read_i16(len)?[0]),
            (GdsRecordType::Plex, I32, 4) => GdsRecord::Plex(self.read_i32(len)?[0]),
            (GdsRecordType::BeginExtn, I32, 4) => GdsRecord::BeginExtn(self.read_i32(len)?[0]),
            (GdsRecordType::EndExtn, I32, 4) => GdsRecord::EndExtn(self.read_i32(len)?[0]),
            (GdsRecordType::TapeNum, I16, 2) => GdsRecord::TapeNum(self.read_i16(len)?[0]),
            (GdsRecordType::TapeCode, I16, 12) => {
                // Note the `try/unwrap` combination here is infalliable, since we know the length is 12 bytes / 6 i16s.
                GdsRecord::TapeCode(self.read_i16(12)?.try_into().unwrap())
            }
            (GdsRecordType::Format, I16, 2) => GdsRecord::Format(self.read_i16(len)?[0]),
            (GdsRecordType::Mask, Str, _) => GdsRecord::Mask(self.read_str(len)?),
            (GdsRecordType::EndMasks, NoData, 0) => GdsRecord::EndMasks,
            (GdsRecordType::LibDirSize, I16, 2) => GdsRecord::LibDirSize(self.read_i16(len)?[0]),
            (GdsRecordType::SrfName, Str, _) => GdsRecord::SrfName(self.read_str(len)?),
            (GdsRecordType::LibSecur, I16, 2) => GdsRecord::LibSecur(self.read_i16(len)?[0]),

            // Failing to meet any of these clauses means this is an invalid record
            _ => return Err(GdsError::RecordDecode(header.rtype, header.dtype, len)),
        };
        Ok(record)
    }
    /// Read `len` bytes and convert to `String`
    fn read_str(&mut self, len: u16) -> GdsResult<String> {
        let len: usize = len.into();
        // ASCII Decode. First load bytes into our buffer.
        let mut data = &mut self.buf[0..len];
        self.source.read_exact(data)?;
        // Strip optional end-of-string chars
        let len = data.len();
        if len == 0 {
            return Ok("".to_string());
        }
        if data[len - 1] == 0x00 {
            data = &mut data[0..len - 1];
        }
        // And convert to string
        let s: String = std::str::from_utf8(&data)?.into();
        Ok(s)
    }
    /// Read `len` bytes
    fn read_bytes(&mut self, len: u16) -> Result<Vec<u8>, std::io::Error> {
        let len: usize = len.into();
        let mut rv: Vec<u8> = vec![0; len];
        self.source.read_exact(&mut rv[0..len])?;
        Ok(rv)
    }
    /// Read `len/2` i16s from `len` bytes
    fn read_i16(&mut self, len: u16) -> Result<Vec<i16>, std::io::Error> {
        let len: usize = len.into();
        self.source.read_exact(&mut self.buf[0..len])?;
        let mut rv: Vec<i16> = vec![0; len / 2];
        self.buf[0..len]
            .as_ref()
            .read_i16_into::<BigEndian>(&mut rv)?;
        Ok(rv)
    }
    /// Read `len/4` i32s from `len` bytes
    fn read_i32(&mut self, len: u16) -> Result<Vec<i32>, std::io::Error> {
        let len: usize = len.into();
        self.source.read_exact(&mut self.buf[0..len])?;
        let mut rv: Vec<i32> = vec![0; len / 4];
        self.buf[0..len]
            .as_ref()
            .read_i32_into::<BigEndian>(&mut rv)?;
        Ok(rv)
    }
    /// Read `len/8` f64s from `len` bytes, decoding GDS's float-format along the way
    fn read_f64(&mut self, len: u16) -> GdsResult<Vec<f64>> {
        let len: usize = len.into();
        let mut u64s = vec![0; len / 8];
        self.source.read_u64_into::<BigEndian>(&mut u64s)?;
        let rv = u64s.into_iter().map(GdsFloat64::decode).collect();
        Ok(rv)
    }
    /// Get the current file position
    #[inline(always)]
    fn pos(&mut self) -> u64 {
        // Note `unwrap` here never panics, as [Cursor.stream_position] *always* returns `Ok`
        self.source.stream_position().unwrap()
    }
}

/// # GdsStructScan
/// Typically generated by first-pass file scanning.
/// Stores a struct name and byte-offsets in its source file.
#[derive(Debug, Default)]
pub struct GdsStructScan {
    /// Struct Name
    name: String,
    /// Starting byte offset, at beginning of [BgnStruct]
    start: u64,
    /// Ending byte offset, at end of [EndStruct]
    end: u64,
}
/// # GdsScanner
/// Scans a file for [GdsStruct] definitions,
/// creating a first-pass list of their names and byte-locations.
pub struct GdsScanner<R> {
    /// Reader-helper
    rdr: GdsReader<R>,
    /// Next record-header, stored for peeking
    nxt: GdsRecordHeader,
}

impl GdsScanner<std::fs::File> {
    /// Open and scan structs in file `fname`
    pub fn scan(fname: impl AsRef<Path>) -> GdsResult<Vec<GdsStructScan>> {
        let rdr = GdsReader::open(fname)?;
        let mut me = Self::new(rdr)?;
        me.scan_lib()
    }
}

impl<R> GdsScanner<R>
where
    R: std::io::Read + std::io::Seek,
{
    /// Create a new GdsReader iterator
    pub fn new(mut rdr: GdsReader<R>) -> GdsResult<Self> {
        // Decode the first record to initialize our "peeker"
        let nxt = rdr.read_record_header()?;
        Ok(Self { rdr, nxt })
    }

    /// Expect / require the next record to be of type `rtype`
    /// Skips over the remainder of its content, if any.
    fn expect(&mut self, rtype: GdsRecordType) -> GdsResult<()> {
        if self.peek().rtype == rtype {
            // Success. Skip over the record and advance to the next.
            self.skip()
        } else {
            self.fail()
        }
    }
    /// Get the next record's header, asserting it must be of type `rtype`.
    /// Consuming the remainder of the record is left to the caller.
    fn get(&mut self, rtype: GdsRecordType) -> GdsResult<&GdsRecordHeader> {
        if self.peek().rtype == rtype {
            Ok(self.peek())
        } else {
            self.fail()
        }
    }
    /// Advance our iterator and return the next element
    fn next(&mut self) -> GdsResult<GdsRecordHeader> {
        if self.peek().rtype == GdsRecordType::EndLib {
            // Once we reach [EndLib], keep returning it forever
            return Ok(self.peek().clone());
        }
        // Decode a new header and swap it with our `nxt`
        let mut rv = self.rdr.read_record_header()?;
        std::mem::swap(&mut rv, &mut self.nxt);
        Ok(rv)
    }
    /// Skip over the current record's content, if any, and load the next.
    fn skip(&mut self) -> GdsResult<()> {
        let len = self.nxt.len.into();
        self.rdr.source.seek(SeekFrom::Current(len))?;
        self.next()?;
        Ok(())
    }
    /// Peek at our next record, without advancing
    #[inline(always)]
    fn peek(&self) -> &GdsRecordHeader {
        &self.nxt
    }
    /// Scan the GDS for cell/ struct definitions
    pub fn scan_lib(&mut self) -> GdsResult<Vec<GdsStructScan>> {
        // Read off header info
        self.expect(GdsRecordType::Header)?;
        self.expect(GdsRecordType::BgnLib)?;
        // Read the Library name
        let len = self.get(GdsRecordType::LibName)?.len;
        let _lib_name = self.rdr.read_str(len)?;
        self.next()?;
        // More header info
        self.expect(GdsRecordType::Units)?;

        // Scan all of the structs
        let mut strukts = Vec::<GdsStructScan>::with_capacity(1024);
        loop {
            let GdsRecordHeader { rtype, .. } = self.peek();
            match rtype {
                GdsRecordType::EndLib => break,
                GdsRecordType::BgnStruct => strukts.push(self.scan_struct()?),
                _ => return self.fail(),
            }
        }
        Ok(strukts)
    }
    /// Scan a single struct definition.
    /// Starts *after* the [BgnStruct] *header* has been read (just above).
    /// Returns a [GdsStructScan] if successful.
    fn scan_struct(&mut self) -> GdsResult<GdsStructScan> {
        // Create our scan-structure, and store the start-position of the struct.
        // Note this requires backing up the four header bytes.
        let mut s = GdsStructScan::default();
        s.start = self.pos() - 4;
        // Skip over the remainder of the [BeginStruct] header
        self.skip()?;

        // Read the Struct name
        let len = self.get(GdsRecordType::StructName)?.len;
        s.name = self.rdr.read_str(len)?;
        self.next()?;

        // Scan the struct content. Skip over everything until [EndStruct]
        loop {
            let GdsRecordHeader { rtype, .. } = self.peek();
            match rtype {
                GdsRecordType::EndStruct => {
                    // Note the `len` of [EndStruct] is also zero, so we need not adjust the `end` position.
                    s.end = self.pos();
                    self.skip()?;
                    break;
                }
                // While *many* record-types are invalid here,
                // there's at least one we should check for: end-of-library,
                // Lest we find one and get stuck forever.
                GdsRecordType::EndLib => return self.fail(),
                // Everything else: skip and continue
                _ => self.skip()?,
            }
        }
        Ok(s)
    }
    /// Get the current file position
    #[inline(always)]
    fn pos(&mut self) -> u64 {
        self.rdr.pos()
    }
    /// Error generation helper
    fn err(&mut self) -> GdsError {
        let pos = self.pos();
        GdsError::Str(format!(
            "Scanned Invalid Record {:?} at Byte Position {}",
            self.nxt.rtype, pos
        ))
    }
    /// Error generation helper
    fn fail<T>(&mut self) -> GdsResult<T> {
        Err(self.err())
    }
}
/// # GdsParser
/// A peekable iterator which loads GdsRecords from file, one at a time,
/// and converters them into a tree of Gds data structures.
pub struct GdsParser<R> {
    /// File being read
    rdr: GdsReader<R>,
    /// Next record, stored for peeking
    nxt: GdsRecord,
    /// Number of records read
    numread: usize,
    /// Context Stack
    ctx: Vec<GdsContext>,
}

impl GdsParser<std::fs::File> {
    /// Create a new GdsReader iterator for the file at path `fname`
    pub fn open(fname: impl AsRef<Path>) -> GdsResult<Self> {
        let rdr = GdsReader::open(fname.as_ref())?;
        Self::new(rdr)
    }

    /// JSON-Serialize and write (all) contents of the Iterator to `writer`
    #[cfg(test)]
    pub fn write_records(&mut self, writer: &mut impl std::io::Write) -> GdsResult<()> {
        loop {
            let r = self.next()?;
            if r == GdsRecord::EndLib {
                return Ok(());
            }
            let entry: (usize, GdsRecord) = (self.numread, r);
            let s = serde_json::to_string(&entry).unwrap();
            write!(writer, "\t")?;
            writer.write_all(s.as_bytes()).unwrap();
            write!(writer, ",\n")?;
        }
    }
    /// Open a GDS file `gds` and write all GdsRecords to JSON file `json`
    #[cfg(test)]
    pub fn dump(gds: &str, json: &str) -> GdsResult<()> {
        use std::io::{BufWriter, Write};

        // This streams one record at a time, rather than loading all into memory.
        // Create a ReaderIter from `gds`
        let mut me = GdsParser::open(gds)?;
        // Create the JSON file
        let mut w = BufWriter::new(std::fs::File::create(json)?);
        // Write it as a JSON list/sequence; add the opening bracket
        write!(w, "[\n")?;
        // Write all the records
        me.write_records(&mut w)?;
        // And close the list
        write!(w, "]\n")?;
        Ok(())
    }
}

impl<'a> GdsParser<Cursor<&'a [u8]>> {
    /// Create a new GdsReader iterator for the byte-vector `bytes`
    pub fn from_bytes(bytes: &'a [u8]) -> GdsResult<Self> {
        let rdr = GdsReader::from_bytes(bytes);
        Self::new(rdr)
    }
}

impl<R> GdsParser<R>
where
    R: std::io::Read + std::io::Seek,
{
    /// Create a new GdsReader iterator
    pub fn new(mut rdr: GdsReader<R>) -> GdsResult<Self> {
        // Decode the first record to initialize our "peeker"
        let nxt = rdr.read_record()?;
        Ok(GdsParser {
            rdr,
            nxt,
            numread: 1,
            ctx: Vec::new(),
        })
    }
    /// Advance our iterator and return the next element
    fn next(&mut self) -> GdsResult<GdsRecord> {
        if self.nxt == GdsRecord::EndLib {
            // Once we reach [EndLib], keep returning it forever
            return Ok(GdsRecord::EndLib);
        }
        // Decode a new Record and swap it with our `nxt`
        let mut rv = self.rdr.read_record()?;
        std::mem::swap(&mut rv, &mut self.nxt);
        self.numread += 1;
        Ok(rv)
    }
    /// Peek at our next record, without advancing
    fn peek(&self) -> &GdsRecord {
        &self.nxt
    }
    /// Parse a [GdsLibrary]. Generally the start-state when reading a GDS file.
    pub fn parse_lib(&mut self) -> GdsResult<GdsLibrary> {
        self.ctx.push(GdsContext::Library);
        let mut lib = GdsLibraryBuilder::default();
        let mut structs = Vec::<GdsStruct>::with_capacity(1024);
        // Read the Header and its version data
        lib = match self.next()? {
            GdsRecord::Header { version: v } => lib.version(v),
            _ => return self.fail("Invalid library: missing GDS HEADER record"),
        };
        // Read the begin-lib
        lib = match self.next()? {
            GdsRecord::BgnLib { dates: d } => lib.dates(self.parse_datetimes(&d)),
            _ => return self.fail("Invalid library: missing GDS BGNLIB record"),
        };
        // Iterate over all others
        loop {
            let r = self.next()?;
            lib = match r {
                GdsRecord::EndLib => break, // End-of-library
                GdsRecord::LibName(d) => lib.name(d),
                GdsRecord::Units(d0, d1) => lib.units(GdsUnits(d0, d1)),
                GdsRecord::BgnStruct { dates } => {
                    let strukt = self.parse_struct(&dates)?;
                    structs.push(strukt);
                    lib
                }
                // Spec-valid but unsupported records
                GdsRecord::LibDirSize(_)
                | GdsRecord::SrfName(_)
                | GdsRecord::LibSecur(_)
                | GdsRecord::RefLibs(_)
                | GdsRecord::Fonts(_)
                | GdsRecord::AttrTable(_)
                | GdsRecord::Generations(_)
                | GdsRecord::Format(_) => {
                    return Err(GdsError::Unsupported(Some(r), Some(GdsContext::Library)))
                }
                // Invalid
                _ => return self.invalid(r),
            };
        }
        // Add the Vec of structs, and create the Library from its builder
        lib = lib.structs(structs);
        Ok(lib.build()?)
    }
    /// Parse a cell ([GdsStruct])
    fn parse_struct(&mut self, dates: &[i16; 12]) -> GdsResult<GdsStruct> {
        self.ctx.push(GdsContext::Struct);
        let mut strukt = GdsStructBuilder::default();
        // Parse and store the header information: `dates` and `name`
        strukt = strukt.dates(self.parse_datetimes(dates));
        strukt = match self.next()? {
            GdsRecord::StructName(d) => strukt.name(d),
            _ => return self.fail("Missing Gds StructName"),
        };
        // Parse [GdsElement] records until hitting a [GdsRecord::EndStruct]
        let mut elems = Vec::<GdsElement>::with_capacity(1024);
        loop {
            let r = self.next()?;
            match r {
                GdsRecord::EndStruct => break, // End-of-struct
                GdsRecord::Boundary => elems.push(self.parse_boundary()?.into()),
                GdsRecord::Text => elems.push(self.parse_text_elem()?.into()),
                GdsRecord::Path => elems.push(self.parse_path()?.into()),
                GdsRecord::Box => elems.push(self.parse_box()?.into()),
                GdsRecord::StructRef => elems.push(self.parse_struct_ref()?.into()),
                GdsRecord::ArrayRef => elems.push(self.parse_array_ref()?.into()),
                GdsRecord::Node => elems.push(self.parse_node()?.into()),
                // Invalid
                _ => return self.invalid(r),
            };
        }
        strukt = strukt.elems(elems);
        let strukt = strukt.build()?;
        self.ctx.pop();
        Ok(strukt)
    }
    /// Parse a [GdsBoundary]
    fn parse_boundary(&mut self) -> GdsResult<GdsBoundary> {
        let mut b = GdsBoundaryBuilder::default();
        let mut props: Vec<GdsProperty> = Vec::new();

        loop {
            let r = self.next()?;
            b = match r {
                GdsRecord::EndElement => break, // End-of-element
                GdsRecord::Layer(d) => b.layer(d),
                GdsRecord::DataType(d) => b.datatype(d),
                GdsRecord::Xy(d) => b.xy(GdsPoint::parse_vec(&d)?),
                GdsRecord::Plex(d) => b.plex(GdsPlex(d)),
                GdsRecord::ElemFlags(d0, d1) => b.elflags(GdsElemFlags(d0, d1)),
                GdsRecord::PropAttr(attr) => {
                    props.push(self.parse_property(attr)?);
                    b
                }
                // Invalid
                _ => return self.invalid(r),
            };
        }
        b = b.properties(props);
        let b = b.build()?;
        self.ctx.pop();
        Ok(b)
    }
    /// Parse a [GdsPath]
    fn parse_path(&mut self) -> GdsResult<GdsPath> {
        self.ctx.push(GdsContext::Path);
        let mut b = GdsPathBuilder::default();
        let mut props: Vec<GdsProperty> = Vec::new();

        loop {
            let r = self.next()?;
            b = match r {
                GdsRecord::EndElement => break, // End-of-element
                GdsRecord::Layer(d) => b.layer(d),
                GdsRecord::DataType(d) => b.datatype(d),
                GdsRecord::Xy(d) => b.xy(GdsPoint::parse_vec(&d)?),
                GdsRecord::Width(d) => b.width(d),
                GdsRecord::PathType(d) => b.path_type(d),
                GdsRecord::BeginExtn(d) => b.begin_extn(d),
                GdsRecord::EndExtn(d) => b.end_extn(d),
                GdsRecord::Plex(d) => b.plex(GdsPlex(d)),
                GdsRecord::ElemFlags(d0, d1) => b.elflags(GdsElemFlags(d0, d1)),
                GdsRecord::PropAttr(attr) => {
                    props.push(self.parse_property(attr)?);
                    b
                }
                // Invalid
                _ => return self.invalid(r),
            };
        }
        b = b.properties(props);
        let b = b.build()?;
        self.ctx.pop();
        Ok(b)
    }
    /// Parse a [GdsTextElem] from an iterator of [GdsRecord]s.
    /// Requires the initial `Text` record has already been parsed.
    fn parse_text_elem(&mut self) -> GdsResult<GdsTextElem> {
        self.ctx.push(GdsContext::Text);
        let mut b = GdsTextElemBuilder::default();
        let mut props: Vec<GdsProperty> = Vec::new();

        loop {
            let r = self.next()?;
            b = match r {
                GdsRecord::EndElement => break, // End-of-element
                GdsRecord::Layer(d) => b.layer(d),
                GdsRecord::TextType(d) => b.texttype(d),
                GdsRecord::Xy(d) => b.xy(GdsPoint::parse(&d)?),
                GdsRecord::String(d) => b.string(d),
                GdsRecord::Presentation(d0, d1) => b.presentation(GdsPresentation(d0, d1)),
                GdsRecord::PathType(d) => b.path_type(d),
                GdsRecord::Width(d) => b.width(d),
                GdsRecord::Plex(d) => b.plex(GdsPlex(d)),
                GdsRecord::ElemFlags(d0, d1) => b.elflags(GdsElemFlags(d0, d1)),
                GdsRecord::Strans(d0, d1) => b.strans(self.parse_strans(d0, d1)?),
                GdsRecord::PropAttr(attr) => {
                    props.push(self.parse_property(attr)?);
                    b
                }
                // Invalid
                _ => return self.invalid(r),
            };
        }
        b = b.properties(props);
        let b = b.build()?;
        self.ctx.pop();
        Ok(b)
    }
    /// Parse a [GdsNode]
    fn parse_node(&mut self) -> GdsResult<GdsNode> {
        self.ctx.push(GdsContext::Node);
        let mut b = GdsNodeBuilder::default();
        let mut props: Vec<GdsProperty> = Vec::new();

        loop {
            let r = self.next()?;
            b = match r {
                GdsRecord::EndElement => break, // End-of-element
                GdsRecord::Layer(d) => b.layer(d),
                GdsRecord::Nodetype(d) => b.nodetype(d),
                GdsRecord::Xy(d) => b.xy(GdsPoint::parse_vec(&d)?),
                GdsRecord::Plex(d) => b.plex(GdsPlex(d)),
                GdsRecord::ElemFlags(d0, d1) => b.elflags(GdsElemFlags(d0, d1)),
                GdsRecord::PropAttr(attr) => {
                    props.push(self.parse_property(attr)?);
                    b
                }
                // Invalid
                _ => return self.invalid(r),
            };
        }
        b = b.properties(props);
        let b = b.build()?;
        self.ctx.pop();
        Ok(b)
    }
    /// Parse a [GdsBox]
    fn parse_box(&mut self) -> GdsResult<GdsBox> {
        self.ctx.push(GdsContext::Box);
        let mut b = GdsBoxBuilder::default();
        let mut props: Vec<GdsProperty> = Vec::new();

        loop {
            let r = self.next()?;
            b = match r {
                GdsRecord::EndElement => break, // End-of-element
                GdsRecord::Layer(d) => b.layer(d),
                GdsRecord::BoxType(d) => b.boxtype(d),
                GdsRecord::Xy(d) => {
                    // XY coordinates must be a five-element array.
                    // First parse a generic [GdsRecord::Xy] to a vector,
                    // and then convert, checking length in the process.
                    let v = GdsPoint::parse_vec(&d)?;
                    let xy: [GdsPoint; 5] = match v.try_into() {
                        Ok(xy) => xy,
                        Err(_) => return self.fail("Invalid XY for GdsBox"),
                    };
                    b.xy(xy)
                }
                GdsRecord::Plex(d) => b.plex(GdsPlex(d)),
                GdsRecord::ElemFlags(d0, d1) => b.elflags(GdsElemFlags(d0, d1)),
                GdsRecord::PropAttr(attr) => {
                    props.push(self.parse_property(attr)?);
                    b
                }
                // Invalid
                _ => return self.invalid(r),
            };
        }
        b = b.properties(props);
        let b = b.build()?;
        self.ctx.pop();
        Ok(b)
    }
    /// Parse a [GdsStructRef]
    fn parse_struct_ref(&mut self) -> GdsResult<GdsStructRef> {
        self.ctx.push(GdsContext::StructRef);
        let mut b = GdsStructRefBuilder::default();
        let mut props: Vec<GdsProperty> = Vec::new();

        loop {
            let r = self.next()?;
            b = match r {
                GdsRecord::EndElement => break, // End-of-element
                GdsRecord::StructRefName(d) => b.name(d),
                GdsRecord::Xy(d) => b.xy(GdsPoint::parse(&d)?),
                GdsRecord::Plex(d) => b.plex(GdsPlex(d)),
                GdsRecord::ElemFlags(d0, d1) => b.elflags(GdsElemFlags(d0, d1)),
                GdsRecord::Strans(d0, d1) => b.strans(self.parse_strans(d0, d1)?),
                GdsRecord::PropAttr(attr) => {
                    props.push(self.parse_property(attr)?);
                    b
                }
                // Invalid
                _ => return self.invalid(r),
            };
        }
        b = b.properties(props);
        let b = b.build()?;
        self.ctx.pop();
        Ok(b)
    }
    /// Parse a [GdsArrayRef]
    fn parse_array_ref(&mut self) -> GdsResult<GdsArrayRef> {
        self.ctx.push(GdsContext::ArrayRef);
        let mut b = GdsArrayRefBuilder::default();
        let mut props: Vec<GdsProperty> = Vec::new();

        loop {
            let r = self.next()?;
            b = match r {
                GdsRecord::EndElement => break, // End-of-element
                GdsRecord::StructRefName(d) => b.name(d),
                GdsRecord::ColRow { rows, cols } => b.rows(rows).cols(cols),
                GdsRecord::Xy(d) => {
                    // XY coordinates must be a three-element array.
                    // First parse a generic [GdsRecord::Xy] to a vector,
                    // and then convert, checking length in the process.
                    let v = GdsPoint::parse_vec(&d)?;
                    let xy: [GdsPoint; 3] = match v.try_into() {
                        Ok(xy) => xy,
                        Err(_) => return self.fail("Invalid XY for GdsArrayRef"),
                    };
                    b.xy(xy)
                }
                GdsRecord::Plex(d) => b.plex(GdsPlex(d)),
                GdsRecord::ElemFlags(d0, d1) => b.elflags(GdsElemFlags(d0, d1)),
                GdsRecord::Strans(d0, d1) => b.strans(self.parse_strans(d0, d1)?),
                GdsRecord::PropAttr(attr) => {
                    props.push(self.parse_property(attr)?);
                    b
                }
                // Invalid
                _ => return self.invalid(r),
            };
        }
        b = b.properties(props);
        let b = b.build()?;
        self.ctx.pop();
        Ok(b)
    }
    /// Parse a [GdsStrans] from records. Header bytes are passed as arguments `d0`, `d1`.
    fn parse_strans(&mut self, d0: u8, d1: u8) -> GdsResult<GdsStrans> {
        // Decode the first two bytes
        let mut s = GdsStrans {
            reflected: d0 & 0x80 != 0,
            abs_mag: d1 & 0x04 != 0,
            abs_angle: d1 & 0x02 != 0,
            ..Default::default()
        };
        // And parse optional magnitude & angle
        loop {
            match self.peek() {
                GdsRecord::Mag(d) => {
                    s.mag = Some(*d);
                    self.next()?; // Advance the iterator
                }
                GdsRecord::Angle(d) => {
                    s.angle = Some(*d);
                    self.next()?; // Advance the iterator
                }
                _ => break,
            }
        }
        Ok(s)
    }
    /// Parse a [GdsProperty]
    /// Numeric attribute `attr` is collected beforehand, as its record is the indication to parse an (attr, value) pair.
    fn parse_property(&mut self, attr: i16) -> GdsResult<GdsProperty> {
        self.ctx.push(GdsContext::Property);
        // `PropAttr` records must *immediately* be followed by `PropValue`, or parsing/ decoding fails.
        let value = if let GdsRecord::PropValue(v) = self.next()? {
            v
        } else {
            return self.fail("Gds Property without PropValue");
        };
        self.ctx.pop();
        Ok(GdsProperty { attr, value })
    }
    /// Parse from GDSII's vector of i16's format
    /// Note this is one of our few parsing methods that *does not* return a `GdsResult`.
    /// Invalid dates are instead stored as raw bytes in the [`GdsDateTime::Bytes`] variant.
    fn parse_datetimes(&mut self, d: &[i16; 12]) -> GdsDateTimes {
        // Note the `try/unwrap` conversions here are infallible, as their lengths are hard-coded.
        GdsDateTimes {
            modified: self.parse_datetime(&d[0..6].try_into().unwrap()),
            accessed: self.parse_datetime(&d[6..12].try_into().unwrap()),
        }
    }
    /// Parse a [`GdsDateTime`]
    /// Note this is one of our few parsing methods that *does not* return a `GdsResult`.
    /// Invalid dates are instead stored as raw bytes in the [`GdsDateTime::Bytes`] variant.
    fn parse_datetime(&mut self, d: &[i16; 6]) -> GdsDateTime {
        GdsDateTime::from(d)
    }
    /// Error helper for an invalid record
    fn invalid<T>(&mut self, record: GdsRecord) -> GdsResult<T> {
        Err(GdsError::Parse {
            msg: "Invalid GDS Record".into(),
            record,
            recordnum: self.numread,
            bytepos: self.rdr.pos(),
            ctx: self.ctx.clone(),
        })
    }
    /// Error helper. Create a Parse error
    fn err(&mut self, msg: impl Into<String>) -> GdsError {
        GdsError::Parse {
            msg: msg.into(),
            record: self.peek().clone(), // FIXME: this will generally be one too far, sadly
            recordnum: self.numread,
            bytepos: self.rdr.pos(),
            ctx: self.ctx.clone(),
        }
    }
    /// Return failure
    fn fail<T>(&mut self, msg: impl Into<String>) -> GdsResult<T> {
        Err(self.err(msg))
    }
}
