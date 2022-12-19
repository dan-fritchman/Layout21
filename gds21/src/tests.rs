#[allow(unused_imports)]
use std::io::prelude::*;
use std::path::Path;
use std::str;

// Crates.io
use chrono::NaiveDate;

// Local Imports
use crate::data::*;
use crate::read::*;
use layout21utils::SerializationFormat::{Json, Toml, Yaml};

/// Specified creation date for test cases
fn test_dates() -> GdsDateTimes {
    let test_date = GdsDateTime::DateTime(NaiveDate::from_ymd(1970, 1, 1).and_hms(0, 0, 1).into());
    GdsDateTimes {
        modified: test_date.clone(),
        accessed: test_date.clone(),
    }
}
#[test]
fn floats() -> GdsResult<()> {
    // Test conversions between normal-human and GDSII floating-point formats
    let f = GdsFloat64::encode(0.0);
    assert_eq!(f, 0);
    let d = GdsFloat64::decode(f);
    assert_eq!(d, 0.0);
    let f = GdsFloat64::encode(1.0);
    let d = GdsFloat64::decode(f);
    assert_eq!(d, 1.0);
    let f = GdsFloat64::encode(1e-11);
    let d = GdsFloat64::decode(f);
    assert_eq!(d, 1e-11);
    let f = GdsFloat64::encode(-0.69);
    let d = GdsFloat64::decode(f);
    assert_eq!(d, -0.69);
    let f = GdsFloat64::encode(-33.33e-33);
    let d = GdsFloat64::decode(f);
    assert_eq!(d, -33.33e-33);
    Ok(())
}
#[test]
fn stats() -> GdsResult<()> {
    // Test collecting statistics
    let fname = resource("sample1.gds");
    let lib = GdsLibrary::load(&fname)?;
    let stats = lib.stats();
    assert_eq!(
        stats,
        GdsStats {
            libraries: 1,
            structs: 1,
            boundaries: 261,
            paths: 0,
            struct_refs: 0,
            array_refs: 0,
            text_elems: 26,
            nodes: 0,
            boxes: 0,
        }
    );
    Ok(())
}
#[test]
fn scan() -> GdsResult<()> {
    // Test first-pass scanning
    let fname = resource("sample1.gds");
    let _scan = GdsScanner::scan(&fname)?;
    Ok(())
}
#[test]
fn it_reads() -> GdsResult<()> {
    // Read a sample GDS and compare to golden data
    let fname = resource("sample1.gds");
    let lib = GdsLibrary::load(&fname)?;
    check(&lib, &resource("sample1.json"));
    Ok(())
}
#[test]
fn it_dumps_records() -> GdsResult<()> {
    GdsParser::dump(&resource("sample1.gds"), &resource("sample1.records.json"))?;
    Ok(())
}
#[test]
fn it_round_trips() -> GdsResult<()> {
    // Read a sample
    let lib = GdsLibrary::load(&resource("sample1.gds"))?;
    // And check it round-trips to file
    roundtrip(&lib)?;
    Ok(())
}
#[test]
fn it_has_gds_properties() -> GdsResult<()> {
    // Read a sample
    let lib = GdsLibrary::load(&resource("has_properties.gds"))?;
    // Check it against golden data
    check(&lib, &resource("has_properties.json"));
    // And check it round-trips to file
    roundtrip(&lib)?;
    Ok(())
}
#[test]
fn it_instantiates() -> GdsResult<()> {
    // Read a sample, add a cell which instantiates it
    let fname = format!("{}/resources/sample1.gds", env!("CARGO_MANIFEST_DIR"));
    let mut lib = GdsLibrary::load(&fname)?;
    lib.name = "has_inst_lib".into();
    let s = GdsStruct {
        name: "has_inst".into(),
        dates: test_dates(),
        elems: vec![GdsElement::GdsStructRef(GdsStructRef {
            name: "dff1".into(),
            xy: GdsPoint::new(11_000, 11_000),
            strans: None,
            elflags: None,
            plex: None,
            properties: Vec::new(),
        })],
    };
    lib.structs.push(s);
    // Check it against golden data
    check(&lib, &resource("sample1_inst.json"));
    // And check it round-trips to file
    roundtrip(&lib)?;
    Ok(())
}
#[test]
fn it_arrays() -> GdsResult<()> {
    // Read a sample, add a cell which arrays it
    let fname = format!("{}/resources/sample1.gds", env!("CARGO_MANIFEST_DIR"));
    let mut lib = GdsLibrary::load(&fname)?;
    lib.name = "has_array_lib".into();
    let s = GdsStruct {
        name: "has_array".into(),
        dates: test_dates(),
        elems: vec![GdsElement::GdsArrayRef(GdsArrayRef {
            name: "dff1".into(),
            xy: [
                GdsPoint::new(0, 0),
                GdsPoint::new(0, 10_000_000),
                GdsPoint::new(10_000_000, 0),
            ],
            cols: 100,
            rows: 100,
            strans: None,
            elflags: None,
            plex: None,
            properties: Vec::new(),
        })],
    };
    lib.structs.push(s);
    // Check it against golden data
    check(&lib, &resource("sample1_array.json"));
    // And check it round-trips to file
    roundtrip(&lib)?;
    Ok(())
}
#[test]
/// Test too-long record length (>16K) generates an error
fn record_too_long() -> GdsResult<()> {
    let mut lib = GdsLibrary::new("mylib");
    let mut newcell = GdsStruct::new("mycell");
    newcell.elems.push(
        GdsBoundary {
            xy: GdsPoint::parse_vec(&vec![0; 20_000])?,
            ..GdsBoundary::default()
        }
        .into(),
    );
    lib.structs.push(newcell);
    // This should generate [GdsError::RecordLen]
    match roundtrip(&lib) {
        Err(GdsError::RecordLen(_)) => Ok(()),
        Ok(_) | Err(_) => Err(GdsError::Str(
            "should generate a [GdsError::RecordLen] error".into(),
        )),
    }
}

/// Create an empty library with known dates
fn empty_lib() -> GdsLibrary {
    // Create an empty library
    let mut lib = GdsLibrary::new("empty");
    // Set its dates to some known value, so we can check it round-trips
    lib.dates = test_dates();
    // And return it for other test
    lib
}
#[test]
fn empty_lib_roundtrip() -> GdsResult<()> {
    // Create an empty, testable library
    let lib = empty_lib();

    // OK now the actual test
    roundtrip(&lib)?;
    check(&lib, &resource("empty.gds.json"));
    Ok(())
}
#[test]
fn empty_lib_to_json() -> GdsResult<()> {
    let lib = empty_lib();
    Json.save(&lib, &resource("empty.gds.json"))
        .expect("save failed");
    Ok(())
}
#[test]
fn empty_lib_to_yaml() -> GdsResult<()> {
    let lib = empty_lib();
    Yaml.save(&lib, &resource("empty.gds.yaml"))
        .expect("save failed");
    Ok(())
}
#[test]
#[ignore] // https://github.com/dan-fritchman/Layout21/issues/33
fn empty_lib_to_toml() -> GdsResult<()> {
    let lib = empty_lib();
    Toml.save(&lib, &resource("empty.gds.toml"))
        .expect("save failed");
    Ok(())
}

#[test]
fn test_invalid_dates() -> GdsResult<()> {
    // Test loading a library with invalid dates
    let lib = GdsLibrary::load(&resource("invalid_dates.gds"))?;
    assert_eq!(
        lib.dates,
        GdsDateTimes {
            modified: GdsDateTime::Bytes([0, 0, 0, 17, 49, 18]),
            accessed: GdsDateTime::Bytes([0, 0, 0, 17, 49, 18])
        }
    );
    Ok(())
}

#[test]
fn it_writes_schema() -> GdsResult<()> {
    // Create the [schemars] JSON-Schema for [GdsLibrary].
    // Compare it against golden data on disk.

    use schemars::schema_for;

    // Create the schema
    let schema = schema_for!(GdsLibrary);

    // NOTE: uncomment to overwrite golden data
    // Json.save(&schema, resource("gds21.schema.json"))?;

    // Load the golden version, and ensure they match
    let golden = Json.open(resource("gds21.schema.json"))?;
    assert_eq!(schema, golden);

    Ok(())
}

/// Compare `lib` to "golden" data loaded from JSON at path `golden`.
fn check(lib: &GdsLibrary, fname: &impl AsRef<Path>) {
    // Uncomment this bit to over-write the golden data
    // Json.save(lib, fname).unwrap();

    let golden = Json.open(fname).unwrap();
    assert_eq!(*lib, golden);
}
/// Grab the full path of resource-file `fname`
fn resource(rname: &str) -> String {
    format!("{}/resources/{}", env!("CARGO_MANIFEST_DIR"), rname)
}
