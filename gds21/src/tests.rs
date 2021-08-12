use super::*;

/// Specified creation date for test cases
fn test_dates() -> GdsDateTimes {
    let test_date = NaiveDate::from_ymd(1970, 1, 1).and_hms(0, 0, 1);
    GdsDateTimes {
        modified: test_date.clone(),
        accessed: test_date.clone(),
    }
}
#[test]
fn scan() -> GdsResult<()> {
    // First-level scan struct names and byte-locations
    let fname = format!(
        "{}/resources/has_properties.gds",
        env!("CARGO_MANIFEST_DIR")
    );
    let lib = GdsLibrary::load(&fname)?;
    dbg!(lib.stats());
    Ok(())
}
#[test]
fn stats() -> GdsResult<()> {
    // Read the stats from a library
    let fname = format!(
        "{}/resources/has_properties.gds",
        env!("CARGO_MANIFEST_DIR")
    );
    let _scan = GdsScanner::scan(&fname)?;
    Ok(())
}
#[test]
fn it_reads() -> GdsResult<()> {
    // Read a sample GDS and compare to golden data
    let fname = format!("{}/resources/sample1.gds", env!("CARGO_MANIFEST_DIR"));
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

/// Compare `lib` to "golden" data loaded from JSON at path `golden`.
fn check(lib: &GdsLibrary, fname: &str) {
    // Uncomment this bit to over-write the golden data
    // save_json(lib, fname);

    let golden = load_json(fname);
    assert_eq!(*lib, golden);
}
/// Grab the full path of resource-file `fname`
fn resource(fname: &str) -> String {
    format!("{}/resources/{}", env!("CARGO_MANIFEST_DIR"), fname)
}
/// Load a library from JSON resource at path `fname`
fn load_json(fname: &str) -> GdsLibrary {
    use std::io::BufReader;
    let file = File::open(&fname).unwrap();
    let golden: GdsLibrary = serde_json::from_reader(BufReader::new(file)).unwrap();
    golden
}
/// Save a `GdsLibrary` as a JSON-format file at path `fname`
#[allow(dead_code)]
fn save_json(lib: &GdsLibrary, fname: &str) {
    let mut file = BufWriter::new(File::create(fname).unwrap());
    let s = serde_json::to_string(lib).unwrap();
    file.write_all(s.as_bytes()).unwrap();
    file.flush().unwrap();
}
