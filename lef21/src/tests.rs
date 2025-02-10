use rust_decimal::Decimal;

use super::read::{parse_str, LefLexer, LefParser, Token};
use super::*;
use crate::utils::SerializationFormat::{Json, Toml, Yaml};
use std::path::Path;

#[test]
fn test_points() -> LefResult<()> {
    // Basic checks on derived [LefPoint] operators
    assert_eq!(
        LefPoint::new(1, 1) + LefPoint::new(2, 2),
        LefPoint::new(3, 3)
    );
    assert_eq!(
        LefPoint::new(2, 2) - LefPoint::new(1, 1),
        LefPoint::new(1, 1)
    );
    let mut p = LefPoint::new(11, 11);
    p += LefPoint::new(2, 2);
    assert_eq!(p, LefPoint::new(13, 13));

    let mut p = LefPoint::new(7, 8);
    p -= LefPoint::new(5, 4);
    assert_eq!(p, LefPoint::new(2, 4));

    Ok(())
}

#[test]
fn it_lexes() -> LefResult<()> {
    let src = "STUFF 101 ; \n # commentary \n";
    let lex = LefLexer::new(src)?;
    let toks_vec: Vec<Token> = lex.collect(); // Collect up all tokens
    let tok_strs: Vec<&str> = toks_vec.iter().map(|t| t.substr(src)).collect();
    assert_eq!(tok_strs, vec!["STUFF", "101", ";"]);
    Ok(())
}
#[test]
fn it_parses() -> LefResult<()> {
    let src = r#"
        VERSION 5 ; # commentary 
        BUSBITCHARS "xy" ; 
        MACRO some_name 
        END some_name
        END LIBRARY
    "#;
    let lib = parse_str(src)?;
    check_yaml(&lib, &resource("lib1.yaml"));
    Ok(())
}
#[test]
fn it_parses_layer_geoms1() -> LefResult<()> {
    let src = r#"
    LAYER some_layers_name ;
        RECT 1.065000 1.075000 1.705000 1.325000 ;
        RECT 1.495000 0.615000 3.335000 0.785000 ;
        RECT 1.495000 0.785000 1.705000 1.075000 ;
        RECT 1.495000 1.325000 1.705000 1.495000 ;
        RECT 1.495000 1.495000 1.785000 2.465000 ;
        RECT 2.180000 0.255000 2.420000 0.615000 ;
        RECT 3.070000 1.915000 4.515000 2.085000 ;
        RECT 3.070000 2.085000 3.400000 2.465000 ;
        RECT 3.090000 0.255000 3.335000 0.615000 ;
        RECT 4.090000 2.085000 4.515000 2.465000 ;
        RECT MASK 1 1.065000 1.075000 1.705000 1.325000 ;
        RECT MASK 1 1.495000 0.615000 3.335000 0.785000 ;
        RECT MASK 1 1.495000 0.785000 1.705000 1.075000 ;
        RECT MASK 1 1.495000 1.325000 1.705000 1.495000 ;
        RECT MASK 1 1.495000 1.495000 1.785000 2.465000 ;
        RECT MASK 1 2.180000 0.255000 2.420000 0.615000 ;
        RECT MASK 1 3.070000 1.915000 4.515000 2.085000 ;
        RECT MASK 1 3.070000 2.085000 3.400000 2.465000 ;
        RECT MASK 1 3.090000 0.255000 3.335000 0.615000 ;
        RECT MASK 1 4.090000 2.085000 4.515000 2.465000 ;
        RECT MASK 2 1.065000 1.075000 1.705000 1.325000 ;
        RECT MASK 2 1.495000 0.615000 3.335000 0.785000 ;
        RECT MASK 2 1.495000 0.785000 1.705000 1.075000 ;
        RECT MASK 2 1.495000 1.325000 1.705000 1.495000 ;
        RECT MASK 2 1.495000 1.495000 1.785000 2.465000 ;
        RECT MASK 2 2.180000 0.255000 2.420000 0.615000 ;
        RECT MASK 2 3.070000 1.915000 4.515000 2.085000 ;
        RECT MASK 2 3.070000 2.085000 3.400000 2.465000 ;
        RECT MASK 2 3.090000 0.255000 3.335000 0.615000 ;
        RECT MASK 2 4.090000 2.085000 4.515000 2.465000 ;
    "#;
    let mut parser = LefParser::new(src)?;
    let geoms = parser.parse_layer_geometries()?;
    check_yaml(&geoms, &resource("geoms1.yaml"));
    Ok(())
}

#[test]
fn it_parses_lib2() -> LefResult<()> {
    let src = r#"
    VERSION 5.4 ; 
    USEMINSPACING OBS OFF ;
    UNITS
        DATABASE MICRONS 2000 ;
    END UNITS
    MACRO macro_name
        CLASS BLOCK ;
        SIZE 999.9 BY 111.1 ;
        SYMMETRY X Y R90 ;
        PIN pin_name
            DIRECTION INPUT ;
            PORT
                LAYER layer_name ;
                    RECT  88.4 0.0 88.78 1.06 ;
            END
        END pin_name
    END macro_name
    END LIBRARY
    "#;
    let lib = parse_str(src)?;
    check_yaml(&lib, &resource("lib2.yaml"));
    Ok(())
}

#[test]
fn it_parses_density_lib() -> LefResult<()> {
    let src = r#"
    VERSION 5.8 ;
    UNITS DATABASE MICRONS 2000 ; END UNITS
    MACRO macro_dens
        CLASS BLOCK ;
        SIZE 100.0 BY 100.0 ;
        DENSITY
        LAYER met6 ;
        RECT 0.0 0.0 40.0 50.0 46.6 ;
        RECT 0.0 50.0 100.0 100.0 90 ;
        LAYER met2 ;
        RECT 1.0 2.0 3.0 4.0 5.55 ;
        END
    END macro_dens
    "#;
    let lib = parse_str(src)?;
    //check_yaml(&lib, &resource("lib2.yaml"));
    Ok(())
}

#[test]
fn it_parses_via_lib() -> LefResult<()> {
    let src = r#"
    VERSION 5.8 ;
    UNITS DATABASE MICRONS 2000 ; END UNITS
    VIA via1 DEFAULT
      RESISTANCE 21.0
      LAYER met1 ;
        RECT MASK 0 -0.5 -0.5 0.5 0.5 ;
      LAYER cut1 ;
        POLYGON -0.2 -0.2 -0.2 0.2 0.3 0.2 0.2 -0.2 ;
        RECT -0.3 -0.3 0.3 0.3 ;
      LAYER met2 ;
        POLYGON MASK 1 -0.3 -0.3 -0.3 0.3 0.4 0.3 0.3 -0.3 ;
        POLYGON -0.4 -0.2 -0.2 0.1 0.2 0.2 0.1 -0.4 ;
    END via1
    VIA via2
      LAYER met2 ;
        RECT MASK 0 -0.5 -0.5 0.5 0.5 ;
      LAYER cut2 ;
        RECT -0.3 -0.3 0.3 0.3 ;
      LAYER met3 ;
        RECT -0.4 -0.4 0.4 0.4 ;
    END via2
    VIA via3 DEFAULT
      VIARULE genvia3 ;
      CUTSIZE 0.2 0.2 ;
      LAYERS met3 via3 met4 ;
      CUTSPACING 0.1 0.1 ;
      ENCLOSURE 0.1 0.2 0.4 0.2 ;
      ROWCOL 2 3 ;
    END via3
    "#;
    let lib = parse_str(src)?;
    assert_eq!(lib.vias.len(), 3);
    let via2 = &lib.vias[1];
    assert_eq!(
        *via2,
        LefViaDef {
            name: "via2".into(),
            default: false,
            data: LefViaDefData::Fixed(LefFixedViaDef {
                resistance_ohms: None,
                layers: vec![
                    LefViaLayerGeometries {
                        layer_name: "met2".into(),
                        shapes: vec![LefViaShape::Rect(
                            Some(LefMask {
                                mask: Decimal::new(0, 0),
                            }),
                            LefPoint {
                                x: Decimal::new(-5, 1),
                                y: Decimal::new(-5, 1)
                            },
                            LefPoint {
                                x: Decimal::new(5, 1),
                                y: Decimal::new(5, 1)
                            }
                        )],
                    },
                    LefViaLayerGeometries {
                        layer_name: "cut2".into(),
                        shapes: vec![LefViaShape::Rect(
                            None,
                            LefPoint {
                                x: Decimal::new(-3, 1),
                                y: Decimal::new(-3, 1)
                            },
                            LefPoint {
                                x: Decimal::new(3, 1),
                                y: Decimal::new(3, 1)
                            }
                        )],
                    },
                    LefViaLayerGeometries {
                        layer_name: "met3".into(),
                        shapes: vec![LefViaShape::Rect(
                            None,
                            LefPoint {
                                x: Decimal::new(-4, 1),
                                y: Decimal::new(-4, 1)
                            },
                            LefPoint {
                                x: Decimal::new(4, 1),
                                y: Decimal::new(4, 1)
                            }
                        )],
                    },
                ],
            }),
            properties: None,
        }
    );
    let via3 = &lib.vias[2];
    assert_eq!(
        *via3,
        LefViaDef {
            name: "via3".into(),
            default: true,
            data: LefViaDefData::Generated(LefGeneratedViaDef {
                via_rule_name: "genvia3".into(),
                cut_size_x: Decimal::new(2, 1),
                cut_size_y: Decimal::new(2, 1),
                bot_metal_layer: "met3".into(),
                cut_layer: "via3".into(),
                top_metal_layer: "met4".into(),
                cut_spacing_x: Decimal::new(1, 1),
                cut_spacing_y: Decimal::new(1, 1),
                bot_enc_x: Decimal::new(1, 1),
                bot_enc_y: Decimal::new(2, 1),
                top_enc_x: Decimal::new(4, 1),
                top_enc_y: Decimal::new(2, 1),
                rowcol: Some(LefRowCol {
                    rows: Decimal::new(2, 0),
                    cols: Decimal::new(3, 0),
                }),
                origin: None,
                offset: None,
                pattern: None,
            }),
            properties: None,
        }
    );
    Ok(())
}

#[test]
fn it_parses_no_end_library_5p6() -> LefResult<()> {
    let src = r#"
    VERSION 5.6 ;
    UNITS DATABASE MICRONS 2000 ; END UNITS
    MACRO macro_name SIZE 2 BY 3 ; END macro_name
    "#;
    parse_str(src)?;
    Ok(())
}

#[test]
fn it_errors_no_end_library_5p5() -> LefResult<()> {
    let src = r#"
    VERSION 5.5 ;
    UNITS DATABASE MICRONS 2000 ; END UNITS
    MACRO macro_name SIZE 2 BY 3 ; END macro_name
    "#;
    assert!(parse_str(src).is_err());
    Ok(())
}

#[test]
fn empty_lib_to_yaml() {
    Yaml.save(&LefLibrary::new(), &resource("empty_lib.lef.yaml"))
        .unwrap();
}
#[test]
fn empty_lib_to_json() {
    Json.save(&LefLibrary::new(), &resource("empty_lib.lef.json"))
        .unwrap();
}
#[test]
fn empty_lib_to_toml() {
    Toml.save(&LefLibrary::new(), &resource("empty_lib.lef.toml"))
        .unwrap();
}

/// Helper function: Assert that `data` equals the content in YAML file `fname`
fn check_yaml<T>(data: &T, fname: impl AsRef<Path>)
where
    T: Eq + std::fmt::Debug + serde::de::DeserializeOwned,
{
    let golden: T = Yaml.open(fname).unwrap();
    assert_eq!(*data, golden);
}
/// Helper function: Grab the full path of resource-file `fname`
fn resource(rname: &str) -> String {
    format!("{}/resources/{}", env!("CARGO_MANIFEST_DIR"), rname)
}

#[test]
fn it_writes_schema() -> LefResult<()> {
    // Create the [schemars] JSON-Schema for [LefLibrary].
    // Compare it against golden data on disk.

    use schemars::schema_for;

    // Create the schema
    let schema = schema_for!(LefLibrary);

    // NOTE: uncomment to overwrite golden data
    // Json.save(&schema, resource("lef21.schema.json"))?;

    // Load the golden version, and ensure they match
    let golden = Json.open(resource("lef21.schema.json"))?;
    assert_eq!(schema, golden);

    Ok(())
}
