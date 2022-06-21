use super::read::{parse_str, LefLexer, LefParser, Token};
use super::*;
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

/// Helper function: Assert that `data` equals the content in YAML file `fname`
fn check_yaml<T>(data: &T, fname: impl AsRef<Path>)
where
    T: Eq + std::fmt::Debug + serde::de::DeserializeOwned,
{
    use crate::utils::SerializationFormat::Yaml;
    let golden: T = Yaml.open(fname).unwrap();
    assert_eq!(*data, golden);
}
/// Helper function: Grab the full path of resource-file `fname`
fn resource(rname: &str) -> String {
    format!("{}/resources/{}", env!("CARGO_MANIFEST_DIR"), rname)
}
