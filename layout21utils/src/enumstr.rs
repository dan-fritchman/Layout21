//!
//! # Enum-String Mapping Module
//!
//! Primarily defines the [enumstr] macro and paired [EnumStr] trait,
//! for defining a mapping between an enum and a string.
//! Widely useful for parsing and writing of many common layout-related formats
//! which expose enumerated values as one of a set of strings.
//!
//! The [EnumStr] trait defines two central methods:
//! * `to_str(&self) -> &'static str` converts the enum to its String values.
//! * `from_str(&str) -> Option<Self>` does the opposite, returning an [Option] indicator of success or failure.
//!
//! The [enumstr] macros is invoked with a colon-separated list of variants and their string-values.
//! It produces a struct and its implementation of [EnumStr].
//!
//! Example:
//!
//! ```rs
//! use layout21utils::enumstr;
//!
//! enumstr!(
//! /// # Light-Switch States: ON and OFF
//! LightSwitch {
//!     On: "ON",
//!     Off: "OFF",
//!  }
//! );
//! ```
//!

///
/// # String-Enumeration Trait
///
/// Defines two central methods:
/// * `to_str(&self) -> &'static str` converts the enum to its String values.
/// * `from_str(&str) -> Option<Self>` does the opposite, returning an [Option] indicator of success or failure.
///
/// While [EnumStr] can be implemented by any struct, its primary intent is
/// for implementation by the [enumstr] macro.
///
pub trait EnumStr: std::marker::Sized {
    fn to_str(&self) -> &'static str;
    fn from_str(txt: &str) -> Option<Self>;
}

///
/// # Enum-String Pairing Macro
///
/// For creating an `enum` which:
/// * (a) Has paired string-values, as commonly arrive in text-format fields.
/// * (b) Automatically implement the [EnumStr] trait for conversions to and from these strings.
/// * (c) Automatically implement [std::fmt::Display] writing the string-values
///
/// All variants are fieldless, and include derived implementations of common traits notably including `serde::{Serialize,Deserialize}`.
///
/// Example:
///
/// ```rs
/// use layout21utils::enumstr;
///
/// enumstr!(
/// /// # Light-Switch States: ON and OFF
/// LightSwitch {
///     On: "ON",
///     Off: "OFF",
///  }
/// );
/// ```
///
#[macro_export]
macro_rules! enumstr {
    (   $(#[$meta: meta])*
        $enum_name: ident {
        $( $variant: ident : $strval: literal ),* $(,)?
    }) => {
        $(#[$meta])*
        #[allow(dead_code)]
        #[derive(Clone, Copy, Debug, Deserialize, Serialize, PartialEq, Eq)]
        pub enum $enum_name {
            $( #[doc=$strval]
                $variant ),*
        }
        impl EnumStr for $enum_name {
            /// Convert a [$enum_name] variant to its paired (static) string value.
            #[allow(dead_code)]
            fn to_str(&self) -> &'static str {
                match self {
                    $( Self::$variant => $strval),*,
                }
            }
            /// Create a [$enum_name] from one of its string-values.
            /// Returns `None` if input `txt` does not match one of [$enum_name]'s variants.
            /// Note `from_str` is case *sensitive*, i.e. uses a native string comparison.
            /// If case-insensitive matching is intended instead, re-case outside `from_str`.
            fn from_str(txt: &str) -> Option<Self> {
                match txt {
                    $( $strval => Some(Self::$variant)),*,
                    _ => None,
                }
            }
        }
        impl ::std::fmt::Display for $enum_name {
            fn fmt(&self, f: &mut ::std::fmt::Formatter) -> ::std::fmt::Result {
                let s = match self {
                    $( Self::$variant => $strval),*,
                };
                write!(f, "{}", s)
            }
        }
    }
}

#[cfg(test)]
pub mod tests {
    use super::*;

    #[test]
    fn test_enumstr() {
        use super::*;

        enumstr!(
            /// # Light-Switch States: ON and OFF
            LightSwitch {
                On: "ON",
                Off: "OFF",
            }
        );

        // Test conversion to string
        assert_eq!(LightSwitch::On.to_str(), "ON");
        assert_eq!(LightSwitch::Off.to_str(), "OFF");

        // Test conversion from string
        assert_eq!(LightSwitch::from_str("ON"), Some(LightSwitch::On));
        assert_eq!(LightSwitch::from_str("OFF"), Some(LightSwitch::Off));
        assert_eq!(LightSwitch::from_str("NEITHER"), None);
    }
}
