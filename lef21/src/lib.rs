//!
//! # Lef21 Library Exchange Format (LEF) Parser & Writer
//!

// Standard Lib Imports
#[allow(unused_imports)]
use std::io::prelude::*;
use std::io::{BufReader, BufWriter, Read, Write};
use std::str::Chars;

// Crates.io Imports
#[allow(unused_imports)]
use rust_decimal::prelude::*;
use serde::{Deserialize, Serialize};
#[macro_use]
extern crate derive_builder;

// Internal Type Alias
type LefDecimal = rust_decimal::Decimal;

/// # Lef Lexer / Tokenizer
///
/// Breaks input string `self.src` into an iteration of [Token]s,
/// consisting of source-locations and type-annotations.
///
/// Operates in an iterator-style mode, producing a [Token] with
/// each call to `lex_one`.
///
struct LefLexer<'src> {
    /// Source-string character iterator
    chars: Chars<'src>,
    /// Peekable next character
    next_char: Option<char>,
    /// Peekable next Token
    next_tok: Option<Token>,
    /// Current Position
    start: usize,
    /// Active character index
    pos: usize,
    /// Active line number
    line: usize,
    /// Boolean indication of "beginning of line",
    /// i.e. whether any semantic content has been encountered on the current line.
    at_bol: bool,
}
impl<'src> LefLexer<'src> {
    fn new(src: &'src str) -> LefResult<Self> {
        // Create our character-iterator
        let mut chars = src.chars();
        // Read the first character into our `next` field
        let next_char = chars.next();
        // Create the Lexer
        let mut lex = Self {
            chars,
            next_char,
            next_tok: None,
            start: 0,
            pos: 0,
            line: 1,
            at_bol: true,
        };
        // Read the first token into our `next_tok` field
        lex.next_tok = lex.advance()?;
        Ok(lex)
    }
    /// Get and return our next character, updating our position along the way
    fn next_char(&mut self) -> Option<char> {
        if self.next_char.is_none() {
            return None;
        }
        self.pos += 1;
        let mut rv = self.chars.next();
        std::mem::swap(&mut rv, &mut self.next_char);
        rv
    }
    /// Peek at our next character, without advancing.
    /// Returns `None` if no characters remain.
    fn peek_char(&self) -> &Option<char> {
        &self.next_char
    }
    /// Get and return our next token, updating internal state along the way
    fn next_token(&mut self) -> LefResult<Option<Token>> {
        if self.next_tok.is_none() {
            return Ok(None);
        }
        let mut tok = self.advance()?;
        std::mem::swap(&mut tok, &mut self.next_tok);
        Ok(tok)
    }
    /// Get an immutable reference to our next [Token], without advancing
    fn peek_token(&self) -> &Option<Token> {
        &self.next_tok
    }
    /// Pull our next [Token], removing ignored items such as commentary and whitespace.
    /// FIXME: it's not yet completely clear whether whitespace for indentation is semantically relevant.
    /// This implementation ignores all WS, presuming it is not.
    /// FIXME: better name here!
    fn advance(&mut self) -> LefResult<Option<Token>> {
        use TokenType::*;
        loop {
            match self.lex_one()? {
                None => return Ok(None),
                Some(t) => match t.ttype {
                    WhiteSpace | Comment => continue,
                    NewLine => {
                        // Only emit NewLine if there has been non-whitespace, non-comment content on the line
                        if self.at_bol {
                            continue;
                        } else {
                            self.at_bol = true;
                            return Ok(Some(t));
                        }
                    }
                    _ => {
                        // All other Tokens. Note we have seen content on the line, and return them.
                        self.at_bol = false;
                        return Ok(Some(t));
                    }
                },
            }
        }
    }
    /// Emit a [Token] of [TokenType] `ttype`
    /// Uses the current Lexer location as its span,
    /// and updates the Lexer start-position upon creation.
    fn emit(&mut self, ttype: TokenType) -> Token {
        let loc = SourceLocation {
            start: self.start,
            stop: self.pos,
            line: self.line,
        };
        let tok = Token { loc, ttype };
        self.start = self.pos;
        tok
    }
    /// Accept a character if it meets predicate-function `f`
    fn accept(&mut self, f: impl Fn(char) -> bool) -> bool {
        match self.peek_char() {
            Some(ref ch) if f(*ch) => {
                self.next_char();
                return true;
            }
            _ => false,
        }
    }
    /// Accept a single-character match
    fn accept_char(&mut self, c: char) -> bool {
        self.accept(|a| a == c)
    }
    /// Lexer start state. Grab the next [Token]
    fn lex_one(&mut self) -> LefResult<Option<Token>> {
        while self.peek_char().is_some() {
            if self.accept(|c| c == '\n') {
                // Create a NewLine Token
                let tok = self.emit(TokenType::NewLine);
                // And advance the line num
                self.line += 1;
                return Ok(Some(tok));
            } else if self.accept(|c| c.is_ascii_whitespace()) {
                return self.lex_whitespace();
            } else if self.accept(|c| c == ';') {
                return Ok(Some(self.emit(TokenType::SemiColon)));
            } else if self.accept(|c| c == '"') {
                return self.lex_string_literal();
            } else if self.accept(|c| c == '/') {
                return self.lex_comment();
            } else if self.accept(|c| c.is_digit(10) || c == '-') {
                return self.lex_number();
            } else if self.accept(|c| c.is_ascii_alphabetic()) {
                return self.lex_name();
            } else {
                // Some other, invalid character. Fail.
                return self.err();
            }
        }
        // All done! End of input. Return `None`.
        Ok(None)
    }
    /// Lex whitespace, which at least some Lef programs interpret
    /// as significant, particularly indentation.
    fn lex_whitespace(&mut self) -> LefResult<Option<Token>> {
        while self.accept(|c| c.is_ascii_whitespace() && c != '\n') {
            continue;
        }
        let tok = self.emit(TokenType::WhiteSpace);
        Ok(Some(tok))
    }
    /// Lex a number
    fn lex_number(&mut self) -> LefResult<Option<Token>> {
        while self.accept(|c| c.is_digit(10) || c == '.') {
            continue;
        }
        let tok = self.emit(TokenType::Number);
        Ok(Some(tok))
    }
    /// Lex a string literal
    fn lex_string_literal(&mut self) -> LefResult<Option<Token>> {
        // First double-quote has been read.
        // Accept everything until a closing double-quote.
        while self.accept(|c| c != '"') {
            continue;
        }
        // And bump over the closing quote
        self.next_char();
        let tok = self.emit(TokenType::StringLiteral);
        Ok(Some(tok))
    }
    /// Lex a comment
    fn lex_comment(&mut self) -> LefResult<Option<Token>> {
        // First slash has been read. Error if we don't get the second.
        if !self.accept_char('/') {
            return self.err();
        }
        // Accept everything until a newline
        while self.accept(|c| c != '\n') {
            continue;
        }
        let tok = self.emit(TokenType::Comment);
        Ok(Some(tok))
    }
    /// Lex a String Name
    fn lex_name(&mut self) -> LefResult<Option<Token>> {
        // Accept alpha, numeric, and underscore characters
        while self.accept(|c| c.is_ascii_alphabetic() || c.is_digit(10) || c == '_') {
            continue;
        }
        let tok = self.emit(TokenType::Name);
        Ok(Some(tok))
    }
    /// Error-Generation Helper 
    /// Collect our current position and content into a [LefError::Lex]
    fn err<T>(&self) -> LefResult<T> {
        Err(LefError::Lex {
            line: self.line,
            pos: self.pos,
        })
    }
}
/// Iterator protocol for [LefLexer]
/// Not used during parsing, but often handy for testing.   
/// Panics on Lexer errors.
impl<'s> Iterator for LefLexer<'s> {
    type Item = Token;
    fn next(&mut self) -> Option<Self::Item> {
        self.next_token().unwrap()
    }
}
/// Location of a [Token] in the source string
#[derive(Debug, Default, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
struct SourceLocation {
    /// Start character index (inclusive)
    start: usize,
    /// End character index (exclusive)
    stop: usize,
    /// Line number
    line: usize,
}
/// Lexer Token
/// Provides indices into the source-string for the (character) start and end of the source text,
/// as well as the line number and type-tag.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
struct Token {
    /// Source Location
    loc: SourceLocation,
    /// Token Type
    ttype: TokenType,
}
impl Token {
    /// Return a sub-string of input-string `src` over our locations
    fn substr<'me, 'src>(&'me self, src: &'src str) -> &'src str {
        &src[self.loc.start..self.loc.stop]
    }
}
/// Token Types Enumeration
/// Provides the type-tagging for initial Tokens
#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
enum TokenType {
    Name,
    Number,
    SemiColon,
    StringLiteral,
    NewLine,
    WhiteSpace,
    Comment,
}
/// Lef Error Enumeration
#[derive(Debug)]
pub enum LefError {
    /// Lexer Errors
    Lex {
        line: usize,
        pos: usize,
    },
    /// Parser Errors
    Parse {
        string: String,
        line: usize,
        pos: usize,
    },
    /// Errors parsing numeric [LefDecimal] values
    ParseNum(rust_decimal::Error),
    /// File I/O Errors
    Io(std::io::Error),
    /// Other wrapped errors, generally from other crates
    Other(Box<dyn std::error::Error>),
    /// Other string-typed errors, generally from other crates
    Str(String),
    Tbd,
}
impl From<std::io::Error> for LefError {
    /// Convert common IO & file errors by wrapping them
    fn from(e: std::io::Error) -> Self {
        Self::Io(e)
    }
}
impl From<rust_decimal::Error> for LefError {
    /// Convert integer-parsing errors by wrapping them
    fn from(e: rust_decimal::Error) -> Self {
        Self::ParseNum(e)
    }
}
// More external error types, all wrapped as [LefError::Other]
impl From<serde_json::Error> for LefError {
    fn from(e: serde_json::Error) -> Self {
        Self::Other(Box::new(e))
    }
}
impl From<serde_yaml::Error> for LefError {
    fn from(e: serde_yaml::Error) -> Self {
        Self::Other(Box::new(e))
    }
}
impl From<toml::ser::Error> for LefError {
    fn from(e: toml::ser::Error) -> Self {
        Self::Other(Box::new(e))
    }
}
impl From<String> for LefError {
    /// Convert string-based errors by wrapping them
    fn from(e: String) -> Self {
        Self::Str(e)
    }
}
/// Lef21 Library-Wide Result Type
pub type LefResult<T> = Result<T, LefError>;
// /// Lef Number
// /// All are decimal format, with signed integer-part
// /// and unsigned fractional part
// #[derive(Debug, Default, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
// pub struct LefDecimal {
//     /// Integer Part
//     pub integ: isize,
//     /// Fractional Part
//     pub frac: usize,
// }
/// Lef Parser
/// Transforms input string of lifetime 'src into a [LefLibrary]
struct LefParser<'src> {
    /// Source string
    src: &'src str,
    /// Lexer
    lex: LefLexer<'src>,
}
impl<'src> LefParser<'src> {
    /// Construct a [LefParser] of input-text `src`
    fn new(src: &'src str) -> LefResult<Self> {
        let lex = LefLexer::new(src)?;
        Ok(Self { src, lex })
    }
    #[inline(always)]
    fn next_token(&mut self) -> LefResult<Option<Token>> {
        self.lex.next_token()
    }
    /// Advance by a [Token] without returning it.
    /// Usually called after matching on "peeked" results.
    #[inline(always)]
    fn advance(&mut self) -> LefResult<()> {
        // Advance the lexer, pass along any errors it generates. And otherwise return Ok.
        self.lex.next_token()?;
        Ok(())
    }
    #[inline(always)]
    fn peek_token(&self) -> &Option<Token> {
        &self.lex.next_tok
    }
    /// Boolean indication of whether our next Token is of [TokenType] `ttype`.
    fn matches(&mut self, ttype: TokenType) -> bool {
        match self.lex.peek_token() {
            Some(t) if t.ttype == ttype => true,
            _ => false,
        }
    }
    /// Assert the expectation that the next [Token] is of [TokenType] `ttype`.
    /// Returns the [Token] if so. Returns an [Err] if not.
    fn expect(&mut self, ttype: TokenType) -> LefResult<Token> {
        let tok = self.next_token()?;
        match tok {
            Some(t) if t.ttype == ttype => return Ok(t),
            _ => self.err(),
        }
    }
    /// Assert the expectation that the next [Token] is of [TokenType] `ttype`.
    /// Returns the [Token] if so. Returns an [Err] if not.
    #[inline(always)]
    fn expect_and_get_str(&mut self, ttype: TokenType) -> LefResult<&str> {
        let tok = self.expect(ttype)?;
        Ok(self.txt(&tok))
    }
    /// Assert the next [Token] is of type [TokenType::Name],
    /// and return its string value.
    #[inline(always)]
    fn get_name(&mut self) -> LefResult<&str> {
        self.expect_and_get_str(TokenType::Name)
    }
    /// Assert that the next [Token] is a [TokenType::Name],
    /// and that its string value matches `name`.
    /// This is often used for pseudo-keywords,
    /// such as the "BY" in lines such as "SIZE x BY y".
    fn expect_name(&mut self, name: &str) -> LefResult<()> {
        let txt = self.get_name()?;
        if txt == name {
            Ok(())
        } else {
            self.err()
        }
    }
    /// Retrieve the text-content of lexer [Token] `tok`
    #[inline(always)]
    fn txt(&self, tok: &Token) -> &str {
        tok.substr(self.src)
    }
    /// Parse a [LefLibrary]
    fn parse_lib(&mut self) -> LefResult<LefLibrary> {
        let mut lib = LefLibraryBuilder::default();
        let mut macros = Vec::new();
        while let Some(t) = self.next_token()? {
            let txt = self.txt(&t);
            match txt {
                "MACRO" => {
                    macros.push(self.parse_macro()?);
                }
                "VERSION" => {
                    lib.version(self.parse_number()?);
                    self.expect(TokenType::SemiColon)?;
                    self.expect(TokenType::NewLine)?;
                }
                "BUSBITCHARS" => {
                    lib.bus_bit_chars(self.parse_bus_bit_chars()?);
                }
                "DIVIDERCHAR" => {
                    lib.divider_char(self.parse_divider_char()?);
                }
                "NAMESCASESENSITIVE" => unimplemented!(),
                "UNITS" => unimplemented!(),
                "BEGINEXT" => unimplemented!(),
                "END" => {
                    // End of MACRO definitions. Expect "END LIBRARY".
                    self.expect_name("LIBRARY")?;
                    break;
                }
                _ => return self.err(),
            }
        }
        lib.macros(macros);
        Ok(lib.build()?)
    }
    /// Parse a Lef MACRO definition
    fn parse_macro(&mut self) -> LefResult<LefMacro> {
        let mut mac = LefMacroBuilder::default();
        // Parse the macro-name
        let name = self.parse_ident()?;
        mac.name(name.clone());
        self.expect(TokenType::NewLine)?;
        // Start parsing attributes, pins, and obstructions
        let mut pins = Vec::new();
        let mut obs = Vec::new();
        while let Some(t) = self.next_token()? {
            match self.txt(&t) {
                "CLASS" => {
                    mac.class(self.parse_macro_class()?);
                }
                "FOREIGN" => unimplemented!(),
                "ORIGIN" => unimplemented!(),
                "SIZE" => unimplemented!(),
                "PIN" => {
                    pins.push(self.parse_macro_pin()?);
                }
                "OBS" => {
                    obs.push(self.parse_layer_geometries()?);
                }
                "END" => break,         // End of Macro
                _ => return self.err(), // Everything else is an error
            }
        }
        // Parse the END-enclosing macro-name
        self.expect_name(&name)?;
        self.expect(TokenType::NewLine)?;
        // Set the pins, build our struct and return it
        mac.pins(pins);
        mac.obs(obs);
        Ok(mac.build()?)
    }
    /// Parse a MACRO::PIN definition into a [LefPin]
    fn parse_macro_pin(&mut self) -> LefResult<LefPin> {
        let mut pin = LefPinBuilder::default();
        // Parse the pin-name
        let name = self.parse_ident()?;
        pin.name(name.clone());
        self.expect(TokenType::NewLine)?;
        let mut ports = Vec::new();
        while let Some(t) = self.next_token()? {
            match self.txt(&t) {
                "PORT" => {
                    ports.push(self.parse_macro_port()?);
                }
                "DIRECTION" => unimplemented!(),
                "TAPERRULE" => unimplemented!(),
                "USE" => unimplemented!(),
                "NETEXPR" => unimplemented!(),
                "SUPPLYSENSITIVITY" => unimplemented!(),
                "GROUNDSENSITIVITY" => unimplemented!(),
                "SHAPE" => unimplemented!(),
                "MUSTJOIN" => unimplemented!(),
                "PROPERTY" => unimplemented!(),
                "ANTENNAPARTIALMETALAREA" => unimplemented!(),
                "ANTENNAPARTIALMETALSIDEAREA" => unimplemented!(),
                "ANTENNAPARTIALCUTAREA" => unimplemented!(),
                "ANTENNAPARTIALDIFFAREA" => unimplemented!(),
                "ANTENNAMODEL" => unimplemented!(),
                "ANTENNAGATEAREA" => unimplemented!(),
                "ANTENNAMAXAREACAR" => unimplemented!(),
                "ANTENNAMAXSIDEAREACAR" => unimplemented!(),
                "ANTENNAMAXCUTCAR" => unimplemented!(),
                "END" => break, // End of Pin
                _ => return self.err(),
            }
        }
        // Get the pin-closing "END <name>"
        self.expect_name(&name)?;
        self.expect(TokenType::NewLine)?;
        // Set our port-objects, build and return the Pin
        pin.ports(ports);
        Ok(pin.build()?)
    }

    /// Parse a MACRO::PIN::PORT definition into a [LefPort]
    fn parse_macro_port(&mut self) -> LefResult<LefPort> {
        let class = None;
        let mut layers = Vec::new();
        // Note this peeks rather than takes the next token
        // FIXME: this should also assert each entry is of TokenType::Name
        while let Some(t) = self.peek_token() {
            match self.txt(&t) {
                "CLASS" => unimplemented!(),
                "LAYER" => {
                    layers.push(self.parse_layer_geometries()?);
                }
                "END" => {
                    // End of Port
                    self.advance()?; // Eat the END Token
                    break;
                }
                _ => return self.err(),
            }
        }
        Ok(LefPort { layers, class })
    }
    /// Parse a set of geometries on a single layer, as commonly specified per-[LefPort]
    fn parse_layer_geometries(&mut self) -> LefResult<LefLayerGeometries> {
        let mut layer = LefLayerGeometriesBuilder::default();
        // Check for the opening "LAYER" keyword
        self.expect_name("LAYER")?;
        // Parse the layer-name
        let layer_name = self.parse_ident()?;
        layer.layer_name(layer_name);
        self.expect(TokenType::SemiColon)?;
        self.expect(TokenType::NewLine)?;
        // LayerGeometries don't have an END card, so this needs to peek at the next token,
        // and exit when another LAYER or END (of a higher-level thing) turn up.
        let mut geoms = Vec::new();
        while let Some(t) = self.peek_token() {
            match self.txt(&t) {
                "EXCEPTPGNET" => unimplemented!(),
                "SPACING" => unimplemented!(),
                "DESIGNRULEWIDTH" => unimplemented!(),
                "WIDTH" => unimplemented!(),
                "VIA" => unimplemented!(),
                "PATH" | "POLYGON" | "RECT" => {
                    geoms.push(self.parse_geometry()?);
                }
                "LAYER" | "END" => break, // End of geometries. (Really start/end of something else.)
                _ => return self.err(),
            }
        }
        layer.geometries(geoms);
        Ok(layer.build()?)
    }
    /// Parse a [LefGeometry] statement
    /// Each can be a shape or iteration thereof
    fn parse_geometry(&mut self) -> LefResult<LefGeometry> {
        // let mut layer = LefLayerGeometriesBuilder::default();
        // let t = self.expect(TokenType::Name)?;
        match self.get_name()? {
            "RECT" => {
                if self.matches(TokenType::Name) {
                    unimplemented!();
                    // // Parse an optional MASK field
                    // self.expect_name("MASK")?;
                    // let mask_num = self.parse_ident()?;
                }
                // if self.matches(TokenType::Name) {
                //     unimplemented!();
                //     // Parse the ITERATE options
                //     self.expect_name("ITERATE")?;
                //     let mask_num = self.parse_ident()?;
                // }
                // Parse the two points
                let p1 = self.parse_point()?;
                let p2 = self.parse_point()?;
                self.expect(TokenType::SemiColon)?;
                self.expect(TokenType::NewLine)?;
                // And return the Rect
                Ok(LefGeometry::Shape(LefShape::Rect(p1, p2)))
            }
            "PATH" | "POLYGON" => unimplemented!(),
            _ => return self.err(),
        }
    }
    fn parse_point(&mut self) -> LefResult<LefPoint> {
        Ok(LefPoint(self.parse_number()?, self.parse_number()?))
    }
    /// Parse the MACRO::CLASS enumerations
    fn parse_macro_class(&mut self) -> LefResult<LefMacroClass> {
        match self.get_name()? {
            "COVER" => unimplemented!(),
            "RING" => unimplemented!(),
            "BLOCK" => {
                let tp = if self.matches(TokenType::Name) {
                    // If the next token is a name, it must be a valid sub-type
                    match self.get_name()? {
                        "BLACKBOX" => Some(LefBlockClassType::BlackBox),
                        "SOFT" => Some(LefBlockClassType::Soft),
                        _ => return self.err(),
                    }
                } else {
                    None
                };
                self.expect(TokenType::SemiColon)?;
                self.expect(TokenType::NewLine)?;
                return Ok(LefMacroClass::Block { tp });
            }
            "PAD" => unimplemented!(),
            "CORE" => {
                let tp = if self.matches(TokenType::Name) {
                    // If the next token is a name, it must be a valid sub-type
                    match self.get_name()? {
                        "FEEDTHRU" => Some(LefCoreClassType::FeedThru),
                        "TIEHIGH" => Some(LefCoreClassType::TieHigh),
                        "TIELOW" => Some(LefCoreClassType::TieLow),
                        "SPACER" => Some(LefCoreClassType::Spacer),
                        "ANTENNACELL" => Some(LefCoreClassType::AntennaCell),
                        "WELLTAP" => Some(LefCoreClassType::WellTap),
                        _ => return self.err(),
                    }
                } else {
                    None
                };
                self.expect(TokenType::SemiColon)?;
                self.expect(TokenType::NewLine)?;
                return Ok(LefMacroClass::Core { tp });
            }
            "ENDCAP" => unimplemented!(),
            _ => return self.err(),
        }
    }
    /// Parse the next token into a [LefDecimal] number
    fn parse_number(&mut self) -> LefResult<LefDecimal> {
        let tok = self.expect(TokenType::Number)?;
        let txt = self.txt(&tok);
        Ok(LefDecimal::from_str(txt)?)
    }
    /// Parse the LefLibrary::BUSBITCHARS key from a two-character string literal
    fn parse_bus_bit_chars(&mut self) -> LefResult<(char, char)> {
        let txt = self.expect_and_get_str(TokenType::StringLiteral)?;
        let chars = txt.chars().collect::<Vec<char>>();
        if chars.len() != 4 {
            return self.err();
        }
        let chars = (chars[1], chars[2]);
        self.expect(TokenType::SemiColon)?;
        self.expect(TokenType::NewLine)?;
        return Ok(chars);
    }
    /// Parse the LefLibrary::DIVIDERCHAR key from a single-character string literal
    fn parse_divider_char(&mut self) -> LefResult<char> {
        let txt = self.expect_and_get_str(TokenType::StringLiteral)?;
        let chars = txt.chars().collect::<Vec<char>>();
        if chars.len() != 3 {
            return self.err();
        }
        self.expect(TokenType::SemiColon)?;
        self.expect(TokenType::NewLine)?;
        Ok(chars[1])
    }
    /// Parse an identifier name, e.g. a macro, pin, or layer name.
    fn parse_ident(&mut self) -> LefResult<String> {
        let txt = self.expect_and_get_str(TokenType::Name)?;
        Ok(String::from(txt))
    }
    /// Error-Generation Helper 
    /// Collect our current position and content into a [LefError::Parse]
    fn err<T>(&self) -> LefResult<T> {
        let string = match self.lex.next_tok {
            Some(t) => self.txt(&t),
            None => "EOF",
        }
        .to_string();
        Err(LefError::Parse {
            string,
            line: self.lex.line,
            pos: self.lex.pos,
        })
    }
}
/// Lef Library
/// Primary store of macro/cell definitions
#[derive(Default, Clone, Builder, Debug, Deserialize, Serialize, PartialEq, Eq)]
#[builder(setter(into), private)]
pub struct LefLibrary {
    // Required
    /// Lef Spec Version
    pub version: LefDecimal,
    /// Macro Definitions
    pub macros: Vec<LefMacro>,

    // Optional
    /// Bus-Bit Separator Characters
    #[serde(default, skip_serializing_if = "Option::is_none")]
    #[builder(default, setter(strip_option))]
    pub bus_bit_chars: Option<(char, char)>,
    /// Divider Character
    #[serde(default, skip_serializing_if = "Option::is_none")]
    #[builder(default, setter(strip_option))]
    pub divider_char: Option<char>,
    #[serde(default, skip_serializing_if = "Option::is_none")]
    #[builder(default, setter(strip_option))]
    pub via: Option<Tbd>,
    #[serde(default, skip_serializing_if = "Option::is_none")]
    #[builder(default, setter(strip_option))]
    pub site: Option<Tbd>,
}
/// Lef Macro Definition
#[derive(Default, Clone, Builder, Debug, Deserialize, Serialize, PartialEq, Eq)]
#[builder(setter(into), private)]
pub struct LefMacro {
    // Required
    /// Macro Name
    pub name: String,
    /// Pin List
    pub pins: Vec<LefPin>,
    /// Obstructions
    pub obs: Vec<LefLayerGeometries>,

    // Optional
    /// Macro Class
    #[serde(default, skip_serializing_if = "Option::is_none")]
    #[builder(default, setter(strip_option))]
    pub class: Option<LefMacroClass>,
    /// Foreign (GDSII) Cell
    #[serde(default, skip_serializing_if = "Option::is_none")]
    #[builder(default, setter(strip_option))]
    pub foreign: Option<LefForeign>,
    /// X-Y Origin
    #[serde(default, skip_serializing_if = "Option::is_none")]
    #[builder(default, setter(strip_option))]
    pub origin: Option<LefPoint>,
    /// Electrically-Equivalent Cell
    #[serde(default, skip_serializing_if = "Option::is_none")]
    #[builder(default, setter(strip_option))]
    pub eeq: Option<Tbd>,
    /// Outline Size
    #[serde(default, skip_serializing_if = "Option::is_none")]
    #[builder(default, setter(strip_option))]
    pub size: Option<LefPoint>,
    /// Rotational & Translation Symmetries
    #[serde(default, skip_serializing_if = "Option::is_none")]
    #[builder(default, setter(strip_option))]
    pub symmetry: Option<Tbd>,
    /// Site Name
    #[serde(default, skip_serializing_if = "Option::is_none")]
    #[builder(default, setter(strip_option))]
    pub site: Option<Tbd>,
    /// Density Objects
    #[serde(default, skip_serializing_if = "Option::is_none")]
    #[builder(default, setter(strip_option))]
    pub density: Option<Tbd>,
    /// Properties
    #[serde(default, skip_serializing_if = "Option::is_none")]
    #[builder(default, setter(strip_option))]
    pub properties: Option<Tbd>,
}
#[derive(Clone, Debug, Deserialize, Serialize, PartialEq, Eq)]
pub enum LefMacroClass {
    Cover { bump: bool },
    Ring,
    Block { tp: Option<LefBlockClassType> },
    Pad { tp: Option<LefPadClassType> },
    Core { tp: Option<LefCoreClassType> },
    EndCap { tp: LefEndCapClassType },
}
#[derive(Clone, Debug, Deserialize, Serialize, PartialEq, Eq)]
pub enum LefBlockClassType {
    BlackBox,
    Soft,
}
#[derive(Clone, Debug, Deserialize, Serialize, PartialEq, Eq)]
pub enum LefCoreClassType {
    FeedThru,
    TieHigh,
    TieLow,
    Spacer,
    AntennaCell,
    WellTap,
}
#[derive(Clone, Debug, Deserialize, Serialize, PartialEq, Eq)]
pub enum LefPadClassType {
    Input,
    Output,
    Inout,
    Power,
    Spacer,
    AreaIo,
}
#[derive(Clone, Debug, Deserialize, Serialize, PartialEq, Eq)]
pub enum LefEndCapClassType {
    Pre,
    Post,
    TopLeft,
    TopRight,
    BottomLeft,
    BottomRight,
}

#[derive(Clone, Builder, Debug, Deserialize, Serialize, PartialEq, Eq)]
#[builder(setter(into), private)]
pub struct LefForeign {
    /// Foreign Cell Name
    pub cell_name: String,
    /// Location
    pub pt: Option<LefPoint>,
    /// Orientation
    pub orient: Option<Tbd>,
}
#[derive(Clone, Builder, Debug, Deserialize, Serialize, PartialEq, Eq)]
#[builder(setter(into), private)]
pub struct LefPin {
    // Required Fields
    /// Pin Name
    pub name: String,
    /// Port Geometries
    pub ports: Vec<LefPort>,

    // Optional Fields
    /// Properties
    #[serde(default, skip_serializing_if = "Option::is_none")]
    #[builder(default, setter(strip_option))]
    pub properties: Option<Tbd>,
    /// Direction
    #[serde(default, skip_serializing_if = "Option::is_none")]
    #[builder(default, setter(strip_option))]
    pub direction: Option<LefPinDirection>,

    /// Usage / Role
    #[serde(default, skip_serializing_if = "Option::is_none")]
    #[builder(default, setter(strip_option))]
    pub r#use: Option<LefPinUse>,

    /// Taper Rule
    #[serde(default, skip_serializing_if = "Option::is_none")]
    #[builder(default, setter(strip_option))]
    pub taper_rule: Option<Tbd>,
    /// Net Expression
    #[serde(default, skip_serializing_if = "Option::is_none")]
    #[builder(default, setter(strip_option))]
    pub net_expr: Option<Tbd>,
    /// Supply Sensitivity
    #[serde(default, skip_serializing_if = "Option::is_none")]
    #[builder(default, setter(strip_option))]
    pub supply_sensitivity: Option<Tbd>,
    /// Ground Sensitivity
    #[serde(default, skip_serializing_if = "Option::is_none")]
    #[builder(default, setter(strip_option))]
    pub ground_sensitivity: Option<Tbd>,
    /// Shape
    #[serde(default, skip_serializing_if = "Option::is_none")]
    #[builder(default, setter(strip_option))]
    pub shape: Option<Tbd>,
    /// Must-Join
    #[serde(default, skip_serializing_if = "Option::is_none")]
    #[builder(default, setter(strip_option))]
    pub must_join: Option<Tbd>,

    /// Antenna
    #[serde(default, skip_serializing_if = "Option::is_none")]
    #[builder(default, setter(strip_option))]
    pub antenna_partial_metal_area: Option<Tbd>,
    #[serde(default, skip_serializing_if = "Option::is_none")]
    #[builder(default, setter(strip_option))]
    pub antenna_partial_metal_side_area: Option<Tbd>,
    #[serde(default, skip_serializing_if = "Option::is_none")]
    #[builder(default, setter(strip_option))]
    pub antenna_partial_cut_area: Option<Tbd>,
    #[serde(default, skip_serializing_if = "Option::is_none")]
    #[builder(default, setter(strip_option))]
    pub antenna_diff_area: Option<Tbd>,
    #[serde(default, skip_serializing_if = "Option::is_none")]
    #[builder(default, setter(strip_option))]
    pub antenna_model: Option<Tbd>,
    #[serde(default, skip_serializing_if = "Option::is_none")]
    #[builder(default, setter(strip_option))]
    pub antenna_gate_area: Option<Tbd>,
    #[serde(default, skip_serializing_if = "Option::is_none")]
    #[builder(default, setter(strip_option))]
    pub antenna_max_area_char: Option<Tbd>,
    #[serde(default, skip_serializing_if = "Option::is_none")]
    #[builder(default, setter(strip_option))]
    pub antenna_side_area_char: Option<Tbd>,
    #[serde(default, skip_serializing_if = "Option::is_none")]
    #[builder(default, setter(strip_option))]
    pub antenna_max_cut_char: Option<Tbd>,
}
#[derive(Clone, Debug, Deserialize, Serialize, PartialEq, Eq)]
pub enum LefPinDirection {
    Input,
    Output { tristate: bool },
    Inout,
    FeedThru,
}
#[derive(Clone, Debug, Deserialize, Serialize, PartialEq, Eq)]
pub enum LefPinUse {
    Signal,
    Analog,
    Power,
    Ground,
    Clock,
}
#[derive(Clone, Builder, Debug, Deserialize, Serialize, PartialEq, Eq)]
pub struct LefPort {
    /// Layers & Geometries
    pub layers: Vec<LefLayerGeometries>,
    /// Port-Class
    pub class: Option<Tbd>,
}
#[derive(Clone, Builder, Debug, Deserialize, Serialize, PartialEq, Eq)]
pub struct LefLayerGeometries {
    // Required
    /// Layer Name
    pub layer_name: String,
    /// Geometries,
    pub geometries: Vec<LefGeometry>,

    // Optional
    #[serde(default, skip_serializing_if = "Option::is_none")]
    #[builder(default, setter(strip_option))]
    pub except_pg_net: Option<bool>,
    #[serde(default, skip_serializing_if = "Option::is_none")]
    #[builder(default, setter(strip_option))]
    pub spacing: Option<Tbd>, // FIXME: merge with `design_rule_width`
    #[serde(default, skip_serializing_if = "Option::is_none")]
    #[builder(default, setter(strip_option))]
    pub design_rule_width: Option<Tbd>, // FIXME: merge with `spacing`
    #[serde(default, skip_serializing_if = "Option::is_none")]
    #[builder(default, setter(strip_option))]
    pub width: Option<Tbd>,
}
/// Lef Geometric Objects -
/// Rectangles, Polygons, Paths, and Iterators thereof
#[derive(Clone, Debug, Deserialize, Serialize, PartialEq, Eq)]
pub enum LefGeometry {
    /// Single Shape
    Shape(LefShape),
    /// Repeated Iteration/ Array of Shapes
    Iterate { shape: LefShape, pattern: Tbd },
}
/// Lef Shapes
/// Individual Geometric Primitives
#[derive(Clone, Debug, Deserialize, Serialize, PartialEq, Eq)]
pub enum LefShape {
    Rect(LefPoint, LefPoint),
    Polygon(Vec<LefPoint>),
    Path(Vec<LefPoint>),
}
/// X-Y Point
#[derive(Clone, Default, Debug, Deserialize, Serialize, PartialEq, Eq)]
pub struct LefPoint(LefDecimal, LefDecimal);
/// Placeholder Struct for Fields to be completed
#[derive(Clone, Default, Debug, Deserialize, Serialize, PartialEq, Eq)]
pub struct Tbd;

/// Parse LEF content from file `fname`
pub fn parse_file(fname: &str) -> LefResult<LefLibrary> {
    let mut file = std::fs::File::open(fname)?;
    let mut src = String::new();
    file.read_to_string(&mut src)?;
    parse_str(&src)
}
/// Parse LEF content `src` from string
pub fn parse_str(src: &str) -> LefResult<LefLibrary> {
    let mut parser = LefParser::new(src)?;
    let lib = parser.parse_lib()?;
    Ok(lib)
}
/// Enumerated, Supported Serialization Formats
pub enum SerializationFormat {
    Json,
    Yaml,
    Toml,
    Lef,
}
impl SerializationFormat {
    fn to_string(&self, data: &impl Serialize) -> LefResult<String> {
        match *self {
            Self::Json => Ok(serde_json::to_string(data)?),
            Self::Yaml => Ok(serde_yaml::to_string(data)?),
            Self::Toml => Ok(toml::to_string(data)?),
            Self::Lef => unimplemented!(),
        }
    }
}
/// Save to file `fname` with serialization-format `fmt`
pub fn save(data: &impl Serialize, fname: &str, fmt: SerializationFormat) -> LefResult<()> {
    let mut file = BufWriter::new(std::fs::File::create(fname)?);
    let s = fmt.to_string(data)?;
    file.write_all(s.as_bytes())?;
    file.flush()?;
    Ok(())
}
/// Load from YAML file at path `fname`
pub fn load_yaml<T: serde::de::DeserializeOwned>(fname: &str) -> T {
    let file = std::fs::File::open(&fname).unwrap();
    serde_yaml::from_reader(BufReader::new(file)).unwrap()
}
#[cfg(test)]
mod tests {
    use super::*;
    type TestResult = LefResult<()>;

    #[test]
    fn it_lexes() -> TestResult {
        let src = "STUFF 101 ; \n // commentary \n";
        let lex = LefLexer::new(src)?;
        let toks_vec: Vec<Token> = lex.collect(); // Collect up all tokens
        let tok_strs: Vec<&str> = toks_vec.iter().map(|t| t.substr(src)).collect();
        assert_eq!(tok_strs, vec!["STUFF", "101", ";", "\n",]);
        Ok(())
    }
    #[test]
    fn it_parses() -> TestResult {
        let src = r#"
            VERSION 5 ; // commentary 
            BUSBITCHARS "xy" ; 
            MACRO some_name 
            END some_name
            END LIBRARY
        "#;
        let lib = parse_str(src)?;
        dbg!(lib);
        Ok(())
    }
    #[test]
    fn it_parses_layer_geoms1() -> TestResult {
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
    /// Helper function: Assert that `data` equals the content in YAML file `fname`
    fn check_yaml<T: Eq + std::fmt::Debug + serde::de::DeserializeOwned>(data: &T, fname: &str) {
        let golden: T = load_yaml(fname);
        assert_eq!(*data, golden);
    }
    /// Helper function: Grab the full path of resource-file `fname`
    fn resource(fname: &str) -> String {
        format!("{}/resources/{}", env!("CARGO_MANIFEST_DIR"), fname)
    }
}
