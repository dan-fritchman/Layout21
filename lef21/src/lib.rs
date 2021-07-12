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
/// each call to `next_token`.
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
    /// Character index at the beginning of the current line
    linestart: usize,
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
            linestart: 0,
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
        use TokenType::{Comment, NewLine, WhiteSpace};
        loop {
            match self.lex_one()? {
                None => return Ok(None),
                Some(t) => match t.ttype {
                    WhiteSpace | Comment => continue, // White-space and comments are not emitted
                    NewLine => {
                        // Only emit NewLine if there has been non-whitespace, non-comment content on the line.
                        // But always update our line-number and its starting position.
                        self.line += 1;
                        self.linestart = self.pos;
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
    /// Uses the current Lexer location as its span, and updates the Lexer start-position upon creation.
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
            if self.accept_char('\n') {
                let tok = self.emit(TokenType::NewLine);
                return Ok(Some(tok));
            } else if self.accept(|c| c.is_ascii_whitespace()) {
                return self.lex_whitespace();
            } else if self.accept(|c| c == ';') {
                return Ok(Some(self.emit(TokenType::SemiColon)));
            } else if self.accept(|c| c == '"') {
                return self.lex_string_literal();
            } else if self.accept(|c| c == '#') {
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
pub enum TokenType {
    Name,
    Number,
    SemiColon,
    StringLiteral,
    NewLine,
    WhiteSpace,
    Comment,
}
/// Enumerated parsing contexts, largely for capturing errors
#[derive(Debug)]
pub enum LefParseContext {
    Library,
    Macro,
    Units,
}
#[derive(Debug)]
pub enum LefParseErrorType {
    /// Invalid Key
    InvalidKey(String),
    /// Invalid Key
    InvalidKey2 { key: String, ctx: LefParseContext },
    /// Unsupported (but spec-valid) Keys
    UnsupportedKey { key: String, ctx: LefParseContext },
    InvalidToken {
        expected: TokenType,
        got: Option<TokenType>,
    },
}
/// Lef Error Enumeration
#[derive(Debug)]
pub enum LefError {
    /// Lexer Errors
    Lex { line: usize, pos: usize },
    /// Parser Errors
    Parse {
        tp: Option<LefParseErrorType>,
        token: String,
        line_content: String,
        linenum: usize,
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
        match self.peek_token() {
            Some(t) if t.ttype == ttype => true,
            _ => false,
        }
    }
    /// Assert the expectation that the next [Token] is of [TokenType] `ttype`.
    /// Returns the [Token] if so. Returns an [Err] if not.
    fn expect(&mut self, ttype: TokenType) -> LefResult<Token> {
        let tok = self.next_token()?;
        match tok {
            Some(t) if t.ttype == ttype => return Ok(t), // Good, usual case
            // Error handling & helpers
            Some(t) => self.err(Some(LefParseErrorType::InvalidToken {
                expected: ttype,
                got: Some(t.ttype),
            })),
            None => self.err(Some(LefParseErrorType::InvalidToken {
                expected: ttype,
                got: None,
            })),
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
            self.err(None)
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
        loop {
            let txt = self.expect_and_get_str(TokenType::Name)?;
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
                "NAMESCASESENSITIVE" => {
                    lib.names_case_sensitive(self.parse_enum::<LefOnOff>()?);
                    self.expect(TokenType::SemiColon)?;
                    self.expect(TokenType::NewLine)?;
                }
                "NOWIREEXTENSIONATPIN" => {
                    lib.no_wire_extension_at_pin(self.parse_enum::<LefOnOff>()?);
                    self.expect(TokenType::SemiColon)?;
                    self.expect(TokenType::NewLine)?;
                }
                "UNITS" => {
                    self.expect(TokenType::NewLine)?;
                    lib.units(self.parse_units()?);
                }
                "BEGINEXT" => unimplemented!(),
                "END" => {
                    // End of MACRO definitions. Expect "END LIBRARY".
                    self.expect_name("LIBRARY")?;
                    break;
                }
                _ => {
                    let txt = String::from(txt);
                    return self.err(Some(LefParseErrorType::InvalidKey(txt)));
                }
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
        loop {
            let txt = self.get_name()?;
            match txt {
                "CLASS" => {
                    mac.class(self.parse_macro_class()?);
                }
                "SITE" => {
                    let site_name = self.parse_ident()?;
                    mac.site(site_name);
                    self.expect(TokenType::SemiColon)?;
                    self.expect(TokenType::NewLine)?;
                }
                "FOREIGN" => {
                    let cell_name = self.parse_ident()?;
                    let mut pt = None;
                    if !self.matches(TokenType::SemiColon) {
                        pt = Some(self.parse_point()?);
                    }
                    // FIXME: the optional `orient` field is not supported
                    self.expect(TokenType::SemiColon)?;
                    self.expect(TokenType::NewLine)?;
                    mac.foreign(LefForeign {
                        cell_name,
                        pt,
                        orient: None,
                    });
                }
                "ORIGIN" => {
                    let origin = self.parse_point()?;
                    mac.origin(origin);
                    self.expect(TokenType::SemiColon)?;
                    self.expect(TokenType::NewLine)?;
                }
                "SIZE" => {
                    let x = self.parse_number()?;
                    self.expect_name("BY")?;
                    let y = self.parse_number()?;
                    mac.size((x, y));
                    self.expect(TokenType::SemiColon)?;
                    self.expect(TokenType::NewLine)?;
                }
                "PIN" => {
                    pins.push(self.parse_macro_pin()?);
                }
                "OBS" => {
                    self.expect(TokenType::NewLine)?;
                    mac.obs(self.parse_obstructions()?);
                }
                "SYMMETRY" => {
                    mac.symmetry(self.parse_macro_symmetries()?);
                }
                "SOURCE" => {
                    // FIXME: only supported in some LEF versions, sort out how to handle this fact
                    mac.source(self.parse_enum::<LefDefSource>()?);
                    self.expect(TokenType::SemiColon)?;
                    self.expect(TokenType::NewLine)?;
                }
                "END" => break, // End of Macro
                _ => {
                    let txt = String::from(txt);
                    return self.err(Some(LefParseErrorType::InvalidKey(txt)));
                }
            }
        }
        // Parse the END-enclosing macro-name
        self.expect_name(&name)?;
        self.expect(TokenType::NewLine)?;
        // Set the pins, build our struct and return it
        mac.pins(pins);
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
        loop {
            let txt = self.get_name()?;
            match txt {
                "PORT" => {
                    self.expect(TokenType::NewLine)?;
                    ports.push(self.parse_macro_port()?);
                }
                "DIRECTION" => {
                    pin.direction(self.parse_pin_direction()?);
                }
                "TAPERRULE" => unimplemented!(),
                "USE" => {
                    pin.r#use(self.parse_enum::<LefPinUse>()?);
                    self.expect(TokenType::SemiColon)?;
                    self.expect(TokenType::NewLine)?;
                }
                "NETEXPR" => unimplemented!(),
                "SUPPLYSENSITIVITY" => unimplemented!(),
                "GROUNDSENSITIVITY" => unimplemented!(),
                "SHAPE" => {
                    pin.shape(self.parse_enum::<LefPinShape>()?);
                    self.expect(TokenType::SemiColon)?;
                    self.expect(TokenType::NewLine)?;
                }
                "MUSTJOIN" => unimplemented!(),
                "PROPERTY" => unimplemented!(),
                "ANTENNAPARTIALMETALAREA" => unimplemented!(),
                "ANTENNAPARTIALMETALSIDEAREA" => unimplemented!(),
                "ANTENNAPARTIALCUTAREA" => unimplemented!(),
                "ANTENNAPARTIALDIFFAREA" => unimplemented!(),
                "ANTENNAMODEL" => unimplemented!(),
                "ANTENNADIFFAREA" => {
                    pin.antenna_diff_area(self.parse_number()?);
                    self.expect(TokenType::SemiColon)?;
                    self.expect(TokenType::NewLine)?;
                }
                "ANTENNAGATEAREA" => {
                    let area = self.parse_number()?;
                    pin.antenna_gate_area(area);
                    self.expect(TokenType::SemiColon)?;
                    self.expect(TokenType::NewLine)?;
                }
                "ANTENNAMAXAREACAR" => unimplemented!(),
                "ANTENNAMAXSIDEAREACAR" => unimplemented!(),
                "ANTENNAMAXCUTCAR" => unimplemented!(),
                "END" => break, // End of Pin
                _ => {
                    let txt = String::from(txt);
                    return self.err(Some(LefParseErrorType::InvalidKey(txt)));
                }
            }
        }
        // Get the pin-closing "END <name>"
        self.expect_name(&name)?;
        self.expect(TokenType::NewLine)?;
        // Set our port-objects, build and return the Pin
        pin.ports(ports);
        Ok(pin.build()?)
    }
    /// Parse a [LefPinDirection]
    fn parse_pin_direction(&mut self) -> LefResult<LefPinDirection> {
        let txt = self.get_name()?;
        let pin = match txt {
            "INPUT" => LefPinDirection::Input,
            "FEEDTHRU" => LefPinDirection::FeedThru,
            "INOUT" => LefPinDirection::Inout,
            "OUTPUT" => {
                if self.matches(TokenType::SemiColon) {
                    LefPinDirection::Output { tristate: false }
                } else {
                    self.expect_name("TRISTATE")?;
                    LefPinDirection::Output { tristate: true }
                }
            }
            _ => return self.err(None), // FIXME: some kinda InvalidValue error-type
        };
        self.expect(TokenType::SemiColon)?;
        self.expect(TokenType::NewLine)?;
        Ok(pin)
    }
    /// Parse a MACRO::PIN::PORT definition into a [LefPort]
    fn parse_macro_port(&mut self) -> LefResult<LefPort> {
        let class: Option<Tbd> = None; // FIXME: will be [LefPortClass] or similar
        let mut layers = Vec::new();
        // Parse attributes and geometries
        // Note this peeks rather than taking the next token,
        // largely to accommodate the closing-delimeter-free "LAYER" / [LefLayerGeometries] definitions.
        // Other keys generally advance by a Token *after* matching.
        while let Some(t) = self.peek_token() {
            if t.ttype != TokenType::Name {
                return self.err(Some(LefParseErrorType::InvalidToken {
                    expected: TokenType::Name,
                    got: Some(t.ttype),
                }));
            }
            let txt = self.txt(&t);
            match txt {
                "CLASS" => unimplemented!(),
                "LAYER" => {
                    layers.push(self.parse_layer_geometries()?);
                }
                "END" => {
                    self.advance()?; // Eat the END Token
                    self.expect(TokenType::NewLine)?;
                    break;
                }
                _ => {
                    let txt = String::from(txt);
                    return self.err(Some(LefParseErrorType::InvalidKey(txt)));
                }
            }
        }
        Ok(LefPort { layers, class })
    }
    /// Parse a [LefMacro]'s obstruction definitions
    fn parse_obstructions(&mut self) -> LefResult<Vec<LefLayerGeometries>> {
        let mut geoms = Vec::new();
        while let Some(t) = self.peek_token() {
            if t.ttype != TokenType::Name {
                return self.err(Some(LefParseErrorType::InvalidToken {
                    expected: TokenType::Name,
                    got: Some(t.ttype),
                }));
            }
            let txt = self.txt(&t);
            match txt {
                "LAYER" => {
                    geoms.push(self.parse_layer_geometries()?);
                }
                "END" => {
                    self.advance()?; // Eat the END Token
                    self.expect(TokenType::NewLine)?;
                    break;
                }
                _ => {
                    let txt = String::from(txt);
                    return self.err(Some(LefParseErrorType::InvalidKey(txt)));
                }
            }
        }
        Ok(geoms)
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
        let mut geoms = Vec::new();
        // LayerGeometries don't have an END card, so this needs to peek at the next token,
        // and exit when another LAYER or END (of a higher-level thing) turn up.
        // Note that on end-of-file, i.e. `peek_token` returning `None`,
        // this will exit and return a valid [LefLayerGeometries].
        // (Objects above it in the tree may error instead.)
        while let Some(t) = self.peek_token() {
            if t.ttype != TokenType::Name {
                return self.err(None);
            }
            let txt = self.txt(&t);
            match txt {
                "EXCEPTPGNET" => unimplemented!(),
                "SPACING" => unimplemented!(),
                "DESIGNRULEWIDTH" => unimplemented!(),
                "WIDTH" => unimplemented!(),
                "VIA" => unimplemented!(),
                "PATH" | "POLYGON" | "RECT" => {
                    geoms.push(self.parse_geometry()?);
                }
                "LAYER" | "END" => break, // End of geometries. (Really start/end of something else.)
                _ => {
                    let txt = String::from(txt);
                    return self.err(Some(LefParseErrorType::InvalidKey(txt)));
                }
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
        let txt = self.get_name()?;
        match txt {
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
            _ => {
                let txt = String::from(txt);
                return self.err(Some(LefParseErrorType::InvalidKey(txt)));
            }
        }
    }
    /// Parse a space-separated x,y [LefPoint] comprising two [LefDecimal]
    fn parse_point(&mut self) -> LefResult<LefPoint> {
        Ok(LefPoint(self.parse_number()?, self.parse_number()?))
    }
    /// Parse a MACRO::PIN definition into a [LefPin]
    fn parse_units(&mut self) -> LefResult<LefUnits> {
        let mut units = LefUnits::default();
        loop {
            let txt = self.get_name()?;
            match txt {
                "DATABASE" => {
                    // Parse the "DATABASE MICRONS" flavor
                    self.expect_name("MICRONS")?;
                    let num = self.parse_number()?;
                    self.expect(TokenType::SemiColon)?;
                    self.expect(TokenType::NewLine)?;
                    units.database_microns = Some(num);
                }
                "END" => {
                    // End of UNITS
                    self.expect_name("UNITS")?;
                    self.expect(TokenType::NewLine)?;
                    break;
                }
                // All the other united quantities are unsupported
                "TIME" | "CAPACITANCE" | "RESISTANCE" | "POWER" | "CURRENT" | "VOLTAGE"
                | "FREQUENCY" => {
                    let txt = String::from(txt);
                    return self.err(Some(LefParseErrorType::UnsupportedKey {
                        key: txt,
                        ctx: LefParseContext::Units,
                    }));
                }
                _ => {
                    let txt = String::from(txt);
                    return self.err(Some(LefParseErrorType::InvalidKey2 {
                        key: txt,
                        ctx: LefParseContext::Units,
                    }));
                }
            }
        }
        Ok(units)
    }
    /// Parse [LefMacro] SYMMETRY options into a vector of [LefSymmetry]
    fn parse_macro_symmetries(&mut self) -> LefResult<Vec<LefSymmetry>> {
        let mut symms = Vec::new();
        loop {
            let tok = self.next_token()?;
            match tok {
                None => return self.err(None),
                Some(t) if t.ttype == TokenType::SemiColon => {
                    // End of symmetries list
                    self.expect(TokenType::NewLine)?;
                    break;
                }
                Some(_) => {
                    // Any other Token must be a [LefSymmetry] variant
                    symms.push(self.parse_enum::<LefSymmetry>()?);
                }
            }
        }
        Ok(symms)
    }
    /// Parse the MACRO::CLASS enumerations
    fn parse_macro_class(&mut self) -> LefResult<LefMacroClass> {
        let txt = self.get_name()?;
        match txt {
            "COVER" => unimplemented!(),
            "RING" => unimplemented!(),
            "BLOCK" => {
                let tp = if self.matches(TokenType::Name) {
                    // If the next token is a name, it must be a valid sub-type
                    match self.get_name()? {
                        "BLACKBOX" => Some(LefBlockClassType::BlackBox),
                        "SOFT" => Some(LefBlockClassType::Soft),
                        _ => return self.err(None),
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
                        _ => return self.err(None),
                    }
                } else {
                    None
                };
                self.expect(TokenType::SemiColon)?;
                self.expect(TokenType::NewLine)?;
                return Ok(LefMacroClass::Core { tp });
            }
            "ENDCAP" => unimplemented!(),
            _ => {
                let txt = String::from(txt);
                return self.err(Some(LefParseErrorType::InvalidKey(txt)));
            }
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
            return self.err(None);
        }
        self.expect(TokenType::SemiColon)?;
        self.expect(TokenType::NewLine)?;
        return Ok((chars[1], chars[2]));
    }
    /// Parse the LefLibrary::DIVIDERCHAR key from a single-character string literal
    fn parse_divider_char(&mut self) -> LefResult<char> {
        let txt = self.expect_and_get_str(TokenType::StringLiteral)?;
        let chars = txt.chars().collect::<Vec<char>>();
        if chars.len() != 3 {
            return self.err(None);
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
    /// Parse an enumerated string-value of type <T>
    fn parse_enum<T: LefEnum>(&mut self) -> LefResult<T> {
        match T::from_str(self.get_name()?) {
            Some(t) => Ok(t),
            None => self.err(None),
        }
    }
    /// Error-Generation Helper
    /// Collect our current position and content into a [LefError::Parse]
    fn err<T>(&self, tp: Option<LefParseErrorType>) -> LefResult<T> {
        // Quick start the line content; just grab, say, 100 characters
        let line_content = self.src[self.lex.linestart..self.lex.linestart + 100].to_string();
        let token = match self.lex.next_tok {
            Some(t) => self.txt(&t),
            None => "EOF",
        }
        .to_string();
        Err(LefError::Parse {
            tp,
            line_content,
            linenum: self.lex.line,
            token,
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
    /// Case-Sensitive Name Setting
    /// FIXME: potentially only some LEF versions
    #[serde(default, skip_serializing_if = "Option::is_none")]
    #[builder(default, setter(strip_option))]
    pub names_case_sensitive: Option<LefOnOff>,
    /// Wire-Extension Pin Settings
    /// FIXME: potentially only some LEF versions
    #[serde(default, skip_serializing_if = "Option::is_none")]
    #[builder(default, setter(strip_option))]
    pub no_wire_extension_at_pin: Option<LefOnOff>,
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
    /// Dimensional Units
    #[serde(default, skip_serializing_if = "Option::is_none")]
    #[builder(default, setter(strip_option))]
    pub units: Option<LefUnits>,
}
/// Lef Macro Definition
#[derive(Default, Clone, Builder, Debug, Deserialize, Serialize, PartialEq, Eq)]
#[builder(setter(into), private)]
pub struct LefMacro {
    // Required
    /// Macro Name
    pub name: String,
    // Optional
    /// Pin List
    #[serde(default, skip_serializing_if = "Vec::is_empty")]
    #[builder(default)]
    pub pins: Vec<LefPin>,
    /// Obstructions
    #[serde(default, skip_serializing_if = "Vec::is_empty")]
    #[builder(default)]
    pub obs: Vec<LefLayerGeometries>,
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
    pub size: Option<(LefDecimal, LefDecimal)>,
    /// Rotational & Translation Symmetries
    #[serde(default, skip_serializing_if = "Option::is_none")]
    #[builder(default, setter(strip_option))]
    pub symmetry: Option<Vec<LefSymmetry>>,
    /// Site Name
    /// Note the optional `sitePattern` is not supported
    #[serde(default, skip_serializing_if = "Option::is_none")]
    #[builder(default, setter(strip_option))]
    pub site: Option<String>,
    /// Density Objects
    #[serde(default, skip_serializing_if = "Option::is_none")]
    #[builder(default, setter(strip_option))]
    pub density: Option<Tbd>,
    /// Properties
    #[serde(default, skip_serializing_if = "Option::is_none")]
    #[builder(default, setter(strip_option))]
    pub properties: Option<Tbd>,
    /// Source
    /// FIXME: supported in earlier versions of LEF only
    #[serde(default, skip_serializing_if = "Option::is_none")]
    #[builder(default, setter(strip_option))]
    pub source: Option<LefDefSource>,
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
    pub shape: Option<LefPinShape>,
    /// Must-Join
    #[serde(default, skip_serializing_if = "Option::is_none")]
    #[builder(default, setter(strip_option))]
    pub must_join: Option<Tbd>,

    /// Antenna
    #[serde(default, skip_serializing_if = "Option::is_none")]
    #[builder(default, setter(strip_option))]
    pub antenna_partial_metal_area: Option<LefDecimal>,
    #[serde(default, skip_serializing_if = "Option::is_none")]
    #[builder(default, setter(strip_option))]
    pub antenna_partial_metal_side_area: Option<LefDecimal>,
    #[serde(default, skip_serializing_if = "Option::is_none")]
    #[builder(default, setter(strip_option))]
    pub antenna_partial_cut_area: Option<LefDecimal>,
    #[serde(default, skip_serializing_if = "Option::is_none")]
    #[builder(default, setter(strip_option))]
    pub antenna_diff_area: Option<LefDecimal>,
    #[serde(default, skip_serializing_if = "Option::is_none")]
    #[builder(default, setter(strip_option))]
    pub antenna_model: Option<LefDecimal>,
    #[serde(default, skip_serializing_if = "Option::is_none")]
    #[builder(default, setter(strip_option))]
    pub antenna_gate_area: Option<LefDecimal>,
    #[serde(default, skip_serializing_if = "Option::is_none")]
    #[builder(default, setter(strip_option))]
    pub antenna_max_area_char: Option<LefDecimal>,
    #[serde(default, skip_serializing_if = "Option::is_none")]
    #[builder(default, setter(strip_option))]
    pub antenna_side_area_char: Option<LefDecimal>,
    #[serde(default, skip_serializing_if = "Option::is_none")]
    #[builder(default, setter(strip_option))]
    pub antenna_max_cut_char: Option<LefDecimal>,
}
#[derive(Clone, Debug, Deserialize, Serialize, PartialEq, Eq)]
pub enum LefPinDirection {
    Input,
    Output { tristate: bool },
    Inout,
    FeedThru,
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
/// Measurement Unit Conversion Factors
#[derive(Clone, Default, Debug, Deserialize, Serialize, PartialEq, Eq)]
pub struct LefUnits {
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub database_microns: Option<LefDecimal>,
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub time_ns: Option<LefDecimal>,
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub capacitance_pf: Option<LefDecimal>,
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub resistance_ohms: Option<LefDecimal>,
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub power_mw: Option<LefDecimal>,
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub current_ma: Option<LefDecimal>,
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub voltage_volts: Option<LefDecimal>,
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub frequency_mhz: Option<LefDecimal>,
}
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
        let src = "STUFF 101 ; \n # commentary \n";
        let lex = LefLexer::new(src)?;
        let toks_vec: Vec<Token> = lex.collect(); // Collect up all tokens
        let tok_strs: Vec<&str> = toks_vec.iter().map(|t| t.substr(src)).collect();
        assert_eq!(tok_strs, vec!["STUFF", "101", ";", "\n",]);
        Ok(())
    }
    #[test]
    fn it_parses() -> TestResult {
        let src = r#"
            VERSION 5 ; # commentary 
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
/// Lef String-Enumeration Trait
/// Defines two central methods:
/// * `to_str(&self) -> &'static str` converts the enum to its Lef-String values.
/// * `from_str(&str) -> Option<Self>` does the opposite, returning an [Option] indicator of success or failure.
trait LefEnum: std::marker::Sized {
    fn to_str(&self) -> &'static str;
    fn from_str(txt: &str) -> Option<Self>;
}
/// Macro for creating `enum`s which:
/// * (a) Have paired string-values, as commonly arrive in enumerated LEF fields such as "ON" / "OFF", "CLASS", etc, and
/// * (b) Automatically implement the [LefEnum] trait for conversions to and from these strings.
/// All variants are fieldless, and include derived implementations of common traits notably including `serde::{Serialize,Deserialize}`.
macro_rules! enumstr {
    (   $(#[$meta:meta])*
        $enum_name:ident {
        $( $variant:ident : $strval:literal ),* $(,)?
    }) => {
        $(#[$meta])*
        #[allow(dead_code)]
        #[derive(Clone, Debug, Deserialize, Serialize, PartialEq, Eq)]
        pub enum $enum_name {
            $( #[doc=$strval]
                $variant ),*
        }
        impl LefEnum for $enum_name {
            /// Convert a [$enum_name] to the (static) string-keyword used in the Lef format
            #[allow(dead_code)]
            fn to_str(&self) -> &'static str {
                match self {
                    $( Self::$variant => $strval),*,
                }
            }
            /// Create a [$enum_name] from one of the string-values specified in the Lef format.
            /// Returns `None` if input `txt` does not match one of [$enum_name]'s variants.
            fn from_str(txt: &str) -> Option<Self> {
                match txt {
                    $( $strval => Some(Self::$variant)),*,
                    _ => None,
                }
            }
        }
    }
}
enumstr!(
    /// Binary On/Off Settings, Denoted by ON and OFF
    LefOnOff {
        On: "ON",
        Off: "OFF",
    }
);
enumstr!(
    /// Specifies the source of a component
    /// In all versions since at least 5.7 (2009), SOURCE is a DEF-only field on COMPONENT definitions.
    /// Prior versions also include this as a field for LEF MACRO definitions.
    LefDefSource {
        Netlist: "NETLIST",
        Dist: "DIST",
        Timing: "TIMING",
        User: "USER",
    }
);
enumstr!(
    /// Specifies which MACRO orientations are valid for placement
    LefSymmetry {
        X: "X",
        Y: "Y",
        R90: "R90"
    }
);
enumstr!(
    /// Specifies the usage-intent for a pin.
    /// Note this is the noun form of "use", pronounced with the hard "s".
    /// Not the verb form pronounced like the New Jersey second-person plural "yous".
    LefPinUse {
        Signal: "SIGNAL",
        Analog: "ANALOG",
        Power: "POWER",
        Ground: "GROUND",
        Clock: "CLOCK",
    }
);
enumstr!(
    /// Specifies a pin with special connection requirements because of its shape
    LefPinShape {
        Abutment: "ABUTMENT",
        Ring: "RING",
        FeedThru: "FEEDTHRU",
    }
);
