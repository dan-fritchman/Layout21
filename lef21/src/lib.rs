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
    /// FIXME: better name here!
    fn advance(&mut self) -> LefResult<Option<Token>> {
        use TokenType::{Comment, NewLine, WhiteSpace};
        loop {
            match self.lex_one()? {
                None => return Ok(None),
                Some(t) => match t.ttype {
                    WhiteSpace | Comment | NewLine => continue, // White-space and comments are not emitted
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
    /// Lex the next [Token]
    fn lex_one(&mut self) -> LefResult<Option<Token>> {
        if self.peek_char().is_none() {
            return Ok(None); // All done! End of input. Return `None`.
        }
        if self.accept_char('\n') {
            return self.lex_newline();
        }
        if self.accept(char::is_whitespace) {
            return self.lex_whitespace();
        }
        if self.accept_char(';') {
            return Ok(Some(self.emit(TokenType::SemiColon)));
        }
        if self.accept_char('"') {
            return self.lex_string_literal();
        }
        if self.accept_char('#') {
            return self.lex_comment();
        }
        if self.accept(|c| c.is_digit(10) || c == '-') {
            return self.lex_number();
        }
        if self.accept(char::is_alphabetic) {
            return self.lex_name();
        }
        return self.err(); // Some other, invalid character. Fail.
    }
    /// Lex newlines, incrementing our line-number
    fn lex_newline(&mut self) -> LefResult<Option<Token>> {
        let tok = self.emit(TokenType::NewLine);
        self.line += 1;
        self.linestart = self.pos;
        self.at_bol = true;
        return Ok(Some(tok));
    }
    /// Lex whitespace
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
        // Accept everything until the next white-space
        while self.accept(|c| !c.is_whitespace()) {
            continue;
        }
        Ok(Some(self.emit(TokenType::Name)))
    }
    /// Error-Generation Helper
    /// Collect our current position and content into a [LefError::Lex]
    fn err<T>(&self) -> LefResult<T> {
        Err(LefError::Lex {
            next_char: self.peek_char().clone(),
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
#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
pub enum TokenType {
    Name,
    Number,
    SemiColon,
    StringLiteral,
    NewLine,
    WhiteSpace,
    Comment,
    End,
}
/// Enumerated parsing contexts, largely for capturing errors
#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
pub enum LefParseContext {
    Library,
    Macro,
    Pin,
    Port,
    Geometry,
    Site,
    Units,
    Unknown,
}
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub enum LefParseErrorType {
    /// Unsupported (But Spec-Valid) Features
    Unsupported,
    /// Invalid Key
    InvalidKey,
    /// Invalid Value
    InvalidValue,
    /// Invalid Token
    InvalidToken { expected: TokenType },
    /// Syntax Error: missing keyword or identifier at a required location
    RequiredWord { expected: String },
    /// All other errors
    Other,
}
/// Lef Error Enumeration
#[derive(Debug)]
pub enum LefError {
    /// Lexer Errors
    Lex {
        next_char: Option<char>,
        line: usize,
        pos: usize,
    },
    /// Parser Errors
    Parse {
        tp: LefParseErrorType,
        ctx: LefParseContext,
        token: String,
        line_content: String,
        line_num: usize,
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
    /// Context Stack
    ctx_stack: Vec<LefParseContext>,
}
impl<'src> LefParser<'src> {
    /// Construct a [LefParser] of input-text `src`
    fn new(src: &'src str) -> LefResult<Self> {
        let lex = LefLexer::new(src)?;
        Ok(Self {
            src,
            lex,
            ctx_stack: Vec::new(),
        })
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
    fn matches(&self, ttype: TokenType) -> bool {
        match self.peek_token() {
            Some(t) if t.ttype == ttype => true,
            _ => false,
        }
    }
    /// An extremely common set of internal operations which:
    /// * Asserts the next token is of type [TokenType::Name]
    /// * Coverts that to a "LEF-keyword-compatible" string - i.e. and all-uppercase string
    /// * *Does not* advance the lexer
    #[inline(always)]
    fn peek_key(&self) -> LefResult<String> {
        match self.peek_token() {
            Some(tok) if tok.ttype == TokenType::Name => Ok(self.txt(&tok).to_ascii_uppercase()),
            None | Some(_) => self.err(LefParseErrorType::InvalidToken {
                expected: TokenType::Name,
            }),
        }
    }
    /// Assert the expectation that the next [Token] is of [TokenType] `ttype`.
    /// Returns the [Token] if so. Returns an [Err] if not.
    fn expect(&mut self, ttype: TokenType) -> LefResult<Token> {
        let tok = self.next_token()?;
        match tok {
            // Good, usual case
            Some(t) if t.ttype == ttype => return Ok(t),
            // Error handling & helpers
            Some(_) => self.err(LefParseErrorType::InvalidToken { expected: ttype }),
            None => self.err(LefParseErrorType::InvalidToken { expected: ttype }),
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
    /// Assert that the next [Token] is a [TokenType::Name], and that its string value matches `kw`.
    /// Note that LEF keywords are case-insensitive. The next [Token] is converted to uppercase before comparison,
    /// where the keyword `kw` is expected to be provided in uppercase by the caller.
    fn expect_keyword(&mut self, kw: &str) -> LefResult<()> {
        let txt = self.get_name()?.to_ascii_uppercase();
        if txt == kw {
            Ok(())
        } else {
            self.err(LefParseErrorType::RequiredWord {
                expected: String::from(kw),
            })
        }
    }
    /// Assert that the next [Token] is a [TokenType::Name], and that its string value matches `ident`.
    /// Unlike [LefParser::expect_keyword], this function matches literally, and does not convert to uppercase.
    fn expect_ident(&mut self, ident: &str) -> LefResult<()> {
        let txt = self.get_name()?;
        if txt == ident {
            Ok(())
        } else {
            self.err(LefParseErrorType::RequiredWord {
                expected: String::from(ident),
            })
        }
    }
    /// Retrieve the text-content of lexer [Token] `tok`
    #[inline(always)]
    fn txt(&self, tok: &Token) -> &str {
        tok.substr(self.src)
    }
    /// Parse a [LefLibrary]
    fn parse_lib(&mut self) -> LefResult<LefLibrary> {
        self.ctx_stack.push(LefParseContext::Library);
        let mut lib = LefLibraryBuilder::default();
        let mut macros = Vec::new();
        let mut sites = Vec::new();
        loop {
            match self.peek_key()?.as_str() {
                "MACRO" => {
                    macros.push(self.parse_macro()?);
                }
                "VERSION" => {
                    self.advance()?; // Eat the "VERSION" key
                    lib.version(self.parse_number()?);
                    self.expect(TokenType::SemiColon)?;
                }
                "BUSBITCHARS" => {
                    lib.bus_bit_chars(self.parse_bus_bit_chars()?);
                }
                "DIVIDERCHAR" => {
                    lib.divider_char(self.parse_divider_char()?);
                }
                "NAMESCASESENSITIVE" => {
                    self.advance()?; // Eat the "NAMESCASESENSITIVE" key
                    lib.names_case_sensitive(self.parse_enum::<LefOnOff>()?);
                    self.expect(TokenType::SemiColon)?;
                }
                "NOWIREEXTENSIONATPIN" => {
                    self.advance()?; // Eat the "NOWIREEXTENSIONATPIN" key
                    lib.no_wire_extension_at_pin(self.parse_enum::<LefOnOff>()?);
                    self.expect(TokenType::SemiColon)?;
                }
                "UNITS" => {
                    lib.units(self.parse_units()?);
                }
                "SITE" => {
                    sites.push(self.parse_site_def()?);
                }
                "END" => {
                    self.advance()?; // Expect "END LIBRARY".
                    self.expect_keyword("LIBRARY")?;
                    break;
                }
                "BEGINEXT" => return self.err(LefParseErrorType::Unsupported),
                _ => return self.err(LefParseErrorType::InvalidKey),
            }
        }
        lib.macros(macros);
        lib.sites(sites);
        self.ctx_stack.pop();
        Ok(lib.build()?)
    }
    /// Parse a Lef MACRO definition
    fn parse_macro(&mut self) -> LefResult<LefMacro> {
        self.expect_keyword("MACRO")?;
        self.ctx_stack.push(LefParseContext::Macro);
        let mut mac = LefMacroBuilder::default();
        // Parse the macro-name
        let name = self.parse_ident()?;
        mac.name(name.clone());
        // Start parsing attributes, pins, and obstructions
        let mut pins = Vec::new();
        loop {
            match self.peek_key()?.as_str() {
                "CLASS" => {
                    mac.class(self.parse_macro_class()?);
                }
                "SITE" => {
                    self.advance()?; // Eat the "SITE" key
                    mac.site(self.parse_ident()?);
                    self.expect(TokenType::SemiColon)?;
                }
                "FOREIGN" => {
                    self.advance()?; // Eat the "FOREIGN" key
                    let cell_name = self.parse_ident()?;
                    let mut pt = None;
                    if !self.matches(TokenType::SemiColon) {
                        pt = Some(self.parse_point()?);
                    }
                    // FIXME: the optional `orient` field is not supported
                    self.expect(TokenType::SemiColon)?;
                    mac.foreign(LefForeign {
                        cell_name,
                        pt,
                        orient: None,
                    });
                }
                "ORIGIN" => {
                    self.advance()?; // Eat the "ORIGIN" key
                    mac.origin(self.parse_point()?);
                    self.expect(TokenType::SemiColon)?;
                }
                "SIZE" => {
                    mac.size(self.parse_size()?);
                }
                "PIN" => {
                    pins.push(self.parse_pin()?);
                }
                "OBS" => {
                    mac.obs(self.parse_obstructions()?);
                }
                "SYMMETRY" => {
                    mac.symmetry(self.parse_symmetries()?);
                }
                "SOURCE" => {
                    // FIXME: only supported in some LEF versions, sort out how to handle this fact
                    self.advance()?; // Eat the "SOURCE" key
                    mac.source(self.parse_enum::<LefDefSource>()?);
                    self.expect(TokenType::SemiColon)?;
                }
                "END" => {
                    self.advance()?; // End of Macro. Eat the "END" key
                    break;
                }
                _ => return self.err(LefParseErrorType::InvalidKey),
            }
        }
        // Parse the END-enclosing macro-name
        self.expect_ident(&name)?;
        // Set the pins, build our struct and return it
        mac.pins(pins);
        self.ctx_stack.pop();
        Ok(mac.build()?)
    }
    /// Parse a MACRO::PIN definition into a [LefPin]
    fn parse_pin(&mut self) -> LefResult<LefPin> {
        self.expect_keyword("PIN")?;
        self.ctx_stack.push(LefParseContext::Pin);
        let mut pin = LefPinBuilder::default();
        // Parse the pin-name
        let name = self.parse_ident()?;
        pin.name(name.clone());
        let mut ports = Vec::new();
        let mut antenna_attrs = Vec::new();
        loop {
            match self.peek_key()?.as_str() {
                "END" => {
                    self.advance()?; // End of Pin. Eat the "END" key.
                    break;
                }
                "PORT" => {
                    ports.push(self.parse_port()?);
                }
                "DIRECTION" => {
                    pin.direction(self.parse_pin_direction()?);
                }
                "USE" => {
                    self.advance()?;
                    pin.r#use(self.parse_enum::<LefPinUse>()?);
                    self.expect(TokenType::SemiColon)?;
                }
                "SHAPE" => {
                    self.advance()?;
                    pin.shape(self.parse_enum::<LefPinShape>()?);
                    self.expect(TokenType::SemiColon)?;
                }
                "ANTENNAMODEL" => {
                    self.advance()?;
                    pin.antenna_model(self.parse_enum::<LefAntennaModel>()?);
                }
                "ANTENNADIFFAREA"
                | "ANTENNAGATEAREA"
                | "ANTENNAPARTIALMETALAREA"
                | "ANTENNAPARTIALMETALSIDEAREA"
                | "ANTENNAPARTIALCUTAREA"
                | "ANTENNAPARTIALDIFFAREA"
                | "ANTENNAMAXAREACAR"
                | "ANTENNAMAXSIDEAREACAR"
                | "ANTENNAMAXCUTCAR" => {
                    let key = self.parse_ident()?;
                    let val = self.parse_number()?;
                    let mut layer = None;
                    if !self.matches(TokenType::SemiColon) {
                        self.expect_keyword("LAYER")?;
                        layer = Some(self.parse_ident()?);
                    }
                    antenna_attrs.push(LefPinAntennaAttr { key, val, layer });
                    self.expect(TokenType::SemiColon)?;
                }
                "TAPERRULE" | "NETEXPR" | "SUPPLYSENSITIVITY" | "GROUNDSENSITIVITY"
                | "MUSTJOIN" | "PROPERTY" => return self.err(LefParseErrorType::Unsupported),
                _ => return self.err(LefParseErrorType::InvalidKey),
            }
        }
        // Get the pin-closing "END <name>"
        self.expect_ident(&name)?;
        // Set our port-objects, build and return the Pin
        pin.ports(ports);
        pin.antenna_attrs(antenna_attrs);
        self.ctx_stack.pop();
        Ok(pin.build()?)
    }
    /// Parse a [LefPinDirection]
    fn parse_pin_direction(&mut self) -> LefResult<LefPinDirection> {
        self.expect_keyword("DIRECTION")?;
        let pin = match self.get_name()?.to_ascii_uppercase().as_str() {
            "INPUT" => LefPinDirection::Input,
            "FEEDTHRU" => LefPinDirection::FeedThru,
            "INOUT" => LefPinDirection::Inout,
            "OUTPUT" => {
                // Outputs include an optional tri-state modifier
                let mut tristate = false;
                if !self.matches(TokenType::SemiColon) {
                    self.expect_keyword("TRISTATE")?;
                    tristate = true;
                }
                LefPinDirection::Output { tristate }
            }
            _ => return self.err(LefParseErrorType::InvalidValue),
        };
        self.expect(TokenType::SemiColon)?;
        Ok(pin)
    }
    /// Parse a MACRO::PIN::PORT definition into a [LefPort]
    fn parse_port(&mut self) -> LefResult<LefPort> {
        self.expect_keyword("PORT")?;
        self.ctx_stack.push(LefParseContext::Port);
        let mut class: Option<LefPortClass> = None;
        let mut layers = Vec::new();
        // Parse attributes and geometries
        // Note this peeks rather than taking the next token,
        // largely to accommodate the closing-delimeter-free "LAYER" / [LefLayerGeometries] definitions.
        // Other keys generally advance by a Token *after* matching.
        loop {
            match self.peek_key()?.as_str() {
                "CLASS" => {
                    self.advance()?; // Eat the CLASS Token
                    class = Some(self.parse_enum::<LefPortClass>()?);
                    self.expect(TokenType::SemiColon)?;
                }
                "LAYER" => {
                    layers.push(self.parse_layer_geometries()?);
                }
                "END" => {
                    self.advance()?; // Eat the END Token
                    break;
                }
                _ => return self.err(LefParseErrorType::InvalidKey),
            }
        }
        self.ctx_stack.pop();
        Ok(LefPort { layers, class })
    }
    /// Parse a [LefMacro]'s obstruction definitions
    fn parse_obstructions(&mut self) -> LefResult<Vec<LefLayerGeometries>> {
        self.expect_keyword("OBS")?;
        let mut geoms = Vec::new();
        while let Some(t) = self.peek_token() {
            if t.ttype != TokenType::Name {
                return self.err(LefParseErrorType::InvalidToken {
                    expected: TokenType::Name,
                });
            }
            let txt = self.txt(&t);
            match txt.to_ascii_uppercase().as_str() {
                "LAYER" => {
                    geoms.push(self.parse_layer_geometries()?);
                }
                "END" => {
                    self.advance()?; // Eat the END Token
                    break;
                }
                _ => return self.err(LefParseErrorType::InvalidKey),
            }
        }
        Ok(geoms)
    }
    /// Parse a set of geometries on a single layer, as commonly specified per-[LefPort]
    fn parse_layer_geometries(&mut self) -> LefResult<LefLayerGeometries> {
        self.ctx_stack.push(LefParseContext::Geometry);
        let mut layer = LefLayerGeometriesBuilder::default();
        // Check for the opening "LAYER" keyword
        self.expect_keyword("LAYER")?;
        // Parse the layer-name
        layer.layer_name(self.parse_ident()?);
        // Parse the options defined inline with the LAYER statement
        while !self.matches(TokenType::SemiColon) {
            match self.get_name()?.to_ascii_uppercase().as_str() {
                "EXCEPTPGNET" => {
                    layer.except_pg_net(true);
                }
                "SPACING" => {
                    layer.spacing(LefLayerSpacing::Spacing(self.parse_number()?));
                }
                "DESIGNRULEWIDTH" => {
                    layer.spacing(LefLayerSpacing::DesignRuleWidth(self.parse_number()?));
                }
                _ => return self.err(LefParseErrorType::InvalidKey),
            }
        }
        self.expect(TokenType::SemiColon)?;
        let mut geoms = Vec::new();
        let mut vias = Vec::new();
        // LayerGeometries don't have an END card, so this needs to peek at the next token,
        // and exit when another LAYER or END (of a higher-level thing) turn up.
        // Note that on end-of-file, i.e. `peek_token` returning `None`,
        // this will exit and return a valid [LefLayerGeometries].
        // (Objects above it in the tree may error instead.)
        while let Some(t) = self.peek_token() {
            if t.ttype != TokenType::Name {
                return self.err(LefParseErrorType::InvalidToken {
                    expected: TokenType::Name,
                });
            }
            let txt = self.txt(&t);
            match txt.to_ascii_uppercase().as_str() {
                "LAYER" | "END" => break, // End of geometries. (Really start/end of something else.)
                "PATH" | "POLYGON" | "RECT" => {
                    geoms.push(self.parse_geometry()?);
                }
                "VIA" => {
                    self.advance()?; // Eat the VIA Token
                    if self.matches(TokenType::Name) {
                        // FIXME: the VIA ITERATE construction is not supported.
                        return self.err(LefParseErrorType::Unsupported);
                    }
                    let pt = self.parse_point()?;
                    let via_name = self.parse_ident()?;
                    self.expect(TokenType::SemiColon)?;
                    vias.push(LefVia { pt, via_name });
                }
                "WIDTH" => {
                    self.advance()?; // Eat the WIDTH Token
                    layer.width(self.parse_number()?);
                    self.expect(TokenType::SemiColon)?;
                }
                _ => return self.err(LefParseErrorType::InvalidKey),
            }
        }
        layer.vias(vias);
        layer.geometries(geoms);
        self.ctx_stack.pop();
        Ok(layer.build()?)
    }
    /// Parse a [LefGeometry] statement
    /// Each can be a shape or iteration thereof
    fn parse_geometry(&mut self) -> LefResult<LefGeometry> {
        match self.get_name()?.to_ascii_uppercase().as_str() {
            "RECT" => {
                if self.matches(TokenType::Name) {
                    // FIXME: the VIA ITERATE construction is not supported.
                    return self.err(LefParseErrorType::Unsupported);
                    // // Parse an optional MASK field
                    // self.expect_keyword("MASK")?;
                    // let mask_num = self.parse_ident()?;
                    // self.expect_keyword("ITERATE")?;
                    // let mask_num = self.parse_ident()?;
                }
                // Parse the two points
                let p1 = self.parse_point()?;
                let p2 = self.parse_point()?;
                self.expect(TokenType::SemiColon)?;
                // And return the Rect
                Ok(LefGeometry::Shape(LefShape::Rect(p1, p2)))
            }
            "POLYGON" => {
                let mut points = Vec::new();
                // Each valid polygon must have at least 3 points. Parse them outside our loop.
                points.push(self.parse_point()?);
                points.push(self.parse_point()?);
                points.push(self.parse_point()?);
                // Now parse any additional points, stopping on the semicolon
                while !self.matches(TokenType::SemiColon) {
                    points.push(self.parse_point()?);
                }
                self.expect(TokenType::SemiColon)?;
                // And return the Polygon
                Ok(LefGeometry::Shape(LefShape::Polygon(points)))
            }
            "PATH" => return self.err(LefParseErrorType::Unsupported),
            _ => return self.err(LefParseErrorType::InvalidKey),
        }
    }
    /// Parse a space-separated x,y [LefPoint] comprising two [LefDecimal]
    fn parse_point(&mut self) -> LefResult<LefPoint> {
        Ok(LefPoint(self.parse_number()?, self.parse_number()?))
    }
    /// Parse [LefUnits] definitions
    fn parse_units(&mut self) -> LefResult<LefUnits> {
        self.expect_keyword("UNITS")?;
        self.ctx_stack.push(LefParseContext::Units);
        let mut units = LefUnits::default();
        loop {
            match self.get_name()?.to_ascii_uppercase().as_str() {
                "DATABASE" => {
                    // Parse the "DATABASE MICRONS" flavor
                    self.expect_keyword("MICRONS")?;
                    let num = self.parse_number()?;
                    self.expect(TokenType::SemiColon)?;
                    units.database_microns = Some(num);
                }
                "END" => {
                    // End of UNITS
                    self.expect_keyword("UNITS")?;
                    break;
                }
                // All the other united quantities are unsupported
                "TIME" | "CAPACITANCE" | "RESISTANCE" | "POWER" | "CURRENT" | "VOLTAGE"
                | "FREQUENCY" => return self.err(LefParseErrorType::Unsupported),
                _ => return self.err(LefParseErrorType::InvalidKey),
            }
        }
        self.ctx_stack.pop();
        Ok(units)
    }
    /// Parse [LefMacro] SYMMETRY options into a vector of [LefSymmetry]
    fn parse_symmetries(&mut self) -> LefResult<Vec<LefSymmetry>> {
        self.expect_keyword("SYMMETRY")?;
        let mut symms = Vec::new();
        while !self.matches(TokenType::SemiColon) {
            symms.push(self.parse_enum::<LefSymmetry>()?);
        }
        self.expect(TokenType::SemiColon)?;
        Ok(symms)
    }
    /// Parse the MACRO::CLASS type-enumeration and its sub-types into a [LefMacroClass]
    fn parse_macro_class(&mut self) -> LefResult<LefMacroClass> {
        self.expect_keyword("CLASS")?;
        match self.get_name()?.to_ascii_uppercase().as_str() {
            "BLOCK" => {
                let mut tp = None;
                if !self.matches(TokenType::SemiColon) {
                    tp = Some(self.parse_enum::<LefBlockClassType>()?);
                }
                self.expect(TokenType::SemiColon)?;
                Ok(LefMacroClass::Block { tp })
            }
            "PAD" => {
                let mut tp = None;
                if !self.matches(TokenType::SemiColon) {
                    tp = Some(self.parse_enum::<LefPadClassType>()?);
                }
                self.expect(TokenType::SemiColon)?;
                Ok(LefMacroClass::Pad { tp })
            }
            "CORE" => {
                let mut tp = None;
                if !self.matches(TokenType::SemiColon) {
                    tp = Some(self.parse_enum::<LefCoreClassType>()?);
                }
                self.expect(TokenType::SemiColon)?;
                Ok(LefMacroClass::Core { tp })
            }
            "ENDCAP" => {
                // Note unlike all other types, an ENDCAP sub-type is *required*.
                let tp = self.parse_enum::<LefEndCapClassType>()?;
                self.expect(TokenType::SemiColon)?;
                Ok(LefMacroClass::EndCap { tp })
            }
            "COVER" => {
                let mut bump = false;
                if !self.matches(TokenType::SemiColon) {
                    self.expect_keyword("BUMP")?;
                    bump = true;
                }
                self.expect(TokenType::SemiColon)?;
                Ok(LefMacroClass::Cover { bump })
            }
            "RING" => {
                self.expect(TokenType::SemiColon)?;
                Ok(LefMacroClass::Ring)
            }
            _ => return self.err(LefParseErrorType::InvalidValue),
        }
    }
    /// Parse a [LefSite] definition
    fn parse_site_def(&mut self) -> LefResult<LefSite> {
        self.expect_keyword("SITE")?;
        self.ctx_stack.push(LefParseContext::Site);
        let mut site = LefSiteBuilder::default();
        // Parse the site name. Keep a copy for later comparison.
        let name = self.parse_ident()?;
        site.name(name.clone());
        loop {
            match self.peek_key()?.as_str() {
                "END" => {
                    self.advance()?; // Eat the "END"
                    self.expect_ident(&name)?;
                    break;
                }
                "CLASS" => {
                    self.advance()?; // Eat the "CLASS"
                    site.class(self.parse_enum::<LefSiteClass>()?);
                    self.expect(TokenType::SemiColon)?;
                }
                "SYMMETRY" => {
                    site.symmetry(self.parse_symmetries()?);
                }
                "SIZE" => {
                    site.size(self.parse_size()?);
                }
                "ROWPATTERN" => return self.err(LefParseErrorType::Unsupported),
                _ => return self.err(LefParseErrorType::InvalidValue),
            }
        }
        self.ctx_stack.pop();
        Ok(site.build()?)
    }
    /// Parse the Lef SIZE statement into an (x, y) pair of [LefDecimal]s
    fn parse_size(&mut self) -> LefResult<(LefDecimal, LefDecimal)> {
        self.expect_keyword("SIZE")?;
        let x = self.parse_number()?;
        self.expect_keyword("BY")?;
        let y = self.parse_number()?;
        self.expect(TokenType::SemiColon)?;
        Ok((x, y))
    }
    /// Parse the next token into a [LefDecimal] number
    fn parse_number(&mut self) -> LefResult<LefDecimal> {
        let tok = self.expect(TokenType::Number)?;
        let txt = self.txt(&tok);
        Ok(LefDecimal::from_str(txt)?)
    }
    /// Parse the LefLibrary::BUSBITCHARS key from a two-character string literal
    fn parse_bus_bit_chars(&mut self) -> LefResult<(char, char)> {
        self.expect_keyword("BUSBITCHARS")?;
        let txt = self.expect_and_get_str(TokenType::StringLiteral)?;
        let chars = txt.chars().collect::<Vec<char>>();
        if chars.len() != 4 {
            return self.err(LefParseErrorType::InvalidValue);
        }
        self.expect(TokenType::SemiColon)?;
        return Ok((chars[1], chars[2]));
    }
    /// Parse the LefLibrary::DIVIDERCHAR key from a single-character string literal
    fn parse_divider_char(&mut self) -> LefResult<char> {
        self.expect_keyword("DIVIDERCHAR")?;
        let txt = self.expect_and_get_str(TokenType::StringLiteral)?;
        let chars = txt.chars().collect::<Vec<char>>();
        if chars.len() != 3 {
            return self.err(LefParseErrorType::InvalidValue);
        }
        self.expect(TokenType::SemiColon)?;
        Ok(chars[1])
    }
    /// Parse an identifier name, e.g. a macro, pin, or layer name.
    fn parse_ident(&mut self) -> LefResult<String> {
        let txt = self.get_name()?;
        Ok(String::from(txt))
    }
    /// Parse an enumerated string-value of type <T>
    fn parse_enum<T: LefEnum>(&mut self) -> LefResult<T> {
        let txt = self.get_name()?;
        match T::from_str(&txt.to_ascii_uppercase()) {
            Some(t) => Ok(t),
            None => self.err(LefParseErrorType::InvalidValue),
        }
    }
    /// Error-Generation Helper
    /// Collect our current position and content into a [LefError::Parse]
    fn err<T>(&self, tp: LefParseErrorType) -> LefResult<T> {
        // Create a string repr of the current token
        let token = match self.lex.next_tok {
            Some(t) => self.txt(&t),
            None => "EOF",
        }
        .to_string();
        // Grab the current parsing-context from our stack
        let ctx = match self.ctx_stack.last() {
            Some(c) => c.clone(),
            None => LefParseContext::Unknown,
        };
        // Sort out the content on our current line, by finding the next newline
        const MAX_CHARS_IN_LINE: usize = 200;
        let mut chars = self.lex.chars.clone();
        let mut line_end = self.lex.linestart;
        for _ in 0..MAX_CHARS_IN_LINE {
            match chars.next() {
                None => break,
                Some(c) if c == '\n' => break,
                Some(_) => line_end += 1,
            }
        }
        let line_content = self.src[self.lex.linestart..line_end].to_string();
        Err(LefError::Parse {
            tp,
            ctx,
            line_content,
            line_num: self.lex.line,
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
    /// Macro Definitions
    #[serde(default, skip_serializing_if = "Vec::is_empty")]
    pub macros: Vec<LefMacro>,
    #[serde(default, skip_serializing_if = "Vec::is_empty")]
    pub sites: Vec<LefSite>,

    // Optional
    /// Lef Spec Version
    #[serde(default, skip_serializing_if = "Option::is_none")]
    #[builder(default, setter(strip_option))]
    pub version: Option<LefDecimal>,
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
    /// Dimensional Units
    #[serde(default, skip_serializing_if = "Option::is_none")]
    #[builder(default, setter(strip_option))]
    pub units: Option<LefUnits>,

    // Not (Yet) Supported
    /// Via Definitions
    #[serde(default, skip_serializing_if = "Option::is_none")]
    #[builder(default, setter(strip_option))]
    pub vias: Option<Tbd>,
}
/// Lef Macro Definition
#[derive(Default, Clone, Builder, Debug, Deserialize, Serialize, PartialEq, Eq)]
#[builder(setter(into), private)]
pub struct LefMacro {
    // Required
    /// Macro Name
    pub name: String,
    /// Pin List
    #[serde(default, skip_serializing_if = "Vec::is_empty")]
    #[builder(default)]
    pub pins: Vec<LefPin>,
    /// Obstructions
    #[serde(default, skip_serializing_if = "Vec::is_empty")]
    #[builder(default)]
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
    /// Source
    /// FIXME: supported in earlier versions of LEF only
    #[serde(default, skip_serializing_if = "Option::is_none")]
    #[builder(default, setter(strip_option))]
    pub source: Option<LefDefSource>,

    // Not (Yet) Supported
    /// Electrically-Equivalent Cell
    #[serde(default, skip_serializing_if = "Option::is_none")]
    #[builder(default, setter(strip_option))]
    pub eeq: Option<Tbd>,
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
    /// Shape
    #[serde(default, skip_serializing_if = "Option::is_none")]
    #[builder(default, setter(strip_option))]
    pub shape: Option<LefPinShape>,
    /// Antenna Model
    #[serde(default, skip_serializing_if = "Option::is_none")]
    #[builder(default, setter(strip_option))]
    pub antenna_model: Option<LefAntennaModel>,
    /// Antenna Attributes
    #[serde(default, skip_serializing_if = "Vec::is_empty")]
    pub antenna_attrs: Vec<LefPinAntennaAttr>,

    // Not (Yet) Supported
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
    /// Must-Join
    #[serde(default, skip_serializing_if = "Option::is_none")]
    #[builder(default, setter(strip_option))]
    pub must_join: Option<Tbd>,
}
#[derive(Clone, Debug, Deserialize, Serialize, PartialEq, Eq)]
pub enum LefPinDirection {
    Input,
    Output { tristate: bool },
    Inout,
    FeedThru,
}
/// Antenna Attributes
///
/// Stored as key-value pairs from string-keys name "ANTENNA*" to [LefDecimal] values.
/// Note each pair may have an optional `layer` specifier,
/// and that each key may have multiple attributes, generally specifying different layers.
#[derive(Clone, Debug, Deserialize, Serialize, PartialEq, Eq)]
pub struct LefPinAntennaAttr {
    key: String,
    val: LefDecimal,
    layer: Option<String>,
}
#[derive(Clone, Builder, Debug, Deserialize, Serialize, PartialEq, Eq)]
pub struct LefPort {
    /// Layers & Geometries
    pub layers: Vec<LefLayerGeometries>,
    /// Port-Class
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub class: Option<LefPortClass>,
}
#[derive(Clone, Builder, Debug, Deserialize, Serialize, PartialEq, Eq)]
pub struct LefLayerGeometries {
    // Required
    /// Layer Name
    pub layer_name: String,
    /// Geometries
    #[serde(default, skip_serializing_if = "Vec::is_empty")]
    pub geometries: Vec<LefGeometry>,
    /// Vias
    #[serde(default, skip_serializing_if = "Vec::is_empty")]
    pub vias: Vec<LefVia>,

    // Optional
    #[serde(default, skip_serializing_if = "Option::is_none")]
    #[builder(default, setter(strip_option))]
    pub except_pg_net: Option<bool>,
    #[serde(default, skip_serializing_if = "Option::is_none")]
    #[builder(default, setter(strip_option))]
    pub spacing: Option<LefLayerSpacing>,
    #[serde(default, skip_serializing_if = "Option::is_none")]
    #[builder(default, setter(strip_option))]
    pub width: Option<LefDecimal>,
}
#[derive(Clone, Debug, Deserialize, Serialize, PartialEq, Eq)]
pub struct LefVia {
    /// Via-Type Name
    pub via_name: String,
    /// Location
    pub pt: LefPoint,
}
#[derive(Clone, Debug, Deserialize, Serialize, PartialEq, Eq)]
pub enum LefLayerSpacing {
    Spacing(LefDecimal),
    DesignRuleWidth(LefDecimal),
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
/// # Lef SITE Definition
///
/// Defines a placement-site in designs.
/// Dictates the placement grid for a family of macros.
///
#[derive(Clone, Builder, Debug, Deserialize, Serialize, PartialEq, Eq)]
pub struct LefSite {
    // Required
    /// Site Name
    pub name: String,
    /// Site Class
    pub class: LefSiteClass,
    /// Size
    pub size: (LefDecimal, LefDecimal),

    // Optional
    /// Rotational & Translation Symmetries
    #[serde(default, skip_serializing_if = "Option::is_none")]
    #[builder(default, setter(strip_option))]
    pub symmetry: Option<Vec<LefSymmetry>>,

    // Not (Yet) Supported
    /// Row Patterns, re other previously defined sites
    #[serde(default, skip_serializing_if = "Option::is_none")]
    #[builder(default, setter(strip_option))]
    pub row_pattern: Option<Tbd>,
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
}
impl SerializationFormat {
    /// Convert any [serde::Serialize] data to a serialized string
    fn to_string(&self, data: &impl Serialize) -> LefResult<String> {
        match *self {
            Self::Json => Ok(serde_json::to_string(data)?),
            Self::Yaml => Ok(serde_yaml::to_string(data)?),
            Self::Toml => Ok(toml::to_string(data)?),
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
        assert_eq!(tok_strs, vec!["STUFF", "101", ";"]);
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

    #[test]
    fn it_parses_lib2() -> TestResult {
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
enumstr!(
    /// Sub-Types for Macros of Class [LefMacroClass::Pad]
    LefPadClassType {
        Input: "INPUT",
        Output: "OUTPUT",
        Inout: "INOUT",
        Power: "POWER",
        Spacer: "SPACER",
        AreaIo: "AREAIO",
    }
);

enumstr!(
    /// Sub-Types for Macros of Class [LefMacroClass:EndCap]
    LefEndCapClassType {
        Pre: "PRE",
        Post: "POST",
        TopLeft: "TOPLEFT",
        TopRight: "TOPRIGHT",
        BottomLeft: "BOTTOMLEFT",
        BottomRight: "BOTTOMRIGHT",
    }
);
enumstr!(
    /// Sub-Types for Macros of Class [LefMacroClass:Block]
    LefBlockClassType {
        BlackBox: "BLACKBOX",
        Soft: "SOFT"
    }
);
enumstr!(
    /// Sub-Types for Macros of Class [LefMacroClass:Core]
    LefCoreClassType {
        FeedThru: "FEEDTHRU",
        TieHigh: "TIEHIGH",
        TieLow: "TIELOW",
        Spacer: "SPACER",
        AntennaCell: "ANTENNACELL",
        WellTap: "WELLTAP",
    }
);
enumstr!(
    /// Sub-Types for Macros of Class [LefMacroClass:Core]
    LefPortClass {
        None: "NONE",
        Core: "CORE",
        Bump: "BUMP",
    }
);
enumstr!(
    /// [LefSite] Classes
    LefSiteClass {
        Pad: "PAD",
        Core: "CORE",
    }
);
enumstr!(
    /// Antenna Models
    LefAntennaModel {
        Oxide1: "OXIDE1",
        Oxide2: "OXIDE2",
        Oxide3: "OXIDE3",
        Oxide4: "OXIDE4",
    }
);
