//!
//! # Lef Reading Module
//!
//! Facilities for reading LEF-encoded content from file or string.
//! Includes the core Lexer and Parser classes.
//!

// Standard Lib Imports
use std::io::Read;
use std::path::Path;
use std::str::Chars;

// Crates.io Imports
#[allow(unused_imports)]
use rust_decimal::prelude::*;
use serde::{Deserialize, Serialize};

// Local imports
use super::data::*;
use super::utils::EnumStr;

/// Parse LEF content from file `fname`
pub fn parse_file(fname: impl AsRef<Path>) -> LefResult<LefLibrary> {
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

/// # Lef Lexer / Tokenizer
///
/// Breaks input string `self.src` into an iteration of [Token]s,
/// consisting of source-locations and type-annotations.
///
/// Operates in an iterator-style mode, producing a [Token] with
/// each call to `next_token`.
///
pub struct LefLexer<'src> {
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
    /// Boolean indication of beginning-of-line,
    /// i.e. whether any semantic content has been encountered on the current line.
    at_bol: bool,
}
impl<'src> LefLexer<'src> {
    pub(crate) fn new(src: &'src str) -> LefResult<Self> {
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
        lex.next_tok = lex._next_token()?;
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
    /// Get an immutable reference to our next [Token], without advancing
    #[inline(always)]
    fn peek_token(&self) -> &Option<Token> {
        &self.next_tok
    }
    /// Get and return our next token, updating internal state along the way
    fn next_token(&mut self) -> LefResult<Option<Token>> {
        if self.next_tok.is_none() {
            return Ok(None);
        }
        let mut tok = self._next_token()?;
        std::mem::swap(&mut tok, &mut self.next_tok);
        Ok(tok)
    }
    /// Internal implementation of `next_token`.
    /// Pull our next [Token], removing ignored items such as commentary and whitespace.
    ///
    /// While the primary API method is `next_token`, this underscore-version
    /// is also called during startup to evade its usual end-of-stream check.
    fn _next_token(&mut self) -> LefResult<Option<Token>> {
        use TokenType::{Comment, NewLine, WhiteSpace};
        loop {
            match self.lex_one()? {
                None => return Ok(None),
                Some(t) => match t.ttype {
                    WhiteSpace | Comment | NewLine => continue, // White-space, newlines, and comments are not emitted
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
        self.fail() // Some other, invalid character. Fail.
    }
    /// Lex newlines, incrementing our line-number
    fn lex_newline(&mut self) -> LefResult<Option<Token>> {
        let tok = self.emit(TokenType::NewLine);
        self.line += 1;
        self.linestart = self.pos;
        self.at_bol = true;
        Ok(Some(tok))
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
    fn fail<T>(&self) -> LefResult<T> {
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
pub struct SourceLocation {
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
pub struct Token {
    /// Source Location
    loc: SourceLocation,
    /// Token Type
    ttype: TokenType,
}
impl Token {
    /// Return a sub-string of input-string `src` over our locations
    pub(crate) fn substr<'me, 'src>(&'me self, src: &'src str) -> &'src str {
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
/// Lef Parsing Session
/// State held over the course of a parser run.
#[derive(Debug)]
struct LefParseSession {
    lef_version: LefDecimal,
}
impl Default for LefParseSession {
    fn default() -> Self {
        Self {
            lef_version: V5P8.clone(), // Version defaults to 5.8
        }
    }
}
/// Lef Parser
/// Transforms input string of lifetime 'src into a [LefLibrary]
pub struct LefParser<'src> {
    /// Source string
    src: &'src str,
    /// Lexer
    lex: LefLexer<'src>,
    /// Session State
    session: LefParseSession,
    /// Context Stack
    ctx: Vec<LefParseContext>,
}
impl<'src> LefParser<'src> {
    /// Construct a [LefParser] of input-text `src`
    pub(crate) fn new(src: &'src str) -> LefResult<Self> {
        let lex = LefLexer::new(src)?;
        Ok(Self {
            src,
            lex,
            ctx: Vec::new(),
            session: LefParseSession::default(),
        })
    }
    /// Advance the lexer and get its next token
    #[inline(always)]
    fn next_token(&mut self) -> LefResult<Option<Token>> {
        self.lex.next_token()
    }
    /// Advance by a [Token] without returning it.
    /// Usually called after matching on "peeked" results.
    #[inline(always)]
    fn advance(&mut self) -> LefResult<()> {
        // Advance the lexer, pass along any errors it generates. And otherwise return Ok.
        let _ = self.lex.next_token()?;
        Ok(())
    }
    #[inline(always)]
    fn peek_token(&self) -> &Option<Token> {
        self.lex.peek_token()
    }
    /// Boolean indication of whether our next Token is of [TokenType] `ttype`.
    fn matches(&self, ttype: TokenType) -> bool {
        match self.peek_token() {
            Some(t) if t.ttype == ttype => true,
            _ => false,
        }
    }
    /// Assert the next token is a valid [LefKey], and if so return the associated [LefKey].  
    /// Does not advance the parser state.
    #[inline(always)]
    fn peek_key(&self) -> LefResult<LefKey> {
        match self.peek_token() {
            Some(tok) if tok.ttype == TokenType::Name => {
                let txt = self.txt(&tok);
                match LefKey::parse(txt) {
                    Some(key) => Ok(key),
                    None => self.fail(LefParseErrorType::InvalidKey),
                }
            }
            None | Some(_) => self.fail(LefParseErrorType::InvalidToken {
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
            Some(_) => self.fail(LefParseErrorType::InvalidToken { expected: ttype }),
            None => self.fail(LefParseErrorType::InvalidToken { expected: ttype }),
        }
    }
    /// Assert the expectation that the next [Token] is of [TokenType] `ttype`.
    /// Returns the [Token] if so. Returns an [Err] if not.
    #[inline(always)]
    fn expect_and_get_str(&mut self, ttype: TokenType) -> LefResult<&str> {
        let tok = self.expect(ttype)?;
        Ok(self.txt(&tok))
    }
    /// Assert the next [Token] is of type [TokenType::Name], and return its string value.
    #[inline(always)]
    fn get_name(&mut self) -> LefResult<&str> {
        self.expect_and_get_str(TokenType::Name)
    }
    /// Assert the next [Token] is a valid [LefKey], and return its value.
    #[inline(always)]
    fn get_key(&mut self) -> LefResult<LefKey> {
        let txt = self.expect_and_get_str(TokenType::Name)?;
        match LefKey::parse(txt) {
            Some(key) => Ok(key),
            None => self.fail(LefParseErrorType::InvalidKey),
        }
    }
    /// Assert that the next [Token] matches [LefKey] `key`.
    fn expect_key(&mut self, key: LefKey) -> LefResult<()> {
        let parsed = self.get_key()?;
        if parsed != key {
            self.fail(LefParseErrorType::RequiredWord {
                expected: key.to_str().to_string(),
            })?;
        }
        Ok(())
    }
    /// Assert that the next [Token] is a [TokenType::Name], and that its string value matches `ident`.
    /// Unlike [LefParser::expect_keyword], this function matches literally, and does not convert to uppercase.
    fn expect_ident(&mut self, ident: &str) -> LefResult<()> {
        let txt = self.get_name()?;
        if txt == ident {
            Ok(())
        } else {
            self.fail(LefParseErrorType::RequiredWord {
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
        self.ctx.push(LefParseContext::Library);
        let mut lib = LefLibraryBuilder::default();
        let mut macros = Vec::new();
        let mut sites = Vec::new();
        loop {
            lib = match self.peek_key()? {
                LefKey::Macro => {
                    macros.push(self.parse_macro()?);
                    lib
                }
                LefKey::Version => lib.version(self.parse_version()?),
                LefKey::BusBitChars => lib.bus_bit_chars(self.parse_bus_bit_chars()?),
                LefKey::DividerChar => lib.divider_char(self.parse_divider_char()?),
                LefKey::NamesCaseSensitive => {
                    // Valid for versions <= 5.4
                    if self.session.lef_version > *V5P4 {
                        self.fail_msg(
                            LefParseErrorType::InvalidKey,
                            "The LEF NAMESCASESENSITIVE option is invalid for LEF versions > 5.4",
                        )?;
                    }
                    self.advance()?; // Eat the NAMESCASESENSITIVE key
                    let e = self.parse_enum::<LefOnOff>()?;
                    self.expect(TokenType::SemiColon)?;
                    lib.names_case_sensitive(e)
                }
                LefKey::NoWireExtensionAtPin => {
                    self.advance()?; // Eat the NOWIREEXTENSIONATPIN key
                    let e = self.parse_enum::<LefOnOff>()?;
                    self.expect(TokenType::SemiColon)?;
                    lib.no_wire_extension_at_pin(e)
                }
                LefKey::Units => lib.units(self.parse_units()?),
                LefKey::Site => {
                    sites.push(self.parse_site_def()?);
                    lib
                }
                LefKey::End => {
                    self.advance()?; // Eat the END key
                    self.expect_key(LefKey::Library)?; // Expect END LIBRARY
                    break;
                }
                LefKey::UseMinSpacing => {
                    self.advance()?;
                    self.expect_key(LefKey::Obs)?;
                    let e = self.parse_enum::<LefOnOff>()?;
                    self.expect(TokenType::SemiColon)?;
                    lib.use_min_spacing(e)
                }
                LefKey::BeginExtension
                | LefKey::ManufacturingGrid
                | LefKey::ClearanceMeasure
                | LefKey::PropertyDefinitions
                | LefKey::MaxViaStack
                | LefKey::ViaRule
                | LefKey::Generate
                | LefKey::NonDefaultRule => self.fail(LefParseErrorType::Unsupported)?,
                _ => self.fail(LefParseErrorType::InvalidKey)?,
            }
        }
        lib = lib.macros(macros);
        lib = lib.sites(sites);
        self.ctx.pop();
        Ok(lib.build()?)
    }
    /// Parse the Lef VERSION declaration
    fn parse_version(&mut self) -> LefResult<LefDecimal> {
        // Parse the content, in format `VERSION MAJOR.MINOR ;`
        self.advance()?; // Eat the VERSION key
        let num = self.parse_number()?;
        self.expect(TokenType::SemiColon)?;

        // Check the values
        let frac = num.fract() * Decimal::from(10);
        if num.floor() != LefDecimal::from(5) // Only major version 5 is supported 
        || !frac.fract().is_zero() // Only "whole number decimal" sub-versions
        || frac.floor() > LefDecimal::from(8)
        {
            self.fail(LefParseErrorType::InvalidValue)?;
        }
        // Checks out - return it. Keep a copy in our session, for future feature-checks.
        self.session.lef_version = num.clone();
        Ok(num)
    }
    /// Parse a Lef MACRO definition
    fn parse_macro(&mut self) -> LefResult<LefMacro> {
        self.ctx.push(LefParseContext::Macro);
        self.expect_key(LefKey::Macro)?;
        let mut mac = LefMacroBuilder::default();
        // Parse the macro-name
        let name = self.parse_ident()?;
        mac = mac.name(name.clone());
        // Start parsing attributes, pins, and obstructions
        let mut pins = Vec::new();
        loop {
            mac = match self.peek_key()? {
                LefKey::Class => mac.class(self.parse_macro_class()?),
                LefKey::Site => {
                    self.advance()?; // Eat the SITE key
                    let id = self.parse_ident()?;
                    self.expect(TokenType::SemiColon)?;
                    mac.site(id)
                }
                LefKey::Foreign => {
                    self.advance()?; // Eat the FOREIGN key
                    let cell_name = self.parse_ident()?;
                    let mut pt = None;
                    if !self.matches(TokenType::SemiColon) {
                        pt = Some(self.parse_point()?);
                    }
                    // The optional `ORIENT` field is not supported
                    if self.matches(TokenType::Name) {
                        self.fail(LefParseErrorType::Unsupported)?;
                    }
                    self.expect(TokenType::SemiColon)?;
                    mac.foreign(LefForeign {
                        cell_name,
                        pt,
                        orient: None,
                    })
                }
                LefKey::Origin => {
                    self.advance()?; // Eat the ORIGIN key
                    let pt = self.parse_point()?;
                    self.expect(TokenType::SemiColon)?;
                    mac.origin(pt)
                }
                LefKey::Size => mac.size(self.parse_size()?),
                LefKey::Pin => {
                    pins.push(self.parse_pin()?);
                    mac
                }
                LefKey::Obs => mac.obs(self.parse_obstructions()?),
                LefKey::Symmetry => mac.symmetry(self.parse_symmetries()?),
                LefKey::Source => {
                    // Valid for versions <= 5.4
                    if self.session.lef_version > *V5P4 {
                        self.fail_msg(
                            LefParseErrorType::InvalidKey,
                            "The Lef MACRO's SOURCE field is invalid for LEF versions > 5.4",
                        )?;
                    }
                    self.advance()?; // Eat the SOURCE key
                    let e = self.parse_enum::<LefDefSource>()?;
                    self.expect(TokenType::SemiColon)?;
                    mac.source(e)
                }
                LefKey::End => {
                    self.advance()?; // End of Macro. Eat the END key
                    break;
                }
                _ => self.fail(LefParseErrorType::InvalidKey)?,
            }
        }
        // Parse the END-enclosing macro-name
        self.expect_ident(&name)?;
        // Set the pins, build our struct and return it
        mac = mac.pins(pins);
        self.ctx.pop();
        Ok(mac.build()?)
    }
    /// Parse a MACRO::PIN definition into a [LefPin]
    fn parse_pin(&mut self) -> LefResult<LefPin> {
        self.ctx.push(LefParseContext::Pin);
        self.expect_key(LefKey::Pin)?;
        let mut pin = LefPinBuilder::default();
        // Parse the pin-name
        let name = self.parse_ident()?;
        pin = pin.name(name.clone());
        let mut ports = Vec::new();
        let mut antenna_attrs = Vec::new();
        loop {
            pin = match self.peek_key()? {
                LefKey::End => {
                    self.advance()?; // End of Pin. Eat the END key.
                    break;
                }
                LefKey::Port => {
                    ports.push(self.parse_port()?);
                    pin
                }
                LefKey::Direction => pin.direction(self.parse_pin_direction()?),
                LefKey::Use => {
                    self.advance()?;
                    let e = self.parse_enum::<LefPinUse>()?;
                    self.expect(TokenType::SemiColon)?;
                    pin.use_(e)
                }
                LefKey::Shape => {
                    self.advance()?;
                    let e = self.parse_enum::<LefPinShape>()?;
                    self.expect(TokenType::SemiColon)?;
                    pin.shape(e)
                }
                LefKey::AntennaModel => {
                    self.advance()?;
                    pin.antenna_model(self.parse_enum::<LefAntennaModel>()?)
                }
                LefKey::AntennaDiffArea
                | LefKey::AntennaGateArea
                | LefKey::AntennaPartialMetalArea
                | LefKey::AntennaPartialMetalSideArea
                | LefKey::AntennaPartialCutArea
                | LefKey::AntennaPartialDiffArea
                | LefKey::AntennaMaxAreaCar
                | LefKey::AntennaMaxSideAreaCar
                | LefKey::AntennaMaxCutCar => {
                    // Parse an ANTENNA attribute
                    let key = self.parse_ident()?;
                    let val = self.parse_number()?;
                    let mut layer = None;
                    if !self.matches(TokenType::SemiColon) {
                        self.expect_key(LefKey::Layer)?;
                        layer = Some(self.parse_ident()?);
                    }
                    self.expect(TokenType::SemiColon)?;
                    antenna_attrs.push(LefPinAntennaAttr { key, val, layer });
                    pin
                }
                LefKey::TaperRule
                | LefKey::NetExpr
                | LefKey::SupplySensitivity
                | LefKey::GroundSensitivity
                | LefKey::MustJoin
                | LefKey::Property => self.fail(LefParseErrorType::Unsupported)?,
                _ => self.fail(LefParseErrorType::InvalidKey)?,
            }
        }
        // Get the pin-closing `END <NAME>`
        self.expect_ident(&name)?;
        // Set our port-objects, build and return the Pin
        pin = pin.ports(ports);
        pin = pin.antenna_attrs(antenna_attrs);
        self.ctx.pop();
        Ok(pin.build()?)
    }
    /// Parse a [LefPinDirection]
    fn parse_pin_direction(&mut self) -> LefResult<LefPinDirection> {
        self.expect_key(LefKey::Direction)?;
        let pin = match self.get_key()? {
            LefKey::Input => LefPinDirection::Input,
            LefKey::FeedThru => LefPinDirection::FeedThru,
            LefKey::Inout => LefPinDirection::Inout,
            LefKey::Output => {
                // Outputs include an optional tri-state modifier
                let mut tristate = false;
                if !self.matches(TokenType::SemiColon) {
                    self.expect_key(LefKey::Tristate)?;
                    tristate = true;
                }
                LefPinDirection::Output { tristate }
            }
            _ => self.fail(LefParseErrorType::InvalidValue)?,
        };
        self.expect(TokenType::SemiColon)?;
        Ok(pin)
    }
    /// Parse a MACRO::PIN::PORT definition into a [LefPort]
    fn parse_port(&mut self) -> LefResult<LefPort> {
        self.ctx.push(LefParseContext::Port);
        self.expect_key(LefKey::Port)?;
        let mut class: Option<LefPortClass> = None;
        let mut layers = Vec::new();
        // Parse attributes and geometries
        // Note this peeks rather than taking the next token,
        // largely to accommodate the closing-delimeter-free `LAYER` / [LefLayerGeometries] definitions.
        // Other keys generally advance by a Token *after* matching.
        loop {
            match self.peek_key()? {
                LefKey::Class => {
                    self.advance()?; // Eat the CLASS Token
                    class = Some(self.parse_enum::<LefPortClass>()?);
                    self.expect(TokenType::SemiColon)?;
                }
                LefKey::Layer => {
                    layers.push(self.parse_layer_geometries()?);
                }
                LefKey::End => {
                    self.advance()?; // Eat the END Token
                    break;
                }
                _ => self.fail(LefParseErrorType::InvalidKey)?,
            }
        }
        self.ctx.pop();
        Ok(LefPort { layers, class })
    }
    /// Parse a [LefMacro]'s obstruction definitions
    fn parse_obstructions(&mut self) -> LefResult<Vec<LefLayerGeometries>> {
        self.expect_key(LefKey::Obs)?;
        let mut geoms = Vec::new();
        loop {
            if self.peek_token().is_none() {
                break; // End of input, which is valid here.
            }
            match self.peek_key()? {
                LefKey::Layer => geoms.push(self.parse_layer_geometries()?),
                LefKey::End => {
                    self.advance()?; // Eat the END Token
                    break;
                }
                _ => self.fail(LefParseErrorType::InvalidKey)?,
            }
        }
        Ok(geoms)
    }
    /// Parse a set of geometries on a single layer, as commonly specified per-[LefPort]
    pub(crate) fn parse_layer_geometries(&mut self) -> LefResult<LefLayerGeometries> {
        self.ctx.push(LefParseContext::Geometry);
        self.expect_key(LefKey::Layer)?; // Eat the opening LAYER keyword
        let mut layer = LefLayerGeometriesBuilder::default();
        layer = layer.layer_name(self.parse_ident()?); // Parse the layer-name

        // Parse the options defined inline with the LAYER statement
        while !self.matches(TokenType::SemiColon) {
            layer = match self.get_key()? {
                LefKey::ExceptPgNet => layer.except_pg_net(true),
                LefKey::Spacing => layer.spacing(LefLayerSpacing::Spacing(self.parse_number()?)),
                LefKey::DesignRuleWidth => {
                    layer.spacing(LefLayerSpacing::DesignRuleWidth(self.parse_number()?))
                }
                _ => self.fail(LefParseErrorType::InvalidKey)?,
            }
        }
        self.expect(TokenType::SemiColon)?;

        // Now parse the layer-geom body.
        //
        // LayerGeometries don't have an END card, so this needs to peek at the next token,
        // and exit when another LAYER or END (of a higher-level thing) turn up.
        // Note that on end-of-file, i.e. `peek_token` returning `None`, this will exit and return a valid [LefLayerGeometries].
        // (Objects above it in the tree may error instead.)
        let mut geoms = Vec::new();
        let mut vias = Vec::new();
        loop {
            if self.peek_token().is_none() {
                break; // End of input, which is valid here.
            }

            // Anything else that shows up here must be one of the following keys:
            match self.peek_key()? {
                LefKey::Layer | LefKey::End => break, // End of geometries. (Really start/end of something else.)
                LefKey::Path | LefKey::Polygon | LefKey::Rect => {
                    geoms.push(self.parse_geometry()?);
                }
                LefKey::Via => {
                    self.advance()?; // Eat the VIA Token
                    if self.matches(TokenType::Name) {
                        // The ITERATE construction is not supported.
                        self.fail(LefParseErrorType::Unsupported)?;
                    }
                    let pt = self.parse_point()?;
                    let via_name = self.parse_ident()?;
                    self.expect(TokenType::SemiColon)?;
                    vias.push(LefVia { pt, via_name });
                }
                LefKey::Width => {
                    self.advance()?; // Eat the WIDTH Token
                    layer = layer.width(self.parse_number()?);
                    self.expect(TokenType::SemiColon)?;
                }
                _ => self.fail(LefParseErrorType::InvalidKey)?,
            }
        }
        layer = layer.vias(vias);
        layer = layer.geometries(geoms);
        let layer = layer.build()?;
        self.ctx.pop();
        Ok(layer)
    }
    /// Parse a [LefGeometry] statement
    /// Each can be a shape or iteration thereof
    fn parse_geometry(&mut self) -> LefResult<LefGeometry> {
        match self.peek_key()? {
            LefKey::Rect => {
                self.advance()?;
                let mut mask = None;
                if self.matches(TokenType::Name) {
                    if self.get_key()? == LefKey::Mask {
                        mask = Some(self.parse_number()?);
                    } else {
                        // The ITERATE construction would go here, but is not supported.
                        self.fail(LefParseErrorType::Unsupported)?;
                    }
                }
                // Parse the two points
                let p1 = self.parse_point()?;
                let p2 = self.parse_point()?;
                self.expect(TokenType::SemiColon)?;
                // And return the Rect
                Ok(LefGeometry::Shape(LefShape::Rect(mask, p1, p2)))
            }
            LefKey::Polygon => {
                self.advance()?;
                let points = self.parse_point_list()?;
                if points.len() < 3 {
                    self.fail(LefParseErrorType::InvalidValue)?;
                }
                self.expect(TokenType::SemiColon)?;
                Ok(LefGeometry::Shape(LefShape::Polygon(points)))
            }
            LefKey::Path => {
                self.advance()?;
                let points = self.parse_point_list()?;
                if points.len() < 2 {
                    self.fail(LefParseErrorType::InvalidValue)?;
                }
                self.expect(TokenType::SemiColon)?;
                Ok(LefGeometry::Shape(LefShape::Path(points)))
            }
            _ => self.fail(LefParseErrorType::InvalidKey)?,
        }
    }
    /// Parse a space-separated list of [LefPoint]. Terminated by [TokenType::SemiColon].
    fn parse_point_list(&mut self) -> LefResult<Vec<LefPoint>> {
        let mut points = Vec::new();
        while !self.matches(TokenType::SemiColon) {
            points.push(self.parse_point()?);
        }
        Ok(points)
    }
    /// Parse a space-separated x,y [LefPoint] comprising two [LefDecimal]
    fn parse_point(&mut self) -> LefResult<LefPoint> {
        Ok(LefPoint::new(self.parse_number()?, self.parse_number()?))
    }
    /// Parse [LefUnits] definitions
    fn parse_units(&mut self) -> LefResult<LefUnits> {
        use LefKey::{
            Capacitance, Current, Database, End, Frequency, Megahertz, Microns, Milliamps,
            Milliwatts, Nanoseconds, Ohms, Picofarads, Power, Resistance, Time, Units, Voltage,
            Volts,
        };
        self.ctx.push(LefParseContext::Units);
        self.expect_key(Units)?;
        let mut units = LefUnits::default();
        loop {
            match self.get_key()? {
                Database => {
                    // Parse the DATABASE MICRONS flavor
                    self.expect_key(Microns)?;
                    let num = self.parse_number()?;
                    self.expect(TokenType::SemiColon)?;
                    units.database_microns = Some(LefDbuPerMicron::try_new(num)?);
                }
                Time => {
                    // Parse the TIME NANOSECONDS flavor
                    self.expect_key(Nanoseconds)?;
                    let num = self.parse_number()?;
                    self.expect(TokenType::SemiColon)?;
                    units.time_ns = Some(num);
                }
                Capacitance => {
                    // Parse the CAPACITANCE PICOFARADS flavor
                    self.expect_key(Picofarads)?;
                    let num = self.parse_number()?;
                    self.expect(TokenType::SemiColon)?;
                    units.capacitance_pf = Some(num);
                }
                Resistance => {
                    // Parse the CAPACITANCE PICOFARADS flavor
                    self.expect_key(Ohms)?;
                    let num = self.parse_number()?;
                    self.expect(TokenType::SemiColon)?;
                    units.resistance_ohms = Some(num);
                }
                Power => {
                    // Parse the POWER MILLIWATTS flavor
                    self.expect_key(Milliwatts)?;
                    let num = self.parse_number()?;
                    self.expect(TokenType::SemiColon)?;
                    units.power_mw = Some(num);
                }
                Current => {
                    // Parse the CURRENT MILLIAMPS flavor
                    self.expect_key(Milliamps)?;
                    let num = self.parse_number()?;
                    self.expect(TokenType::SemiColon)?;
                    units.current_ma = Some(num);
                }
                Voltage => {
                    // Parse the VOLTAGE VOLTS flavor
                    self.expect_key(Volts)?;
                    let num = self.parse_number()?;
                    self.expect(TokenType::SemiColon)?;
                    units.voltage_volts = Some(num);
                }
                Frequency => {
                    // Parse the FREQUENCY MEGAHERTZ flavor
                    self.expect_key(Megahertz)?;
                    let num = self.parse_number()?;
                    self.expect(TokenType::SemiColon)?;
                    units.frequency_mhz = Some(num);
                }
                End => {
                    self.expect_key(Units)?;
                    break; // End of UNITS definitions
                }
                _ => self.fail(LefParseErrorType::InvalidKey)?,
            }
        }
        self.ctx.pop();
        Ok(units)
    }
    /// Parse [LefMacro] SYMMETRY options into a vector of [LefSymmetry]
    fn parse_symmetries(&mut self) -> LefResult<Vec<LefSymmetry>> {
        self.expect_key(LefKey::Symmetry)?;
        let mut symms = Vec::new();
        while !self.matches(TokenType::SemiColon) {
            symms.push(self.parse_enum::<LefSymmetry>()?);
        }
        self.expect(TokenType::SemiColon)?;
        Ok(symms)
    }
    /// Parse the MACRO::CLASS type-enumeration and its sub-types into a [LefMacroClass]
    fn parse_macro_class(&mut self) -> LefResult<LefMacroClass> {
        self.expect_key(LefKey::Class)?;
        match self.parse_enum::<LefMacroClassName>()? {
            LefMacroClassName::Block => {
                let mut tp = None;
                if !self.matches(TokenType::SemiColon) {
                    tp = Some(self.parse_enum::<LefBlockClassType>()?);
                }
                self.expect(TokenType::SemiColon)?;
                Ok(LefMacroClass::Block { tp })
            }
            LefMacroClassName::Pad => {
                let mut tp = None;
                if !self.matches(TokenType::SemiColon) {
                    tp = Some(self.parse_enum::<LefPadClassType>()?);
                }
                self.expect(TokenType::SemiColon)?;
                Ok(LefMacroClass::Pad { tp })
            }
            LefMacroClassName::Core => {
                let mut tp = None;
                if !self.matches(TokenType::SemiColon) {
                    tp = Some(self.parse_enum::<LefCoreClassType>()?);
                }
                self.expect(TokenType::SemiColon)?;
                Ok(LefMacroClass::Core { tp })
            }
            LefMacroClassName::EndCap => {
                // Note unlike all other types, an ENDCAP sub-type is *required*.
                let tp = self.parse_enum::<LefEndCapClassType>()?;
                self.expect(TokenType::SemiColon)?;
                Ok(LefMacroClass::EndCap { tp })
            }
            LefMacroClassName::Cover => {
                let mut bump = false;
                if !self.matches(TokenType::SemiColon) {
                    self.expect_key(LefKey::Bump)?;
                    bump = true;
                }
                self.expect(TokenType::SemiColon)?;
                Ok(LefMacroClass::Cover { bump })
            }
            LefMacroClassName::Ring => {
                self.expect(TokenType::SemiColon)?;
                Ok(LefMacroClass::Ring)
            }
        }
    }
    /// Parse a [LefSite] definition
    fn parse_site_def(&mut self) -> LefResult<LefSite> {
        self.ctx.push(LefParseContext::Site);
        self.expect_key(LefKey::Site)?;
        let mut site = LefSiteBuilder::default();
        // Parse the site name. Keep a copy for later comparison.
        let name = self.parse_ident()?;
        site = site.name(name.clone());
        loop {
            site = match self.peek_key()? {
                LefKey::End => {
                    self.advance()?; // Eat the END
                    self.expect_ident(&name)?;
                    break;
                }
                LefKey::Class => {
                    self.advance()?; // Eat the CLASS
                    let e = self.parse_enum::<LefSiteClass>()?;
                    self.expect(TokenType::SemiColon)?;
                    site.class(e)
                }
                LefKey::Symmetry => site.symmetry(self.parse_symmetries()?),
                LefKey::Size => site.size(self.parse_size()?),
                LefKey::RowPattern => self.fail(LefParseErrorType::Unsupported)?, // ROWPATTERN is not supported
                _ => self.fail(LefParseErrorType::InvalidValue)?,
            }
        }
        self.ctx.pop();
        Ok(site.build()?)
    }
    /// Parse the Lef SIZE statement into an (x, y) pair of [LefDecimal]s
    fn parse_size(&mut self) -> LefResult<(LefDecimal, LefDecimal)> {
        self.expect_key(LefKey::Size)?;
        let x = self.parse_number()?;
        self.expect_key(LefKey::By)?;
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
        self.expect_key(LefKey::BusBitChars)?;
        let txt = self.expect_and_get_str(TokenType::StringLiteral)?;
        let chars = txt.chars().collect::<Vec<char>>();
        if chars.len() != 4 {
            self.fail(LefParseErrorType::InvalidValue)?;
        }
        self.expect(TokenType::SemiColon)?;
        Ok((chars[1], chars[2]))
    }
    /// Parse the LefLibrary::DIVIDERCHAR key from a single-character string literal
    fn parse_divider_char(&mut self) -> LefResult<char> {
        self.expect_key(LefKey::DividerChar)?;
        let txt = self.expect_and_get_str(TokenType::StringLiteral)?;
        let chars = txt.chars().collect::<Vec<char>>();
        if chars.len() != 3 {
            self.fail(LefParseErrorType::InvalidValue)?;
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
    fn parse_enum<T: EnumStr>(&mut self) -> LefResult<T> {
        let txt = self.get_name()?;
        match T::from_str(&txt.to_ascii_uppercase()) {
            Some(t) => Ok(t),
            None => self.fail(LefParseErrorType::InvalidValue),
        }
    }
    /// Error-Generation Helper
    fn fail<T>(&self, tp: LefParseErrorType) -> LefResult<T> {
        let state = self.state();
        Err(LefError::Parse {
            tp,
            msg: None,
            state,
        })
    }
    /// Error-Generation Helper
    fn fail_msg<T>(&self, tp: LefParseErrorType, msg: impl Into<String>) -> LefResult<T> {
        let msg: String = msg.into();
        let state = self.state();
        Err(LefError::Parse {
            tp,
            msg: Some(msg),
            state,
        })
    }
    /// Extract the state of the parser. Generally for error reporting.
    fn state(&self) -> ParserState {
        // Create a string repr of the current token
        let token = match self.lex.next_tok {
            Some(t) => self.txt(&t),
            None => "EOF",
        }
        .to_string();
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
        ParserState {
            ctx: self.ctx.clone(),
            line_content,
            line_num: self.lex.line,
            token,
            pos: self.lex.pos,
        }
    }
}
/// State of the parser, generally exposed when providing error info.
#[allow(dead_code)]
#[derive(Debug)]
pub struct ParserState {
    ctx: Vec<LefParseContext>,
    token: String,
    line_content: String,
    line_num: usize,
    pos: usize,
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn it_parses_units() -> LefResult<()> {
        // Parse UNITS content into LefUnits

        // Check that for version 5.3 this same MACRO parses successfully
        let src = r#"
        UNITS
            DATABASE MICRONS 1000 ;
            TIME NANOSECONDS 1 ;
            CAPACITANCE PICOFARADS 2 ;
            RESISTANCE OHMS 3 ;
            POWER MILLIWATTS 5 ;
            CURRENT MILLIAMPS 6 ;
            VOLTAGE VOLTS 7 ;
            FREQUENCY MEGAHERTZ 8 ;
        END UNITS
        "#;
        let mut parser = LefParser::new(src)?;
        let units = parser.parse_units()?;
        assert_eq!(
            units,
            LefUnits {
                database_microns: Some(LefDbuPerMicron(1000)),
                time_ns: Some(LefDecimal::from(1)),
                capacitance_pf: Some(LefDecimal::from(2)),
                resistance_ohms: Some(LefDecimal::from(3)),
                power_mw: Some(LefDecimal::from(5)),
                current_ma: Some(LefDecimal::from(6)),
                voltage_volts: Some(LefDecimal::from(7)),
                frequency_mhz: Some(LefDecimal::from(8)),
            }
        );
        Ok(())
    }
    #[test]
    fn it_parses_with_source() -> LefResult<()> {
        // Test that the SOURCE keyword works for old versions of the LEF spec, and fails for new ones.

        // Check that for version 5.3 this same MACRO parses successfully
        let src = r#"
        VERSION 5.3;
        MACRO valid_source_user
            SOURCE USER ;
        END valid_source_user
        END LIBRARY
        "#;
        parse_str(src)?;

        // Check that for version 5.5 the same MACRO produces an error
        let src = r#"
        VERSION 5.5;
        MACRO invalid_source_user
            SOURCE USER ;
        END invalid_source_user
        END LIBRARY
        "#;
        assert!(parse_str(src).is_err());
        Ok(())
    }
}
