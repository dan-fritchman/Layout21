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
        match self.peek_char() {
            Some(ref c) if c.is_digit(10) || *c == '.' || *c == '-' => {
                return self.lex_number(*c);
            }
            _ => {}
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
    /// Lex a number, but if not truly a number, return token with ttype [TokenType::Name]
    fn lex_number(&mut self, leadchar: char) -> LefResult<Option<Token>> {
        let mut nstring = String::new();
        nstring.push(leadchar);
        // get string slice for current char iterator here
        let buf = self.chars.as_str();
        // Accept everything until the next white-space
        while self.accept(|c| !c.is_whitespace()) {
            continue;
        }
        // add the rest of the numberish &str to the String
        let subbuf = &buf[0..self.pos - self.start - 1];
        nstring.push_str(subbuf);
        let numslice = nstring.as_str();
        let tok = if i32::from_str(numslice).is_ok() || f64::from_str(numslice).is_ok() {
            self.emit(TokenType::Number)
        } else {
            self.emit(TokenType::Name)
        };
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
    PropertyDefinitions,
    Geometry,
    Site,
    Units,
    Density,
    Via,
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
    /// Unlike [LefParser::expect_key], this function matches literally, and does not convert to uppercase.
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
        let mut vias = Vec::new();
        let mut extensions = Vec::new();
        let mut property_definitions = Vec::new();
        loop {
            if self.peek_token().is_none() && self.session.lef_version >= *V5P6 {
                break; // End of input (without END LIBRARY), which is valid for lef 5.6+
            }
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
                LefKey::FixedMask => {
                    self.advance()?; // Eat the FIXEDMASK key
                    self.expect(TokenType::SemiColon)?;
                    lib.fixed_mask(true)
                }
                LefKey::UseMinSpacing => {
                    self.advance()?;
                    self.expect_key(LefKey::Obs)?;
                    let e = self.parse_enum::<LefOnOff>()?;
                    self.expect(TokenType::SemiColon)?;
                    lib.use_min_spacing(e)
                }
                LefKey::Via => {
                    vias.push(self.parse_via()?);
                    lib
                }
                LefKey::ClearanceMeasure => {
                    self.advance()?;
                    let e = self.parse_enum::<LefClearanceStyle>()?;
                    self.expect(TokenType::SemiColon)?;
                    lib.clearance_measure(e)
                }
                LefKey::ManufacturingGrid => {
                    self.advance()?;
                    let e = self.parse_number()?;
                    self.expect(TokenType::SemiColon)?;
                    lib.manufacturing_grid(e)
                }
                LefKey::BeginExtension => {
                    self.advance()?;
                    let value_token = self.expect(TokenType::StringLiteral)?;
                    let name = String::from(self.txt(&value_token));
                    let mut data = String::from("");
                    loop {
                        // Keep grabbing tokens (including whitespace and newlines) until
                        // we hit the ENDEXT token.
                        let nexttok = self.next_token()?;
                        match nexttok {
                            Some(tok) => {
                                if tok.ttype == TokenType::Name {
                                    let txt = self.txt(&tok);
                                    match LefKey::parse(txt) {
                                        Some(key) => {
                                            if key == LefKey::EndExtension {
                                                break;
                                            }
                                            ()
                                        }
                                        None => {
                                            let _ignore = self
                                                .fail::<LefError>(LefParseErrorType::InvalidKey);
                                            ()
                                        }
                                    }
                                }
                                data.push_str(self.txt(&tok));
                                data.push_str(" ")
                            }
                            None => {
                                self.fail(LefParseErrorType::InvalidKey)?;
                            }
                        }
                    }
                    extensions.push(LefExtension { name, data });
                    lib
                }
                LefKey::PropertyDefinitions => {
                    property_definitions.extend(self.parse_property_definitions()?);
                    lib
                }
                LefKey::MaxViaStack
                | LefKey::ViaRule
                | LefKey::Generate
                | LefKey::NonDefaultRule => self.fail(LefParseErrorType::Unsupported)?,
                _ => self.fail(LefParseErrorType::InvalidKey)?,
            }
        }
        lib = lib.macros(macros);
        lib = lib.sites(sites);
        lib = lib.vias(vias);
        lib = lib.extensions(extensions);
        lib = lib.property_definitions(property_definitions);
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
        let mut properties = Vec::new();
        loop {
            mac = match self.peek_key()? {
                LefKey::Class => mac.class(self.parse_macro_class()?),
                LefKey::Site => {
                    self.advance()?; // Eat the SITE key
                    let id = self.parse_ident()?;
                    self.expect(TokenType::SemiColon)?;
                    mac.site(id)
                }
                LefKey::Eeq => {
                    self.advance()?; // Eat the EEQ key
                    let cell_name = self.parse_ident()?;
                    self.expect(TokenType::SemiColon)?;
                    mac.eeq(cell_name)
                }
                LefKey::FixedMask => {
                    self.advance()?; // Eat the FIXEDMASK key
                    self.expect(TokenType::SemiColon)?;
                    mac.fixed_mask(true)
                }
                LefKey::Foreign => {
                    self.advance()?; // Eat the FOREIGN key
                    let cell_name = self.parse_ident()?;

                    let mut pt = None;
                    if !self.matches(TokenType::SemiColon) {
                        pt = Some(self.parse_point()?);
                    }
                    let mut orient = None;
                    if !self.matches(TokenType::SemiColon) {
                        orient = Some(self.parse_enum::<LefOrient>()?);
                    }
                    self.expect(TokenType::SemiColon)?;
                    mac.foreign(LefForeign {
                        cell_name,
                        pt,
                        orient: orient,
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
                LefKey::Property => {
                    properties = self.parse_property(properties)?;
                    mac
                }
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
                LefKey::Density => mac.density(self.parse_density()?),
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
        let mut properties = Vec::new();
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
                    let e = self.parse_enum::<LefAntennaModel>()?;
                    self.expect(TokenType::SemiColon)?;
                    pin.antenna_model(e)
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
                LefKey::TaperRule => {
                    self.advance()?;
                    let value = self.parse_ident()?;
                    self.expect(TokenType::SemiColon)?;
                    pin.taper_rule(value)
                }
                LefKey::MustJoin => {
                    self.advance()?;
                    let value = self.parse_ident()?;
                    self.expect(TokenType::SemiColon)?;
                    pin.must_join(value)
                }
                LefKey::SupplySensitivity => {
                    self.advance()?;
                    let value = self.parse_ident()?;
                    self.expect(TokenType::SemiColon)?;
                    pin.supply_sensitivity(value)
                }
                LefKey::GroundSensitivity => {
                    self.advance()?;
                    let value = self.parse_ident()?;
                    self.expect(TokenType::SemiColon)?;
                    pin.ground_sensitivity(value)
                }
                LefKey::NetExpr => {
                    self.advance()?;
                    let value_token = self.expect(TokenType::StringLiteral)?;
                    self.expect(TokenType::SemiColon)?;
                    pin.net_expr(String::from(self.txt(&value_token)))
                }
                LefKey::Property => {
                    properties = self.parse_property(properties)?;
                    pin
                }
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
    /// Parse a MACRO::DENSITY definition into a [Vec<LefDensityGeometries>]
    fn parse_density(&mut self) -> LefResult<Vec<LefDensityGeometries>> {
        self.ctx.push(LefParseContext::Density);
        self.expect_key(LefKey::Density)?;
        let mut dens_geoms: Vec<LefDensityGeometries> = Vec::new();

        // Parse attributes and geometries
        // Note this peeks rather than taking the next token,
        // largely to accommodate the closing-delimeter-free `LAYER` / [LefDensityGeometries] definitions.
        // Other keys generally advance by a Token *after* matching.

        loop {
            match self.peek_key()? {
                LefKey::Layer => {
                    self.expect_key(LefKey::Layer)?; // Eat the opening LAYER keyword
                    let mut layer = LefDensityGeometriesBuilder::default();
                    layer = layer.layer_name(self.parse_ident()?); // Parse the layer-name
                    self.expect(TokenType::SemiColon)?;
                    let mut rects: Vec<LefDensityRectangle> = Vec::new();

                    loop {
                        match self.peek_key()? {
                            LefKey::Layer | LefKey::End => break, // End of geometries. (Really start/end of something else.)
                            LefKey::Rect => {
                                self.advance()?; // Eat the RECT keyword
                                let p1: LefPoint = self.parse_point()?;
                                let p2: LefPoint = self.parse_point()?;
                                let dens_value: LefDecimal = self.parse_number()?;
                                rects.push(LefDensityRectangle {
                                    pt1: p1,
                                    pt2: p2,
                                    density_value: dens_value,
                                });
                                self.expect(TokenType::SemiColon)?;
                            }
                            _ => self.fail(LefParseErrorType::InvalidKey)?,
                        }
                    }
                    layer = layer.geometries(rects);
                    let layer = layer.build()?;
                    dens_geoms.push(layer);
                }
                LefKey::End => {
                    self.advance()?; // Eat the END Token
                    break;
                }
                _ => self.fail(LefParseErrorType::InvalidKey)?,
            }
        }
        self.ctx.pop();
        Ok(dens_geoms)
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
        let mut geoms: Vec<LefGeometry> = Vec::new();
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
    /// Parse a set of via geometries on a single layer.
    pub(crate) fn parse_via_layer_geometries(&mut self) -> LefResult<LefViaLayerGeometries> {
        self.ctx.push(LefParseContext::Geometry);
        self.expect_key(LefKey::Layer)?; // Eat the opening LAYER keyword
        let mut layer = LefViaLayerGeometriesBuilder::default();
        layer = layer.layer_name(self.parse_ident()?); // Parse the layer-name
        self.expect(TokenType::SemiColon)?;

        let mut shapes: Vec<LefViaShape> = Vec::new();
        loop {
            if self.peek_token().is_none() {
                break; // End of input, which is valid here.
            }

            // Anything else that shows up here must be one of the following keys:
            match self.peek_key()? {
                LefKey::Layer | LefKey::Property | LefKey::End => break, // End of geometries. (Really start/end of something else.)
                LefKey::Polygon | LefKey::Rect => {
                    shapes.push(self.parse_via_shape()?);
                }
                _ => self.fail(LefParseErrorType::InvalidKey)?,
            }
        }
        layer = layer.shapes(shapes);
        let layer = layer.build()?;
        self.ctx.pop();
        Ok(layer)
    }
    /// Parse a [LefViaShape] statement
    fn parse_via_shape(&mut self) -> LefResult<LefViaShape> {
        match self.peek_key()? {
            LefKey::Rect => {
                self.advance()?;
                let mut mask = None;
                if self.matches(TokenType::Name) {
                    if self.get_key()? == LefKey::Mask {
                        mask = Some(LefMask::new(self.parse_number()?));
                    } else {
                        self.fail(LefParseErrorType::Unsupported)?;
                    }
                }
                // Parse the two points
                let p1 = self.parse_point()?;
                let p2: LefPoint = self.parse_point()?;
                self.expect(TokenType::SemiColon)?;
                // And return the Rect
                Ok(LefViaShape::Rect(mask, p1, p2))
            }
            LefKey::Polygon => {
                self.advance()?;
                let mut mask = None;
                if self.matches(TokenType::Name) {
                    if self.get_key()? == LefKey::Mask {
                        mask = Some(LefMask::new(self.parse_number()?));
                    } else {
                        self.fail(LefParseErrorType::Unsupported)?;
                    }
                }
                let points = self.parse_point_list()?;
                if points.len() < 3 {
                    self.fail(LefParseErrorType::InvalidValue)?;
                }
                self.expect(TokenType::SemiColon)?;
                Ok(LefViaShape::Polygon(mask, points))
            }
            _ => self.fail(LefParseErrorType::InvalidKey)?,
        }
    }

    /// Parse optional ITERATE and return a wrapper boolean indicating success
    fn parse_iterate(&mut self) -> LefResult<bool> {
        if self.matches(TokenType::Name) {
            if self.peek_key()? == LefKey::Iterate {
                self.advance()?;
                return Ok(true);
            }
        }
        Ok(false)
    }
    /// Parse the [LefMask] statement on a [LefGeometry]
    fn parse_geometry_mask(&mut self) -> LefResult<Option<LefMask>> {
        //let mut mask: Option<LefMask> = None;
        if self.matches(TokenType::Name) {
            if self.peek_key()? == LefKey::Mask {
                self.advance()?;
                return Ok(Some(LefMask::new(self.parse_number()?)));
            }
        }
        Ok(None)
    }
    fn parse_step_pattern(&mut self) -> LefResult<LefStepPattern> {
        self.expect_key(LefKey::Do)?;
        let numx = self.parse_number()?;
        self.expect_key(LefKey::By)?;
        let numy = self.parse_number()?;
        self.expect_key(LefKey::Step)?;
        let spacex = self.parse_number()?;
        let spacey = self.parse_number()?;
        Ok(LefStepPattern {
            numx,
            numy,
            spacex,
            spacey,
        })
    }
    /// Parse a [LefGeometry] statement
    /// Each can be a shape or iteration thereof
    fn parse_geometry(&mut self) -> LefResult<LefGeometry> {
        match self.get_key()? {
            LefKey::Rect => {
                let mask = self.parse_geometry_mask()?;
                let is_iterate = self.parse_iterate()?;
                // Parse the two points
                let p1 = self.parse_point()?;
                let p2 = self.parse_point()?;
                let shape = LefShape::Rect(mask, p1, p2);
                Ok(self.parse_geometry_tail(is_iterate, shape)?)
            }
            LefKey::Polygon => {
                let mask = self.parse_geometry_mask()?;
                let is_iterate = self.parse_iterate()?;
                let points = self.parse_point_list()?;
                if points.len() < 3 {
                    self.fail(LefParseErrorType::InvalidValue)?;
                }
                let shape = LefShape::Polygon(mask, points);
                Ok(self.parse_geometry_tail(is_iterate, shape)?)
            }
            LefKey::Path => {
                let mask = self.parse_geometry_mask()?;
                let is_iterate = self.parse_iterate()?;
                let points = self.parse_point_list()?;
                if points.len() < 2 {
                    self.fail(LefParseErrorType::InvalidValue)?;
                }
                let shape = LefShape::Path(mask, points);
                Ok(self.parse_geometry_tail(is_iterate, shape)?)
            }
            _ => self.fail(LefParseErrorType::InvalidKey)?,
        }
    }
    /// Parse the tail end of a geometry statement after the point-set.
    ///   either it will end with a semicolon (non-iterate case)
    ///   or have a [LefStepPattern] in the iterate case.
    fn parse_geometry_tail(&mut self, is_iterate: bool, shape: LefShape) -> LefResult<LefGeometry> {
        if is_iterate {
            let pattern = self.parse_step_pattern()?;
            self.expect(TokenType::SemiColon)?;
            Ok(LefGeometry::Iterate { shape, pattern })
        } else {
            self.expect(TokenType::SemiColon)?;
            Ok(LefGeometry::Shape(shape)) // And return the Rect
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

    fn parse_property(&mut self, mut propvec: Vec<LefProperty>) -> LefResult<Vec<LefProperty>> {
        self.expect_key(LefKey::Property)?;
        while !self.matches(TokenType::SemiColon) {
            let name = self.parse_ident()?;
            match self.peek_token() {
                Some(t)
                    if (t.ttype == TokenType::Name
                        || t.ttype == TokenType::Number
                        || t.ttype == TokenType::StringLiteral) =>
                {
                    let value = String::from(self.txt(&t));
                    self.advance()?;
                    propvec.push(LefProperty { name, value })
                }
                None | Some(_) => {
                    self.fail_msg(
                        LefParseErrorType::InvalidValue,
                        "Unexpected token while parsing PROPERTY, must be string/number/name",
                    )?;
                    //self.fail(LefParseErrorType::InvalidValue)?;
                }
            }
        }
        self.expect(TokenType::SemiColon)?;
        Ok(propvec)
    }

    // get the tail portion of a numeric PROPERTYDEFINITION entry which has an optional value and range
    fn parse_property_definition_tail(
        &mut self,
    ) -> LefResult<(Option<LefDecimal>, Option<LefPropertyRange>)> {
        let mut lvalue: Option<LefDecimal> = None;
        let mut lrange: Option<LefPropertyRange> = None;
        if self.matches(TokenType::Name) {
            self.expect_key(LefKey::Range)?;
            let rbegin = self.parse_number()?;
            let rend = self.parse_number()?;
            lrange = Some(LefPropertyRange {
                begin: rbegin,
                end: rend,
            });
        }
        if self.matches(TokenType::Number) {
            lvalue = Some(self.parse_number()?);
        }
        self.expect(TokenType::SemiColon)?;
        return Ok((lvalue, lrange));
    }

    /// Parse [LefPropertyDefinition]s
    fn parse_property_definitions(&mut self) -> LefResult<Vec<LefPropertyDefinition>> {
        use LefKey::{
            End, Layer, Library, Macro, NonDefaultRule, Pin, PropertyDefinitions, Via, ViaRule,
        };
        self.ctx.push(LefParseContext::PropertyDefinitions);
        self.expect_key(PropertyDefinitions)?;
        let mut propdefs = Vec::new();
        loop {
            match self.peek_key()? {
                Layer | Library | Macro | NonDefaultRule | Pin | Via | ViaRule => {
                    let objtype = self.parse_enum::<LefPropertyDefinitionObjectType>()?;
                    let propname = String::from(self.get_name()?);
                    match self.get_key()? {
                        LefKey::String => {
                            let value = if self.matches(TokenType::SemiColon) {
                                None
                            } else {
                                let tok = self.expect(TokenType::StringLiteral)?;
                                let txt = self.txt(&tok);
                                Some(String::from(txt))
                            };
                            self.expect(TokenType::SemiColon)?;
                            propdefs
                                .push(LefPropertyDefinition::LefString(objtype, propname, value));
                        }
                        LefKey::Real => {
                            let (optval, optrange) = self.parse_property_definition_tail()?;
                            propdefs.push(LefPropertyDefinition::LefReal(
                                objtype, propname, optval, optrange,
                            ));
                        }
                        LefKey::Integer => {
                            let (optval, optrange) = self.parse_property_definition_tail()?;
                            propdefs.push(LefPropertyDefinition::LefInteger(
                                objtype, propname, optval, optrange,
                            ));
                        }
                        _ => self.fail(LefParseErrorType::InvalidKey)?,
                    }
                }
                End => {
                    self.advance()?;
                    self.expect_key(PropertyDefinitions)?;
                    break; // End of PROPERTYDEFINITIONS definitions
                }
                _ => self.fail(LefParseErrorType::InvalidKey)?,
            }
        }
        self.ctx.pop();
        Ok(propdefs)
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
    /// Parse a Lef VIA definition
    fn parse_via(&mut self) -> LefResult<LefViaDef> {
        self.ctx.push(LefParseContext::Via);
        self.expect_key(LefKey::Via)?;
        let mut via: LefViaDefBuilder = Default::default();
        let name = self.parse_ident()?;
        via = via.name(name.clone());

        if let LefKey::Default = self.peek_key()? {
            self.advance()?; // Consume the DEFAULT
            via = via.default(true);
        }

        if let LefKey::ViaRule = self.peek_key()? {
            self.advance()?; // Eat the VIARULE key
            let mut data = LefGeneratedViaDefBuilder::default();
            data = data.via_rule_name(self.parse_ident()?);
            self.expect(TokenType::SemiColon)?;
            loop {
                match self.peek_key()? {
                    LefKey::CutSize => {
                        self.advance()?; // Eat the CUTSIZE key
                        data = data.cut_size_x(self.parse_number()?);
                        data = data.cut_size_y(self.parse_number()?);
                        self.expect(TokenType::SemiColon)?;
                    }
                    LefKey::Layers => {
                        self.advance()?; // Eat the LAYERS key
                        data = data.bot_metal_layer(self.parse_ident()?);
                        data = data.cut_layer(self.parse_ident()?);
                        data = data.top_metal_layer(self.parse_ident()?);
                        self.expect(TokenType::SemiColon)?;
                    }
                    LefKey::CutSpacing => {
                        self.advance()?; // Eat the CUTSPACING key
                        data = data.cut_spacing_x(self.parse_number()?);
                        data = data.cut_spacing_y(self.parse_number()?);
                        self.expect(TokenType::SemiColon)?;
                    }
                    LefKey::Enclosure => {
                        self.advance()?; // Eat the ENCLOSURE key
                        data = data.bot_enc_x(self.parse_number()?);
                        data = data.bot_enc_y(self.parse_number()?);
                        data = data.top_enc_x(self.parse_number()?);
                        data = data.top_enc_y(self.parse_number()?);
                        self.expect(TokenType::SemiColon)?;
                    }
                    LefKey::RowCol => {
                        self.advance()?; // Eat the ROWCOL key
                        data = data.rowcol(LefRowCol {
                            rows: self.parse_number()?,
                            cols: self.parse_number()?,
                        });
                        self.expect(TokenType::SemiColon)?;
                    }
                    LefKey::Origin => {
                        self.advance()?; // Eat the ORIGIN key
                        data = data.origin(self.parse_point()?);
                        self.expect(TokenType::SemiColon)?;
                    }
                    LefKey::Offset => {
                        self.advance()?; // Eat the OFFSET key
                        data = data.offset(LefOffset {
                            bot_x: self.parse_number()?,
                            bot_y: self.parse_number()?,
                            top_x: self.parse_number()?,
                            top_y: self.parse_number()?,
                        });
                        self.expect(TokenType::SemiColon)?;
                    }
                    LefKey::Pattern | LefKey::Property => {
                        self.fail(LefParseErrorType::Unsupported)?
                    }
                    LefKey::End => {
                        break;
                    }
                    _ => self.fail(LefParseErrorType::InvalidKey)?,
                }
            }
            via = via.data(LefViaDefData::Generated(data.build()?));
        } else {
            let mut data = LefFixedViaDefBuilder::default();

            if let LefKey::Resistance = self.peek_key()? {
                self.advance()?; // Eat the RESISTANCE key
                data = data.resistance_ohms(self.parse_number()?);
                self.expect(TokenType::SemiColon)?;
            }

            let mut layers = Vec::new();
            while let LefKey::Layer = self.peek_key()? {
                layers.push(self.parse_via_layer_geometries()?);
            }
            data = data.layers(layers);
            via = via.data(LefViaDefData::Fixed(data.build()?));
        }

        match self.peek_key()? {
            LefKey::Property => self.fail(LefParseErrorType::Unsupported)?,
            LefKey::End => {
                self.advance()?; // End of Via. Eat the END key
            }
            _ => self.fail(LefParseErrorType::InvalidKey)?,
        }
        // Parse the END-enclosing via-name
        self.expect_ident(&name)?;
        self.ctx.pop();
        Ok(via.build()?)
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
    fn it_lexes_numbers_and_names() -> LefResult<()> {
        let src = "STUFF 101 -98765 .01 1.23 -0.87 1e6 1.06E-7 18T ;";
        let lex = LefLexer::new(src)?;
        let toks_vec: Vec<Token> = lex.collect(); // Collect up all tokens
        let tok_strs: Vec<&str> = toks_vec.iter().map(|t| t.substr(src)).collect();
        assert_eq!(
            tok_strs,
            vec!["STUFF", "101", "-98765", ".01", "1.23", "-0.87", "1e6", "1.06E-7", "18T", ";"]
        );
        let tok_types: Vec<TokenType> = toks_vec.iter().map(|t| t.ttype).collect();
        assert_eq!(
            tok_types,
            vec![
                TokenType::Name,
                TokenType::Number,
                TokenType::Number,
                TokenType::Number,
                TokenType::Number,
                TokenType::Number,
                TokenType::Number,
                TokenType::Number,
                TokenType::Name,
                TokenType::SemiColon
            ]
        );
        Ok(())
    }
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
        VERSION 5.3 ;
        MACRO valid_source_user
            SOURCE USER ;
        END valid_source_user
        END LIBRARY
        "#;
        parse_str(src)?;

        // Check that for version 5.5 the same MACRO produces an error
        let src = r#"
        VERSION 5.5 ;
        MACRO invalid_source_user
            SOURCE USER ;
        END invalid_source_user
        END LIBRARY
        "#;
        assert!(parse_str(src).is_err());
        Ok(())
    }
}
