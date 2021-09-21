//!
//! # Lef Reading Module
//!
//! Facilities for reading LEF-encoded content from file or string.
//! Includes the core Lexer and Parser classes.
//!

// Standard Lib Imports
use std::io::Read;
use std::str::Chars;

// Local imports
use super::*;

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
    /// Boolean indication of "beginning of line",
    /// i.e. whether any semantic content has been encountered on the current line.
    at_bol: bool,
}
impl<'src> LefLexer<'src> {
    pub fn new(src: &'src str) -> LefResult<Self> {
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
    /// While the primary API method is `next_token`, this "underscore version"
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
    pub fn substr<'me, 'src>(&'me self, src: &'src str) -> &'src str {
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
            lef_version: LefDecimal::new(5, 8), // Version defaults to 5.8
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
    pub fn new(src: &'src str) -> LefResult<Self> {
        let lex = LefLexer::new(src)?;
        Ok(Self {
            src,
            lex,
            ctx: Vec::new(),
            session: LefParseSession::default(),
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
        self.lex.peek_token()
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
            self.fail(LefParseErrorType::RequiredWord {
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
            lib = match self.peek_key()?.as_str() {
                "MACRO" => {
                    macros.push(self.parse_macro()?);
                    lib
                }
                "VERSION" => lib.version(self.parse_version()?),
                "BUSBITCHARS" => lib.bus_bit_chars(self.parse_bus_bit_chars()?),
                "DIVIDERCHAR" => lib.divider_char(self.parse_divider_char()?),
                "NAMESCASESENSITIVE" => {
                    // Valid for versions <= 5.4
                    if self.session.lef_version > LefDecimal::new(5, 4) {
                        self.fail(LefParseErrorType::InvalidKey)?;
                    }
                    self.advance()?; // Eat the "NAMESCASESENSITIVE" key
                    let e = self.parse_enum::<LefOnOff>()?;
                    self.expect(TokenType::SemiColon)?;
                    lib.names_case_sensitive(e)
                }
                "NOWIREEXTENSIONATPIN" => {
                    self.advance()?; // Eat the "NOWIREEXTENSIONATPIN" key
                    let e = self.parse_enum::<LefOnOff>()?;
                    self.expect(TokenType::SemiColon)?;
                    lib.no_wire_extension_at_pin(e)
                }
                "UNITS" => lib.units(self.parse_units()?),
                "SITE" => {
                    sites.push(self.parse_site_def()?);
                    lib
                }
                "END" => {
                    self.advance()?; // Expect "END LIBRARY".
                    self.expect_keyword("LIBRARY")?;
                    break;
                }
                "BEGINEXT" => return self.fail(LefParseErrorType::Unsupported),
                _ => return self.fail(LefParseErrorType::InvalidKey),
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
        self.advance()?; // Eat the "VERSION" key
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
        self.expect_keyword("MACRO")?;
        self.ctx.push(LefParseContext::Macro);
        let mut mac = LefMacroBuilder::default();
        // Parse the macro-name
        let name = self.parse_ident()?;
        mac = mac.name(name.clone());
        // Start parsing attributes, pins, and obstructions
        let mut pins = Vec::new();
        loop {
            mac = match self.peek_key()?.as_str() {
                "CLASS" => mac.class(self.parse_macro_class()?),
                "SITE" => {
                    self.advance()?; // Eat the "SITE" key
                    let id = self.parse_ident()?;
                    self.expect(TokenType::SemiColon)?;
                    mac.site(id)
                }
                "FOREIGN" => {
                    self.advance()?; // Eat the "FOREIGN" key
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
                "ORIGIN" => {
                    self.advance()?; // Eat the "ORIGIN" key
                    let pt = self.parse_point()?;
                    self.expect(TokenType::SemiColon)?;
                    mac.origin(pt)
                }
                "SIZE" => mac.size(self.parse_size()?),
                "PIN" => {
                    pins.push(self.parse_pin()?);
                    mac
                }
                "OBS" => mac.obs(self.parse_obstructions()?),
                "SYMMETRY" => mac.symmetry(self.parse_symmetries()?),
                "SOURCE" => {
                    // Valid for versions <= 5.4
                    if self.session.lef_version > LefDecimal::new(5, 4) {
                        self.fail(LefParseErrorType::InvalidKey)?;
                    }
                    self.advance()?; // Eat the "SOURCE" key
                    let e = self.parse_enum::<LefDefSource>()?;
                    self.expect(TokenType::SemiColon)?;
                    mac.source(e)
                }
                "END" => {
                    self.advance()?; // End of Macro. Eat the "END" key
                    break;
                }
                _ => return self.fail(LefParseErrorType::InvalidKey),
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
        self.expect_keyword("PIN")?;
        self.ctx.push(LefParseContext::Pin);
        let mut pin = LefPinBuilder::default();
        // Parse the pin-name
        let name = self.parse_ident()?;
        pin = pin.name(name.clone());
        let mut ports = Vec::new();
        let mut antenna_attrs = Vec::new();
        loop {
            pin = match self.peek_key()?.as_str() {
                "END" => {
                    self.advance()?; // End of Pin. Eat the "END" key.
                    break;
                }
                "PORT" => {
                    ports.push(self.parse_port()?);
                    pin
                }
                "DIRECTION" => pin.direction(self.parse_pin_direction()?),
                "USE" => {
                    self.advance()?;
                    self.expect(TokenType::SemiColon)?;
                    let e = self.parse_enum::<LefPinUse>()?;
                    pin.use_(e)
                }
                "SHAPE" => {
                    self.advance()?;
                    let e = self.parse_enum::<LefPinShape>()?;
                    self.expect(TokenType::SemiColon)?;
                    pin.shape(e)
                }
                "ANTENNAMODEL" => {
                    self.advance()?;
                    pin.antenna_model(self.parse_enum::<LefAntennaModel>()?)
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
                    pin
                }
                "TAPERRULE" | "NETEXPR" | "SUPPLYSENSITIVITY" | "GROUNDSENSITIVITY"
                | "MUSTJOIN" | "PROPERTY" => return self.fail(LefParseErrorType::Unsupported),
                _ => return self.fail(LefParseErrorType::InvalidKey),
            }
        }
        // Get the pin-closing "END <name>"
        self.expect_ident(&name)?;
        // Set our port-objects, build and return the Pin
        pin = pin.ports(ports);
        pin = pin.antenna_attrs(antenna_attrs);
        self.ctx.pop();
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
            _ => return self.fail(LefParseErrorType::InvalidValue),
        };
        self.expect(TokenType::SemiColon)?;
        Ok(pin)
    }
    /// Parse a MACRO::PIN::PORT definition into a [LefPort]
    fn parse_port(&mut self) -> LefResult<LefPort> {
        self.expect_keyword("PORT")?;
        self.ctx.push(LefParseContext::Port);
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
                _ => return self.fail(LefParseErrorType::InvalidKey),
            }
        }
        self.ctx.pop();
        Ok(LefPort { layers, class })
    }
    /// Parse a [LefMacro]'s obstruction definitions
    fn parse_obstructions(&mut self) -> LefResult<Vec<LefLayerGeometries>> {
        self.expect_keyword("OBS")?;
        let mut geoms = Vec::new();
        while let Some(t) = self.peek_token() {
            if t.ttype != TokenType::Name {
                return self.fail(LefParseErrorType::InvalidToken {
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
                _ => return self.fail(LefParseErrorType::InvalidKey),
            }
        }
        Ok(geoms)
    }
    /// Parse a set of geometries on a single layer, as commonly specified per-[LefPort]
    pub(crate) fn parse_layer_geometries(&mut self) -> LefResult<LefLayerGeometries> {
        self.ctx.push(LefParseContext::Geometry);
        let mut layer = LefLayerGeometriesBuilder::default();
        // Check for the opening "LAYER" keyword
        self.expect_keyword("LAYER")?;
        // Parse the layer-name
        layer = layer.layer_name(self.parse_ident()?);
        // Parse the options defined inline with the LAYER statement
        while !self.matches(TokenType::SemiColon) {
            layer = match self.get_name()?.to_ascii_uppercase().as_str() {
                "EXCEPTPGNET" => layer.except_pg_net(true),
                "SPACING" => layer.spacing(LefLayerSpacing::Spacing(self.parse_number()?)),
                "DESIGNRULEWIDTH" => {
                    layer.spacing(LefLayerSpacing::DesignRuleWidth(self.parse_number()?))
                }
                _ => return self.fail(LefParseErrorType::InvalidKey),
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
                return self.fail(LefParseErrorType::InvalidToken {
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
                        // The ITERATE construction is not supported.
                        return self.fail(LefParseErrorType::Unsupported);
                    }
                    let pt = self.parse_point()?;
                    let via_name = self.parse_ident()?;
                    self.expect(TokenType::SemiColon)?;
                    vias.push(LefVia { pt, via_name });
                }
                "WIDTH" => {
                    self.advance()?; // Eat the WIDTH Token
                    layer = layer.width(self.parse_number()?);
                    self.expect(TokenType::SemiColon)?;
                }
                _ => self.fail(LefParseErrorType::InvalidKey)?,
            }
        }
        layer = layer.vias(vias);
        layer = layer.geometries(geoms);
        self.ctx.pop();
        Ok(layer.build()?)
    }
    /// Parse a [LefGeometry] statement
    /// Each can be a shape or iteration thereof
    fn parse_geometry(&mut self) -> LefResult<LefGeometry> {
        match self.get_name()?.to_ascii_uppercase().as_str() {
            "RECT" => {
                if self.matches(TokenType::Name) {
                    // The ITERATE construction is not supported.
                    self.fail(LefParseErrorType::Unsupported)?;
                }
                // Parse the two points
                let p1 = self.parse_point()?;
                let p2 = self.parse_point()?;
                self.expect(TokenType::SemiColon)?;
                // And return the Rect
                Ok(LefGeometry::Shape(LefShape::Rect(p1, p2)))
            }
            "POLYGON" => {
                let points = self.parse_point_list()?;
                if points.len() < 3 {
                    self.fail(LefParseErrorType::InvalidValue)?;
                }
                self.expect(TokenType::SemiColon)?;
                Ok(LefGeometry::Shape(LefShape::Polygon(points)))
            }
            "PATH" => {
                let points = self.parse_point_list()?;
                if points.len() < 2 {
                    self.fail(LefParseErrorType::InvalidValue)?;
                }
                self.expect(TokenType::SemiColon)?;
                Ok(LefGeometry::Shape(LefShape::Path(points)))
            }
            _ => return self.fail(LefParseErrorType::InvalidKey),
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
        self.expect_keyword("UNITS")?;
        self.ctx.push(LefParseContext::Units);
        let mut units = LefUnits::default();
        loop {
            match self.get_name()?.to_ascii_uppercase().as_str() {
                "DATABASE" => {
                    // Parse the "DATABASE MICRONS" flavor
                    self.expect_keyword("MICRONS")?;
                    let num = self.parse_number()?;
                    self.expect(TokenType::SemiColon)?;
                    units.database_microns = Some(LefDbuPerMicron::try_new(num)?);
                }
                "END" => {
                    // End of UNITS
                    self.expect_keyword("UNITS")?;
                    break;
                }
                // All the other united quantities are unsupported
                "TIME" | "CAPACITANCE" | "RESISTANCE" | "POWER" | "CURRENT" | "VOLTAGE"
                | "FREQUENCY" => self.fail(LefParseErrorType::Unsupported)?,
                _ => self.fail(LefParseErrorType::InvalidKey)?,
            }
        }
        self.ctx.pop();
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
            _ => return self.fail(LefParseErrorType::InvalidValue),
        }
    }
    /// Parse a [LefSite] definition
    fn parse_site_def(&mut self) -> LefResult<LefSite> {
        self.expect_keyword("SITE")?;
        self.ctx.push(LefParseContext::Site);
        let mut site = LefSiteBuilder::default();
        // Parse the site name. Keep a copy for later comparison.
        let name = self.parse_ident()?;
        site = site.name(name.clone());
        loop {
            site = match self.peek_key()?.as_str() {
                "END" => {
                    self.advance()?; // Eat the "END"
                    self.expect_ident(&name)?;
                    break;
                }
                "CLASS" => {
                    self.advance()?; // Eat the "CLASS"
                    let e = self.parse_enum::<LefSiteClass>()?;
                    self.expect(TokenType::SemiColon)?;
                    site.class(e)
                }
                "SYMMETRY" => site.symmetry(self.parse_symmetries()?),
                "SIZE" => site.size(self.parse_size()?),
                "ROWPATTERN" => self.fail(LefParseErrorType::Unsupported)?,
                _ => self.fail(LefParseErrorType::InvalidValue)?,
            }
        }
        self.ctx.pop();
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
            return self.fail(LefParseErrorType::InvalidValue);
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
            return self.fail(LefParseErrorType::InvalidValue);
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
            None => self.fail(LefParseErrorType::InvalidValue),
        }
    }
    /// Error-Generation Helper
    /// Collect our current position and content into a [LefError::Parse]
    fn fail<T>(&self, tp: LefParseErrorType) -> LefResult<T> {
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
        Err(LefError::Parse {
            tp,
            ctx: self.ctx.clone(),
            line_content,
            line_num: self.lex.line,
            token,
            pos: self.lex.pos,
        })
    }
}
