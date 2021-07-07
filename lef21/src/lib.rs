//!
//! # Lef21 Library Exchange Format (LEF) Parser & Writer
//!

use std::fmt;
use std::fs::File;
use std::io::{BufReader, BufWriter};
use std::io::{Read, Write};
use std::marker::PhantomData;
use std::mem;
use std::str;
use std::str::Chars;

#[allow(unused_imports)]
use std::io::prelude::*;

use serde::{Deserialize, Serialize};

#[macro_use]
extern crate derive_builder;

// struct Source<'s>(&'s str);

/// # Lef Lexer / Tokenizer
///
/// Breaks input string `self.src` into an iteration of [Token]s,
/// consisting of source-locations and type-annotations.
///
/// Operates in an iterator-style mode, producing a [Token] with
/// each call to `lex_one`.
///
struct LefLexer<'src> {
    /// Source string
    src: &'src str,
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
    /// i.e. whether any semantic content has been
    /// encountered on the current line
    at_bol: bool,
}
/// [LefLexer] Implementation
/// Note that while [LefLexer] *data* is generic over the source-string lifetime `'src`,
/// many implementation methods must include the "self lifetime",
/// or the lifetime of the lexer, `'lex`.
/// Typically we expect that the source-string ('src) *can* outlive `self` ('lex),
/// as can many of the generated [FatToken] objects which include references to `'src`.
impl<'src> LefLexer<'src> {
    fn new(src: &'src str) -> LefResult<Self> {
        // Create our character-iterator
        let mut chars = src.chars();
        // Read the first character into our `next` field
        let next_char = chars.next();
        // Create the Lexer
        let mut lex = Self {
            src,
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
        mem::swap(&mut rv, &mut self.next_char);
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
        mem::swap(&mut tok, &mut self.next_tok);
        Ok(tok)
    }
    /// Get an immutable reference to our next [Token], without advancing
    fn peek_token(&self) -> &Option<Token> {
        &self.next_tok
    }
    /// Pull our next [Token], removing ignored items such as commentary and whitespace.
    /// FIXME: it's not yet completely clear whether whitespace for indentation is semantically relevant.
    /// This implementation ignores all WS, presuming it is not.
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
    /// Lex all of input `src`, collecting into a vector of tokens.
    /// Primarily used for debug and test;
    /// parsing instead uses the one-at-a-time token-iterator.
    fn lex_all(src: &'src str) -> LefResult<Vec<Token>> {
        let mut lex = LefLexer::new(src)?;
        let mut toks = Vec::new();
        while let Some(t) = lex.next_token()? {
            toks.push(t);
        }
        Ok(toks)
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
                return Err(LefError::Lex {
                    line: self.line,
                    pos: self.pos,
                });
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
            return Err(LefError::Tbd);
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
    /// Ignore active characters by bumping our start-index to the current index.
    fn ignore(&mut self) {
        self.start = self.pos;
    }
    /// Return a [LefError::Lex] for our current position
    fn err<T>(&self) -> LefResult<T> {
        dbg!(5);
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
    fn substr<'me, 'src>(&self, src: &'src str) -> &'src str {
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
    ParseInt(std::num::ParseIntError),
    Io(std::io::Error),
    Str(String),
    Lex {
        line: usize,
        pos: usize,
    },
    Parse {
        string: String,
        line: usize,
        pos: usize,
    },
    Tbd,
}
impl From<std::io::Error> for LefError {
    /// Convert common IO & file errors by wrapping them
    fn from(e: std::io::Error) -> Self {
        Self::Io(e)
    }
}
impl From<std::num::ParseIntError> for LefError {
    /// Convert integer-parsing errors by wrapping them
    fn from(e: std::num::ParseIntError) -> Self {
        Self::ParseInt(e)
    }
}
impl From<String> for LefError {
    /// Convert string-based errors by wrapping them
    fn from(e: String) -> Self {
        Self::Str(e)
    }
}
type LefResult<T> = Result<T, LefError>;
/// Lef Number
/// All are decimal format, with signed integer-part
/// and unsigned fractional part
#[derive(Debug, Default, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
struct LefDecimal {
    /// Integer Part
    integ: isize,
    /// Fractional Part
    frac: usize,
}
struct LefParser<'src> {
    /// Lexer
    lex: LefLexer<'src>,
    /// Parsed Library
    lib: LefLibrary,
}
impl<'src> LefParser<'src> {
    /// Construct a [LefParser] of input-text `src`
    fn new(src: &'src str) -> LefResult<Self> {
        Ok(Self {
            lex: LefLexer::new(src)?,
            lib: LefLibrary::default(),
        })
    }
    #[inline(always)]
    fn next_token(&mut self) -> LefResult<Option<Token>> {
        self.lex.next_token()
    }
    /// Boolean indication of whether our next Token is of [TokenType] `ttype`.
    /// Advances and returns
    fn matches(&mut self, ttype: TokenType) -> bool {
        match self.lex.peek_token() {
            Some(t) if t.ttype == ttype => true,
            _ => false,
        }
    }
    /// Assert the expectation that the next [Token] is of [TokenType] `ttype`.
    /// Returns the [Token] if so. Returns an [Err] if not.
    fn expect(&mut self, ttype: TokenType) -> LefResult<Token> {
        match self.next_token()? {
            Some(t) if t.ttype == ttype => return Ok(t),
            _ => self.err(),
        }
    }
    /// Assert that the next [Token] is a [TokenType::Name],
    /// and that its string value matches `name`.
    /// This is often used for pseudo-keywords,
    /// such as the "BY" in lines such as "SIZE x BY y".
    fn expect_name(&mut self, name: &str) -> LefResult<Token> {
        let tok = self.expect(TokenType::Name)?;
        let txt = self.txt(tok);
        if txt == name {
            Ok(tok)
        } else {
            self.err()
        }
    }
    /// Assert the next [Token] is of type [TokenType::Name],
    /// and return its string value.
    #[inline(always)]
    fn get_name(&mut self) -> LefResult<&str> {
        let tok = self.expect(TokenType::Name)?;
        Ok(self.txt(tok))
    }
    /// Retrieve the text-content of lexer [Token] `tok`
    fn txt(&self, tok: Token) -> &str {
        tok.substr(self.lex.src)
    }
    /// Parse a [LefLibrary]
    fn parse_lib(&mut self) -> LefResult<LefLibrary> {
        let mut lib = LefLibraryBuilder::default();
        let mut macros = Vec::new();
        while let Some(t) = self.next_token()? {
            let txt = self.txt(t);
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
        let pins = Vec::new();
        let obs = Vec::new();
        while let Some(t) = self.next_token()? {
            let txt = self.txt(t);
            match txt {
                "CLASS" => {
                    mac.class(self.parse_macro_class()?);
                }
                "FOREIGN" => unimplemented!(),
                "ORIGIN" => unimplemented!(),
                "SIZE" => unimplemented!(),
                "PIN" => unimplemented!(),
                "OBS" => unimplemented!(),
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
        let txt = self.txt(tok);
        if txt.contains(".") {
            // Decimal-valued
            let parts: Vec<&str> = txt.split(".").collect();
            if parts.len() != 2 {
                return self.err();
            }
            let integ: isize = parts[0].parse()?;
            let frac: usize = parts[1].parse()?;
            return Ok(LefDecimal { integ, frac });
        }
        // Integer-valued
        let integ: isize = txt.parse()?;
        Ok(LefDecimal { integ, frac: 0 })
    }
    /// Parse the LefLibrary::BUSBITCHARS key,
    /// from a two-character string literal
    fn parse_bus_bit_chars(&mut self) -> LefResult<(char, char)> {
        let tok = self.expect(TokenType::StringLiteral)?;
        let txt = self.txt(tok);
        if txt.len() != 4 {
            return self.err();
        }
        let mut chars = txt.chars();
        chars.next(); // Bump opening paren
        let open = chars.next().ok_or(LefError::Tbd)?;
        let close = chars.next().ok_or(LefError::Tbd)?;
        chars.next(); // Bump closing paren
        self.expect(TokenType::SemiColon)?;
        self.expect(TokenType::NewLine)?;
        Ok((open, close))
    }
    /// Parse the LefLibrary::DIVIDERCHAR key,
    /// from a single-character string literal
    fn parse_divider_char(&mut self) -> LefResult<char> {
        let tok = self.expect(TokenType::StringLiteral)?;
        let txt = self.txt(tok);
        if txt.len() != 3 {
            return self.err();
        }
        let mut chars = txt.chars();
        chars.next(); // Bump opening paren
        let c = chars.next().ok_or(LefError::Tbd)?;
        chars.next(); // Bump closing paren
        self.expect(TokenType::SemiColon)?;
        self.expect(TokenType::NewLine)?;
        Ok(c)
    }
    /// Parse an identifier name, e.g. a macro, pin, or layer name.
    fn parse_ident(&mut self) -> LefResult<String> {
        let tok = self.expect(TokenType::Name)?;
        let txt = self.txt(tok);
        Ok(String::from(txt))
    }
    fn err<T>(&self) -> LefResult<T> {
        dbg!(5);
        let string = match self.lex.next_tok {
            Some(t) => self.txt(t).to_string(),
            None => "EOF".to_string(),
        };
        Err(LefError::Parse {
            string,
            line: self.lex.line,
            pos: self.lex.pos,
        })
    }
}
#[derive(Default, Clone, Builder, Debug, Deserialize, Serialize, PartialEq)]
#[builder(setter(into), private)]
struct LefLibrary {
    /// Lef Spec Version
    version: LefDecimal,
    /// Macro Definitions
    macros: Vec<LefMacro>,
    /// Bus-Bit Separator Characters
    #[serde(default)]
    #[builder(default, setter(strip_option))]
    bus_bit_chars: Option<(char, char)>,
    /// Divider Character
    #[serde(default)]
    #[builder(default, setter(strip_option))]
    divider_char: Option<char>,
    #[serde(default)]
    #[builder(default, setter(strip_option))]
    via: Tbd,
    #[serde(default)]
    #[builder(default, setter(strip_option))]
    site: Tbd,
}
#[derive(Clone, Builder, Debug, Deserialize, Serialize, PartialEq)]
#[builder(setter(into), private)]
struct LefMacro {
    /// Macro Name
    name: String,
    /// Macro Class
    #[serde(default)]
    #[builder(default, setter(strip_option))]
    class: Option<LefMacroClass>,

    /// Foreign (GDSII) Cell
    #[serde(default)]
    #[builder(default, setter(strip_option))]
    foreign: Option<LefForeign>,

    /// X-Y Origin
    #[serde(default)]
    #[builder(default, setter(strip_option))]
    origin: Option<(LefDecimal, LefDecimal)>,

    /// Electrically-Equivalent Cell
    #[serde(default)]
    #[builder(default, setter(strip_option))]
    eeq: Option<Tbd>,

    /// Outline Size
    #[serde(default)]
    #[builder(default, setter(strip_option))]
    size: Option<(LefDecimal, LefDecimal)>,
    /// Rotational & Translation Symmetries
    #[serde(default)]
    #[builder(default, setter(strip_option))]
    symmetry: Tbd,

    /// Site Name
    #[serde(default)]
    #[builder(default, setter(strip_option))]
    site: Tbd,
    /// Density Objects
    #[serde(default)]
    #[builder(default, setter(strip_option))]
    density: Option<Tbd>,

    /// Properties
    #[serde(default)]
    #[builder(default, setter(strip_option))]
    properties: Option<Tbd>,
    /// Pin List
    pins: Vec<LefPin>,

    /// Obstructions
    obs: Vec<Tbd>,
}
#[derive(Clone, Debug, Deserialize, Serialize, PartialEq)]
enum LefMacroClass {
    Cover { bump: bool },
    Ring,
    Block { tp: Option<LefBlockClassType> },
    Pad { tp: Option<LefPadClassType> },
    Core { tp: Option<LefCoreClassType> },
    EndCap { tp: LefEndCapClassType },
}
#[derive(Clone, Debug, Deserialize, Serialize, PartialEq)]
enum LefBlockClassType {
    BlackBox,
    Soft,
}
#[derive(Clone, Debug, Deserialize, Serialize, PartialEq)]
enum LefCoreClassType {
    FeedThru,
    TieHigh,
    TieLow,
    Spacer,
    AntennaCell,
    WellTap,
}
#[derive(Clone, Debug, Deserialize, Serialize, PartialEq)]
enum LefPadClassType {
    Input,
    Output,
    Inout,
    Power,
    Spacer,
    AreaIo,
}
#[derive(Clone, Debug, Deserialize, Serialize, PartialEq)]
enum LefEndCapClassType {
    Pre,
    Post,
    TopLeft,
    TopRight,
    BottomLeft,
    BottomRight,
}

#[derive(Clone, Builder, Debug, Deserialize, Serialize, PartialEq)]
#[builder(setter(into), private)]
struct LefForeign {
    /// Foreign Cell Name
    cell_name: String,
    /// Location
    pt: Option<(LefDecimal, LefDecimal)>,
    /// Orientation
    orient: Tbd,
}
#[derive(Clone, Builder, Debug, Deserialize, Serialize, PartialEq)]
#[builder(setter(into), private)]
struct LefPin {
    // Required Fields 

    /// Pin Name
    name: String,
    /// Port Geometries 
    ports: Vec<LefPort>,

    // Optional Fields 

    /// Properties
    #[serde(default)]
    #[builder(default, setter(strip_option))]
    properties: Option<Tbd>,
    
    /// Direction
    #[serde(default)]
    #[builder(default, setter(strip_option))]
    direction: Option<LefPinDirection>,
    
    /// Usage / Role 
    #[serde(default)]
    #[builder(default, setter(strip_option))]
    r#use: Option<LefPinUse>,

    /// Taper Rule
    #[serde(default)]
    #[builder(default, setter(strip_option))]
    taper_rule: Option<Tbd>,
    
    /// Net Expression
    #[serde(default)]
    #[builder(default, setter(strip_option))]
    net_expr: Option<Tbd>,
    
    /// Supply Sensitivity
    #[serde(default)]
    #[builder(default, setter(strip_option))]
    supply_sensitivity: Option<Tbd>,
    
    /// Ground Sensitivity
    #[serde(default)]
    #[builder(default, setter(strip_option))]
    ground_sensitivity: Option<Tbd>,
    /// Shape
    #[serde(default)]
    #[builder(default, setter(strip_option))]
    shape: Option<Tbd>,
    /// Must-Join
    #[serde(default)]
    #[builder(default, setter(strip_option))]
    must_join: Option<Tbd>,

    /// Antenna 
    #[serde(default)]
    #[builder(default, setter(strip_option))]
    antenna_partial_metal_area: Option<Tbd>,
    #[serde(default)]
    #[builder(default, setter(strip_option))]
    antenna_partial_metal_side_area: Option<Tbd>,
    #[serde(default)]
    #[builder(default, setter(strip_option))]
    antenna_partial_cut_area: Option<Tbd>,
    #[serde(default)]
    #[builder(default, setter(strip_option))]
    antenna_diff_area: Option<Tbd>,
    #[serde(default)]
    #[builder(default, setter(strip_option))]
    antenna_model: Option<Tbd>,
    #[serde(default)]
    #[builder(default, setter(strip_option))]
    antenna_gate_area: Option<Tbd>,
    #[serde(default)]
    #[builder(default, setter(strip_option))]
    antenna_max_area_char: Option<Tbd>,
    #[serde(default)]
    #[builder(default, setter(strip_option))]
    antenna_side_area_char: Option<Tbd>,
    #[serde(default)]
    #[builder(default, setter(strip_option))]
    antenna_max_cut_char: Option<Tbd>,
}
#[derive(Clone, Debug, Deserialize, Serialize, PartialEq)]
enum LefPinDirection {
    Input,
    Output { tristate: bool },
    Inout,
    FeedThru,
}
#[derive(Clone, Debug, Deserialize, Serialize, PartialEq)]
enum LefPinUse {
    Signal,
    Analog,
    Power,
    Ground,
    Clock,
}
#[derive(Clone, Builder, Debug, Deserialize, Serialize, PartialEq)]
struct LefPort {
    /// Layers & Geometries
    layers: Vec<LefPortLayerGeometries>,
    /// Port-Class
    class: Option<Tbd>,
}
#[derive(Clone, Builder, Debug, Deserialize, Serialize, PartialEq)]
struct LefPortLayerGeometries {
    /// Layer Name
    layer: String,
    
    #[serde(default)]
    #[builder(default, setter(strip_option))]
    except_pg_net: Option<bool>,
    #[serde(default)]
    #[builder(default, setter(strip_option))]
    spacing: Option<Tbd>,// FIXME: merge with `design_rule_width`
    #[serde(default)]
    #[builder(default, setter(strip_option))]
    design_rule_width: Option<Tbd>, // FIXME: merge with `spacing`
    #[serde(default)]
    #[builder(default, setter(strip_option))]
    width: Option<Tbd>, 

    /// Geometries, 
    geometries: Vec<LefGeometry>
}
/// Lef Geometric Objects - 
/// Rectangles, Polygons, Paths, and Iterators thereof
#[derive(Clone, Debug, Deserialize, Serialize, PartialEq)]
enum LefGeometry {
    Shape(LefShape),
    Iterate{shape: LefShape, pattern: Tbd},
}
#[derive(Clone, Debug, Deserialize, Serialize, PartialEq)]
enum LefShape {
    Rect(LefPoint, LefPoint),
    Polygon(Vec<LefPoint>),
    Path(Vec<LefPoint>),
}
/// X-Y Point
#[derive(Clone, Default, Debug, Deserialize, Serialize, PartialEq)]
struct LefPoint(LefDecimal, LefDecimal);
/// Placeholder Struct for Fields to be completed 
#[derive(Clone, Default, Debug, Deserialize, Serialize, PartialEq)]
struct Tbd;

// pub fn parse_file(fname: &str) -> LefResult<()> {
//     let mut file = File::open(fname)?;
//     let mut src = String::new();
//     file.read_to_string(&mut src)?;
//     let res = LefParse::parse(&src)?;
//     Ok(())
// }
fn parse_str(src: &str) -> LefResult<LefLibrary> {
    let mut parser = LefParser::new(src)?;
    let lib = parser.parse_lib()?;
    Ok(lib)
}

#[cfg(test)]
mod tests {
    use super::*;
    type TestResult = LefResult<()>;

    #[test]
    fn it_lexes() -> TestResult {
        let src = "STUFF 101 ; \n // commentary \n";
        let toks = LefLexer::new(src)?;
        let tokstrs = toks.map(|t| t.substr(src)).collect::<Vec<&str>>();
        assert_eq!(tokstrs, vec!["STUFF", "101", ";", "\n",]);
        Ok(())
    }

    #[test]
    fn it_parses() -> TestResult {
        let src = r#"
            VERSION 5 ; // commentary 
            BUSBITCHARS "xy" ; 
            MACRO some_name 
            END some_name
        "#;
        let lib = parse_str(src)?;
        dbg!(lib);
        Ok(())
    }
}
