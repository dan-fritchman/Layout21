//!
//! # Lef21 Library Exchange Format (LEF) Parser & Writer
//!

use std::fmt;
use std::fs::File;
use std::io::{BufReader, BufWriter};
use std::io::{Read, Write};
use std::mem;
use std::str;

#[allow(unused_imports)]
use std::io::prelude::*;

use byteorder::{BigEndian, ReadBytesExt, WriteBytesExt};
use chrono::prelude::*;
use chrono::{Datelike, NaiveDate, NaiveDateTime};
use enum_dispatch::enum_dispatch;
use num_derive::FromPrimitive;
use num_traits::FromPrimitive;
use serde::{Deserialize, Serialize};

use std::str::Chars;

struct LefLexer<'src> {
    /// Lexed source string
    src: &'src str,
    /// Source character iterator
    chars: Chars<'src>,
    /// Peekable next character
    next_char: Option<char>,
    /// Peekable next Token
    next_tok: Option<Token>,
    /// Source length
    len: usize,
    /// Active Lexeme start-character index
    start: usize,
    /// Active character index
    pos: usize,
    /// Active line number
    line: usize,
    /// Lexer state-function
    state: Option<LexStateFunc>,
}
impl<'src> LefLexer<'src> {
    fn new(src: &'src str) -> LefResult<Self> {
        // Create our character-iterator
        let mut chars = src.chars();
        // Read the first character into our `next` field
        let next_char = chars.next();
        let mut lex = Self {
            src,
            chars,
            next_char,
            next_tok: None,
            state: None,
            len: src.len(),
            start: 0,
            pos: 0,
            line: 0,
        };
        // Read the first token into our `next` field
        lex.next_tok = lex.lex_one()?;
        Ok(lex)
    }
    /// Get and return our next character
    fn next_char(&mut self) -> Option<char> {
        if self.next_char.is_none() {
            return None;
        }
        self.pos += 1;
        let mut rv = self.chars.next();
        mem::swap(&mut rv, &mut self.next_char);
        rv
    }
    fn peek_char(&self) -> &Option<char> {
        &self.next_char
    }
    /// Get and return our next token, updating internal state along the way
    fn next_token(&mut self) -> LefResult<Option<Token>> {
        if self.next_tok.is_none() {
            return Ok(None);
        }
        let mut rv = self.lex_one()?;
        mem::swap(&mut rv, &mut self.next_tok);
        Ok(rv)
    }
    /// Emit a [Token] of [TokenType] `ttype`
    fn emit(&mut self, ttype: TokenType) -> Token {
        let tok = Token {
            start: self.start,
            stop: self.pos,
            line: self.line,
            ttype,
        };
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
    /// Lex all [Token]s in our input stream, returning as a [Vec]
    fn lex_all(&mut self) -> LefResult<Vec<Token>> {
        let mut toks = Vec::new();
        while let Some(tok) = self.next_token()? {
            toks.push(tok);
        }
        Ok(toks)
    }
    /// Lexer start state. Grab the next [Token]
    fn lex_one(&mut self) -> LefResult<Option<Token>> {
        while !self.peek_char().is_none() {
            if self.accept(|c| c == '\n') {
                // Create a NewLine Token
                let tok = self.emit(TokenType::NewLine);
                // And advance the line num
                self.line += 1;
                return Ok(Some(tok));
            } else if self.accept(|c| c.is_ascii_whitespace()) {
                // Skip other white-space
                self.ignore();
            } else if self.accept(|c| c == ';') {
                return Ok(Some(self.emit(TokenType::SemiColon)));
            } else if self.accept(|c| c == '"') {
                return Ok(Some(self.emit(TokenType::DoubleQuote)));
            } else if self.accept(|c| c == '/') {
                return self.lex_comment();
            } else if self.accept(|c| c.is_digit(10)) {
                return self.lex_number();
            } else if self.accept(|c| c.is_ascii_alphabetic()) {
                return self.lex_name();
            } else {
                // Some other, invalid character. Fail.
                return Err(LefError::Tbd);
            }
        }
        // All done! Return `None`.
        Ok(None)
    }
    /// Lex a (thus far, integer) number
    fn lex_number(&mut self) -> LefResult<Option<Token>> {
        while self.accept(|c| c.is_digit(10)) {
            continue;
        }
        let tok = self.emit(TokenType::Number);
        Ok(Some(tok))
    }
    /// Lex a comment
    fn lex_comment(&mut self) -> LefResult<Option<Token>> {
        // First slash has been read. Error if we don't get the second.
        if !self.accept_char('/') {
            return Err(LefError::Tbd);
        }
        while self.accept(|c| c.is_digit(10)) {
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
}
struct Token {
    /// Start character index
    start: usize,
    /// End character index
    stop: usize,
    /// Line number
    line: usize,
    /// Token Type
    ttype: TokenType,
}
enum TokenType {
    Name,
    Number,
    SemiColon,
    DoubleQuote,
    NewLine,
    Comment,
}
/// # Lexer State Function
///
/// Tuple-struct wrapper around a function which processes lexemes,
/// and then returns the next Token and the lexer's next state.
struct LexStateFunc(fn(&mut LefLexer) -> (Option<Token>, Option<LexStateFunc>));

pub fn parse(fname: &str) -> Result<(), LefError> {
    Err(LefError::Tbd)
}
#[derive(Debug)]
pub enum LefError {
    Tbd,
}
type LefResult<T> = Result<T, LefError>;

#[cfg(test)]
mod tests {
    use super::*;

    type TestResult = Result<(), LefError>;

    #[test]
    fn it_lexes() -> TestResult {
        let src = "STUFF 101 ; \n // commentary \n";
        let mut lex = LefLexer::new(src);
        Ok(())
    }
}
