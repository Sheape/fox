use std::{fmt::Display, slice};

use crate::{Error, Result};

#[allow(non_camel_case_types)]
#[allow(clippy::upper_case_acronyms)]
#[derive(Debug)]
pub enum TokenType {
    // Single chars
    LEFT_PAREN,
    RIGHT_PAREN,
    LEFT_BRACE,
    RIGHT_BRACE,
    COMMA,
    DOT,
    MINUS,
    PLUS,
    SLASH,
    STAR,
    SEMICOLON,

    // Single/Double chars
    BANG,
    BANG_EQUAL,
    EQUAL,
    EQUAL_EQUAL,
    GREATER,
    GREATER_EQUAL,
    LESS,
    LESS_EQUAL,

    // Literals
    IDENTIFIER,
    STRING,
    NUMBER,

    // Reserved Keywords
    AND,
    CLASS,
    ELSE,
    FALSE,
    FUN,
    FOR,
    IF,
    NIL,
    OR,
    PRINT,
    RETURN,
    SUPER,
    THIS,
    TRUE,
    VAR,
    WHILE,

    // Terminator
    EOF,
}

#[derive(Debug)]
pub struct Line<'a> {
    pub tokens: Vec<Result<Token<'a>>>,
    pub characters: &'a [u8],
    pub line_number: usize,
    current_token_index: usize,
}

#[derive(Debug)]
pub struct Token<'a> {
    pub characters: &'a [u8],
    pub literal: Option<String>,
    pub token_type: TokenType,
}

impl<'a> Line<'a> {
    pub fn from_string(line_string: &'a str, line_number: usize) -> Self {
        let characters = line_string.as_bytes();
        Self {
            tokens: Vec::with_capacity(characters.len()),
            characters,
            line_number,
            current_token_index: 0,
        }
    }

    pub fn tokenize(mut self) -> Self {
        while self.current_token_index < self.characters.len() {
            let starting_index = self.current_token_index;
            let byte_char = self.characters[self.current_token_index];
            let token_type: Result<(TokenType, bool)> = match byte_char {
                b'(' => Ok((TokenType::LEFT_PAREN, false)),
                b')' => Ok((TokenType::RIGHT_PAREN, false)),
                b'{' => Ok((TokenType::LEFT_BRACE, false)),
                b'}' => Ok((TokenType::RIGHT_BRACE, false)),
                b',' => Ok((TokenType::COMMA, false)),
                b'.' => Ok((TokenType::DOT, false)),
                b'-' => Ok((TokenType::MINUS, false)),
                b'+' => Ok((TokenType::PLUS, false)),
                b'*' => Ok((TokenType::STAR, false)),
                b';' => Ok((TokenType::SEMICOLON, false)),
                b'=' => self.next_token(b'=', TokenType::EQUAL_EQUAL, TokenType::EQUAL),
                b'!' => self.next_token(b'=', TokenType::BANG_EQUAL, TokenType::BANG),
                b'<' => self.next_token(b'=', TokenType::LESS_EQUAL, TokenType::LESS),
                b'>' => self.next_token(b'=', TokenType::GREATER_EQUAL, TokenType::GREATER),
                b'/' => {
                    if self.peek_at(1) == Some(b'/') {
                        break;
                    } else {
                        Ok((TokenType::SLASH, false))
                    }
                }
                _ => Err(Error::InvalidTokenError {
                    line_number: self.line_number,
                    token: (byte_char as char).to_string(),
                }),
            };

            let chars = if starting_index != self.current_token_index {
                &self.characters[starting_index..=self.current_token_index]
            } else {
                slice::from_ref(&self.characters[starting_index])
            };

            if let Ok(token) = token_type {
                self.tokens.push(Ok(Token::new(
                    chars,
                    token
                        .1
                        .then_some(String::from_utf8_lossy(chars).to_string()),
                    token.0,
                )));
            } else {
                self.tokens.push(Err(token_type.unwrap_err()));
            }

            self.step_by(1);
        }

        self
    }

    fn peek_at(&self, index: usize) -> Option<u8> {
        if self.current_token_index + index < self.characters.len() {
            Some(self.characters[self.current_token_index + index])
        } else {
            None
        }
    }

    fn step_by(&mut self, index: usize) {
        self.current_token_index += index;
    }

    fn next_token(
        &mut self,
        byte_char: u8,
        token_type_double: TokenType,
        token_type: TokenType,
    ) -> Result<(TokenType, bool)> {
        if self.peek_at(1) == Some(byte_char) {
            self.step_by(1);
            Ok((token_type_double, false))
        } else {
            Ok((token_type, false))
        }
    }
}

impl<'a> Token<'a> {
    pub fn new(characters: &'a [u8], literal: Option<String>, token_type: TokenType) -> Self {
        Self {
            characters,
            literal,
            token_type,
        }
    }

    pub fn from_eof() -> Self {
        Self {
            token_type: TokenType::EOF,
            characters: "".as_bytes(),
            literal: None,
        }
    }
}

impl Display for Token<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{:?} {} {}",
            self.token_type,
            String::from_utf8_lossy(self.characters),
            self.literal.as_deref().unwrap_or("null")
        )
    }
}
