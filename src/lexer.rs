use std::{char, fmt::Display};

use crate::{Error, Result};

#[allow(non_camel_case_types)]
#[allow(clippy::upper_case_acronyms)]
#[derive(Debug, PartialEq, Clone)]
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
    // TODO: Optimize to use &str instead of String
    IDENTIFIER(String),
    STRING(String),
    NUMBER_FLOAT(String, f64),
    NUMBER_INT(u64),

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
pub struct Lexer<'a> {
    //program: &'a mut Program<'a, State>,
    pub tokens: Vec<Token>,
    pub errors: Vec<Error>,
    pub line_offsets: Vec<usize>,
    pub input: &'a [u8],
    offset: usize,
}

#[derive(Debug, PartialEq, Clone)]
pub struct Token {
    pub token_type: TokenType,
    pub start: usize,
}

#[derive(Debug)]
pub struct TokenMetadata {
    pub start: usize,
    pub end: usize,
    pub line_number: usize,
}

impl<'a> Lexer<'a> {
    //pub fn print(&self) -> bool {
    //    let mut has_error = false;
    //    self.program.tokens.iter().for_each(|result| match result {
    //        Ok(token) => println!("{token}"),
    //        Err(err) => {
    //            eprintln!("{err}");
    //            has_error = true
    //        }
    //    });
    //
    //    has_error
    //}
    //
    //pub fn print_errors(&self) -> bool {
    //    let mut has_error = false;
    //    self.program.tokens.iter().for_each(|result| match result {
    //        Ok(_) => (),
    //        Err(err) => {
    //            eprintln!("{err}");
    //            has_error = true
    //        }
    //    });
    //
    //    has_error
    //}
    pub fn new(input: &'a [u8]) -> Self {
        Self {
            tokens: vec![],
            errors: vec![],
            line_offsets: vec![],
            input,
            offset: 0,
        }
    }

    // TODO: 'a or not to 'a
    pub fn tokenize(mut self) -> Self {
        while self.offset < self.input.len() {
            let start = self.offset;
            let token_type: Result<TokenType> = match self.input[start] {
                b'(' => Ok(TokenType::LEFT_PAREN),
                b')' => Ok(TokenType::RIGHT_PAREN),
                b'{' => Ok(TokenType::LEFT_BRACE),
                b'}' => Ok(TokenType::RIGHT_BRACE),
                b',' => Ok(TokenType::COMMA),
                b'.' => Ok(TokenType::DOT),
                b'-' => Ok(TokenType::MINUS),
                b'+' => Ok(TokenType::PLUS),
                b'*' => Ok(TokenType::STAR),
                b';' => Ok(TokenType::SEMICOLON),
                b'=' => Ok(self.read_double(TokenType::EQUAL, TokenType::EQUAL_EQUAL)),
                b'!' => Ok(self.read_double(TokenType::BANG, TokenType::BANG_EQUAL)),
                b'<' => Ok(self.read_double(TokenType::LESS, TokenType::LESS_EQUAL)),
                b'>' => Ok(self.read_double(TokenType::GREATER, TokenType::GREATER_EQUAL)),
                b'/' => {
                    if self.peek() == Some(b'/') {
                        self.read_char();
                        self.read_comment();
                        continue;
                    } else {
                        Ok(TokenType::SLASH)
                    }
                }
                b'\t' | b' ' => {
                    self.read_char();
                    continue;
                }
                #[cfg(not(target_os = "windows"))]
                b'\n' => {
                    self.read_char();
                    self.read_line();
                    continue;
                }
                #[cfg(target_os = "windows")]
                b'\r' => {
                    // TODO: Update this to use let-chains in Rust 1.88.0
                    match self.peek() {
                        Some(next_token) if next_token == b'\n' => {
                            self.read_char();
                            self.read_char();
                            self.read_line();
                            continue;
                        }
                        _ => continue, // TODO: maybe throw an error if its just \r
                    }
                }
                b'"' => self.read_string(),
                b'0'..=b'9' => self.read_number(),
                b'a'..=b'z' | b'A'..=b'Z' | b'_' => {
                    let identifier = self.read_identifier();
                    match identifier.as_str() {
                        "and" => Ok(TokenType::AND),
                        "class" => Ok(TokenType::CLASS),
                        "if" => Ok(TokenType::IF),
                        "else" => Ok(TokenType::ELSE),
                        "false" => Ok(TokenType::FALSE),
                        "for" => Ok(TokenType::FOR),
                        "fun" => Ok(TokenType::FUN),
                        "nil" => Ok(TokenType::NIL),
                        "or" => Ok(TokenType::OR),
                        "print" => Ok(TokenType::PRINT),
                        "return" => Ok(TokenType::RETURN),
                        "super" => Ok(TokenType::SUPER),
                        "this" => Ok(TokenType::THIS),
                        "true" => Ok(TokenType::TRUE),
                        "var" => Ok(TokenType::VAR),
                        "while" => Ok(TokenType::WHILE),
                        _ => Ok(TokenType::IDENTIFIER(identifier)),
                    }
                }
                _ => Err(Error::InvalidTokenError {
                    line_number: self.line_offsets.len() + 1,
                    token: (self.input[start] as char).to_string(),
                }),
            };
            match token_type {
                Ok(tok_type) => self.tokens.push(Token {
                    token_type: tok_type,
                    start,
                }),
                Err(err) => self.errors.push(err),
            }

            self.read_char();
        }

        self.tokens.push(Token {
            token_type: TokenType::EOF,
            start: self.offset - 1,
        });

        Self {
            tokens: self.tokens,
            errors: self.errors,
            line_offsets: self.line_offsets,
            input: self.input,
            offset: self.offset,
        }
    }

    fn peek(&self) -> Option<u8> {
        self.input.get(self.offset + 1).copied()
    }

    fn read_line(&mut self) {
        self.line_offsets.push(self.offset);
    }

    fn read_char(&mut self) {
        self.offset += 1;
    }

    fn read_double(
        &mut self,
        single_token_type: TokenType,
        double_token_type: TokenType,
    ) -> TokenType {
        if self.peek() == Some(b'=') {
            self.read_char();
            double_token_type
        } else {
            single_token_type
        }
    }

    fn read_comment(&mut self) {
        while let Some(char) = self.peek() {
            self.read_char();
            if char == b'\n' {
                self.read_line();
                break;
            }
        }
    }

    fn read_string(&mut self) -> Result<TokenType> {
        let starting_index = self.offset;
        while let Some(char) = self.peek() {
            self.read_char();
            match char {
                b'"' => {
                    return Ok(TokenType::STRING(
                        String::from_utf8_lossy(&self.input[starting_index + 1..self.offset])
                            .into_owned(),
                    ));
                }
                #[cfg(not(target_os = "windows"))]
                b'\n' => {
                    self.read_line();
                }
                #[cfg(target_os = "windows")]
                b'\r' => {
                    // TODO: Update this to use let-chains in Rust 1.88.0
                    if let Some(next_token) = self.peek() {
                        if next_token == b'\n' {
                            self.read_char();
                            self.read_line();
                        }
                    }
                }
                _ => continue,
            }
        }

        Err(Error::UnterminatedStringError {
            line_number: self.line_offsets.len() + 1,
        })
    }

    fn read_identifier(&mut self) -> String {
        let starting_index = self.offset;
        while let Some(char) = self.peek() {
            if !(char.is_ascii_alphanumeric() || char == b'_') {
                break;
            }
            self.read_char();
        }

        String::from_utf8_lossy(&self.input[starting_index..=self.offset]).into_owned()
    }

    fn read_number(&mut self) -> Result<TokenType> {
        let starting_index = self.offset;
        let mut floating_point_count: usize = 0;
        while let Some(current_char) = self.peek() {
            match current_char {
                b'0'..=b'9' => self.read_char(),
                b'.' => {
                    floating_point_count += 1;
                    self.read_char();
                }
                _ => break,
            }
        }

        let number_as_str = String::from_utf8_lossy(&self.input[starting_index..=self.offset]);
        match floating_point_count {
            0 => Ok(TokenType::NUMBER_INT(number_as_str.parse::<u64>().unwrap())),
            1 => {
                let float_literal = number_as_str.into_owned();
                // TODO: Maybe there's a way to avoid clone() here, just maybe.
                Ok(TokenType::NUMBER_FLOAT(
                    float_literal.clone(),
                    float_literal.parse::<f64>().unwrap(),
                ))
            }
            _ => Err(Error::MultipleFloatingPointError {
                line_number: self.line_offsets.len() + 1,
            }),
        }
    }
}

impl TokenType {
    fn format_float(&self, literal: &str, float_value: &f64) -> String {
        if let Some((integer, exponent)) = literal.split_once(".") {
            if exponent.parse::<usize>() == Ok(0) {
                return integer.to_string() + ".0";
            }
        }

        float_value.to_string()
    }
}

impl Display for TokenType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            TokenType::LEFT_PAREN => write!(f, "("),
            TokenType::RIGHT_PAREN => write!(f, ")"),
            TokenType::LEFT_BRACE => write!(f, "{{"),
            TokenType::RIGHT_BRACE => write!(f, "}}"),
            TokenType::COMMA => write!(f, ","),
            TokenType::DOT => write!(f, "."),
            TokenType::MINUS => write!(f, "-"),
            TokenType::PLUS => write!(f, "+"),
            TokenType::SLASH => write!(f, "/"),
            TokenType::STAR => write!(f, "*"),
            TokenType::SEMICOLON => write!(f, ";"),
            TokenType::BANG => write!(f, "!"),
            TokenType::BANG_EQUAL => write!(f, "!="),
            TokenType::EQUAL => write!(f, "="),
            TokenType::EQUAL_EQUAL => write!(f, "=="),
            TokenType::GREATER => write!(f, ">"),
            TokenType::GREATER_EQUAL => write!(f, ">="),
            TokenType::LESS => write!(f, "<"),
            TokenType::LESS_EQUAL => write!(f, "<="),
            TokenType::IDENTIFIER(ident) => write!(f, "{ident}"),
            TokenType::STRING(value) => write!(f, "{value}"),
            TokenType::NUMBER_FLOAT(literal, float_value) => {
                write!(f, "{}", self.format_float(literal, float_value))
            }
            TokenType::NUMBER_INT(value) => write!(f, "{value}.0"),
            TokenType::AND => write!(f, "and"),
            TokenType::CLASS => write!(f, "class"),
            TokenType::ELSE => write!(f, "else"),
            TokenType::FALSE => write!(f, "false"),
            TokenType::FUN => write!(f, "fun"),
            TokenType::FOR => write!(f, "for"),
            TokenType::IF => write!(f, "if"),
            TokenType::NIL => write!(f, "nil"),
            TokenType::OR => write!(f, "or"),
            TokenType::PRINT => write!(f, "print"),
            TokenType::RETURN => write!(f, "return"),
            TokenType::SUPER => write!(f, "super"),
            TokenType::THIS => write!(f, "this"),
            TokenType::TRUE => write!(f, "true"),
            TokenType::VAR => write!(f, "var"),
            TokenType::WHILE => write!(f, "while"),
            TokenType::EOF => write!(f, "EOF"),
        }
    }
}

impl Display for Token {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match &self.token_type {
            TokenType::LEFT_PAREN => write!(f, "LEFT_PAREN ( null"),
            TokenType::RIGHT_PAREN => write!(f, "RIGHT_PAREN ) null"),
            TokenType::LEFT_BRACE => write!(f, "LEFT_BRACE {{ null"),
            TokenType::RIGHT_BRACE => write!(f, "RIGHT_BRACE }} null"),
            TokenType::COMMA => write!(f, "COMMA , null"),
            TokenType::DOT => write!(f, "DOT . null"),
            TokenType::MINUS => write!(f, "MINUS - null"),
            TokenType::PLUS => write!(f, "PLUS + null"),
            TokenType::SLASH => write!(f, "SLASH / null"),
            TokenType::STAR => write!(f, "STAR * null"),
            TokenType::SEMICOLON => write!(f, "SEMICOLON ; null"),
            TokenType::BANG => write!(f, "BANG ! null"),
            TokenType::BANG_EQUAL => write!(f, "BANG_EQUAL != null"),
            TokenType::EQUAL => write!(f, "EQUAL = null"),
            TokenType::EQUAL_EQUAL => write!(f, "EQUAL_EQUAL == null"),
            TokenType::GREATER => write!(f, "GREATER > null"),
            TokenType::GREATER_EQUAL => write!(f, "GREATER_EQUAL >= null"),
            TokenType::LESS => write!(f, "LESS < null"),
            TokenType::LESS_EQUAL => write!(f, "LESS_EQUAL <= null"),
            TokenType::IDENTIFIER(ident) => write!(f, "IDENTIFIER {ident} null"),
            TokenType::STRING(value) => write!(f, "STRING \"{value}\" {value}"),
            TokenType::NUMBER_FLOAT(literal, float) => write!(
                f,
                "NUMBER {literal} {}",
                &self.token_type.format_float(literal, float)
            ),
            TokenType::NUMBER_INT(int) => write!(f, "NUMBER {int} {int}.0"),
            TokenType::AND => write!(f, "AND and null"),
            TokenType::CLASS => write!(f, "CLASS class null"),
            TokenType::ELSE => write!(f, "ELSE else null"),
            TokenType::FALSE => write!(f, "FALSE false null"),
            TokenType::FUN => write!(f, "FUN fun null"),
            TokenType::FOR => write!(f, "FOR for null"),
            TokenType::IF => write!(f, "IF if null"),
            TokenType::NIL => write!(f, "NIL nil null"),
            TokenType::OR => write!(f, "OR or null"),
            TokenType::PRINT => write!(f, "PRINT print null"),
            TokenType::RETURN => write!(f, "RETURN return null"),
            TokenType::SUPER => write!(f, "SUPER super null"),
            TokenType::THIS => write!(f, "THIS this null"),
            TokenType::TRUE => write!(f, "TRUE true null"),
            TokenType::VAR => write!(f, "VAR var null"),
            TokenType::WHILE => write!(f, "WHILE while null"),
            TokenType::EOF => write!(f, "EOF  null"),
        }
    }
}

impl Token {
    pub fn from_eof() -> Self {
        Self {
            token_type: TokenType::EOF,
            start: 0,
        }
    }

    pub fn compute_metadata_from_offset<'a>(
        &'a self,
        line_number_offsets: &'a [usize],
    ) -> TokenMetadata {
        // The line number offsets are guaranteed to be sorted.
        let line_number_index = line_number_offsets
            .binary_search(&self.start)
            .unwrap_or_else(|x| x);
        let start = self.start - (line_number_offsets[line_number_index - 1] + 1);
        let line_number = line_number_index + 1;

        let length = match &self.token_type {
            TokenType::LEFT_PAREN
            | TokenType::RIGHT_PAREN
            | TokenType::LEFT_BRACE
            | TokenType::RIGHT_BRACE
            | TokenType::COMMA
            | TokenType::DOT
            | TokenType::MINUS
            | TokenType::PLUS
            | TokenType::SLASH
            | TokenType::STAR
            | TokenType::SEMICOLON
            | TokenType::BANG
            | TokenType::LESS
            | TokenType::GREATER
            | TokenType::EQUAL => 1,
            TokenType::BANG_EQUAL
            | TokenType::EQUAL_EQUAL
            | TokenType::GREATER_EQUAL
            | TokenType::LESS_EQUAL => 2,
            TokenType::IDENTIFIER(str) => str.len(),
            TokenType::STRING(str) => str.len() + 2, // str.len() is just the str content without ""
            TokenType::NUMBER_FLOAT(str, _) => str.len(),
            TokenType::NUMBER_INT(int) => {
                if *int == 0 {
                    1
                } else {
                    (*int as f64).log10().floor() as usize + 1
                }
            }
            TokenType::RETURN => 6,
            TokenType::CLASS
            | TokenType::FALSE
            | TokenType::SUPER
            | TokenType::WHILE
            | TokenType::PRINT => 5,
            TokenType::THIS | TokenType::TRUE | TokenType::ELSE => 4,
            TokenType::AND | TokenType::FUN | TokenType::FOR | TokenType::NIL | TokenType::VAR => 3,
            TokenType::IF | TokenType::OR => 2,
            TokenType::EOF => 1,
        };

        let end = length - 1;

        TokenMetadata {
            start,
            end,
            line_number,
        }
    }
}
