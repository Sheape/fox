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
    pub tokens: Vec<Result<Token>>,
    input: Vec<&'a [u8]>,
    line: &'a [u8],
    line_number: usize,
    current_char_index: usize,
}

#[derive(Debug, PartialEq)]
pub struct Token {
    pub token_type: TokenType,
    pub line_number: usize,
    pub column_number: usize,
}

impl Lexer<'_> {
    pub fn tokenize(mut self) -> Self {
        let input = std::mem::take(&mut self.input);
        for line in input {
            self.line = line;
            self.tokenize_line(line);
        }

        self
    }

    pub fn print(&self) -> bool {
        let mut has_error = false;
        self.tokens.iter().for_each(|result| match result {
            Ok(token) => println!("{token}"),
            Err(err) => {
                eprintln!("{err}");
                has_error = true
            }
        });

        println!("{}", Token::from_eof());

        has_error
    }

    pub fn print_errors(&self) -> bool {
        let mut has_error = false;
        self.tokens.iter().for_each(|result| match result {
            Ok(_) => (),
            Err(err) => {
                eprintln!("{err}");
                has_error = true
            }
        });

        has_error
    }

    fn tokenize_line(&mut self, line: &[u8]) {
        while self.current_char_index < line.len() {
            let token_type: Result<TokenType> = match line[self.current_char_index] {
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
                        break;
                    } else {
                        Ok(TokenType::SLASH)
                    }
                }
                b'\t' | b' ' => {
                    self.read_char();
                    continue;
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
                    line_number: self.line_number,
                    token: (line[self.current_char_index] as char).to_string(),
                }),
            };

            self.tokens.push(token_type.map(|tok_type| Token {
                token_type: tok_type,
                column_number: self.current_char_index,
                line_number: self.line_number,
            }));

            self.read_char();
        }

        self.read_line();
    }

    fn peek(&self) -> Option<u8> {
        self.line.get(self.current_char_index + 1).copied()
    }

    fn read_line(&mut self) {
        self.current_char_index = 0;
        self.line_number += 1;
    }

    fn read_char(&mut self) {
        self.current_char_index += 1;
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

    fn read_string(&mut self) -> Result<TokenType> {
        let starting_index = self.current_char_index;
        while let Some(char) = self.peek() {
            self.read_char();
            if char == b'"' {
                return Ok(TokenType::STRING(
                    String::from_utf8_lossy(
                        &self.line[starting_index + 1..self.current_char_index],
                    )
                    .into_owned(),
                ));
            }
        }

        Err(Error::UnterminatedStringError {
            line_number: self.line_number,
        })
    }

    fn read_identifier(&mut self) -> String {
        let starting_index = self.current_char_index;
        while let Some(char) = self.peek() {
            if !(char.is_ascii_alphanumeric() || char == b'_') {
                break;
            }
            self.read_char();
        }

        String::from_utf8_lossy(&self.line[starting_index..=self.current_char_index]).into_owned()
    }

    fn read_number(&mut self) -> Result<TokenType> {
        let starting_index = self.current_char_index;
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

        match floating_point_count {
            0 => Ok(TokenType::NUMBER_INT(
                String::from_utf8_lossy(&self.line[starting_index..=self.current_char_index])
                    .parse::<u64>()
                    .unwrap(),
            )),
            1 => {
                let float_literal =
                    String::from_utf8_lossy(&self.line[starting_index..=self.current_char_index])
                        .into_owned();
                // TODO: Maybe there's a way to avoid clone() here, just maybe.
                Ok(TokenType::NUMBER_FLOAT(
                    float_literal.clone(),
                    float_literal.parse::<f64>().unwrap(),
                ))
            }
            _ => Err(Error::MultipleFloatingPointError {
                line_number: self.line_number,
            }),
        }
    }
}

impl<'a> From<&'a str> for Lexer<'a> {
    fn from(value: &'a str) -> Self {
        let input = value
            .lines()
            .map(|line| line.as_bytes())
            .collect::<Vec<&'a [u8]>>();

        Self {
            tokens: vec![],
            input,
            line: &[],
            line_number: 0,
            current_char_index: 0,
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
            line_number: 999,
            column_number: 999,
        }
    }
}
