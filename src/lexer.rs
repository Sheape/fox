use std::{fmt::Display, slice};

use crate::{Error, Result};

#[allow(non_camel_case_types)]
#[allow(clippy::upper_case_acronyms)]
#[derive(Debug, PartialEq)]
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
    pub line_number: usize,
}

impl PartialEq for Token<'_> {
    fn eq(&self, other: &Self) -> bool {
        self.token_type == other.token_type
            && self.literal == other.literal
            && self.characters == other.characters
    }
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

    pub fn tokenize_from_file(file_contents: &'a str) -> Vec<Result<Token<'a>>> {
        let mut tokens: Vec<Result<Token>> = vec![];

        for (line_number, line) in file_contents.lines().enumerate() {
            let current_line = Line::from_string(line.trim(), line_number + 1).tokenize();
            current_line
                .tokens
                .into_iter()
                .for_each(|token| tokens.push(token));
        }

        tokens
    }

    pub fn tokenize(mut self) -> Self {
        while self.current_token_index < self.characters.len() {
            let starting_index = self.current_token_index;
            let byte_char = self.characters[self.current_token_index];
            let token_type: Result<(TokenType, Option<String>)> = match byte_char {
                b'(' => Ok((TokenType::LEFT_PAREN, None)),
                b')' => Ok((TokenType::RIGHT_PAREN, None)),
                b'{' => Ok((TokenType::LEFT_BRACE, None)),
                b'}' => Ok((TokenType::RIGHT_BRACE, None)),
                b',' => Ok((TokenType::COMMA, None)),
                b'.' => Ok((TokenType::DOT, None)),
                b'-' => Ok((TokenType::MINUS, None)),
                b'+' => Ok((TokenType::PLUS, None)),
                b'*' => Ok((TokenType::STAR, None)),
                b';' => Ok((TokenType::SEMICOLON, None)),
                b'=' => self.next_token(b'=', TokenType::EQUAL_EQUAL, TokenType::EQUAL),
                b'!' => self.next_token(b'=', TokenType::BANG_EQUAL, TokenType::BANG),
                b'<' => self.next_token(b'=', TokenType::LESS_EQUAL, TokenType::LESS),
                b'>' => self.next_token(b'=', TokenType::GREATER_EQUAL, TokenType::GREATER),
                b'/' => {
                    if self.peek_at(1) == Some(b'/') {
                        break;
                    } else {
                        Ok((TokenType::SLASH, None))
                    }
                }
                b' ' | b'\t' => {
                    self.step_by(1);
                    continue;
                }
                b'"' => {
                    let token = self.next_identifier(TokenType::STRING);
                    if let Err(err) = token {
                        self.tokens.push(Err(err));
                        break;
                    } else {
                        token
                    }
                }
                b'0'..=b'9' => Ok(self.next_identifier(TokenType::NUMBER).unwrap()),
                b'a'..=b'z' | b'A'..=b'Z' | b'_' => {
                    Ok(self.next_identifier(TokenType::IDENTIFIER).unwrap())
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
                self.tokens
                    .push(Ok(Token::new(chars, token.1, token.0, self.line_number)));
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
    ) -> Result<(TokenType, Option<String>)> {
        if self.peek_at(1) == Some(byte_char) {
            self.step_by(1);
            Ok((token_type_double, None))
        } else {
            Ok((token_type, None))
        }
    }

    fn next_identifier(
        &mut self,
        identifier_type: TokenType,
    ) -> Result<(TokenType, Option<String>)> {
        match identifier_type {
            TokenType::STRING => {
                let closing_quotation = self.characters[self.current_token_index + 1..]
                    .iter()
                    .position(|&char| char == b'"');

                if let Some(i) = closing_quotation {
                    let literal = String::from_utf8_lossy(
                        &self.characters
                            [self.current_token_index + 1..=self.current_token_index + i],
                    )
                    .to_string();
                    self.current_token_index += i + 1;
                    Ok((identifier_type, Some(literal)))
                } else {
                    Err(Error::UnterminatedStringError {
                        line_number: self.line_number,
                    })
                }
            }
            TokenType::NUMBER => {
                let mut valid_float = true;
                let mut float_end_index: usize = self.current_token_index;
                let mut floating_point_count: u8 = 0;
                while valid_float
                    && floating_point_count < 2
                    && float_end_index < self.characters.len()
                {
                    match &self.characters[float_end_index] {
                        b'.' => {
                            floating_point_count += 1;
                            float_end_index += 1;
                        }
                        b'0'..=b'9' => float_end_index += 1,
                        _ => valid_float = false,
                    }
                }

                let num_literal = Some(format_number(
                    String::from_utf8_lossy(
                        &self.characters[self.current_token_index..float_end_index],
                    )
                    .to_string(),
                ));

                self.current_token_index = float_end_index - 1;

                Ok((identifier_type, num_literal))
            }
            TokenType::IDENTIFIER => {
                let mut identifier_end_index = self.current_token_index;
                while self.characters[identifier_end_index].is_ascii_alphanumeric()
                    || self.characters[identifier_end_index] == b'_'
                {
                    identifier_end_index += 1;
                    if identifier_end_index >= self.characters.len() {
                        break;
                    }
                }

                let identifier_literal = String::from_utf8_lossy(
                    &self.characters[self.current_token_index..identifier_end_index],
                )
                .to_string();

                self.current_token_index = identifier_end_index - 1;
                let token_type = match identifier_literal.as_str() {
                    "and" => TokenType::AND,
                    "class" => TokenType::CLASS,
                    "else" => TokenType::ELSE,
                    "false" => TokenType::FALSE,
                    "for" => TokenType::FOR,
                    "fun" => TokenType::FUN,
                    "if" => TokenType::IF,
                    "nil" => TokenType::NIL,
                    "or" => TokenType::OR,
                    "print" => TokenType::PRINT,
                    "return" => TokenType::RETURN,
                    "super" => TokenType::SUPER,
                    "this" => TokenType::THIS,
                    "true" => TokenType::TRUE,
                    "var" => TokenType::VAR,
                    "while" => TokenType::WHILE,
                    _ => identifier_type,
                };

                Ok((token_type, None))
            }
            _ => Err(Error::UnterminatedStringError {
                line_number: self.line_number,
            }),
        }
    }
}

impl<'a> Token<'a> {
    pub fn new(
        characters: &'a [u8],
        literal: Option<String>,
        token_type: TokenType,
        line_number: usize,
    ) -> Self {
        Self {
            characters,
            literal,
            token_type,
            line_number,
        }
    }

    pub fn from_eof() -> Self {
        Self {
            token_type: TokenType::EOF,
            characters: "".as_bytes(),
            literal: None,
            line_number: 9999,
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

fn format_number(number: String) -> String {
    let splitted_num = number.split('.').collect::<Vec<&str>>();

    if splitted_num.len() == 1 {
        return format!("{number}.0");
    }

    let (integer, exponent) = (splitted_num[0], splitted_num[1]);

    if exponent.parse::<u32>() == Ok(0) {
        format!("{integer}.0")
    } else {
        format!("{}", number.parse::<f64>().unwrap())
    }
}

#[cfg(test)]
mod test {
    use super::*;

    #[cfg(test)]
    fn create_token(
        characters: &str,
        literal: Option<String>,
        token_type: TokenType,
        line_number: usize,
    ) -> Token<'_> {
        Token {
            characters: characters.as_bytes(),
            literal,
            token_type,
            line_number,
        }
    }

    #[cfg(test)]
    fn from_input_ok(input: &str, line_number: usize) -> Vec<Token<'_>> {
        Line::from_string(input, line_number)
            .tokenize()
            .tokens
            .into_iter()
            .filter_map(Result::ok)
            .collect()
    }

    #[test]
    fn test_lex_parenthesis_ok() {
        let input = "(()";
        let output_tokens: Vec<Token<'_>> = vec![
            create_token("(", None, TokenType::LEFT_PAREN, 1),
            create_token("(", None, TokenType::LEFT_PAREN, 1),
            create_token(")", None, TokenType::RIGHT_PAREN, 1),
        ];

        let lexed_input: Vec<Token<'_>> = from_input_ok(input, 1);

        assert_eq!(lexed_input, output_tokens);
    }

    #[test]
    fn test_lex_braces_ok() {
        let input = "{{}}";
        let output_tokens: Vec<Token<'_>> = vec![
            create_token("{", None, TokenType::LEFT_BRACE, 1),
            create_token("{", None, TokenType::LEFT_BRACE, 1),
            create_token("}", None, TokenType::RIGHT_BRACE, 1),
            create_token("}", None, TokenType::RIGHT_BRACE, 1),
        ];

        let lexed_input: Vec<Token<'_>> = from_input_ok(input, 1);

        assert_eq!(lexed_input, output_tokens);
    }

    #[test]
    fn test_lex_braces_paren_mixed_ok() {
        let input = "({*.,+*})";
        let output_tokens: Vec<Token<'_>> = vec![
            create_token("(", None, TokenType::LEFT_PAREN, 1),
            create_token("{", None, TokenType::LEFT_BRACE, 1),
            create_token("*", None, TokenType::STAR, 1),
            create_token(".", None, TokenType::DOT, 1),
            create_token(",", None, TokenType::COMMA, 1),
            create_token("+", None, TokenType::PLUS, 1),
            create_token("*", None, TokenType::STAR, 1),
            create_token("}", None, TokenType::RIGHT_BRACE, 1),
            create_token(")", None, TokenType::RIGHT_PAREN, 1),
        ];

        let lexed_input: Vec<Token<'_>> = from_input_ok(input, 1);

        assert_eq!(lexed_input, output_tokens);
    }
}
