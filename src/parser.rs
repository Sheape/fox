use std::{cell::Cell, fmt::Display};

use crate::{
    lexer::{Token, TokenType},
    Error, Result,
};

#[derive(Debug)]
pub enum Statement<'a> {
    Literal(Expression<'a>),
}

#[derive(Debug)]
pub enum Expression<'a> {
    Binary(Box<Expression<'a>>, &'a Token<'a>, Box<Expression<'a>>),
    Unary(&'a Token<'a>, Box<Expression<'a>>),
    Literal(&'a Token<'a>),
    Grouping(Box<Expression<'a>>),
}

#[derive(Debug)]
pub struct Parser<'a> {
    pub tokens: Vec<Token<'a>>,
    pub statements: Vec<Statement<'a>>,
    current_token_index: Cell<usize>,
    next_token_index: Cell<usize>,
}

impl Display for Statement<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Statement::Literal(expr) => write!(f, "{}", expr),
        }
    }
}

impl Display for Expression<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Expression::Binary(first, operator, second) => {
                write!(
                    f,
                    "({} {} {})",
                    String::from_utf8_lossy(operator.characters),
                    first,
                    second
                )
            }
            Expression::Grouping(expr) => {
                write!(f, "(group {})", expr)
            }
            Expression::Unary(prefix, expr) => {
                write!(
                    f,
                    "({} {})",
                    String::from_utf8_lossy(prefix.characters),
                    expr
                )
            }
            Expression::Literal(token) => match token.token_type {
                TokenType::STRING | TokenType::NUMBER => {
                    write!(f, "{}", token.literal.as_ref().unwrap())
                }
                _ => write!(f, "{}", String::from_utf8_lossy(token.characters)),
            },
        }
    }
}

impl<'a> Parser<'a> {
    pub fn from_tokens(tokens: Vec<Token<'a>>) -> Self {
        Self {
            tokens,
            statements: vec![],
            current_token_index: Cell::new(0),
            next_token_index: Cell::new(1),
        }
    }

    fn peek(&self) -> Option<&Token> {
        self.tokens.get(self.next_token_index.get())
    }

    fn get_token(&self) -> Option<&Token> {
        self.tokens.get(self.current_token_index.get())
    }

    fn read_token(&self) -> Option<&Token> {
        let next_token_idx = self.next_token_index.get();
        let current_token_idx = self.current_token_index.get();
        //print!("Current token index: {} | ", current_token_idx);
        //print!("Next token index: {} | ", next_token_idx);
        if current_token_idx < self.tokens.len() {
            let current_token = self.tokens.get(current_token_idx);
            //println!("Current reading: {}", &current_token.unwrap());
            self.current_token_index.set(next_token_idx);
            self.next_token_index.set(next_token_idx + 1);
            return current_token;
        }
        None
    }

    pub fn parse(&self) -> Option<Statement<'_>> {
        Some(Statement::Literal(*self.parse_expr().unwrap()))
    }

    fn parse_expr(&self) -> Result<Box<Expression>> {
        let mut node = self.parse_comparison()?;
        loop {
            match self.get_token() {
                Some(token)
                    if matches!(
                        token.token_type,
                        TokenType::EQUAL_EQUAL | TokenType::BANG_EQUAL
                    ) =>
                {
                    let _ = self.read_token();
                    let rhs = self.parse_comparison()?;
                    node = Box::new(Expression::Binary(node, token, rhs));
                }
                _ => {
                    break;
                }
            }
        }
        Ok(node)
    }

    fn parse_comparison(&self) -> Result<Box<Expression>> {
        let mut node = self.parse_term()?;
        loop {
            match self.get_token() {
                Some(token)
                    if matches!(
                        token.token_type,
                        TokenType::LESS
                            | TokenType::LESS_EQUAL
                            | TokenType::GREATER
                            | TokenType::GREATER_EQUAL
                    ) =>
                {
                    let _ = self.read_token();
                    let rhs = self.parse_term()?;
                    node = Box::new(Expression::Binary(node, token, rhs));
                }
                _ => {
                    break;
                }
            }
        }
        Ok(node)
    }

    fn parse_term(&self) -> Result<Box<Expression>> {
        let mut node = self.parse_factor()?;
        loop {
            match self.get_token() {
                Some(token) if matches!(token.token_type, TokenType::PLUS | TokenType::MINUS) => {
                    let _ = self.read_token();
                    let rhs = self.parse_factor()?;
                    node = Box::new(Expression::Binary(node, token, rhs));
                }
                _ => {
                    break;
                }
            }
        }
        Ok(node)
    }

    fn parse_factor(&self) -> Result<Box<Expression>> {
        let mut node = self.parse_unary()?;
        loop {
            match self.get_token() {
                Some(token) if matches!(token.token_type, TokenType::STAR | TokenType::SLASH) => {
                    let _ = self.read_token();
                    let rhs = self.parse_unary()?;
                    node = Box::new(Expression::Binary(node, token, rhs))
                }
                _ => {
                    break;
                }
            }
        }
        Ok(node)
    }

    fn parse_unary(&self) -> Result<Box<Expression>> {
        match self.get_token() {
            Some(token) if matches!(token.token_type, TokenType::BANG | TokenType::MINUS) => {
                let _ = self.read_token();
                let primary = self.parse_unary()?;
                Ok(Box::new(Expression::Unary(token, primary)))
            }
            _ => Ok(self.parse_primary()?),
        }
    }

    fn parse_primary(&self) -> Result<Box<Expression>> {
        let current_token = self.read_token().unwrap();
        match current_token.token_type {
            TokenType::LEFT_PAREN => {
                let node = self.parse_expr()?;
                let closing_paren = self.read_token();
                if closing_paren.is_some_and(|token| token.token_type == TokenType::RIGHT_PAREN) {
                    Ok(Box::new(Expression::Grouping(node)))
                } else {
                    Err(Error::SyntaxError {
                        line_number: current_token.line_number,
                        token: String::from_utf8_lossy(current_token.characters).to_string(),
                    })
                }
            }
            TokenType::STRING
            | TokenType::NUMBER
            | TokenType::TRUE
            | TokenType::FALSE
            | TokenType::NIL => Ok(Box::new(Expression::Literal(current_token))),
            _ => Ok(Box::new(Expression::Literal(current_token))),
        }
    }

    pub fn collect(self) -> Vec<Statement<'a>> {
        self.statements
    }
}
