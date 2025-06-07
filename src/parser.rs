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
            Expression::Literal(token) => match token.token_type {
                TokenType::STRING | TokenType::NUMBER => {
                    write!(f, "{}", token.literal.as_ref().unwrap())
                }
                _ => write!(f, "{}", String::from_utf8_lossy(token.characters)),
            },
            _ => Ok(()),
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
        let token = self.tokens.get(self.current_token_index.get()).unwrap();

        match token.token_type {
            TokenType::TRUE
            | TokenType::FALSE
            | TokenType::NIL
            | TokenType::STRING
            | TokenType::NUMBER
                if self.tokens.len() == 1 =>
            {
                Some(Statement::Literal(Expression::Literal(token)))
            }
            TokenType::NUMBER | TokenType::LEFT_PAREN => {
                Some(Statement::Literal(*self.parse_expr().unwrap()))
            }
            _ => None,
        }
    }

    fn parse_expr(&self) -> Result<Box<Expression>> {
        //println!(
        //    "Calling parse_expr at index: {}",
        //    self.current_token_index.get()
        //);
        let node = self.parse_term()?;
        match self.get_token() {
            Some(token) if matches!(token.token_type, TokenType::PLUS | TokenType::MINUS) => {
                let _ = self.read_token();
                let rhs = self.parse_term()?;
                //println!(
                //    "Successfully returned {} from expr",
                //    String::from_utf8_lossy(token.characters)
                //);

                //println!("Token at RHS: {}", *rhs);
                Ok(Box::new(Expression::Binary(node, token, rhs)))
            }
            _ => {
                //println!("OPERATOR IS NOT + -");
                //println!(
                //    "Token is {}",
                //    String::from_utf8_lossy(self.get_token().unwrap().characters)
                //);
                Ok(node)
            }
        }
    }

    fn parse_term(&self) -> Result<Box<Expression>> {
        //println!(
        //    "Calling parse_term at index: {}",
        //    self.current_token_index.get()
        //);
        let node = self.parse_factor()?;
        match self.get_token() {
            Some(token) if matches!(token.token_type, TokenType::STAR | TokenType::SLASH) => {
                let _ = self.read_token();
                let rhs = self.parse_factor()?;
                //println!(
                //    "Successfully returned {} from term",
                //    String::from_utf8_lossy(token.characters)
                //);
                Ok(Box::new(Expression::Binary(node, token, rhs)))
            }
            _ => {
                //println!("OPERATOR IS NOT * /");
                Ok(node)
            }
        }
    }

    fn parse_factor(&self) -> Result<Box<Expression>> {
        //println!(
        //    "Calling parse_factor at index: {}",
        //    self.current_token_index.get()
        //);
        let current_token = self.read_token().unwrap(); // handle error for ex when its like "2 *"
        match current_token.token_type {
            TokenType::LEFT_PAREN => {
                let _ = self.read_token();
                let node = self.parse_expr()?;
                if self
                    .peek()
                    .is_some_and(|token| token.token_type == TokenType::RIGHT_PAREN)
                {
                    Ok(node)
                } else {
                    Err(Error::SyntaxError {
                        line_number: current_token.line_number,
                        token: String::from_utf8_lossy(current_token.characters).to_string(),
                    })
                }
            }
            TokenType::NUMBER => {
                //println!(
                //    "Successfully returned {} from factor",
                //    String::from_utf8_lossy(current_token.characters)
                //);
                Ok(Box::new(Expression::Literal(current_token)))
            }
            _ => Ok(Box::new(Expression::Literal(current_token))),
        }
    }

    pub fn collect(self) -> Vec<Statement<'a>> {
        self.statements
    }
}
