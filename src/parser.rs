use std::{cell::Cell, fmt::Display};

use crate::{
    lexer::{Lexer, Token, TokenType},
    Error, Result,
};

#[derive(Debug)]
pub enum Statement<'a> {
    Literal(Expression<'a>),
    Expr(Expression<'a>, &'a Token),             // expression ";"
    Print(&'a Token, Expression<'a>, &'a Token), // "print" expression ";"
}

#[derive(Debug)]
pub enum Expression<'a> {
    Binary(Box<Expression<'a>>, &'a Token, Box<Expression<'a>>),
    Unary(&'a Token, Box<Expression<'a>>),
    Literal(&'a Token),
    Grouping(Box<Expression<'a>>),
}

#[derive(Debug)]
pub struct Parser<'a> {
    pub tokens: Vec<Token>,
    pub statements: Vec<Statement<'a>>,
    current_token_index: Cell<usize>,
}

impl Display for Statement<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Statement::Literal(expr) => write!(f, "{expr}"),
            Statement::Expr(expr, _) => write!(f, "{expr};"),
            Statement::Print(_, expr, _) => write!(f, "print {expr};"),
        }
    }
}

impl Display for Expression<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Expression::Binary(first, operator, second) => {
                write!(f, "({} {} {})", operator.token_type, first, second)
            }
            Expression::Grouping(expr) => {
                write!(f, "(group {})", expr)
            }
            Expression::Unary(prefix, expr) => {
                write!(f, "({} {})", prefix.token_type, expr)
            }
            Expression::Literal(token) => write!(f, "{}", token.token_type),
        }
    }
}

impl From<Lexer<'_>> for Parser<'_> {
    fn from(value: Lexer<'_>) -> Self {
        value.print_errors().then(|| std::process::exit(65));

        let lexed_tokens: Vec<Token> = value
            .tokens
            .into_iter()
            .map(|result| result.unwrap())
            .collect();
        Self {
            tokens: lexed_tokens,
            statements: vec![],
            current_token_index: Cell::new(0),
        }
    }
}

impl Parser<'_> {
    fn get_token(&self) -> Option<&Token> {
        self.tokens.get(self.current_token_index.get())
    }

    fn read_token(&self) -> Option<&Token> {
        let current_token_idx = self.current_token_index.get();
        if current_token_idx < self.tokens.len() {
            let current_token = self.tokens.get(current_token_idx);
            self.current_token_index.set(current_token_idx + 1);
            return current_token;
        }
        None
    }

    pub fn parse(&self) -> Result<Statement<'_>> {
        self.parse_expr()
            .map(|expr| Ok(Statement::Literal(*expr)))?
    }

    pub fn parse_expr_statement(&self) -> Result<Statement<'_>> {
        let next_token = self.read_token();
        match next_token {
            Some(semi_colon) if semi_colon.token_type == TokenType::SEMICOLON => self
                .parse_expr()
                .map(|expr| Ok(Statement::Expr(*expr, semi_colon)))?,
            _ => Err(Error::MissingSemiColon),
        }
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
                        token: format!("{}", current_token.token_type),
                    })
                }
            }
            TokenType::STRING(_)
            | TokenType::NUMBER_FLOAT(_, _)
            | TokenType::NUMBER_INT(_)
            | TokenType::TRUE
            | TokenType::FALSE
            | TokenType::NIL => Ok(Box::new(Expression::Literal(current_token))),
            _ => Err(Error::SyntaxError {
                line_number: current_token.line_number,
                token: format!("{}", current_token.token_type),
            }),
        }
    }
}
