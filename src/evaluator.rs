use std::{
    fmt::Display,
    ops::{Add, Div, Mul, Neg, Not, Sub},
};

use crate::{
    lexer::{Token, TokenType},
    parser::{Expression, Statement},
    Error, Result,
};

pub struct Evaluator<'a> {
    pub statements: Result<Statement<'a>>,
    pub current_index: usize,
}

#[derive(Debug)]
pub enum Value {
    String(String),
    Float(f64),
    Integer(i64),
    Boolean(bool),
    Nil,
}

impl Evaluator<'_> {
    pub fn evaluate_statement(self) -> Result<Value> {
        let statement = self.statements.as_ref().unwrap();

        match statement {
            Statement::Literal(expression) => self.evaluate_expr(expression),
            Statement::Expr(expression, _) => self.evaluate_expr(expression),
            Statement::Print(_, expression, _) => self.evaluate_expr(expression),
        }
    }

    fn evaluate_expr(&self, expr: &Expression) -> Result<Value> {
        match expr {
            Expression::Literal(token) => self.evaluate_literal(token),
            Expression::Binary(first_expr, operator, second_expr) => {
                let first_expr_value = self.evaluate_expr(first_expr)?;
                let second_expr_value = self.evaluate_expr(second_expr)?;
                self.evaluate_binary(first_expr_value, operator, second_expr_value)
            }
            Expression::Grouping(expr) => self.evaluate_expr(expr),
            Expression::Unary(prefix, expr) => {
                let literal = self.evaluate_expr(expr)?;
                self.evaluate_unary(prefix, literal)
            }
        }
    }

    fn evaluate_literal(&self, literal: &Token) -> Result<Value> {
        match &literal.token_type {
            TokenType::STRING(str) => Ok(Value::String(str.to_string())),
            TokenType::NUMBER_INT(int) => Ok(Value::Integer(*int as i64)),
            TokenType::NUMBER_FLOAT(_, float) => Ok(Value::Float(*float)),
            TokenType::TRUE => Ok(Value::Boolean(true)),
            TokenType::FALSE => Ok(Value::Boolean(false)),
            TokenType::NIL => Ok(Value::Nil),
            token_type => Err(Error::InvalidLiteralError {
                token_type: token_type.clone(),
            }),
        }
    }

    fn evaluate_binary(&self, first: Value, operator: &Token, second: Value) -> Result<Value> {
        match &operator.token_type {
            TokenType::PLUS => first + second,
            TokenType::MINUS => first - second,
            TokenType::STAR => first * second,
            TokenType::SLASH => first / second,
            TokenType::GREATER
            | TokenType::GREATER_EQUAL
            | TokenType::LESS
            | TokenType::LESS_EQUAL => match first.partial_cmp(&second) {
                Some(order) => match order {
                    std::cmp::Ordering::Less => Ok(Value::Boolean(first < second)),
                    std::cmp::Ordering::Equal => Ok(Value::Boolean(first == second)),
                    std::cmp::Ordering::Greater => Ok(Value::Boolean(first > second)),
                },
                None => Err(Error::InvalidComparisonError {
                    left: first,
                    right: second,
                }),
            },
            TokenType::EQUAL_EQUAL => Ok(Value::Boolean(first == second)),
            TokenType::BANG_EQUAL => Ok(Value::Boolean(first != second)),
            _ => Err(Error::InvalidBinaryOperatorError {
                left: first,
                right: second,
            }),
        }
    }

    fn evaluate_unary(&self, prefix: &Token, literal: Value) -> Result<Value> {
        match prefix.token_type {
            TokenType::BANG => Ok(!literal),
            TokenType::MINUS => -literal,
            _ => Err(Error::CannotApplyNegationError { value: literal }),
        }
    }
}

impl Display for Value {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Value::Float(float) => write!(f, "{float}"),
            Value::Integer(int) => write!(f, "{int}"),
            Value::Boolean(bool) => write!(f, "{bool}"),
            Value::String(str) => write!(f, "{str}"),
            Value::Nil => write!(f, "nil"),
        }
    }
}

impl Add for Value {
    type Output = Result<Value>;

    fn add(self, rhs: Self) -> Self::Output {
        match (self, rhs) {
            (Value::Float(left), Value::Float(right)) => Ok(Value::Float(left + right)),
            (Value::Float(left), Value::Integer(right)) => Ok(Value::Float(left + (right as f64))),
            (Value::Integer(left), Value::Float(right)) => Ok(Value::Float((left as f64) + right)),
            (Value::Integer(left), Value::Integer(right)) => Ok(Value::Integer(left + right)),
            (Value::String(left), Value::String(right)) => Ok(Value::String(left + right.as_str())),
            (left, right) => Err(Error::InvalidOperandError { left, right }),
        }
    }
}

impl Sub for Value {
    type Output = Result<Value>;

    fn sub(self, rhs: Self) -> Self::Output {
        match (self, rhs) {
            (Value::Float(left), Value::Float(right)) => Ok(Value::Float(left - right)),
            (Value::Float(left), Value::Integer(right)) => Ok(Value::Float(left - (right as f64))),
            (Value::Integer(left), Value::Float(right)) => Ok(Value::Float((left as f64) - right)),
            (Value::Integer(left), Value::Integer(right)) => Ok(Value::Integer(left - right)),
            (left, right) => Err(Error::InvalidOperandError { left, right }),
        }
    }
}

impl Mul for Value {
    type Output = Result<Value>;

    fn mul(self, rhs: Self) -> Self::Output {
        match (self, rhs) {
            (Value::Float(left), Value::Float(right)) => Ok(Value::Float(left * right)),
            (Value::Float(left), Value::Integer(right)) => Ok(Value::Float(left * (right as f64))),
            (Value::Integer(left), Value::Float(right)) => Ok(Value::Float((left as f64) * right)),
            (Value::Integer(left), Value::Integer(right)) => Ok(Value::Integer(left * right)),
            (left, right) => Err(Error::InvalidOperandError { left, right }),
        }
    }
}

impl Div for Value {
    type Output = Result<Value>;

    fn div(self, rhs: Self) -> Self::Output {
        if rhs == Self::Integer(0) || rhs == Self::Float(0.0) {
            return Err(Error::CannotDivideByZeroError { left: self });
        }
        match (self, rhs) {
            (Value::Float(left), Value::Float(right)) => Ok(Value::Float(left / right)),
            (Value::Float(left), Value::Integer(right)) => Ok(Value::Float(left / (right as f64))),
            (Value::Integer(left), Value::Float(right)) => Ok(Value::Float((left as f64) / right)),
            (Value::Integer(left), Value::Integer(right)) => {
                Ok(Value::Float((left as f64) / (right as f64)))
            }
            (left, right) => Err(Error::InvalidOperandError { left, right }),
        }
    }
}

impl PartialEq for Value {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Self::String(left), Self::String(right)) => left == right,
            (Self::Float(left), Self::Float(right)) => left == right,
            (Self::Integer(left), Self::Integer(right)) => left == right,
            (Self::Boolean(left), Self::Boolean(right)) => left == right,
            _ => false,
        }
    }
}

impl PartialOrd for Value {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        match (self, other) {
            (Value::Float(left), Value::Float(right)) => Some(left.partial_cmp(right)?),
            (Value::Float(left), Value::Integer(right)) => {
                Some(left.partial_cmp(&(*right as f64))?)
            }
            (Value::Integer(left), Value::Float(right)) => Some((*left as f64).partial_cmp(right)?),
            (Value::Integer(left), Value::Integer(right)) => Some(left.partial_cmp(right)?),
            _ => None,
        }
    }
}

impl Not for Value {
    type Output = Value;

    fn not(self) -> Self::Output {
        match self {
            Value::Boolean(bool) => Value::Boolean(!bool),
            Value::Nil => Value::Boolean(true),
            _ => Value::Boolean(false),
        }
    }
}

impl Neg for Value {
    type Output = Result<Value>;

    fn neg(self) -> Self::Output {
        match self {
            Value::Float(float) => Ok(Value::Float(-float)),
            Value::Integer(int) => Ok(Value::Integer(-int)),
            value => Err(Error::CannotApplyNegationError { value }),
        }
    }
}
