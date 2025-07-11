use std::{
    fmt::Display,
    ops::{Add, Div, Mul, Neg, Not, Sub},
};

use crate::{
    lexer::{Token, TokenType},
    parser::{ExprNodeType, Expression, NodeId, Statement},
    program::{ASTNode, Declaration, AST},
    vm::{
        opcode::{ADD, CMP_EQ, CMP_GREATER, CMP_LESS, DIV, LOAD_CONST, MUL, NEG, NOT, PRINT, SUB},
        Bytecode,
    },
    Error, Result,
};

#[derive(Debug, Clone)]
pub enum Value {
    String(String),
    Float(f64),
    Integer(i64),
    Boolean(bool),
    Nil,
}

pub struct Compiler<'a> {
    pub ast: AST<'a>,
    pub bytecode: Bytecode,
    pub constant_pool: Vec<Value>,
}

fn u16_to_u8(index: u16) -> (u8, u8) {
    let bytes = index.to_be_bytes();
    (bytes[0], bytes[1])
}

impl<'a> Compiler<'a> {
    pub fn new(ast: AST<'a>) -> Self {
        Self {
            ast,
            bytecode: vec![],
            constant_pool: vec![],
        }
    }

    pub fn compile(mut self) -> Self {
        let _: Vec<_> = self
            .ast
            .filter_root_nodes()
            .iter()
            .map(|node_id| self.compile_node(node_id))
            .collect();
        println!("Bytecode instructions:");
        let _ = &self.bytecode.iter().for_each(|byte| println!("{byte:#x}"));
        //dbg!(&self.constant_pool);

        Self {
            ast: self.ast,
            bytecode: self.bytecode,
            constant_pool: self.constant_pool,
        }
    }

    fn compile_node(&mut self, node_id: &NodeId) -> Result<()> {
        match &self.ast.nodes.get(*node_id).cloned().unwrap() {
            ASTNode::Declaration(declaration) => self.compile_declaration(declaration),
            ASTNode::Statement(statement) => self.compile_statement(statement),
            ASTNode::Expression(expression) => self.compile_expr(expression),
        }
    }

    fn compile_declaration(&mut self, declaration: &Declaration) -> Result<()> {
        match declaration {
            Declaration::Class {
                name,
                inherited_class,
                methods,
            } => todo!(),
            Declaration::Function(function) => todo!(),
            Declaration::Variable { name, expression } => todo!(),
            Declaration::Statement(node_id) => self.compile_node(node_id)?,
        };

        Ok(())
    }

    fn compile_statement(&mut self, statement: &Statement) -> Result<()> {
        match statement {
            Statement::Expr(node_id) => self.compile_node(node_id)?,
            Statement::Print(node_id) => {
                self.compile_node(node_id)?;
                self.bytecode.push(PRINT);
            }
            Statement::Return(_) => todo!(),
            Statement::While {
                condition,
                statement,
            } => todo!(),
            Statement::If {
                condition,
                statement,
                else_block,
            } => todo!(),
            Statement::For {
                initial,
                condition,
                after_expr,
                statement,
            } => todo!(),
            Statement::Block { start, length } => todo!(),
        }

        Ok(())
    }

    fn compile_expr(&mut self, expression: &Expression) -> Result<()> {
        match expression.node_type {
            ExprNodeType::Binary => {
                let _ = self.compile_node(&expression.lhs.unwrap());
                let _ = self.compile_node(&expression.rhs.unwrap());
                match expression.main_token.token_type {
                    TokenType::PLUS => Ok(self.bytecode.push(ADD)),
                    TokenType::MINUS => Ok(self.bytecode.push(SUB)),
                    TokenType::STAR => Ok(self.bytecode.push(MUL)),
                    TokenType::SLASH => Ok(self.bytecode.push(DIV)),
                    TokenType::LESS => {
                        self.bytecode.push(CMP_LESS);
                        self.bytecode.push(0u8);
                        Ok(())
                    }
                    TokenType::LESS_EQUAL => {
                        self.bytecode.push(CMP_LESS);
                        self.bytecode.push(1u8);
                        Ok(())
                    }
                    TokenType::GREATER => {
                        self.bytecode.push(CMP_GREATER);
                        self.bytecode.push(0u8);
                        Ok(())
                    }
                    TokenType::GREATER_EQUAL => {
                        self.bytecode.push(CMP_GREATER);
                        self.bytecode.push(1u8);
                        Ok(())
                    }
                    TokenType::EQUAL_EQUAL => Ok(self.bytecode.push(CMP_EQ)),
                    TokenType::BANG_EQUAL => {
                        self.bytecode.push(CMP_EQ);
                        self.bytecode.push(NOT);
                        Ok(())
                    }
                    _ => Err(Error::PlaceholderError), // TODO: Put proper error types here
                }
            }
            ExprNodeType::Unary => {
                self.compile_node(&expression.lhs.unwrap())?;
                match expression.main_token.token_type {
                    TokenType::MINUS => Ok(self.bytecode.push(NEG)),
                    TokenType::BANG => Ok(self.bytecode.push(NOT)),
                    _ => Err(Error::PlaceholderError),
                }
            }
            ExprNodeType::Grouping => self.compile_node(&expression.lhs.unwrap()),
            ExprNodeType::Super => todo!(),
            ExprNodeType::Literal => {
                self.compile_literal(expression.main_token.clone())?;
                Ok(())
            }
            ExprNodeType::Call => todo!(),
            ExprNodeType::Property => todo!(),
            ExprNodeType::Arguments => todo!(),
            ExprNodeType::Assignment => todo!(),
        }
    }

    fn compile_literal(&mut self, token: Token) -> Result<()> {
        let value = match token.token_type {
            TokenType::STRING(str) => Ok(Value::String(str)),
            TokenType::NUMBER_FLOAT(_, float) => Ok(Value::Float(float)),
            TokenType::NUMBER_INT(int) => Ok(Value::Integer(int as i64)),
            TokenType::TRUE => Ok(Value::Boolean(true)),
            TokenType::FALSE => Ok(Value::Boolean(false)),
            TokenType::NIL => Ok(Value::Nil),
            _ => Err(Error::PlaceholderError), // TODO: Assign a proper error type
        }?;

        self.constant_pool.push(value);

        let (high, low) = u16_to_u8((self.constant_pool.len() - 1) as u16);
        self.bytecode.push(LOAD_CONST);
        self.bytecode.push(high);
        self.bytecode.push(low);

        Ok(())
    }
}

// region: impl traits for Value
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
// endregion
