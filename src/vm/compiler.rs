use std::{
    fmt::Display,
    ops::{Add, Div, Mul, Neg, Not, Sub},
};

use crate::{
    lexer::{Token, TokenType},
    parser::{ExprNodeType, Expression, NodeId, Statement},
    program::{ASTNode, Declaration, AST},
    vm::{opcode::*, Bytecode},
    Error, Result,
};

#[derive(Debug, Clone)]
pub enum Value {
    String(String),
    Utf8(String), // this is used for class names, variables names, fucntion names, etc.
    Float(f64),
    Integer(i64),
    Boolean(bool),
    Nil,
}

pub struct Compiler<'a> {
    pub ast: AST<'a>,
    pub bytecode: Bytecode,
    pub constant_pool: Vec<Value>,
    local_names: Vec<String>,
    local_count: u8,
    scope_level: u8,
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
            local_names: vec![],
            local_count: 0,
            scope_level: 0,
        }
    }

    pub fn compile(mut self) -> Self {
        let root_nodes = self.ast.filter_root_nodes();
        let _: Vec<Result<_>> = root_nodes
            .iter()
            .map(|node_id| self.compile_node(node_id))
            .collect();
        println!("\nBytecode instructions:");
        self.bytecode.iter().for_each(|byte| print!("{byte:#} "));
        println!();
        dbg!(&self.constant_pool);

        Self {
            ast: self.ast,
            bytecode: self.bytecode,
            constant_pool: self.constant_pool,
            local_names: vec![],
            local_count: 0,
            scope_level: 0,
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
            Declaration::Variable { name, expression } => {
                if self.scope_level == 0 {
                    self.constant_pool.push(Value::Utf8(name.to_string()));
                    let (high, low) = u16_to_u8((self.constant_pool.len() - 1) as u16);
                    match expression {
                        Some(node_id) => {
                            self.compile_node(node_id)?;
                            self.bytecode.push(SET_GLOBAL);
                        }
                        None => self.bytecode.push(DECLARE_GLOBAL),
                    }

                    self.bytecode.push(high);
                    self.bytecode.push(low);
                } else {
                    self.local_names.push(name.to_string());
                    self.local_count += 1;
                    if let Some(node_id) = expression {
                        self.compile_node(node_id)?;
                        self.bytecode.push(SET_LOCAL);
                        self.bytecode.push(self.scope_level + self.local_count);
                    }
                }
            }
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
            Statement::Block { start, length } => {
                if let Some(start) = start {
                    self.scope_level += 1;
                    let mut node_index = *start;
                    while node_index < *length + 1 {
                        self.compile_node(&node_index)?;
                        node_index += 1;
                    }
                    self.bytecode.push(DROP);
                    self.scope_level -= 1;
                    return Ok(());
                } else {
                    return Ok(());
                }
            }
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
            ExprNodeType::Assignment => {
                if let TokenType::IDENTIFIER(ident_name) = &expression.main_token.token_type {
                    let ident_metadata = self.find_var(ident_name)?;
                    self.compile_node(&expression.rhs.unwrap())?;
                    if ident_metadata.1 {
                        self.bytecode.push(SET_LOCAL);
                        self.bytecode.push(ident_metadata.0 as u8);
                    } else {
                        let (high, low) = u16_to_u8(ident_metadata.0 as u16);
                        self.bytecode.push(SET_GLOBAL);
                        self.bytecode.push(high);
                        self.bytecode.push(low);
                    }

                    Ok(())
                } else {
                    Err(Error::PlaceholderError)
                }
            }
        }
    }

    fn find_var(&mut self, name: &str) -> Result<(usize, bool)> {
        let scope_level = self.scope_level as usize;
        let local_count = self.local_count as usize;
        let mut index = 0;
        let mut is_local = true;
        let mut requires_upvalue = true;

        (scope_level == 0).then(|| is_local = false);

        if is_local {
            let scope_range = &self.local_names[scope_level..scope_level + local_count];
            for (idx, var_name) in scope_range.iter().enumerate() {
                (var_name == name).then(|| {
                    index = scope_level + idx;
                    requires_upvalue = false;
                });
            }
        } else if requires_upvalue {
            let scope_range = &self.local_names[..scope_level];
            for (idx, var_name) in scope_range.iter().enumerate() {
                (var_name == name).then(|| index = idx);
            }
        } else {
            index = self
                .constant_pool
                .iter()
                .position(|val| {
                    if let Value::Utf8(var_name) = val {
                        var_name == name
                    } else {
                        false
                    }
                })
                .ok_or(Error::PlaceholderError)?;
        }

        Ok((index, is_local))
    }

    fn compile_literal(&mut self, token: Token) -> Result<()> {
        if let TokenType::IDENTIFIER(ident_name) = token.token_type {
            let ident_idx = self.find_var(&ident_name)?;

            if ident_idx.1 {
                self.bytecode.push(LOAD_LOCAL);
                self.bytecode.push(ident_idx.0 as u8);
            } else {
                let (high, low) = u16_to_u8(ident_idx.0 as u16);
                self.bytecode.push(LOAD_GLOBAL);
                self.bytecode.push(high);
                self.bytecode.push(low);
            }

            return Ok(());
        }

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
            Value::Utf8(name) => write!(f, "{name}"),
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
