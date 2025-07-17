use std::{
    fmt::Display,
    ops::{Add, BitAnd, BitOr, Div, Mul, Neg, Not, Sub},
};

use crate::{
    lexer::{Token, TokenType},
    parser::{ASTNodeRef, ExprNodeType, Expression, NodeId, Statement},
    program::{ASTNode, Declaration, AST},
    vm::{disassembler::Disassembler, opcode::*, Bytecode},
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

pub struct Compiler {
    pub ast: AST,
    pub bytecode: Bytecode,
    pub constant_pool: Vec<Value>,
    local_names: Vec<String>,
    ip: usize,
    previous_scope_count: u8,
    scope_level: u8,
}

fn u16_to_u8(index: u16) -> (u8, u8) {
    let bytes = index.to_be_bytes();
    (bytes[0], bytes[1])
}

impl Compiler {
    pub fn new(ast: AST) -> Self {
        Self {
            ast,
            bytecode: vec![],
            constant_pool: vec![],
            local_names: vec![],
            ip: 0,
            previous_scope_count: 0,
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
        Disassembler::new(self.bytecode.clone(), self.constant_pool.clone()).disassemble();
        //self.bytecode.iter().for_each(|byte| print!("{byte:#x} "));
        //dbg!(&self.ast.filter_scoped_declaration(1, 2, 2));
        //dbg!(&self.constant_pool);
        //dbg!(&self.local_names);

        Self {
            ast: self.ast,
            bytecode: self.bytecode,
            constant_pool: self.constant_pool,
            local_names: vec![],
            ip: 0,
            previous_scope_count: 0,
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
            Declaration::Function {
                name,
                parameters,
                body,
            } => {
                self.bytecode.push(JMP);
                let function_index = self.bytecode.len();
                self.bytecode.push(0);
                self.bytecode.push(0);
                self.local_names
                    .push(format!("fun:{name}:{function_index}"));
                if let Some(param_node_id) = parameters {
                    self.compile_node(param_node_id)?;
                }
                self.compile_node(body)?;
                self.bytecode.push(JMP);
                let (high, low) = u16_to_u8((self.bytecode.len() - self.ip + 2) as u16);
                self.bytecode.push(high);
                self.bytecode.push(low);

                let (high, low) = u16_to_u8((self.bytecode.len() - function_index - 2) as u16);
                self.bytecode[function_index] = high;
                self.bytecode[function_index + 1] = low;
            }
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
                    if let Some(node_id) = expression {
                        self.compile_node(node_id)?;
                        self.bytecode.push(DECLARE_LOCAL);
                        self.bytecode.push(1);
                    } else {
                        self.bytecode.push(DECLARE_LOCAL);
                        self.bytecode.push(0);
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
            } => {
                let initial_idx = self.bytecode.len();
                self.compile_node(condition)?;
                self.bytecode.push(JMP_IF_FALSE);
                let jmp_false_index = self.bytecode.len();
                self.bytecode.push(0);
                self.bytecode.push(0);
                self.compile_node(statement)?;

                self.bytecode.push(JMP_UP);
                let (high, low) = u16_to_u8((self.bytecode.len() - initial_idx + 2) as u16);
                self.bytecode.push(high);
                self.bytecode.push(low);

                let (high, low) = u16_to_u8((self.bytecode.len() - jmp_false_index - 2) as u16);
                self.bytecode[jmp_false_index] = high;
                self.bytecode[jmp_false_index + 1] = low;
            }
            Statement::If {
                condition,
                statement,
                else_block,
            } => {
                self.compile_node(condition)?;
                self.bytecode.push(JMP_IF_FALSE);
                let jmp_false_index = self.bytecode.len();
                self.bytecode.push(0); // replace with the real index later
                self.bytecode.push(0);

                self.compile_node(statement)?;

                let jmp_index = self.bytecode.len() + 1;
                if else_block.is_some() {
                    self.bytecode.push(JMP);
                    self.bytecode.push(0); // replace with the real index later
                    self.bytecode.push(0);
                }

                let (high, low) = u16_to_u8((self.bytecode.len() - jmp_false_index - 2) as u16);
                self.bytecode[jmp_false_index] = high;
                self.bytecode[jmp_false_index + 1] = low;

                if let Some(else_idx) = else_block {
                    self.compile_node(else_idx)?;

                    let (high, low) = u16_to_u8((self.bytecode.len() - jmp_index - 2) as u16);
                    self.bytecode[jmp_index] = high;
                    self.bytecode[jmp_index + 1] = low;
                }
            }
            Statement::For {
                initial,
                condition,
                after_expr,
                statement,
            } => {
                // initial
                if let Some(initial_idx) = initial {
                    self.compile_node(initial_idx)?;
                }

                // statement body
                let statement_idx = self.bytecode.len();
                self.compile_node(statement)?;

                // after expr
                if let Some(after_expr_idx) = after_expr {
                    self.compile_node(after_expr_idx)?;
                }

                if let Some(condition_idx) = condition {
                    self.compile_node(condition_idx)?;
                    self.bytecode.push(JMP_UP_IF_TRUE);
                } else {
                    self.bytecode.push(JMP_UP);
                }

                let (high, low) = u16_to_u8((self.bytecode.len() - statement_idx + 2) as u16);
                self.bytecode.push(high);
                self.bytecode.push(low);
            }
            Statement::Block { start, length } => {
                if let Some(start) = start {
                    self.scope_level += 1;
                    self.previous_scope_count = self.local_names.len() as u8;
                    let prev = self.previous_scope_count;
                    for node_id in self
                        .ast
                        .filter_scoped_declaration(*start, *length, self.scope_level)
                        .iter()
                    {
                        self.compile_node(node_id)?;
                    }
                    self.previous_scope_count = prev;
                    (prev != self.local_names.len() as u8).then(|| {
                        self.bytecode.push(DROP);
                        self.bytecode.push(
                            (self.previous_scope_count as usize..self.local_names.len()).count()
                                as u8,
                        );
                        self.local_names
                            .drain(self.previous_scope_count as usize..self.local_names.len());
                    });
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
                    TokenType::AND => Ok(self.bytecode.push(AND)),
                    TokenType::OR => Ok(self.bytecode.push(OR)),
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
            ExprNodeType::Call => {
                self.ip = self.bytecode.len();
                let ident_node = expression.lhs.unwrap();
                let arguments_node = expression.rhs.unwrap();
                self.compile_node(&arguments_node)?;

                if let ASTNode::Expression(Expression {
                    node_type: _,
                    main_token: identifier,
                    lhs: _,
                    rhs: _,
                }) = self.ast.nodes[ident_node].clone()
                    && let TokenType::IDENTIFIER(identifier_name) = identifier.token_type
                {
                    println!("Calling: {identifier_name}");
                    let (index, is_local) = self.find_function(&identifier_name)?;
                    self.bytecode.push(JMP_UP);
                    let (high, low) = u16_to_u8((self.bytecode.len() - 2 - index) as u16);
                    self.bytecode.push(high);
                    self.bytecode.push(low);
                } else {
                    return Err(Error::PlaceholderError);
                }
                Ok(())
            }
            ExprNodeType::Property => todo!(),
            ExprNodeType::Arguments => {
                let start = &expression.lhs.unwrap();
                let length = &expression.lhs.unwrap();
                for node_id in self.ast.filter_method_arguments(*start, *length).iter() {
                    self.compile_node(node_id)?;
                }
                Ok(())
            }
            ExprNodeType::Parameters => {
                let TokenType::IDENTIFIER(func_name) = &expression.main_token.token_type else {
                    panic!("Expected identifier")
                };
                let start = expression.lhs.unwrap();
                let length = expression.rhs.unwrap();
                for node_id in self.ast.filter_function_parameters(start, length) {
                    if let ASTNode::Expression(Expression {
                        node_type: _,
                        main_token,
                        lhs: _,
                        rhs: _,
                    }) = &self.ast.nodes[node_id]
                    {
                        let TokenType::IDENTIFIER(param_name) = main_token.token_type.clone()
                        else {
                            panic!("Expected identifier")
                        };
                        //self.local_names
                        //    .push(format!("fun_param:{func_name}:{param_name}"));
                        self.local_names.push(param_name);
                        self.bytecode.push(DECLARE_LOCAL);
                        self.bytecode.push(1);
                    } else {
                        return Err(Error::PlaceholderError);
                    }
                }

                Ok(())
            }
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
        let local_count = self.local_names.len();
        let mut is_local = true;

        (scope_level == 0).then(|| is_local = false);

        if is_local {
            let scope_range = &self.local_names[self.previous_scope_count as usize..local_count];
            for (idx, var_name) in scope_range.iter().enumerate().rev() {
                if var_name == name {
                    return Ok((scope_level + idx, true));
                }
            }

            let scope_range = &self.local_names[..self.previous_scope_count as usize];
            for (idx, var_name) in scope_range.iter().enumerate().rev() {
                if var_name == name {
                    return Ok((idx, true));
                }
            }
        }

        let index = self
            .constant_pool
            .iter()
            .position(|val| {
                if let Value::Utf8(var_name) = val {
                    is_local = false;
                    var_name == name
                } else {
                    false
                }
            })
            .ok_or(Error::PlaceholderError)?;

        Ok((index, is_local))
    }

    fn find_function(&mut self, name: &str) -> Result<(usize, bool)> {
        let scope_level = self.scope_level as usize;
        let local_count = self.local_names.len();
        let mut is_local = true;

        (scope_level == 0).then(|| is_local = false);

        if is_local {
            let scope_range = &self.local_names[self.previous_scope_count as usize..local_count];
            let name_to_find = format!("fun:{name}:");
            for var_name in scope_range.iter().rev() {
                if var_name.starts_with(name_to_find.as_str()) {
                    let index = var_name
                        .strip_prefix(&name_to_find)
                        .unwrap()
                        .parse::<usize>()
                        .unwrap();
                    return Ok((index, true));
                }
            }

            let scope_range = &self.local_names[..self.previous_scope_count as usize];
            for var_name in scope_range.iter().rev() {
                if var_name.starts_with(name_to_find.as_str()) {
                    let index = var_name
                        .strip_prefix(&name_to_find)
                        .unwrap()
                        .parse::<usize>()
                        .unwrap();
                    return Ok((index, true));
                }
            }
        }

        // TODO: Check for global function too
        let index = self
            .constant_pool
            .iter()
            .position(|val| {
                if let Value::Utf8(var_name) = val {
                    is_local = false;
                    var_name == name
                } else {
                    false
                }
            })
            .ok_or(Error::PlaceholderError)?;

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

impl BitOr for Value {
    type Output = Value;

    fn bitor(self, rhs: Self) -> Self::Output {
        match (self, rhs) {
            (Value::String(str), _) => Value::String(str),
            (_, Value::String(str)) => Value::String(str),
            (Value::Float(float), _) => Value::Float(float),
            (_, Value::Float(float)) => Value::Float(float),
            (Value::Integer(int), _) => Value::Integer(int),
            (_, Value::Integer(int)) => Value::Integer(int),
            (Value::Boolean(bool1), Value::Boolean(bool2)) => Value::Boolean(bool1 || bool2),
            (Value::Boolean(bool), Value::Nil) => Value::Boolean(bool),
            (Value::Nil, Value::Boolean(bool)) => Value::Boolean(bool),
            (Value::Nil, Value::Nil) => todo!(),
            _ => todo!(),
        }
    }
}

impl BitAnd for Value {
    type Output = Value;

    fn bitand(self, rhs: Self) -> Self::Output {
        match (self, rhs) {
            (Value::Boolean(bool), other) | (other, Value::Boolean(bool)) => {
                if bool {
                    other
                } else {
                    Value::Boolean(false)
                }
            }
            (Value::Nil, _) => Value::Boolean(false),
            (_, Value::Nil) => Value::Boolean(false),
            (first, _) => first,
            _ => Value::Boolean(false),
        }
    }
}
