use std::collections::HashMap;

use crate::Error;
use crate::vm::opcode::{
    ADD, CMP_EQ, CMP_GREATER, CMP_LESS, DECLARE_GLOBAL, DIV, LOAD_CONST, LOAD_GLOBAL, MUL, NEG,
    NOT, PRINT, SET_GLOBAL, SUB,
};

mod compiler;
mod opcode;

pub use compiler::Compiler;
pub(crate) use compiler::Value;
pub use opcode::Bytecode;

pub struct VM {
    global: HashMap<String, Value>,
    constant_pool: Vec<Value>,
    bytecode: Bytecode,
    local_stack: Vec<Value>,
    ip: usize,
}

impl VM {
    pub fn init(bytecode: Bytecode, constant_pool: Vec<Value>) -> Self {
        Self {
            global: HashMap::new(),
            constant_pool,
            bytecode,
            local_stack: vec![],
            ip: 0,
        }
    }

    fn read_byte(&mut self) {
        self.ip += 1;
    }

    fn get_byte(&mut self) -> u8 {
        self.bytecode[self.ip]
    }

    fn next_u1(&mut self) -> u8 {
        let byte = self.bytecode[self.ip];
        self.ip += 1;
        byte
    }

    fn next_u2(&mut self) -> u16 {
        let high = self.next_u1();
        let low = self.next_u1();
        u16::from_be_bytes([high, low])
    }

    fn pop_local_stack(&mut self) -> Value {
        self.local_stack.pop().unwrap()
    }

    pub fn execute(&mut self) {
        println!("----- Execution starts ------");
        while self.ip < self.bytecode.len() {
            match self.next_u1() {
                LOAD_CONST => {
                    let index = self.next_u2();
                    let value = self.constant_pool[index as usize].clone();
                    self.local_stack.push(value);
                }
                ADD => {
                    let second_operand = self.pop_local_stack();
                    let first_operand = self.pop_local_stack();
                    let result = first_operand + second_operand;
                    self.local_stack.push(result.unwrap());
                }
                SUB => {
                    let subtrahend = self.pop_local_stack();
                    let minued = self.pop_local_stack();
                    let result = minued - subtrahend;
                    self.local_stack.push(result.unwrap());
                }
                MUL => {
                    let result = self.pop_local_stack() * self.pop_local_stack();
                    self.local_stack.push(result.unwrap());
                }
                DIV => {
                    let divisor = self.pop_local_stack();
                    let dividend = self.pop_local_stack();
                    let result = dividend / divisor;
                    self.local_stack.push(result.unwrap());
                }
                NEG => {
                    let result = -self.pop_local_stack();
                    self.local_stack.push(result.unwrap());
                }
                NOT => {
                    let result = !self.pop_local_stack();
                    self.local_stack.push(result);
                }
                CMP_GREATER => {
                    let flag = self.next_u1();
                    let second_operand = self.pop_local_stack();
                    let first_operand = self.pop_local_stack();
                    let result = match flag {
                        0 => Ok(first_operand > second_operand),
                        1 => Ok(first_operand >= second_operand),
                        _ => Err(Error::PlaceholderError),
                    };
                    self.local_stack.push(Value::Boolean(result.unwrap()));
                }
                CMP_LESS => {
                    let flag = self.next_u1();
                    let second_operand = self.pop_local_stack();
                    let first_operand = self.pop_local_stack();
                    let result = match flag {
                        0 => Ok(first_operand < second_operand),
                        1 => Ok(first_operand <= second_operand),
                        _ => Err(Error::PlaceholderError),
                    };
                    self.local_stack.push(Value::Boolean(result.unwrap()));
                }
                CMP_EQ => {
                    let result = self.pop_local_stack() == self.pop_local_stack();
                    self.local_stack.push(Value::Boolean(result));
                }
                PRINT => {
                    println!("{}", self.local_stack.last().unwrap());
                }
                DECLARE_GLOBAL => {
                    let index = self.next_u2();
                    if let Value::Utf8(var_name) = &self.constant_pool[index as usize] {
                        self.global.insert(var_name.to_string(), Value::Nil);
                    }
                }
                SET_GLOBAL => {
                    let index = self.next_u2();
                    let value = match self.get_byte() {
                        SET_GLOBAL | PRINT => self.local_stack.last().unwrap().clone(),
                        _ => self.pop_local_stack(),
                    };

                    if let Value::Utf8(var_name) = &self.constant_pool[index as usize] {
                        self.global.insert(var_name.to_string(), value);
                    }
                }
                LOAD_GLOBAL => {
                    let index = self.next_u2();
                    if let Value::Utf8(var_name) = &self.constant_pool[index as usize] {
                        let value = self.global.get(var_name).unwrap();
                        self.local_stack.push(value.clone());
                    }
                }
                _ => (),
            }
        }
    }
}
