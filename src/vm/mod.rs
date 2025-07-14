use std::collections::HashMap;

use crate::vm::opcode::*;
use crate::Error;

mod compiler;
mod opcode;

pub use compiler::Compiler;
pub(crate) use compiler::Value;
pub use opcode::Bytecode;

pub struct VM {
    global: HashMap<String, Value>,
    local: Vec<Value>,
    stack: Vec<Value>,
    constant_pool: Vec<Value>,
    bytecode: Bytecode,
    ip: usize,
    local_count: u8,
}

impl VM {
    pub fn init(bytecode: Bytecode, constant_pool: Vec<Value>) -> Self {
        Self {
            global: HashMap::new(),
            local: vec![],
            stack: vec![],
            constant_pool,
            bytecode,
            ip: 0,
            local_count: 0,
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

    fn pop_stack(&mut self) -> Value {
        self.stack.pop().unwrap()
    }

    fn pop_local_stack(&mut self) -> Value {
        let value = self.local.pop().unwrap();
        self.local_count = self.local.len() as u8;
        value
    }

    pub fn execute(&mut self) {
        println!("----- Execution starts ------");
        while self.ip < self.bytecode.len() {
            match self.next_u1() {
                LOAD_CONST => {
                    let index = self.next_u2();
                    let value = self.constant_pool[index as usize].clone();
                    self.stack.push(value);
                }
                ADD => {
                    let second_operand = self.pop_stack();
                    let first_operand = self.pop_stack();
                    let result = first_operand + second_operand;
                    self.stack.push(result.unwrap());
                }
                SUB => {
                    let subtrahend = self.pop_stack();
                    let minued = self.pop_stack();
                    let result = minued - subtrahend;
                    self.stack.push(result.unwrap());
                }
                MUL => {
                    let result = self.pop_stack() * self.pop_stack();
                    self.stack.push(result.unwrap());
                }
                DIV => {
                    let divisor = self.pop_stack();
                    let dividend = self.pop_stack();
                    let result = dividend / divisor;
                    self.stack.push(result.unwrap());
                }
                NEG => {
                    let result = -self.pop_stack();
                    self.stack.push(result.unwrap());
                }
                NOT => {
                    let result = !self.pop_stack();
                    self.stack.push(result);
                }
                AND => {
                    let second_operand = self.pop_stack();
                    let first_operand = self.pop_stack();
                    let result = first_operand & second_operand;
                    self.stack.push(result);
                }
                OR => {
                    let second_operand = self.pop_stack();
                    let first_operand = self.pop_stack();
                    let result = first_operand | second_operand;
                    self.stack.push(result);
                }
                CMP_GREATER => {
                    let flag = self.next_u1();
                    let second_operand = self.pop_stack();
                    let first_operand = self.pop_stack();
                    let result = match flag {
                        0 => Ok(first_operand > second_operand),
                        1 => Ok(first_operand >= second_operand),
                        _ => Err(Error::PlaceholderError),
                    };
                    self.stack.push(Value::Boolean(result.unwrap()));
                }
                CMP_LESS => {
                    let flag = self.next_u1();
                    let second_operand = self.pop_stack();
                    let first_operand = self.pop_stack();
                    let result = match flag {
                        0 => Ok(first_operand < second_operand),
                        1 => Ok(first_operand <= second_operand),
                        _ => Err(Error::PlaceholderError),
                    };
                    self.stack.push(Value::Boolean(result.unwrap()));
                }
                CMP_EQ => {
                    let result = self.pop_stack() == self.pop_stack();
                    self.stack.push(Value::Boolean(result));
                }
                PRINT => println!("{}", self.stack.last().unwrap()),
                DECLARE_GLOBAL => {
                    let index = self.next_u2();
                    if let Value::Utf8(name) = &self.constant_pool[index as usize] {
                        self.global.insert(name.to_string(), Value::Nil);
                    }
                }
                SET_GLOBAL => {
                    let index = self.next_u2();
                    let value = match self.get_byte() {
                        SET_GLOBAL | PRINT => self.stack.last().unwrap().clone(),
                        _ => self.pop_stack(),
                    };

                    if let Value::Utf8(name) = &self.constant_pool[index as usize] {
                        self.global.insert(name.to_string(), value);
                    }
                }
                LOAD_GLOBAL => {
                    let index = self.next_u2();
                    if let Value::Utf8(name) = &self.constant_pool[index as usize] {
                        let value = self.global.get(name).unwrap();
                        self.stack.push(value.clone());
                    }
                }
                DECLARE_LOCAL => {
                    let flag = self.next_u1();
                    if flag == 0 {
                        self.local.push(Value::Nil);
                        return;
                    }

                    let value = match self.get_byte() {
                        SET_GLOBAL | PRINT => self.stack.last().unwrap().clone(),
                        _ => self.pop_stack(),
                    };
                    self.local.push(value);
                }
                SET_LOCAL => {
                    let local_index = self.next_u1();
                    let value = match self.get_byte() {
                        SET_LOCAL | PRINT => self.stack.last().unwrap().clone(),
                        _ => self.pop_stack(),
                    };

                    self.local[local_index as usize] = value;
                }
                LOAD_LOCAL => {
                    let index = self.next_u1();
                    let value = &self.local[index as usize];
                    self.stack.push(value.clone());
                }
                DROP => {
                    let count = self.next_u1();
                    self.local
                        .drain(self.local.len() - count as usize..self.local.len());
                }
                JMP_IF_FALSE => {
                    let jmp_offset = self.next_u2();
                    if self.pop_stack() == Value::Boolean(false) {
                        self.ip += jmp_offset as usize;
                    }
                }
                JMP => {
                    let jmp_offset = self.next_u2();
                    self.ip += jmp_offset as usize;
                }
                JMP_UP => {
                    let jmp_offset = self.next_u2();
                    self.ip -= jmp_offset as usize;
                }
                JMP_UP_IF_TRUE => {
                    let jmp_offset = self.next_u2();
                    if self.pop_stack() == Value::Boolean(true) {
                        self.ip -= jmp_offset as usize;
                    }
                }
                _ => (),
            }
        }
    }
}
