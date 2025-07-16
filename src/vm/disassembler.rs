use crate::vm::{opcode::*, Bytecode, Value};

pub struct Disassembler {
    bytecode: Bytecode,
    constant_pool: Vec<Value>,
    ip: usize,
}

impl Disassembler {
    pub fn new(bytecode: Bytecode, constant_pool: Vec<Value>) -> Self {
        Self {
            bytecode,
            constant_pool,
            ip: 0,
        }
    }

    fn read_get_u1(&mut self) -> u8 {
        self.ip += 1;
        self.bytecode[self.ip]
    }

    fn read_get_u2(&mut self) -> u16 {
        let high = self.read_get_u1();
        let low = self.read_get_u1();
        u16::from_be_bytes([high, low])
    }

    fn get_from_constant_pool(&self, high: u8, low: u8) -> Value {
        let index = u16::from_be_bytes([high, low]);
        self.constant_pool[index as usize].clone()
    }

    fn retrieve_from_const_pool(&mut self) -> (u8, u8, Value) {
        let high = self.read_get_u1();
        let low = self.read_get_u1();
        let index = u16::from_be_bytes([high, low]);
        (high, low, self.constant_pool[index as usize].clone())
    }

    pub fn disassemble(mut self) {
        while self.ip < self.bytecode.len() {
            let current_line = self.ip;
            let line = match self.bytecode[self.ip] {
                LOAD_CONST => {
                    let const_pool = self.retrieve_from_const_pool();
                    format!(
                        "LOAD_CONST {} {}    --> {}",
                        const_pool.0, const_pool.1, const_pool.2
                    )
                }
                DECLARE_GLOBAL => {
                    let const_pool = self.retrieve_from_const_pool();
                    format!(
                        "DECLARE_GLOBAL {} {}   --> {}",
                        const_pool.0, const_pool.1, const_pool.2
                    )
                }
                LOAD_GLOBAL => {
                    let const_pool = self.retrieve_from_const_pool();
                    format!(
                        "LOAD_GLOBAL {} {}   --> \"{}\"",
                        const_pool.0, const_pool.1, const_pool.2
                    )
                }
                SET_GLOBAL => {
                    let const_pool = self.retrieve_from_const_pool();
                    format!(
                        "SET_GLOBAL {} {}   --> \"{}\"",
                        const_pool.0, const_pool.1, const_pool.2
                    )
                }
                DECLARE_LOCAL => match self.read_get_u1() {
                    0 => "DECLARE_LOCAL uninitialized".to_owned(),
                    _ => "DECLARE_LOCAL initialized".to_owned(),
                },
                LOAD_LOCAL => format!("LOAD_LOCAL {}", self.read_get_u1()),
                SET_LOCAL => format!("SET_LOCAL {}", self.read_get_u1()),
                ADD => "ADD".to_owned(),
                SUB => "SUB".to_owned(),
                MUL => "MUL".to_owned(),
                DIV => "DIV".to_owned(),
                NEG => "NEG".to_owned(),
                NOT => "NOT".to_owned(),
                AND => "AND".to_owned(),
                OR => "OR".to_owned(),
                CMP_LESS => match self.read_get_u1() {
                    0 => "CMP_LESS <".to_owned(),
                    _ => "CMP_LESS <=".to_owned(),
                },
                CMP_GREATER => match self.read_get_u1() {
                    0 => "CMP_GREATER >".to_owned(),
                    _ => "CMP_GREATER >=".to_owned(),
                },
                CMP_EQ => "CMP_EQ".to_owned(),
                DROP => format!("DROP {}", self.read_get_u1()),
                PRINT => "PRINT".to_owned(),
                JMP_IF_FALSE => format!("JMP_IF_FALSE {}", self.ip + self.read_get_u1() as usize),
                JMP => format!("JMP {}", self.ip + self.read_get_u2() as usize),
                JMP_UP => format!("JMP_UP {}", self.ip as i32 - self.read_get_u2() as i32),
                JMP_UP_IF_TRUE => {
                    format!("JMP_UP_IF_TRUE {}", self.ip - self.read_get_u2() as usize)
                }
                _ => String::from(""),
            };

            println!("{current_line:<6}{line}");
            self.ip += 1;
        }
    }
}
