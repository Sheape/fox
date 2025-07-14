#![cfg_attr(any(), rustfmt::skip)]

pub type Bytecode = Vec<u8>;

pub const LOAD_CONST    : u8 = 0; // Takes u16 as args which is the index of constant in the constant pool.
pub const DECLARE_GLOBAL: u8 = 1; // Takes u16 as args which is the index of utf-8 in the constant pool.
pub const LOAD_GLOBAL   : u8 = 2;
pub const SET_GLOBAL    : u8 = 3;
pub const DECLARE_LOCAL : u8 = 4; // Takes u8 as args. 0 if its just declaration, 1 if it has an initial expression.
pub const LOAD_LOCAL    : u8 = 5; // Takes u8 as args which is the index in local
pub const SET_LOCAL     : u8 = 6; // Takes u8 as args which is the index in local
pub const ADD           : u8 = 7;
pub const SUB           : u8 = 8;
pub const MUL           : u8 = 9;
pub const DIV           : u8 = 10;
pub const NEG           : u8 = 11;
pub const NOT           : u8 = 12;
pub const AND           : u8 = 13;
pub const OR            : u8 = 14;
pub const CMP_LESS      : u8 = 15; // Requires u8 arg. 0x00 for < only and 0x01 for <=
pub const CMP_GREATER   : u8 = 16; // Requires u8 arg. 0x00 for > only and 0x01 for >=
pub const CMP_EQ        : u8 = 17;
pub const DROP          : u8 = 18; // Takes u8 as args which is the number of elements to be popped from local
pub const PRINT         : u8 = 19;
pub const JMP_IF_FALSE  : u8 = 20;
pub const JMP           : u8 = 21;
pub const JMP_UP        : u8 = 22;
pub const JMP_UP_IF_TRUE: u8 = 23;
