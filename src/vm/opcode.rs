#![cfg_attr(any(), rustfmt::skip)]

pub type Bytecode = Vec<u8>;

pub const LOAD_CONST    : u8 = 0; // Takes u16 as args which is the index of constant in the constant pool.
pub const DECLARE_GLOBAL: u8 = 1;// Takes u16 as args which is the index of utf-8 in the constant pool.
pub const LOAD_GLOBAL   : u8 = 2;
pub const SET_GLOBAL    : u8 = 3;
pub const LOAD_LOCAL    : u8 = 4;
pub const SET_LOCAL     : u8 = 5;
pub const ADD           : u8 = 6;
pub const SUB           : u8 = 7;
pub const MUL           : u8 = 8;
pub const DIV           : u8 = 9;
pub const NEG           : u8 = 10;
pub const NOT           : u8 = 11;
pub const CMP_LESS      : u8 = 12;  // Requires u8 arg. 0x00 for < only and 0x01 for <=
pub const CMP_GREATER   : u8 = 13;  // Requires u8 arg. 0x00 for > only and 0x01 for >=
pub const CMP_EQ        : u8 = 14;
pub const DROP          : u8 = 15;
pub const PRINT         : u8 = 16;
