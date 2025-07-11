#![cfg_attr(any(), rustfmt::skip)]

pub type Bytecode = Vec<u8>;

pub const LOAD_CONST   : u8 = 0; // Takes u16 as args which is the index of constant in the constant pool.
pub const SET_LOCAL    : u8 = 1;
pub const LOAD_GLOBAL  : u8 = 2;
pub const SET_GLOBAL   : u8 = 3;
pub const ADD          : u8 = 4;
pub const SUB          : u8 = 5;
pub const MUL          : u8 = 6;
pub const DIV          : u8 = 7;
pub const NEG          : u8 = 8;
pub const NOT          : u8 = 9;
pub const DROP         : u8 = 10;
pub const PRINT        : u8 = 11;
