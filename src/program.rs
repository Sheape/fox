use std::marker::PhantomData;

use crate::function::Function;
use crate::lexer::Token;
use crate::parser::Node;
use crate::{Error, Result};

// Program states depending on which part of the process they are
#[derive(Debug)]
pub enum None {}
#[derive(Debug)]
pub enum Lexed {}
#[derive(Debug)]
pub enum Parsed {}
#[derive(Debug)]
pub enum Evaluated {}

#[derive(Debug)]
pub struct Program<'a, State = None> {
    pub tokens: Vec<Token>,
    pub line_offsets: Vec<usize>,
    pub errors: Vec<Error>,
    pub ast: Vec<Node<'a>>,
    //pub declarations: Vec<Statement<'a>>,
    pub input: &'a [u8],
    _state: PhantomData<State>,
}

#[derive(Debug)]
pub enum Declaration<'a> {
    Class {
        name: &'a Token,
        inherited_class: Option<&'a Token>,
        methods: Vec<&'a Function<'a>>,
    },
    Function(&'a Function<'a>),
    Variable {
        name: &'a Token,
        //expression: Option<
    },
}

impl<'a> Program<'a, None> {
    pub fn new(file_content: &str) -> Program<'_, None> {
        Program {
            tokens: vec![],
            line_offsets: vec![],
            errors: vec![],
            ast: vec![],
            input: file_content.as_bytes(),
            _state: PhantomData,
        }
    }

    pub fn lex(self) -> Program<'a, Lexed> {
        todo!()
    }
}

impl<'a> Program<'a, Lexed> {
    pub fn add_ast_node(&mut self, node: Node<'a>) {
        self.ast.push(node);
    }

    pub fn parse(self) -> Program<'a, Parsed> {
        todo!()
    }
}

impl<'a> Program<'a, Parsed> {
    pub fn evaluate(self) -> Program<'a, Evaluated> {
        todo!()
    }
}
