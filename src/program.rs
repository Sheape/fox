use std::marker::PhantomData;

use crate::function::Function;
use crate::lexer::{Lexer, Token};
use crate::parser::{Expression, NodeId, Parser, Statement};
use crate::Error;

// Program states depending on which part of the process they are
#[derive(Debug)]
pub struct None;
#[derive(Debug)]
pub struct Lexed;
#[derive(Debug)]
pub struct Parsed;
#[derive(Debug)]
pub struct Evaluated;

#[derive(Debug)]
pub struct Program<'a, State = None> {
    tokens: Vec<Token>,
    line_offsets: Vec<usize>,
    errors: Vec<Error>,
    ast: Vec<ASTNode<'a>>,
    declarations: Vec<Declaration<'a>>,
    input: &'a [u8],
    _state: PhantomData<State>,
}

#[derive(Debug)]
pub enum ASTNode<'a> {
    Declaration(Declaration<'a>),
    Statement(Statement),
    Expression(Expression),
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
        expression: Vec<&'a Expression>,
    },
    Statement(Statement),
}

impl<'a> Program<'a, None> {
    pub fn new(file_content: &str) -> Program<'_, None> {
        Program {
            tokens: vec![],
            line_offsets: vec![],
            errors: vec![],
            ast: vec![],
            declarations: vec![],
            input: file_content.as_bytes(),
            _state: PhantomData,
        }
    }

    pub fn lex(self) -> Program<'a, Lexed> {
        let lexer = Lexer::new(self.input).tokenize();
        dbg!(&lexer.tokens);
        Program {
            tokens: lexer.tokens,
            line_offsets: lexer.line_offsets,
            errors: lexer.errors,
            ast: self.ast,
            declarations: self.declarations,
            input: lexer.input,
            _state: PhantomData,
        }
    }
}

impl<'a> Program<'a, Lexed> {
    pub fn parse(self) -> Program<'a, Parsed> {
        let parser = Parser::new(self.tokens).parse();
        dbg!(&parser.ast);
        dbg!(&parser.errors);
        Program {
            tokens: parser.tokens,
            line_offsets: self.line_offsets,
            errors: parser.errors,
            ast: parser.ast,
            declarations: self.declarations,
            input: self.input,
            _state: PhantomData,
        }
    }
}

impl<'a> Program<'a, Parsed> {
    pub fn evaluate(self) -> Program<'a, Evaluated> {
        todo!()
    }
}
