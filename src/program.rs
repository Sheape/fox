use std::marker::PhantomData;

use crate::lexer::{Lexer, Token};
use crate::parser::{ASTNodeRef, Expression, NodeId, Parser, Statement};
use crate::vm::{Compiler, VM};
use crate::Error;

// Program states depending on which part of the process they are
#[derive(Debug)]
pub struct None;
#[derive(Debug)]
pub struct Lexed;
#[derive(Debug)]
pub struct Parsed;
#[derive(Debug)]
pub struct Compiled;

#[derive(Debug)]
pub struct Program<'a, State = None> {
    tokens: Vec<Token>,
    line_offsets: Vec<usize>,
    ast: AST,
    declarations: Vec<Declaration>,
    input: &'a [u8],
    _state: PhantomData<State>,
}

#[allow(non_camel_case_types)]
#[allow(clippy::upper_case_acronyms)]
#[derive(Debug)]
pub struct AST {
    pub nodes: Vec<ASTNode>,
    pub mem_arena: Vec<ASTNodeRef>,
}

#[derive(Debug, Clone)]
pub enum ASTNode {
    Declaration(Declaration),
    Statement(Statement),
    Expression(Expression),
}

#[derive(Debug, Clone)]
pub enum Declaration {
    Class {
        name: String,
        inherited_class: Option<String>,
        methods: NodeId,
    },
    Function {
        name: String,
        parameters: Option<NodeId>,
        body: NodeId,
    },
    Variable {
        name: String,
        expression: Option<NodeId>,
    },
    Statement(NodeId),
}

impl<'a> Program<'a, None> {
    pub fn new(file_content: &str) -> Program<'_, None> {
        Program {
            tokens: vec![],
            line_offsets: vec![],
            ast: AST {
                nodes: vec![],
                mem_arena: vec![],
            },
            declarations: vec![],
            input: file_content.as_bytes(),
            _state: PhantomData,
        }
    }

    pub fn lex(self, debug: bool) -> Program<'a, Lexed> {
        let lexer = Lexer::new(self.input).tokenize(debug);

        Program {
            tokens: lexer.tokens,
            line_offsets: lexer.line_offsets,
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
        //dbg!(&parser.ast);
        //dbg!(&parser.errors);
        //dbg!(&parser.mem_arena);
        //dbg!(&parser.root_nodes);
        //dbg!(&parser.ast[11]);
        //let _ = &parser.mem_arena.iter().for_each(|index| {
        //    dbg!(&parser.ast[*index]);
        //});
        let program = Program {
            tokens: parser.tokens,
            line_offsets: self.line_offsets,
            ast: AST {
                nodes: parser.ast,
                mem_arena: parser.mem_arena,
            },
            declarations: self.declarations,
            input: self.input,
            _state: PhantomData,
        };

        println!("{}", program.ast);
        program
    }
}

impl<'a> Program<'a, Parsed> {
    pub fn compile(self) -> VM {
        let compiler = Compiler::new(self.ast).compile();
        VM::init(compiler.bytecode, compiler.constant_pool)
    }
}
