use crate::{
    lexer::Token,
    parser::{Expression, NodeId},
};

#[derive(Debug)]
pub struct Function<'a> {
    name: &'a Token,
    parameters: Vec<&'a Token>,
    arguments: Vec<&'a Expression>,
    block: NodeId,
}
