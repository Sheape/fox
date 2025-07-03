use crate::{
    lexer::{Token, TokenType},
    program::{Declaration, AST},
    Result,
};
use crate::{program::ASTNode, Error};
use std::fmt::{Debug, Display};

#[derive(Debug)]
pub struct Parser<'a> {
    pub ast: Vec<ASTNode<'a>>,
    pub errors: Vec<Error>,
    pub tokens: Vec<Token>,
    current_token_idx: usize,
}

#[derive(Debug)]
pub struct Expression {
    pub node_type: ExprNodeType,
    pub main_token: Token, // PERF: Use &'a Token instead as we dont want a copy of the token.
    pub lhs: Option<NodeId>,
    pub rhs: Option<NodeId>,
}

//#[derive(Debug)]
//pub enum LiteralType {
//    Int,
//    Float,
//    Bool,
//    String,
//}

#[derive(Debug)]
pub enum Statement {
    ExprStatement(NodeId),
    PrintStatement(NodeId),
}

#[derive(Debug)]
pub enum ExprNodeType {
    Binary,
    Unary,
    Grouping,
    Literal,
}

#[derive(Debug, Clone)]
pub struct NodeId(usize);

//impl<'a> Iterator for Parser<'a> {
//    type Item = Result<ASTNode<'a>>;
//
//    fn next(&mut self) -> Option<Self::Item> {
//        //self.program.tokens.get(self.current_token_idx);
//        //let current_token = self.get_token();
//
//        match self.get_token_type() {
//            Some(TokenType::PRINT) => Some(self.parse_print_statement()),
//            _ => None,
//        }
//    }
//}

impl<'a> Parser<'a> {
    pub fn new(tokens: Vec<Token>) -> Self {
        Self {
            tokens,
            ast: vec![],
            errors: vec![],
            current_token_idx: 0,
        }
    }

    // PERF: Use secondary lifetime to avoid cloning tokens
    fn get_token(&mut self) -> Option<Token> {
        self.tokens.get(self.current_token_idx).cloned()
    }

    // PERF: Try to avoid clone in token_type tho technically its cheap to do except for
    // TokenType::STRING, IDENTIFIER, NUMBER_FLOAT, NUMBER_INT.
    fn get_token_type(&mut self) -> Option<TokenType> {
        self.tokens
            .get(self.current_token_idx)
            .map(|token| token.token_type.clone())
    }

    fn read_token(&mut self) {
        self.current_token_idx += 1;
    }

    pub fn parse(mut self) -> Self {
        while let Some(current_token) = self.get_token() {
            let declaration = match current_token.token_type {
                TokenType::EOF => break,
                _ => self
                    .parse_statement()
                    .map(|statement| ASTNode::Declaration(Declaration::Statement(statement))),
            };

            match declaration {
                Ok(node) => self.ast.push(node),
                Err(err) => self.errors.push(err),
            }
        }

        Self {
            ast: self.ast,
            errors: self.errors,
            tokens: self.tokens,
            current_token_idx: 0,
        }
    }

    fn parse_statement(&mut self) -> Result<NodeId> {
        let statement = match self.get_token_type() {
            Some(TokenType::PRINT) => self.parse_print_statement().map(Statement::PrintStatement),
            //_ => self.parse_expr_statement(),
            _ => todo!(),
        };

        self.ast.push(ASTNode::Statement(statement?));
        Ok(NodeId(self.ast.len() - 1))
    }

    fn parse_print_statement(&mut self) -> Result<NodeId> {
        self.read_token(); // Skip "print"
        self.parse_expr_statement()
    }

    fn parse_expr_statement(&mut self) -> Result<NodeId> {
        let expr_node = self.parse_expr();

        match self.get_token_type() {
            Some(TokenType::SEMICOLON) => {
                self.read_token();
                expr_node
            }
            _ => Err(Error::MissingSemiColon),
        }
    }

    fn parse_expr(&mut self) -> Result<NodeId> {
        let mut comparison = self.parse_comparison()?;
        while let Some(current_token) = self.get_token() {
            match current_token.token_type {
                TokenType::EQUAL_EQUAL | TokenType::BANG_EQUAL => {
                    self.read_token();
                    let rhs = self.parse_comparison()?;
                    self.ast.push(ASTNode::Expression(Expression {
                        node_type: ExprNodeType::Binary,
                        main_token: current_token,
                        lhs: Some(comparison.clone()),
                        rhs: Some(rhs),
                    }));

                    comparison = NodeId(self.ast.len() - 1);
                }
                _ => {
                    break;
                }
            }
        }

        Ok(comparison)
    }

    fn parse_comparison(&mut self) -> Result<NodeId> {
        let mut term = self.parse_term()?;
        while let Some(current_token) = self.get_token() {
            match current_token.token_type {
                TokenType::LESS
                | TokenType::LESS_EQUAL
                | TokenType::GREATER
                | TokenType::GREATER_EQUAL => {
                    self.read_token();
                    let rhs = self.parse_term()?;
                    self.ast.push(ASTNode::Expression(Expression {
                        node_type: ExprNodeType::Binary,
                        main_token: current_token,
                        lhs: Some(term),
                        rhs: Some(rhs),
                    }));
                    term = NodeId(self.ast.len() - 1)
                }
                _ => {
                    break;
                }
            }
        }

        Ok(term)
    }

    fn parse_term(&mut self) -> Result<NodeId> {
        let mut factor = self.parse_factor()?;
        while let Some(current_token) = self.get_token() {
            match current_token.token_type {
                TokenType::PLUS | TokenType::MINUS => {
                    self.read_token();
                    let rhs = self.parse_factor()?;
                    self.ast.push(ASTNode::Expression(Expression {
                        node_type: ExprNodeType::Binary,
                        main_token: current_token,
                        lhs: Some(factor),
                        rhs: Some(rhs),
                    }));
                    factor = NodeId(self.ast.len() - 1);
                }
                _ => {
                    break;
                }
            }
        }

        Ok(factor)
    }

    fn parse_factor(&mut self) -> Result<NodeId> {
        let mut unary = self.parse_unary()?;
        while let Some(current_token) = self.get_token() {
            match current_token.token_type {
                TokenType::STAR | TokenType::SLASH => {
                    self.read_token();
                    let rhs = self.parse_unary()?;
                    self.ast.push(ASTNode::Expression(Expression {
                        node_type: ExprNodeType::Binary,
                        main_token: current_token,
                        lhs: Some(unary),
                        rhs: Some(rhs),
                    }));
                    unary = NodeId(self.ast.len() - 1);
                }
                _ => {
                    break;
                }
            }
        }

        Ok(unary)
    }

    fn parse_unary(&mut self) -> Result<NodeId> {
        match self.get_token() {
            Some(token) if matches!(token.token_type, TokenType::BANG | TokenType::MINUS) => {
                self.read_token();
                let primary = self.parse_unary()?;
                self.ast.push(ASTNode::Expression(Expression {
                    node_type: ExprNodeType::Unary,
                    main_token: token,
                    lhs: Some(primary),
                    rhs: None,
                }));

                Ok(NodeId(self.ast.len() - 1))
            }
            _ => Ok(self.parse_primary()?),
        }
    }

    fn parse_primary(&mut self) -> Result<NodeId> {
        let current_token = self.get_token().unwrap();
        self.read_token();
        let expression = match current_token.token_type {
            TokenType::LEFT_PAREN => {
                let node = self.parse_expr()?;
                //let closing_paren = self.get_token();
                match self.get_token() {
                    Some(closing_paren) if closing_paren.token_type == TokenType::RIGHT_PAREN => {
                        self.read_token();
                        Ok(Expression {
                            node_type: ExprNodeType::Grouping,
                            main_token: current_token,
                            lhs: Some(node),
                            rhs: None,
                        })
                    }
                    _ => Err(Error::SyntaxError {
                        line_number: current_token.start,
                        token: format!("{}", current_token.token_type),
                    }),
                }
            }
            TokenType::STRING(_)
            | TokenType::NUMBER_FLOAT(_, _)
            | TokenType::NUMBER_INT(_)
            | TokenType::TRUE
            | TokenType::FALSE
            | TokenType::NIL => Ok(Expression {
                node_type: ExprNodeType::Literal,
                main_token: current_token,
                lhs: None,
                rhs: None,
            }),
            _ => Err(Error::SyntaxError {
                line_number: current_token.start,
                token: format!("{}", current_token.token_type),
            }),
        };

        self.ast.push(ASTNode::Expression(expression?));
        Ok(NodeId(self.ast.len() - 1))
    }
}

//impl Display for Vec<ASTNode<'_>> {
//    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
//        todo!()
//    }
//}

impl<'a> AST<'a> {
    // We implement our own display method instead of impl Display because we still need access to AST.
    fn display(&self, node: &ASTNode<'a>) -> String {
        match node {
            ASTNode::Declaration(declaration) => match declaration {
                Declaration::Class {
                    name,
                    inherited_class,
                    methods,
                } => todo!(),
                Declaration::Function(function) => todo!(),
                Declaration::Variable { name, expression } => todo!(),
                Declaration::Statement(statement) => self.display(&self.0[statement.0]),
            },
            ASTNode::Statement(statement) => match statement {
                Statement::ExprStatement(node_id) => {
                    format!("(expression {})", self.display(&self.0[node_id.0]))
                }
                Statement::PrintStatement(node_id) => {
                    format!("(print {})", self.display(&self.0[node_id.0]))
                }
            },
            ASTNode::Expression(expression) => match expression.node_type {
                ExprNodeType::Binary => {
                    // Its okay to clone here because we're just displaying the parsed debug info.
                    let lhs = self.display(&self.0[expression.lhs.clone().unwrap().0]);
                    let rhs = self.display(&self.0[expression.rhs.clone().unwrap().0]);
                    format!("({} {lhs} {rhs})", expression.main_token.token_type)
                }
                ExprNodeType::Unary => {
                    let unary = self.display(&self.0[expression.lhs.clone().unwrap().0]);
                    format!("({} {unary})", expression.main_token.token_type)
                }
                ExprNodeType::Grouping => format!(
                    "(group {})",
                    self.display(&self.0[expression.lhs.clone().unwrap().0])
                ),
                ExprNodeType::Literal => format!("{}", expression.main_token.token_type),
            },
        }
    }
}

impl Display for AST<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let ast = &self.0;

        let parsed = ast
            .iter()
            .filter_map(|node| {
                if let ASTNode::Declaration(_) = node {
                    Some(self.display(node))
                } else {
                    None
                }
            })
            .reduce(|acc, e| format!("{acc}\n{e}"))
            .unwrap_or("".to_owned());

        write!(f, "{parsed}")
    }
}
