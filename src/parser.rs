use crate::{
    lexer::{Token, TokenType},
    program::{Declaration, AST},
    Result,
};
use crate::{program::ASTNode, Error};
use std::fmt::{Debug, Display};

#[derive(Debug)]
pub struct Parser {
    pub ast: Vec<ASTNode>,
    pub mem_arena: Vec<ASTNodeRef>,
    pub errors: Vec<Error>,
    pub tokens: Vec<Token>,
    current_token_idx: usize,
    scope_level: u8,
}

#[derive(Debug, Clone)]
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

#[derive(Debug, Clone)]
pub enum Statement {
    Expr(NodeId),
    Print(NodeId),
    Return(NodeId),
    While {
        condition: NodeId,
        statement: NodeId,
    },
    If {
        condition: NodeId,
        statement: NodeId,
        else_block: Option<NodeId>,
    },
    For {
        initial: Option<NodeId>,
        condition: Option<NodeId>,
        after_expr: Option<NodeId>,
        statement: NodeId,
    },
    Block {
        start: Option<NodeId>, // None for having no declarations
        length: usize,         // 0 for an empty block
    },
}

#[derive(Debug, Clone)]
pub enum ExprNodeType {
    Binary,
    Unary,
    Grouping,
    Super,
    Literal,
    Call,
    Property,
    Parameters,
    Arguments,
    Assignment,
}

#[derive(Debug, Clone)]
pub enum ASTNodeRef {
    ScopedDeclaration { reference: usize, depth: u8 },
    MethodArgs(usize),
    FunctionParams(usize),
}

pub type NodeId = usize;

impl Parser {
    // region: Utility methods
    pub fn new(tokens: Vec<Token>) -> Self {
        Self {
            tokens,
            ast: vec![],
            mem_arena: vec![],
            errors: vec![],
            current_token_idx: 0,
            scope_level: 0,
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

    fn revert_token(&mut self) {
        self.current_token_idx -= 1;
    }

    fn read_token(&mut self) {
        self.current_token_idx += 1;
    }
    // endregion
}

impl Parser {
    // region: Main parsing methods
    pub fn parse(mut self) -> Self {
        while let Some(current_token_type) = self.get_token_type() {
            let _ = match current_token_type {
                TokenType::EOF => break,
                _ => self.parse_declaration(),
            };
        }

        Self {
            ast: self.ast,
            mem_arena: self.mem_arena,
            errors: self.errors,
            tokens: self.tokens,
            current_token_idx: 0,
            scope_level: 0,
        }
    }

    fn parse_declaration(&mut self) -> Result<NodeId> {
        let is_root = self.scope_level == 0;
        let ast_node = match self.get_token_type() {
            Some(TokenType::VAR) => self
                .parse_var_declaration()
                .map(|(name, expression)| Declaration::Variable { name, expression }),
            Some(TokenType::FUN) => {
                self.read_token(); // reads "fun"
                self.parse_function_declaration()
                    .map(|(name, parameters, body)| Declaration::Function {
                        name,
                        parameters,
                        body,
                    })
            }
            _ => self.parse_statement().map(Declaration::Statement),
        }
        .map(ASTNode::Declaration);

        match ast_node {
            Ok(node) => self.ast.push(node),
            Err(err) => self.errors.push(err),
        }

        is_root.then(|| {
            self.mem_arena.push(ASTNodeRef::ScopedDeclaration {
                reference: self.ast.len() - 1,
                depth: 0,
            })
        });

        Ok(self.ast.len() - 1)
    }

    fn parse_function_declaration(&mut self) -> Result<(String, Option<NodeId>, NodeId)> {
        if let Some(TokenType::IDENTIFIER(function_name)) = self.get_token_type() {
            self.read_token(); // reads function_name
            if let Some(TokenType::LEFT_PAREN) = self.get_token_type() {
                self.read_token(); // reads '('
                let parameter_node_id = if let Some(TokenType::RIGHT_PAREN) = self.get_token_type()
                {
                    None
                } else {
                    Some(self.parse_parameters()?)
                };

                if let Some(TokenType::RIGHT_PAREN) = self.get_token_type() {
                    self.read_token(); // reads ')'
                    let block_statement = self.parse_statement()?; // reads block
                    Ok((function_name, parameter_node_id, block_statement))
                } else {
                    Err(Error::PlaceholderError)
                }
            } else {
                Err(Error::PlaceholderError)
            }
        } else {
            Err(Error::PlaceholderError)
        }
    }

    fn parse_var_declaration(&mut self) -> Result<(String, Option<NodeId>)> {
        self.read_token(); // reads "var"
        match self.get_token_type() {
            Some(TokenType::IDENTIFIER(name)) => {
                self.read_token(); // reads identifier
                match self.get_token_type() {
                    Some(TokenType::EQUAL) => {
                        self.read_token(); // reads '='
                        self.parse_statement()
                            .map(|expression| (name, Some(expression)))
                    }
                    Some(TokenType::SEMICOLON) => {
                        self.read_token(); // reads semicolon
                        Ok((name, None))
                    }
                    _ => Err(Error::MissingSemiColon),
                }
            }
            _ => Err(Error::IdentifierExpected),
        }
    }

    fn parse_statement(&mut self) -> Result<NodeId> {
        let statement = match self.get_token_type() {
            Some(TokenType::PRINT) => self.parse_print_statement().map(Statement::Print),
            Some(TokenType::RETURN) => self.parse_return_statement().map(Statement::Return),
            Some(TokenType::WHILE) => {
                self.parse_while_statement()
                    .map(|(condition, statement)| Statement::While {
                        condition,
                        statement,
                    })
            }
            Some(TokenType::IF) => {
                self.parse_if_statement()
                    .map(|(condition, statement, else_block)| Statement::If {
                        condition,
                        statement,
                        else_block,
                    })
            }
            Some(TokenType::FOR) => {
                self.parse_for_statement()
                    .map(
                        |(initial, condition, after_expr, statement)| Statement::For {
                            initial,
                            condition,
                            after_expr,
                            statement,
                        },
                    )
            }
            Some(TokenType::LEFT_BRACE) => self
                .parse_block_statement()
                .map(|(start, length)| Statement::Block { start, length }),
            _ => self.parse_expr_statement().map(Statement::Expr),
        };

        self.ast.push(ASTNode::Statement(statement?));
        Ok(self.ast.len() - 1)
    }

    fn parse_if_statement(&mut self) -> Result<(NodeId, NodeId, Option<NodeId>)> {
        let (condition, statement, else_block);

        self.read_token();
        if self.get_token_type() == Some(TokenType::LEFT_PAREN) {
            self.read_token();
            condition = self.parse_expr()?;
            if self.get_token_type() == Some(TokenType::RIGHT_PAREN) {
                self.read_token();
                statement = self.parse_statement()?;
                if self.get_token_type() == Some(TokenType::ELSE) {
                    self.read_token();
                    else_block = Some(self.parse_statement()?);
                } else {
                    else_block = None;
                }
            } else {
                return Err(Error::MissingRightParen);
            }
        } else {
            return Err(Error::MissingLeftParen);
        }

        Ok((condition, statement, else_block))
    }

    // TODO: Refactor this with a better return statement
    fn parse_for_statement(
        &mut self,
    ) -> Result<(Option<NodeId>, Option<NodeId>, Option<NodeId>, NodeId)> {
        self.read_token();
        if self.get_token_type() == Some(TokenType::LEFT_PAREN) {
            self.read_token();
            let initial = match self.get_token_type() {
                Some(TokenType::VAR) => Some(
                    self.parse_declaration()?, // Guaranteed to be var declaration
                ),
                Some(TokenType::SEMICOLON) => {
                    self.read_token();
                    None
                }
                _ => Some(self.parse_expr_statement()?),
            };

            let condition = if self.get_token_type() != Some(TokenType::SEMICOLON) {
                Some(self.parse_expr_statement()?)
            } else {
                self.read_token();
                None
            };

            let after_expr = if self.get_token_type() != Some(TokenType::RIGHT_PAREN) {
                Some(self.parse_expr()?)
                // TODO: Handle error for missing ) after expr
            } else {
                None
            };

            self.read_token(); // reading )
            Ok((initial, condition, after_expr, self.parse_statement()?))
        } else {
            Err(Error::MissingLeftParen)
        }
    }

    fn parse_while_statement(&mut self) -> Result<(NodeId, NodeId)> {
        self.read_token();
        let expr_node = match self.get_token_type() {
            Some(TokenType::LEFT_PAREN) => {
                self.read_token();
                self.parse_expr()
            }
            _ => Err(Error::MissingLeftParen),
        };

        let statement_node = match self.get_token_type() {
            Some(TokenType::RIGHT_PAREN) => {
                self.read_token();
                self.parse_statement()
            }
            _ => Err(Error::MissingRightParen),
        };

        Ok((expr_node?, statement_node?))
    }

    fn parse_block_statement(&mut self) -> Result<(Option<NodeId>, usize)> {
        self.read_token(); // Reads '{'
        let mut length = 0usize;
        let mut start = None; // This should never be mutated.
        self.scope_level += 1;
        while self.get_token_type() != Some(TokenType::RIGHT_BRACE) {
            let declaration_index = self.parse_declaration()?;
            self.mem_arena.push(ASTNodeRef::ScopedDeclaration {
                reference: declaration_index,
                depth: self.scope_level,
            });

            if length == 0 {
                start = Some(self.mem_arena.len() - 1);
            }

            length += 1;
        }
        self.scope_level -= 1;
        self.read_token(); // Reads '}'

        Ok((start, length))
    }

    fn parse_return_statement(&mut self) -> Result<NodeId> {
        self.read_token();
        self.parse_expr_statement()
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
    // endregion
}

impl Parser {
    // region: Recursive descent parsing for expr
    fn parse_expr(&mut self) -> Result<NodeId> {
        match self.get_token() {
            Some(token)
                if matches!(
                    token.token_type,
                    TokenType::THIS | TokenType::SUPER | TokenType::IDENTIFIER(_)
                ) =>
            {
                let identifier_node = self.parse_call()?;
                if self.get_token_type() == Some(TokenType::EQUAL) {
                    self.read_token();
                    let value = self.parse_expr()?;

                    self.ast.push(ASTNode::Expression(Expression {
                        node_type: ExprNodeType::Assignment,
                        main_token: token,
                        lhs: Some(identifier_node),
                        rhs: Some(value),
                    }));

                    Ok(self.ast.len() - 1)
                } else {
                    self.revert_token();
                    Ok(self.parse_logic_or()?)
                }
            }
            _ => Ok(self.parse_logic_or()?),
        }
    }

    fn parse_logic_or(&mut self) -> Result<NodeId> {
        let mut logic_and = self.parse_logic_and()?;
        while let Some(current_token) = self.get_token() {
            match current_token.token_type {
                TokenType::OR => {
                    self.read_token();
                    let rhs = self.parse_logic_and()?;
                    self.ast.push(ASTNode::Expression(Expression {
                        node_type: ExprNodeType::Binary,
                        main_token: current_token,
                        lhs: Some(logic_and),
                        rhs: Some(rhs),
                    }));

                    logic_and = self.ast.len() - 1;
                }
                _ => break,
            }
        }

        Ok(logic_and)
    }

    fn parse_logic_and(&mut self) -> Result<NodeId> {
        let mut equality = self.parse_equality()?;
        while let Some(current_token) = self.get_token() {
            match current_token.token_type {
                TokenType::AND => {
                    self.read_token();
                    let rhs = self.parse_equality()?;
                    self.ast.push(ASTNode::Expression(Expression {
                        node_type: ExprNodeType::Binary,
                        main_token: current_token,
                        lhs: Some(equality),
                        rhs: Some(rhs),
                    }));

                    equality = self.ast.len() - 1;
                }
                _ => break,
            }
        }

        Ok(equality)
    }

    fn parse_equality(&mut self) -> Result<NodeId> {
        let mut comparison = self.parse_comparison()?;
        while let Some(current_token) = self.get_token() {
            match current_token.token_type {
                TokenType::EQUAL_EQUAL | TokenType::BANG_EQUAL => {
                    self.read_token();
                    let rhs = self.parse_comparison()?;
                    self.ast.push(ASTNode::Expression(Expression {
                        node_type: ExprNodeType::Binary,
                        main_token: current_token,
                        lhs: Some(comparison),
                        rhs: Some(rhs),
                    }));

                    comparison = self.ast.len() - 1;
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
                    term = self.ast.len() - 1
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
                    factor = self.ast.len() - 1;
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
                    unary = self.ast.len() - 1;
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
                let call = self.parse_unary()?;
                self.ast.push(ASTNode::Expression(Expression {
                    node_type: ExprNodeType::Unary,
                    main_token: token,
                    lhs: Some(call),
                    rhs: None,
                }));

                Ok(self.ast.len() - 1)
            }
            _ => Ok(self.parse_call()?),
        }
    }

    fn parse_call(&mut self) -> Result<NodeId> {
        let mut primary = self.parse_primary()?;
        while let Some(current_token) = self.get_token() {
            match current_token.token_type {
                TokenType::LEFT_PAREN => {
                    self.read_token(); // reads '('
                    let rhs = if self.get_token_type() == Some(TokenType::RIGHT_PAREN) {
                        None
                    } else {
                        let arg_node = self.parse_arguments()?;
                        Some(arg_node)
                    };

                    self.ast.push(ASTNode::Expression(Expression {
                        node_type: ExprNodeType::Call,
                        main_token: current_token,
                        lhs: Some(primary),
                        rhs,
                    }));

                    self.read_token(); // reads ')'
                    primary = self.ast.len() - 1;
                }
                TokenType::DOT => {
                    self.read_token(); // reads '.'
                    let identifier_node = self.parse_primary()?;
                    self.ast.push(ASTNode::Expression(Expression {
                        node_type: ExprNodeType::Property,
                        main_token: current_token,
                        lhs: Some(identifier_node),
                        rhs: Some(primary),
                    }));
                    primary = self.ast.len() - 1;
                }
                _ => break,
            }
        }

        Ok(primary)
    }

    fn parse_parameters(&mut self) -> Result<NodeId> {
        let first_parameter = self.get_token().unwrap();
        let mut length = 1usize;
        let ident = self.parse_primary()?;
        self.mem_arena.push(ASTNodeRef::FunctionParams(ident));
        let start = self.mem_arena.len() - 1;

        while self.get_token_type() == Some(TokenType::COMMA) {
            self.read_token(); // reads ','
            let ident = self.parse_primary()?;
            self.mem_arena.push(ASTNodeRef::FunctionParams(ident));
            length += 1;
        }

        self.ast.push(ASTNode::Expression(Expression {
            node_type: ExprNodeType::Parameters,
            main_token: first_parameter,
            lhs: Some(start),
            rhs: Some(length),
        }));

        Ok(self.ast.len() - 1)
    }

    fn parse_arguments(&mut self) -> Result<NodeId> {
        let first_arg = self.get_token().unwrap();
        let expr_node = self.parse_expr()?;
        let mut length = 1usize;
        self.mem_arena.push(ASTNodeRef::MethodArgs(expr_node));
        let start = self.mem_arena.len() - 1;

        while self.get_token_type() == Some(TokenType::COMMA) {
            self.read_token(); // reads ','
            let expr_node = self.parse_expr()?;
            self.mem_arena.push(ASTNodeRef::MethodArgs(expr_node));
            length += 1;
        }

        self.ast.push(ASTNode::Expression(Expression {
            node_type: ExprNodeType::Arguments,
            main_token: first_arg,
            lhs: Some(start),
            rhs: Some(length),
        }));

        Ok(self.ast.len() - 1)
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
            TokenType::SUPER => {
                if self.get_token_type() == Some(TokenType::DOT) {
                    self.read_token();
                    if let Some(TokenType::IDENTIFIER(_)) = self.get_token_type() {
                        let identifier_node = self.parse_primary()?;
                        Ok(Expression {
                            node_type: ExprNodeType::Super,
                            main_token: current_token,
                            lhs: Some(identifier_node),
                            rhs: None,
                        })
                    } else {
                        Err(Error::IdentifierExpected)
                    }
                } else {
                    Err(Error::MissingDot)
                }
            }
            TokenType::STRING(_)
            | TokenType::NUMBER_FLOAT(_, _)
            | TokenType::NUMBER_INT(_)
            | TokenType::IDENTIFIER(_)
            | TokenType::THIS
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
        Ok(self.ast.len() - 1)
    }
    // endregion
}

//impl Display for Vec<ASTNode<'_>> {
//    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
//        todo!()
//    }
//}

impl AST {
    pub fn filter_root_nodes(&self) -> Vec<usize> {
        self.mem_arena
            .iter()
            .filter_map(|node_ref| {
                if let ASTNodeRef::ScopedDeclaration { reference, depth } = *node_ref {
                    (depth == 0).then_some(reference)
                } else {
                    None
                }
            })
            .collect()
    }

    pub fn filter_scoped_declaration(
        &self,
        start: usize,
        length: usize,
        scope_level: u8,
    ) -> Vec<usize> {
        let mut count = 0usize;
        self.mem_arena[start..]
            .iter()
            .filter_map(|node_ref| {
                if let ASTNodeRef::ScopedDeclaration { reference, depth } = *node_ref
                    && depth == scope_level
                    && count < length
                {
                    count += 1;
                    Some(reference)
                } else {
                    None
                }
            })
            .collect()
    }

    fn goto_node(&self, node_id: &NodeId) -> String {
        self.display(&self.nodes[*node_id], &self.mem_arena)
    }

    // We implement our own display method instead of impl Display because we still need access to AST.
    fn display(&self, node: &ASTNode, mem_arena: &[ASTNodeRef]) -> String {
        match node {
            ASTNode::Declaration(declaration) => match declaration {
                Declaration::Class {
                    name,
                    inherited_class,
                    methods,
                } => todo!(),
                Declaration::Function {
                    name,
                    parameters,
                    body,
                } => match parameters {
                    Some(param_id) => format!(
                        "(function {name} {} {})",
                        self.goto_node(param_id),
                        self.goto_node(body)
                    ),
                    None => format!("(function {name} {})", self.goto_node(body)),
                },
                Declaration::Variable { name, expression } => match expression {
                    Some(expr) => format!("(var {name} {})", self.goto_node(expr)),
                    None => format!("(var {name})"),
                },
                Declaration::Statement(statement) => self.goto_node(statement),
            },
            ASTNode::Statement(statement) => match statement {
                Statement::Expr(node_id) => {
                    format!("(expression {})", self.goto_node(node_id))
                }
                Statement::Print(node_id) => {
                    format!("(print {})", self.goto_node(node_id))
                }
                Statement::Return(node_id) => {
                    format!("(return {})", self.goto_node(node_id))
                }
                Statement::While {
                    condition,
                    statement,
                } => {
                    format!(
                        "(while {} {})",
                        self.goto_node(condition),
                        self.goto_node(statement)
                    )
                }
                Statement::If {
                    condition,
                    statement,
                    else_block,
                } => match else_block {
                    Some(else_node) => format!(
                        "(if ({}) {} {})",
                        self.goto_node(condition),
                        self.goto_node(statement),
                        self.goto_node(else_node)
                    ),
                    None => format!(
                        "(if ({}) {})",
                        self.goto_node(condition),
                        self.goto_node(statement)
                    ),
                },
                Statement::For {
                    initial,
                    condition,
                    after_expr,
                    statement,
                } => {
                    let initial_str = if let Some(initial_node) = initial {
                        self.goto_node(initial_node)
                    } else {
                        "()".to_owned()
                    };

                    let condition_str = if let Some(condition_node) = condition {
                        self.goto_node(condition_node)
                    } else {
                        "()".to_owned()
                    };

                    let after_expr_str = if let Some(expr_node) = after_expr {
                        self.goto_node(expr_node)
                    } else {
                        "()".to_owned()
                    };

                    format!(
                        "(for {} {} {} {})",
                        initial_str,
                        condition_str,
                        after_expr_str,
                        self.goto_node(statement)
                    )
                }
                Statement::Block { start, length } => {
                    let mut block = String::from("(block [");
                    if let Some(start) = start {
                        // Guaranteed to have start_idx
                        if let ASTNodeRef::ScopedDeclaration {
                            reference: _,
                            depth,
                        } = self.mem_arena[*start]
                        {
                            for idx in self.filter_scoped_declaration(*start, *length, depth) {
                                block.push_str(self.goto_node(&idx).as_str());
                                block.push(' ');
                            }
                            block.pop();
                            block.push(']');
                            block.push(')');
                            block
                        } else {
                            String::from("(block)")
                        }
                    } else {
                        String::from("(block)")
                    }
                }
            },
            ASTNode::Expression(expression) => match expression.node_type {
                ExprNodeType::Binary => {
                    let lhs = self.goto_node(&expression.lhs.unwrap());
                    let rhs = self.goto_node(&expression.rhs.unwrap());
                    format!("({} {lhs} {rhs})", expression.main_token.token_type)
                }
                ExprNodeType::Unary => {
                    let unary = self.goto_node(&expression.lhs.unwrap());
                    format!("({} {unary})", expression.main_token.token_type)
                }
                ExprNodeType::Grouping => {
                    format!("(group {})", self.goto_node(&expression.lhs.unwrap()))
                }
                ExprNodeType::Literal => format!("{}", expression.main_token.token_type),
                ExprNodeType::Super => {
                    format!("(super {})", self.goto_node(&expression.lhs.unwrap()))
                }
                ExprNodeType::Call => match (expression.lhs, expression.rhs) {
                    (Some(lhs), None) => format!("(call_method {})", self.goto_node(&lhs)),
                    (Some(lhs), Some(rhs)) => format!(
                        "(call_method {} {})",
                        self.goto_node(&lhs),
                        self.goto_node(&rhs)
                    ),
                    _ => todo!(),
                },
                ExprNodeType::Property => format!(
                    "(property {} {})",
                    self.goto_node(&expression.rhs.unwrap()),
                    self.goto_node(&expression.lhs.unwrap())
                ),
                ExprNodeType::Arguments => {
                    let mut args = String::from("(args");
                    let start = expression.lhs.unwrap();
                    let length = expression.rhs.unwrap();
                    // Guaranteed to have start_idx
                    for idx in &mem_arena[start..start + length] {
                        let ASTNodeRef::MethodArgs(idx) = idx else {
                            panic!("Invalid Method args!!");
                        };
                        args.push(' ');
                        args.push_str(self.goto_node(idx).as_str());
                    }
                    args.push(')');
                    args
                }
                ExprNodeType::Parameters => {
                    let mut params = String::from("(params");
                    let start = expression.lhs.unwrap();
                    let length = expression.rhs.unwrap();
                    // Guaranteed to have start_idx
                    for idx in &mem_arena[start..start + length] {
                        let ASTNodeRef::FunctionParams(idx) = idx else {
                            panic!("Invalid Function params!!");
                        };
                        params.push(' ');
                        params.push_str(self.goto_node(idx).as_str());
                    }
                    params.push(')');
                    params
                }
                ExprNodeType::Assignment => format!(
                    "(assignment {} {})",
                    self.goto_node(&expression.lhs.unwrap()),
                    self.goto_node(&expression.rhs.unwrap())
                ),
            },
        }
    }
}

impl Display for AST {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let parsed = self
            .filter_root_nodes()
            .iter()
            .map(|index| self.display(&self.nodes[*index], &self.mem_arena))
            .reduce(|acc, e| {
                if !e.is_empty() {
                    format!("{acc}\n{e}")
                } else {
                    String::from("")
                }
            })
            .unwrap_or("".to_owned());

        write!(f, "{parsed}")
    }
}
