use crate::token::Token;

/// NodeType
///
#[derive(Debug)]
pub enum NodeType {
    Statement,
    Expression,
    Identifier,
}

/// Node
///
#[derive(Debug)]
pub enum Node {
    Program(Program),
    LetStatement(LetStatement),
    Identifier(Identifier),
}

impl Node {
    pub fn token_literal(&self) -> String {
        match self {
            Node::Program(program) => program.token_literal(),
            Node::LetStatement(let_statement) => let_statement.token_literal(),
            Node::Identifier(identifier) => identifier.token_literal(),
        }
    }
}

#[derive(Debug)]
pub struct Program {
    pub statements: Vec<Node>,
}

impl Program {
    pub fn new() -> Self {
        Program {
            statements: Vec::new(),
        }
    }
    pub fn token_literal(&self) -> String {
        if let Some(statement) = self.statements.first() {
            statement.token_literal()
        } else {
            "".to_string()
        }
    }
}

#[derive(Debug)]
pub struct Identifier {
    pub token: Token,
    pub value: String,
}

impl Identifier {
    pub fn token_literal(&self) -> String {
        self.token.literal.clone()
    }
}

#[derive(Debug)]
pub struct LetStatement {
    pub name: Identifier,
    // pub value: Box<Node>,
    pub token: Token,
}

impl LetStatement {
    pub fn token_literal(&self) -> String {
        self.token.literal.clone()
    }
}
