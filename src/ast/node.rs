use crate::token::Token;

#[derive(Debug, PartialEq, Copy, Clone, PartialOrd, Eq, Hash)]
pub enum Precedence {
    Lowest = 0,
    Equals = 1,      // ==
    LessGreater = 2, // > or <
    Sum = 3,         // +
    Product = 4,     // *
    Prefix = 5,      // -X or !X
    Call = 6,        // myFunction(X)
}

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
    Empty,
    Program(Program),
    LetStatement(LetStatement),
    Identifier(Identifier),
    ReturnStatement(ReturnStatement),
    ExpressionStatement(ExpressionStatement),
    IntegerLiteral(IntegerLiteral),
    PrefixExpression(PrefixExpression),
    InfixExpression(InfixExpression),
}

impl Node {
    pub fn token_literal(&self) -> String {
        match self {
            Node::Empty => "".to_string(),
            Node::Program(program) => program.token_literal(),
            Node::LetStatement(let_statement) => let_statement.token_literal(),
            Node::Identifier(identifier) => identifier.token_literal(),
            Node::ReturnStatement(statement) => statement.token_literal(),
            Node::ExpressionStatement(statement) => statement.token_literal(),
            Node::IntegerLiteral(statement) => statement.token_literal(),
            Node::PrefixExpression(statement) => statement.token_literal(),
            Node::InfixExpression(statement) => statement.token_literal(),
        }
    }

    pub fn string(&self) -> String {
        match self {
            Node::Empty => "".to_string(),
            Node::Program(program) => program.string(),
            Node::LetStatement(let_statement) => let_statement.string(),
            Node::Identifier(identifier) => identifier.string(),
            Node::ReturnStatement(statement) => statement.string(),
            Node::ExpressionStatement(statement) => statement.string(),
            Node::IntegerLiteral(statement) => statement.string(),
            Node::PrefixExpression(statement) => statement.string(),
            Node::InfixExpression(statement) => statement.string(),
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

    pub fn string(&self) -> String {
        let mut buffer = String::new();

        self.statements.iter().fold(&mut buffer, |acc, s| {
            acc.push_str(&s.string());

            acc
        });

        buffer
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

    pub fn string(&self) -> String {
        self.value.clone()
    }
}

#[derive(Debug)]
pub struct LetStatement {
    pub name: Identifier,
    pub value: Box<Node>,
    pub token: Token,
}

impl LetStatement {
    pub fn token_literal(&self) -> String {
        self.token.literal.clone()
    }

    pub fn string(&self) -> String {
        format!(
            "{} {} = {};",
            self.token_literal(),
            self.name.string(),
            self.value.string()
        )
    }
}

#[derive(Debug)]
pub struct ReturnStatement {
    pub token: Token,
    pub return_value: Box<Node>,
}

impl ReturnStatement {
    pub fn token_literal(&self) -> String {
        self.token.literal.clone()
    }

    pub fn string(&self) -> String {
        format!("{} {};", self.token_literal(), self.return_value.string())
    }
}

#[derive(Debug)]
pub struct ExpressionStatement {
    pub token: Token,
    pub expression: Box<Node>,
}

impl ExpressionStatement {
    pub fn token_literal(&self) -> String {
        self.token.literal.clone()
    }

    pub fn string(&self) -> String {
        self.expression.string()
    }
}

#[derive(Debug)]
pub struct IntegerLiteral {
    pub token: Token,
    pub value: i64,
}

impl IntegerLiteral {
    pub fn token_literal(&self) -> String {
        self.token.literal.clone()
    }
    pub fn string(&self) -> String {
        self.token.literal.clone()
    }
}

#[derive(Debug)]
pub struct PrefixExpression {
    pub token: Token,
    pub operator: String,
    pub right: Box<Node>,
}

impl PrefixExpression {
    pub fn token_literal(&self) -> String {
        self.token.literal.clone()
    }
    pub fn string(&self) -> String {
        format!("({} {})", self.operator, self.right.string())
    }
}

#[derive(Debug)]
pub struct InfixExpression {
    pub token: Token,
    pub left: Box<Node>,
    pub operator: String,
    pub right: Box<Node>,
}

impl InfixExpression {
    pub fn token_literal(&self) -> String {
        self.token.literal.clone()
    }
    pub fn string(&self) -> String {
        format!(
            "({} {} {})",
            self.left.string(),
            self.operator,
            self.right.string()
        )
    }
}

#[cfg(test)]
mod tests {
    use crate::{ast::node::Node, token::TokenType};

    use super::*;
    #[test]
    fn test_string() {
        tracing_subscriber::fmt().init();
        let program = Program {
            statements: vec![Node::LetStatement(LetStatement {
                token: Token {
                    token_type: TokenType::Let,
                    literal: "let".to_string(),
                },
                name: Identifier {
                    token: Token {
                        token_type: TokenType::Ident,
                        literal: "myVar".to_string(),
                    },
                    value: "myVar".to_string(),
                },
                value: Box::new(Node::Identifier(Identifier {
                    token: Token {
                        token_type: TokenType::Ident,
                        literal: "anotherVar".to_string(),
                    },
                    value: "anotherVar".to_string(),
                })),
            })],
        };

        assert_eq!("let myVar = anotherVar;".to_string(), program.string());
    }
}
