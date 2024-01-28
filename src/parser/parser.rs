use tracing::{error, info};

use crate::{
    ast::node::{Identifier, LetStatement, Node, Program},
    lexer::Lexer,
    token::{Token, TokenType},
};

pub struct Parser {
    lexer: Lexer,
    curr_token: Option<Token>,
    peek_token: Option<Token>,

    errors: Vec<String>,
}

#[derive(Debug)]
pub struct ParserError {
    pub errors: Vec<String>,
}

impl Parser {
    pub fn new(lexer: Lexer) -> Self {
        let mut l = lexer;
        let curr_token = l.next_token();
        let peek_token = l.next_token();
        Parser {
            lexer: l,
            curr_token: Some(curr_token),
            peek_token: Some(peek_token),
            errors: Vec::new(),
        }
    }

    pub fn errors(&self) -> &Vec<String> {
        &self.errors
    }

    pub fn parse_program(&mut self) -> Result<Program, ParserError> {
        let mut program = Program::new();

        while let Some(tt) = &self.curr_token {
            info!("parse curr_token: {:?}", tt);
            match tt.token_type {
                TokenType::EOF => break,

                _ => {
                    if let Some(statement) = self.parse_statement() {
                        program.statements.push(statement);
                    }

                    self.next_token();
                }
            }
        }

        if self.errors.is_empty() {
            Ok(program)
        } else {
            Err(ParserError {
                errors: self.errors.clone(),
            })
        }
    }

    fn peek_error(&mut self, token_type: TokenType) {
        let message = format!(
            "expected next token to be {:?} instead of {:?}",
            &token_type,
            self.peek_token.clone().unwrap().token_type
        );

        error!("{}", message);
        self.errors.push(message);
    }

    fn next_token(&mut self) {
        self.curr_token = self.peek_token.take();
        self.peek_token = Some(self.lexer.next_token());
    }

    fn parse_statement(&mut self) -> Option<Node> {
        if let Some(token) = &self.curr_token {
            match token.token_type {
                TokenType::Let => {
                    info!("parse_let_statement");

                    self.parse_let_statement()
                }
                _ => None,
            }
        } else {
            None
        }
    }

    fn parse_let_statement(&mut self) -> Option<Node> {
        if let Some(token) = self.curr_token.clone() {
            if !self.expect_peak(TokenType::Ident) {
                info!("parse_let_statement: missing Ident");
                return None;
            }
            if !self.expect_peak(TokenType::Assign) {
                info!("parse_let_statement: missing Assign");
                return None;
            }
            let name = Identifier {
                token: token.clone(),
                value: token.literal.clone(),
            };

            // TODO: We're skipping expressions until we encounter a semicolon

            while !self.current_token_is(TokenType::Semicolon) {
                self.next_token();
            }

            let statement = LetStatement {
                token: token.clone(),
                name,
            };

            return Some(Node::LetStatement(statement));
        }
        None
    }

    fn current_token_is(&self, token_type: TokenType) -> bool {
        if let Some(tt) = &self.curr_token {
            if tt.token_type == token_type {
                return true;
            }
        }

        false
    }

    fn expect_peak(&mut self, token_type: TokenType) -> bool {
        if self.peek_token_is(token_type) {
            self.next_token();
            true
        } else {
            self.peek_error(token_type);
            false
        }
    }

    fn peek_token_is(&self, token_type: TokenType) -> bool {
        if let Some(tt) = &self.peek_token {
            if tt.token_type == token_type {
                return true;
            }
        }

        false
    }
}

#[cfg(test)]
mod tests {
    use crate::{ast::node::Node, lexer::Lexer};

    use super::*;
    #[test]
    fn test_let_statement() {
        tracing_subscriber::fmt().init();
        //
        let input = r#"
let x = 5;

let y = 10;

let foobar = 838383;
"#;

        let tests = &["x", "y", "foobar"];
        let lexer = Lexer::new(input.to_string());

        let mut parser = Parser::new(lexer);
        let result = parser.parse_program();
        match result {
            Ok(program) => {
                test_program_statement(program, tests);
            }
            Err(e) => {
                panic!("Error parsing program: {:?}", e);
            }
        }
    }

    fn test_program_statement(program: Program, tests: &[&str; 3]) {
        let len = program.statements.len();
        assert_eq!(
            len, 3,
            "Program.statements does not contain 3 statements. got={}",
            len
        );
        for (i, tt) in tests.iter().enumerate() {
            let statement = &program.statements[i];
            if !test_let_node(statement, tt.to_string()) {
                break;
            }
        }
    }

    fn test_let_node(statement: &Node, name: String) -> bool {
        if statement.token_literal() != "let" {
            println!(
                "statement.token_literal not 'let'. got={}",
                statement.token_literal()
            );
            return false;
        }

        if let Node::LetStatement(let_statement) = statement {
            if let_statement.name.value != name {
                println!(
                    "let_statement.name.value not '{}'. got={}",
                    name, let_statement.name.value
                );
                return false;
            }

            if let_statement.name.token_literal() != name {
                println!(
                    "let_statement.name.token_literal not '{}'. got={}",
                    name,
                    let_statement.name.token_literal()
                );
                return false;
            }
        } else {
            println!(
                "statement not LetStatement. got={}",
                statement.token_literal()
            );
            return false;
        }

        true
    }
}
