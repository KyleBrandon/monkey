use std::collections::HashMap;

use tracing::{error, info};

use crate::{
    ast::node::{
        ExpressionStatement, Identifier, InfixExpression, IntegerLiteral, LetStatement, Node,
        Precedence, PrefixExpression, Program, ReturnStatement,
    },
    lexer::Lexer,
    token::{Token, TokenType},
};

pub struct Parser {
    lexer: Lexer,
    curr_token: Option<Token>,
    peek_token: Option<Token>,

    errors: Vec<String>,

    precedence: HashMap<TokenType, Precedence>,
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
            precedence: init_precedence_lookup(),
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
                TokenType::Let => self.parse_let_statement(),
                TokenType::Return => self.parse_return_statement(),
                _ => self.parse_expression_statement(),
            }
        } else {
            None
        }
    }

    fn parse_let_statement(&mut self) -> Option<Node> {
        info!("parse_let_statement");
        if let Some(token) = self.curr_token.clone() {
            if !self.expect_peak(TokenType::Ident) {
                error!("parse_let_statement: missing Ident");
                return None;
            }
            if !self.expect_peak(TokenType::Assign) {
                error!("parse_let_statement: missing Assign");
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
                value: Box::new(Node::Empty),
            };

            return Some(Node::LetStatement(statement));
        }
        None
    }

    fn parse_return_statement(&mut self) -> Option<Node> {
        info!("parse_return_statement");
        if let Some(token) = self.curr_token.clone() {
            self.next_token();

            // TODO: We're skipping expressions until we encounter a semicolon

            while !self.current_token_is(TokenType::Semicolon) {
                self.next_token();
            }

            let statement = ReturnStatement {
                token: token.clone(),
                return_value: Box::new(Node::Empty),
            };

            return Some(Node::ReturnStatement(statement));
        }
        None
    }

    fn parse_expression_statement(&mut self) -> Option<Node> {
        info!("parse_expression_statement");
        if let Some(token) = self.curr_token.clone() {
            if let Some(expression) = self.parse_expression(Precedence::Lowest) {
                let statement = ExpressionStatement {
                    token: token.clone(),
                    expression: Box::new(expression),
                };

                if self.peek_token_is(TokenType::Semicolon) {
                    self.next_token();
                }

                return Some(Node::ExpressionStatement(statement));
            }
        }

        None
    }

    fn parse_expression(&mut self, precedence: Precedence) -> Option<Node> {
        info!("parse_expression");

        let Some(token) = self.curr_token.clone() else {
            return None;
        };

        let prefix = match token.token_type {
            TokenType::Ident => self.parse_identifier(token),
            TokenType::Int => self.parse_integer_literal(token),
            TokenType::Bang => self.parse_prefix_expression(token),
            TokenType::Minus => self.parse_prefix_expression(token),
            _ => {
                let message = format!("no prefix parse function for {:?} found", token.token_type);

                error!("{}", message);
                self.errors.push(message);
                None
            }
        };

        let Some(mut expr) = prefix else {
            return None;
        };

        while !self.peek_token_is(TokenType::Semicolon) && precedence < self.peek_precedence() {
            let Some(token) = self.peek_token.clone() else {
                return Some(expr);
            };

            self.next_token();

            let right = match token.token_type {
                TokenType::Plus => self.parse_infix_expression(expr),
                TokenType::Minus => self.parse_infix_expression(expr),
                TokenType::Slash => self.parse_infix_expression(expr),
                TokenType::Asterisk => self.parse_infix_expression(expr),
                TokenType::Equal => self.parse_infix_expression(expr),
                TokenType::NotEqual => self.parse_infix_expression(expr),
                TokenType::LT => self.parse_infix_expression(expr),
                TokenType::GT => self.parse_infix_expression(expr),
                _ => Some(expr),
            };

            let Some(right) = right else {
                return None;
            };

            expr = right;
        }

        Some(expr)
    }

    fn stuff(token: Option<String>) -> Option<u32> {
        token.map(|t| t.parse::<u32>().unwrap_or_default())
    }

    fn parse_identifier(&self, token: Token) -> Option<Node> {
        info!("parse_identifier");
        let value = token.literal.clone();
        let ident = Identifier { token, value };

        Some(Node::Identifier(ident))
    }

    fn parse_integer_literal(&self, token: Token) -> Option<Node> {
        info!("parse_integer_literal");
        if let Ok(value) = token.literal.parse::<i64>() {
            Some(Node::IntegerLiteral(IntegerLiteral { token, value }))
        } else {
            None
        }
    }

    fn parse_prefix_expression(&mut self, token: Token) -> Option<Node> {
        info!("parse_prefix_expression");
        let operator = token.literal.clone();
        self.next_token();
        self.parse_expression(Precedence::Prefix).map(|e| {
            Node::PrefixExpression(PrefixExpression {
                token,
                operator,
                right: Box::new(e),
            })
        })
    }

    fn parse_infix_expression(&mut self, left: Node) -> Option<Node> {
        if let Some(token) = self.curr_token.clone() {
            let precedence = self.curr_precedence();
            self.next_token();
            self.parse_expression(precedence).map(|right| {
                let operator = token.literal.clone();
                Node::InfixExpression(InfixExpression {
                    token,
                    operator,
                    left: Box::new(left),
                    right: Box::new(right),
                })
            })
        } else {
            None
        }
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

    fn peek_precedence(&self) -> Precedence {
        if let Some(token) = &self.peek_token {
            if let Some(precedence) = self.precedence.get(&token.token_type) {
                return *precedence;
            }
        }
        Precedence::Lowest
    }

    fn curr_precedence(&self) -> Precedence {
        if let Some(token) = &self.curr_token {
            if let Some(precedence) = self.precedence.get(&token.token_type) {
                return *precedence;
            }
        }
        Precedence::Lowest
    }
}

fn init_precedence_lookup() -> HashMap<TokenType, Precedence> {
    let mut map: HashMap<TokenType, Precedence> = HashMap::new();
    map.insert(TokenType::Equal, Precedence::Equals);
    map.insert(TokenType::NotEqual, Precedence::Equals);
    map.insert(TokenType::LT, Precedence::LessGreater);
    map.insert(TokenType::GT, Precedence::LessGreater);
    map.insert(TokenType::Plus, Precedence::Sum);
    map.insert(TokenType::Minus, Precedence::Sum);
    map.insert(TokenType::Slash, Precedence::Product);
    map.insert(TokenType::Asterisk, Precedence::Product);

    map
}

#[cfg(test)]
mod tests {

    use std::ops::Deref;

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

    #[test]
    fn test_return_statement() {
        tracing_subscriber::fmt().init();

        let input = r#"
return 5;

return 10;

return 993322;
"#;

        let lexer = Lexer::new(input.to_string());

        let mut parser = Parser::new(lexer);
        let result = parser.parse_program();
        match result {
            Ok(program) => {
                let len = program.statements.len();
                assert_eq!(
                    len, 3,
                    "Program.statements does not contain 3 statements. got={}",
                    len
                );
                for statement in program.statements.iter() {
                    if let Node::ReturnStatement(ret_statement) = statement {
                        if ret_statement.token_literal() != "let" {
                            println!(
                                "statement.token_literal not 'let'. got={}",
                                ret_statement.token_literal()
                            );
                            continue;
                        }
                    } else {
                        println!(
                            "statement not ReturnStatement. got={}",
                            statement.token_literal()
                        );
                    }
                }
            }
            Err(e) => {
                panic!("Error parsing program: {:?}", e);
            }
        }
    }

    #[test]
    fn test_identifier_expression() {
        let input = "foobar";
        let lexer = Lexer::new(input.to_string());
        let mut parser = Parser::new(lexer);
        let result = parser.parse_program();
        match result {
            Ok(program) => {
                let len = program.statements.len();
                assert_eq!(
                    len, 1,
                    "Program.statements does not contain 1 statements. got={}",
                    len
                );
                for statement in program.statements.iter() {
                    if let Node::ExpressionStatement(expr_statement) = statement {
                        let expr = expr_statement.expression.deref();
                        if let Node::Identifier(ident) = expr {
                            if ident.value != "foobar" {
                                panic!("ident.value not {}. got={}", "foobar", ident.value);
                            }
                            if ident.token_literal() != "foobar" {
                                panic!(
                                    "ident.token_literal not {}. got={}",
                                    "foobar",
                                    ident.token_literal()
                                );
                            }
                        } else {
                            panic!("expression not Identifier. got={}", expr.token_literal());
                        }
                    } else {
                        panic!(
                            "statement not ExpressionStatement. got={}",
                            statement.token_literal()
                        );
                    }
                }
            }
            Err(e) => {
                panic!("Error parsing program: {:?}", e);
            }
        }
    }

    #[test]
    fn test_integer_literal_expression() {
        tracing_subscriber::fmt().init();
        let input = "5;";
        let lexer = Lexer::new(input.to_string());
        let mut parser = Parser::new(lexer);
        let result = parser.parse_program();
        match result {
            Ok(program) => {
                let len = program.statements.len();
                assert_eq!(
                    len, 1,
                    "Program.statements does not contain 1 statements. got={}",
                    len
                );
                for statement in program.statements.iter() {
                    if let Node::ExpressionStatement(expr_statement) = statement {
                        let expr = expr_statement.expression.deref();
                        if !test_integer_literal(expr, 5) {
                            return;
                        }
                    } else {
                        panic!(
                            "statement not ExpressionStatement. got={}",
                            statement.token_literal()
                        );
                    }
                }
            }
            Err(e) => {
                panic!("Error parsing program: {:?}", e);
            }
        }
    }

    #[test]
    fn test_parsing_prefix_expression() {
        let prefix_tests = [("!5", "!", 5), ("-15", "-", 15)];

        for (input, operator, value) in prefix_tests.iter() {
            let lexer = Lexer::new(input.to_string());
            let mut parser = Parser::new(lexer);
            let result = parser.parse_program();
            match result {
                Ok(program) => {
                    let len = program.statements.len();
                    assert_eq!(
                        len, 1,
                        "Program.statements does not contain 1 statements. got={}",
                        len
                    );
                    for statement in program.statements.iter() {
                        if let Node::ExpressionStatement(expr_statement) = statement {
                            let expr = expr_statement.expression.deref();
                            if let Node::PrefixExpression(prefix_expr) = expr {
                                if prefix_expr.operator != *operator {
                                    panic!(
                                        "prefix_expr.operator not {}. got={}",
                                        operator, prefix_expr.operator
                                    );
                                }
                                if !test_integer_literal(&prefix_expr.right, *value) {
                                    return;
                                }
                            } else {
                                panic!("expression not PrefixExpression. got={:?}", statement);
                            }
                        } else {
                            panic!(
                                "statement not ExpressionStatement. got={}",
                                statement.token_literal()
                            );
                        }
                    }
                }
                Err(e) => {
                    panic!("Error parsing program: {:?}", e);
                }
            }
        }
    }

    #[test]
    fn test_parsing_infix_expression() {
        let infix_tests = [
            ("5 + 5;", 5, "+", 5),
            ("5 - 5;", 5, "-", 5),
            ("5 * 5;", 5, "*", 5),
            ("5 / 5;", 5, "/", 5),
            ("5 > 5;", 5, ">", 5),
            ("5 < 5;", 5, "<", 5),
            ("5 == 5;", 5, "==", 5),
            ("5 != 5;", 5, "!=", 5),
        ];

        for (input, left_value, operator, right_value) in infix_tests.iter() {
            let lexer = Lexer::new(input.to_string());
            let mut parser = Parser::new(lexer);
            let result = parser.parse_program();
            match result {
                Ok(program) => {
                    let len = program.statements.len();
                    assert_eq!(
                        len, 1,
                        "Program.statements does not contain 1 statements. got={}",
                        len
                    );
                    for statement in program.statements.iter() {
                        if let Node::ExpressionStatement(expr_statement) = statement {
                            let expr = expr_statement.expression.deref();
                            if let Node::InfixExpression(prefix_expr) = expr {
                                if !test_integer_literal(&prefix_expr.right, *left_value) {
                                    return;
                                }
                                if prefix_expr.operator != *operator {
                                    panic!(
                                        "infix_expr.operator not {}. got={}",
                                        operator, prefix_expr.operator
                                    );
                                }
                                if !test_integer_literal(&prefix_expr.right, *right_value) {
                                    return;
                                }
                            } else {
                                panic!("expression not InfixExpression. got={:?}", statement);
                            }
                        } else {
                            panic!(
                                "statement not ExpressionStatement. got={}",
                                statement.token_literal()
                            );
                        }
                    }
                }
                Err(e) => {
                    panic!("Error parsing program: {:?}", e);
                }
            }
        }
    }

    fn test_integer_literal(expr: &Node, value: i64) -> bool {
        if let Node::IntegerLiteral(integer_literal) = expr {
            if integer_literal.value != value {
                println!(
                    "integer_literal.value not {}. got={}",
                    value, integer_literal.value
                );
                return false;
            }
            if integer_literal.token_literal() != value.to_string() {
                println!(
                    "integer_literal.token_literal not {}. got={}",
                    value,
                    integer_literal.token_literal()
                );
                return false;
            }
        } else {
            println!(
                "expression not IntegerLiteral. got={:?}",
                expr.token_literal()
            );
            return false;
        }
        true
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
