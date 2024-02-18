use std::{collections::HashMap, rc::Rc};

use crate::{
    lexer::Lexer,
    node::{
        ArrayLiteral, BlockStatement, BooleanLiteral, CallExpression, ExpressionStatement,
        FunctionLiteral, Identifier, IfExpression, IndexExpression, InfixExpression,
        IntegerLiteral, LetStatement, Node, Precedence, PrefixExpression, Program, ReturnStatement,
        StringLiteral,
    },
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

    pub fn parse_program(&mut self) -> Result<Node, ParserError> {
        let mut program = Program::new();

        while let Some(tt) = &self.curr_token {
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
            Ok(Node::Program(program))
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
        let Some(let_token) = self.curr_token.clone() else {
            return None;
        };

        if !self.expect_peak(TokenType::Ident) {
            return None;
        }

        let Some(ident_token) = self.curr_token.clone() else {
            return None;
        };
        let name = Identifier {
            token: ident_token.clone(),
            value: ident_token.literal.clone(),
        };

        if !self.expect_peak(TokenType::Assign) {
            return None;
        }

        self.next_token();

        let Some(value) = self.parse_expression(Precedence::Lowest) else {
            return None;
        };

        if self.peek_token_is(TokenType::Semicolon) {
            self.next_token();
        }

        let statement = LetStatement {
            token: let_token.clone(),
            name,
            value: Box::new(value),
        };

        Some(Node::LetStatement(statement))
    }

    fn parse_return_statement(&mut self) -> Option<Node> {
        let Some(token) = self.curr_token.clone() else {
            return None;
        };

        self.next_token();
        let Some(ret_value) = self.parse_expression(Precedence::Lowest) else {
            return None;
        };

        if self.peek_token_is(TokenType::Semicolon) {
            self.next_token();
        }

        let statement = ReturnStatement {
            token: token.clone(),
            return_value: Box::new(ret_value),
        };

        Some(Node::ReturnStatement(statement))
    }

    fn parse_expression_statement(&mut self) -> Option<Node> {
        if let Some(token) = self.curr_token.clone() {
            if let Some(expression) = self.parse_expression(Precedence::Lowest) {
                let statement = ExpressionStatement {
                    token,
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
        // get the current token
        let Some(token) = self.curr_token.clone() else {
            return None;
        };

        let prefix = match token.token_type {
            TokenType::Ident => self.parse_identifier(token),
            TokenType::Int => self.parse_integer_literal(token),
            TokenType::Bang => self.parse_prefix_expression(token),
            TokenType::Minus => self.parse_prefix_expression(token),
            TokenType::True => self.parse_boolean_literal(token),
            TokenType::False => self.parse_boolean_literal(token),
            TokenType::LParen => self.parse_grouped_expression(),
            TokenType::If => self.parse_if_expression(token),
            TokenType::Function => self.parse_function_literal(token),
            TokenType::String => self.parse_string_literal(token),
            TokenType::LBracket => self.parse_array_literal(token),
            _ => {
                let message = format!("no prefix parse function for {:?} found", token.token_type);

                self.errors.push(message);
                None
            }
        };

        // strip the expression out of the Option
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
                TokenType::LParen => self.parse_call_expression(expr),
                TokenType::LBracket => self.parse_index_expression(expr),
                _ => Some(expr),
            };

            let Some(right) = right else {
                return None;
            };

            expr = right;
        }

        Some(expr)
    }

    fn parse_grouped_expression(&mut self) -> Option<Node> {
        self.next_token();

        let expression = self.parse_expression(Precedence::Lowest);
        if !self.expect_peak(TokenType::RParen) {
            return None;
        }

        expression
    }

    fn parse_identifier(&self, token: Token) -> Option<Node> {
        let value = token.literal.clone();
        let ident = Identifier { token, value };

        Some(Node::Identifier(ident))
    }

    fn parse_integer_literal(&self, token: Token) -> Option<Node> {
        if let Ok(value) = token.literal.parse::<i64>() {
            Some(Node::IntegerLiteral(IntegerLiteral { token, value }))
        } else {
            None
        }
    }

    fn parse_boolean_literal(&self, token: Token) -> Option<Node> {
        Some(Node::BooleanLiteral(BooleanLiteral {
            token,
            value: self.current_token_is(TokenType::True),
        }))
    }

    fn parse_string_literal(&self, token: Token) -> Option<Node> {
        Some(Node::StringLiteral(StringLiteral {
            token: token.clone(),
            value: token.literal.clone(),
        }))
    }

    fn parse_array_literal(&mut self, token: Token) -> Option<Node> {
        Some(Node::ArrayLiteral(ArrayLiteral {
            token: token.clone(),
            elements: self.parse_expression_list(TokenType::RBracket),
        }))
    }

    fn parse_expression_list(&mut self, end_token: TokenType) -> Vec<Node> {
        let mut list = Vec::new();
        if self.peek_token_is(end_token) {
            self.next_token();
            return list;
        }

        self.next_token();
        let Some(expr) = self.parse_expression(Precedence::Lowest) else {
            panic!("Failed to parse expression");
        };

        list.push(expr);

        while self.peek_token_is(TokenType::Comma) {
            self.next_token();
            self.next_token();
            let Some(expr) = self.parse_expression(Precedence::Lowest) else {
                panic!("Failed to parse expression");
            };
            list.push(expr);
        }

        if !self.expect_peak(end_token) {
            panic!("Failed to parse expression");
        }

        list
    }

    fn parse_prefix_expression(&mut self, token: Token) -> Option<Node> {
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
        let Some(token) = self.curr_token.clone() else {
            return None;
        };

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
    }

    fn parse_if_expression(&mut self, token: Token) -> Option<Node> {
        let token = token.clone();

        if !self.expect_peak(TokenType::LParen) {
            return None;
        }

        self.next_token();

        let Some(condition) = self.parse_expression(Precedence::Lowest) else {
            return None;
        };

        if !self.expect_peak(TokenType::RParen) {
            return None;
        }

        let Some(consequence) = self.parse_block_statement() else {
            return None;
        };

        let alternative = if self.peek_token_is(TokenType::Else) {
            self.next_token();
            self.parse_block_statement()
        } else {
            None
        };

        Some(Node::IfExpression(IfExpression {
            token,
            condition: Box::new(condition),
            consequence: Box::new(consequence),
            alternative: alternative.map(Box::new),
        }))
    }

    fn parse_block_statement(&mut self) -> Option<Node> {
        if !self.expect_peak(TokenType::LBrace) {
            return None;
        }

        let Some(token) = self.curr_token.clone() else {
            panic!("Expected token in block statement");
        };

        self.next_token();

        let mut statements = Vec::new();

        while !self.current_token_is(TokenType::RBrace) && !self.current_token_is(TokenType::EOF) {
            if let Some(statement) = self.parse_statement() {
                statements.push(statement);
            }

            self.next_token();
        }

        Some(Node::BlockStatement(BlockStatement { token, statements }))
    }

    fn parse_function_literal(&mut self, token: Token) -> Option<Node> {
        let token = token.clone();

        let Some(parameters) = self.parse_function_parameters() else {
            return None;
        };

        let Some(body) = self.parse_block_statement() else {
            return None;
        };

        Some(Node::FunctionLiteral(FunctionLiteral {
            token,
            parameters: Rc::new(parameters),
            body: Rc::new(body),
        }))
    }

    fn parse_function_parameters(&mut self) -> Option<Vec<Identifier>> {
        let mut identifiers = Vec::new();

        if !self.expect_peak(TokenType::LParen) {
            return None;
        }

        if self.peek_token_is(TokenType::RParen) {
            self.next_token();
            return Some(identifiers);
        }

        self.next_token();

        let Some(token) = self.curr_token.clone() else {
            return None;
        };

        let identifier = Identifier {
            token: token.clone(),
            value: token.literal.clone(),
        };

        identifiers.push(identifier);

        while self.peek_token_is(TokenType::Comma) {
            self.next_token();
            self.next_token();

            let Some(token) = self.curr_token.clone() else {
                return None;
            };

            let identifier = Identifier {
                token: token.clone(),
                value: token.literal.clone(),
            };
            identifiers.push(identifier);
        }

        if !self.expect_peak(TokenType::RParen) {
            return None;
        }

        Some(identifiers)
    }

    fn parse_call_expression(&mut self, function: Node) -> Option<Node> {
        let Some(token) = self.curr_token.clone() else {
            return None;
        };

        Some(Node::CallExpression(CallExpression {
            token,
            function: Box::new(function),
            arguments: self.parse_expression_list(TokenType::RParen),
        }))
    }

    fn parse_index_expression(&mut self, expr: Node) -> Option<Node> {
        let Some(token) = self.curr_token.clone() else {
            return None;
        };
        self.next_token();
        let Some(index) = self.parse_expression(Precedence::Lowest) else {
            return None;
        };
        if !self.expect_peak(TokenType::RBracket) {
            return None;
        }
        Some(Node::IndexExpression(IndexExpression {
            token,
            left: Box::new(expr),
            index: Box::new(index),
        }))
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
    map.insert(TokenType::LParen, Precedence::Call);
    map.insert(TokenType::LBracket, Precedence::Index);

    map
}

#[cfg(test)]
mod tests {

    use std::ops::Deref;

    use crate::{
        lexer::Lexer,
        node::{IfExpression, Node},
    };

    use super::*;
    #[test]
    fn test_let_statement() {
        //
        let tests = [
            ("let x = 5;", "x", "5"),
            ("let y = true;", "y", "true"),
            ("let foobar = y;", "foobar", "y"),
        ];

        for tt in tests {
            let lexer = Lexer::new(tt.0.to_string());

            let mut parser = Parser::new(lexer);
            let result = parser.parse_program();
            match result {
                Ok(program) => {
                    let Node::Program(program) = program else {
                        panic!("program is not Program. got={:?}", program);
                    };

                    if program.statements.len() != 1 {
                        panic!(
                            "program.statements does not contain 1 statement. got={:?}",
                            program.statements.len()
                        );
                    }

                    let statement = &program.statements[0];

                    if !test_let_node(statement, tt.1.to_string()) {
                        return;
                    }

                    if !test_literal_expression(statement, tt.2) {
                        return;
                    }
                }
                Err(e) => {
                    panic!("Error parsing program: {:?}", e);
                }
            }
        }
    }

    #[test]
    fn test_return_statement() {
        let tests = [
            ("return 5;", "5"),
            ("return true;", "true"),
            ("return foobar;", "foobar"),
        ];

        for tt in tests {
            let lexer = Lexer::new(tt.0.to_string());

            let mut parser = Parser::new(lexer);
            let result = parser.parse_program();
            match result {
                Ok(program) => {
                    let Node::Program(program) = program else {
                        panic!("program is not Program. got={:?}", program);
                    };

                    let len = program.statements.len();
                    assert_eq!(
                        len, 1,
                        "Program.statements does not contain 3 statements. got={}",
                        len
                    );
                    for statement in program.statements.iter() {
                        let Node::ReturnStatement(ret_statement) = statement else {
                            panic!(
                                "statement not ReturnStatement. got={}",
                                statement.token_literal()
                            );
                        };

                        if ret_statement.token_literal() != "return" {
                            panic!(
                                "statement.token_literal not 'let'. got={}",
                                ret_statement.token_literal()
                            );
                        }

                        if !test_literal_expression(ret_statement.return_value.deref(), tt.1) {
                            //
                            return;
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
    fn test_identifier_expression() {
        let input = "foobar";
        let lexer = Lexer::new(input.to_string());
        let mut parser = Parser::new(lexer);
        let result = parser.parse_program();
        match result {
            Ok(program) => {
                let Node::Program(program) = program else {
                    panic!("program is not Program. got={:?}", program);
                };

                let len = program.statements.len();
                assert_eq!(
                    len, 1,
                    "Program.statements does not contain 1 statements. got={}",
                    len
                );
                for statement in program.statements.iter() {
                    let Node::ExpressionStatement(expr_statement) = statement else {
                        panic!(
                            "statement not ExpressionStatement. got={}",
                            statement.token_literal()
                        );
                    };

                    let expr = expr_statement.expression.deref();
                    let Node::Identifier(ident) = expr else {
                        panic!("expression not Identifier. got={}", expr.token_literal());
                    };

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
                }
            }
            Err(e) => {
                panic!("Error parsing program: {:?}", e);
            }
        }
    }

    #[test]
    fn test_integer_literal_expression() {
        let input = "5;";
        let lexer = Lexer::new(input.to_string());
        let mut parser = Parser::new(lexer);
        let result = parser.parse_program();
        match result {
            Ok(program) => {
                let Node::Program(program) = program else {
                    panic!("program is not Program. got={:?}", program);
                };

                let len = program.statements.len();
                assert_eq!(
                    len, 1,
                    "Program.statements does not contain 1 statements. got={}",
                    len
                );
                for statement in program.statements.iter() {
                    if let Node::ExpressionStatement(expr_statement) = statement {
                        let expr = expr_statement.expression.deref();
                        if !test_integer_literal(expr, "5") {
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
        let prefix_tests = [
            ("!5", "!", "5"),
            ("-15", "-", "15"),
            ("!true;", "!", "true"),
            ("!false;", "!", "false"),
        ];

        for (input, operator, value) in prefix_tests.iter() {
            let lexer = Lexer::new(input.to_string());
            let mut parser = Parser::new(lexer);
            let result = parser.parse_program();
            match result {
                Ok(program) => {
                    let Node::Program(program) = program else {
                        panic!("program is not Program. got={:?}", program);
                    };

                    let len = program.statements.len();
                    assert_eq!(
                        len, 1,
                        "Program.statements does not contain 1 statements. got={}",
                        len
                    );
                    for statement in program.statements.iter() {
                        let Node::ExpressionStatement(expr_statement) = statement else {
                            panic!(
                                "statement not ExpressionStatement. got={}",
                                statement.token_literal()
                            );
                        };

                        let Node::PrefixExpression(prefix_expr) = expr_statement.expression.deref()
                        else {
                            panic!("expression not PrefixExpression. got={:?}", statement);
                        };

                        if prefix_expr.operator != *operator {
                            panic!(
                                "prefix_expr.operator not {}. got={}",
                                operator, prefix_expr.operator
                            );
                        }

                        if !test_integer_literal(&prefix_expr.right, value) {
                            return;
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
            ("5 + 5;", "5", "+", "5"),
            ("5 - 5;", "5", "-", "5"),
            ("5 * 5;", "5", "*", "5"),
            ("5 / 5;", "5", "/", "5"),
            ("5 > 5;", "5", ">", "5"),
            ("5 < 5;", "5", "<", "5"),
            ("5 == 5;", "5", "==", "5"),
            ("5 != 5;", "5", "!=", "5"),
            ("true == true", "true", "==", "true"),
            ("true != false", "true", "!=", "false"),
            ("false == false", "false", "==", "false"),
            // ("3 + 4 * 5;", 3, "+", (4 * 5)),
            // ("3 * 1 + 4 * 5;", (3 * 1), "+", (4 * 5)
        ];

        for (input, left_value, operator, right_value) in infix_tests.iter() {
            let lexer = Lexer::new(input.to_string());
            let mut parser = Parser::new(lexer);
            let result = parser.parse_program();
            match result {
                Ok(program) => {
                    let Node::Program(program) = program else {
                        panic!("program is not Program. got={:?}", program);
                    };

                    let len = program.statements.len();
                    assert_eq!(
                        len, 1,
                        "Program.statements does not contain 1 statements. got={}",
                        len
                    );
                    for statement in program.statements.iter() {
                        let Node::ExpressionStatement(expr_statement) = statement else {
                            panic!(
                                "statement not ExpressionStatement. got={}",
                                statement.token_literal()
                            );
                        };

                        let expr = expr_statement.expression.deref();
                        if !test_infix_expression(expr, left_value, operator, right_value) {
                            return;
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
    fn test_operator_precedence() {
        let tests = [
            ("-a * b", "((-a) * b)"),
            ("!-a", "(!(-a))"),
            ("a + b + c", "((a + b) + c)"),
            ("a + b - c", "((a + b) - c)"),
            ("a * b * c", "((a * b) * c)"),
            ("a * b / c", "((a * b) / c)"),
            ("a + b / c", "(a + (b / c))"),
            ("a + b * c + d / e - f", "(((a + (b * c)) + (d / e)) - f)"),
            ("3 + 4; -5 * 5", "(3 + 4)((-5) * 5)"),
            ("5 > 4 == 3 < 4", "((5 > 4) == (3 < 4))"),
            ("5 < 4 != 3 > 4", "((5 < 4) != (3 > 4))"),
            (
                "3 + 4 * 5 == 3 * 1 + 4 * 5",
                "((3 + (4 * 5)) == ((3 * 1) + (4 * 5)))",
            ),
            ("true", "true"),
            ("false", "false"),
            ("3 > 5 == false", "((3 > 5) == false)"),
            ("3 < 5 == true", "((3 < 5) == true)"),
            ("1 + (2 + 3) + 4", "((1 + (2 + 3)) + 4)"),
            ("(5 + 5) * 2", "((5 + 5) * 2)"),
            ("2 / (5 + 5)", "(2 / (5 + 5))"),
            ("-(5 + 5)", "(-(5 + 5))"),
            ("!(true == true)", "(!(true == true))"),
            ("a + add(b * c) + d", "((a + add((b * c))) + d)"),
            (
                "add(a, b, 1, 2 * 3, 4 + 5, add(6, 7 * 8))",
                "add(a, b, 1, (2 * 3), (4 + 5), add(6, (7 * 8)))",
            ),
            (
                "add(a + b + c * d / f + g)",
                "add((((a + b) + ((c * d) / f)) + g))",
            ),
            (
                "a * [1, 2, 3, 4][b * c] * d",
                "((a * ([1, 2, 3, 4][(b * c)])) * d)",
            ),
            (
                "add(a * b[2], b[1], 2 * [1, 2][1])",
                "add((a * (b[2])), (b[1]), (2 * ([1, 2][1])))",
            ),
        ];
        for tt in tests {
            let lexer = Lexer::new(tt.0.to_string());
            let mut parser = Parser::new(lexer);
            let result = parser.parse_program();
            match result {
                Ok(program) => {
                    //
                    assert_eq!(tt.1, program.string());
                }
                Err(e) => {
                    //
                    panic!("Error parsing program: {:?}", e);
                }
            }
        }
    }

    #[test]
    fn test_if_expression() {
        let input = "if (x < y) { x }";

        let lexer = Lexer::new(input.to_string());
        let mut parser = Parser::new(lexer);
        let result = parser.parse_program();
        match result {
            Ok(program) => {
                let Node::Program(program) = program else {
                    panic!("program is not Program. got={:?}", program);
                };

                let len = program.statements.len();
                assert_eq!(
                    len, 1,
                    "Program.statements does not contain 1 statements. got={}",
                    len
                );
                for statement in program.statements.iter() {
                    let Node::ExpressionStatement(expr_statement) = statement else {
                        panic!(
                            "statement not IfExpression. got={}",
                            statement.token_literal()
                        );
                    };
                    let Node::IfExpression(expr) = expr_statement.expression.deref() else {
                        panic!(
                            "statement not IfExpression. got={}",
                            expr_statement.token_literal()
                        );
                    };

                    if !test_if_expr(expr) {
                        return;
                    }

                    if let Some(alternative) = &expr.alternative {
                        panic!(
                            "expr.alternative.statements as not None. got={:?}",
                            alternative
                        );
                    }
                }
            }
            Err(e) => panic!("Error parsing program: {:?}", e),
        }
    }

    #[test]
    fn test_if_else_expression() {
        let input = "if (x < y) { x } else { y }";

        let lexer = Lexer::new(input.to_string());
        let mut parser = Parser::new(lexer);
        let result = parser.parse_program();
        match result {
            Ok(program) => {
                let Node::Program(program) = program else {
                    panic!("program is not Program. got={:?}", program);
                };

                let len = program.statements.len();
                assert_eq!(
                    len, 1,
                    "Program.statements does not contain 1 statements. got={}",
                    len
                );
                for statement in program.statements.iter() {
                    let Node::ExpressionStatement(expr_statement) = statement else {
                        panic!(
                            "statement not IfExpression. got={}",
                            statement.token_literal()
                        );
                    };
                    let Node::IfExpression(expr) = expr_statement.expression.deref() else {
                        panic!(
                            "statement not IfExpression. got={}",
                            expr_statement.token_literal()
                        );
                    };

                    if !test_if_expr(expr) {
                        return;
                    }

                    if let Some(boxed_alternative) = &expr.alternative {
                        if let Node::BlockStatement(alternative) = boxed_alternative.deref() {
                            if alternative.statements.len() != 1 {
                                panic!(
                                "exp.alternative.statements does not contain 1 statements. got={}",
                                alternative.statements.len()
                            );
                            }
                            let Node::ExpressionStatement(alt) = &alternative.statements[0] else {
                                panic!(
                                    "Statements[0] is not ExpressionStatement. got={:?}",
                                    alternative.statements[0]
                                );
                            };

                            if !test_identifier(alt.expression.deref(), "y") {
                                return;
                            }
                        };
                    }
                }
            }
            Err(e) => panic!("Error parsing program: {:?}", e),
        }
    }

    fn test_if_expr(expr: &IfExpression) -> bool {
        if !test_infix_expression(expr.condition.deref(), "x", "<", "y") {
            return false;
        }

        if let Node::BlockStatement(consequence) = expr.consequence.deref() {
            if consequence.statements.len() != 1 {
                panic!(
                    "consequence is not 1 statement. got={}",
                    consequence.statements.len()
                );
            }

            let Node::ExpressionStatement(consequence) = &consequence.statements[0] else {
                panic!(
                    "Statements is not ExpressionStatement. got={:?}",
                    consequence.statements[0]
                );
            };

            if !test_identifier(consequence.expression.deref(), "x") {
                return false;
            }
        };

        true
    }

    #[test]
    fn test_function_literal_parsing() {
        let input = "fn(x, y) { x + y; }";

        let lexer = Lexer::new(input.to_string());
        let mut parser = Parser::new(lexer);
        let result = parser.parse_program();
        match result {
            Ok(program) => {
                let Node::Program(program) = program else {
                    panic!("program is not Program. got={:?}", program);
                };

                //
                let len = program.statements.len();
                assert_eq!(
                    len, 1,
                    "Program.statements does not contain 1 statements. got={}",
                    len
                );
                for statement in program.statements.iter() {
                    let Node::ExpressionStatement(expr_statement) = statement else {
                        panic!(
                            "statement not IfExpression. got={}",
                            statement.token_literal()
                        );
                    };
                    let Node::FunctionLiteral(function) = expr_statement.expression.deref() else {
                        panic!(
                            "statement not IfExpression. got={}",
                            expr_statement.token_literal()
                        );
                    };

                    if function.parameters.len() != 2 {
                        panic!(
                            "function literal parameters expected 2. got={}",
                            function.parameters.len()
                        );
                    }

                    test_literal_identifier(&function.parameters[0], "x");
                    test_literal_identifier(&function.parameters[0], "y");

                    if let Node::BlockStatement(body) = function.body.deref() {
                        if body.statements.len() != 1 {
                            panic!(
                                "function.body.statements has not 1 statement. got={}",
                                body.statements.len()
                            );
                        }

                        test_infix_expression(&body.statements[0], "x", "+", "y");
                    };
                }
            }
            Err(e) => panic!("Error parsing program: {:?}", e),
        }
    }

    #[test]
    fn test_function_parameter_parsing() {
        let tests = [
            ("fn() {};", vec![]),
            ("fn(x) {};", vec!["x"]),
            ("fn(x, y, z) {};", vec!["x", "y", "z"]),
        ];

        for tt in tests {
            let lexer = Lexer::new(tt.0.to_string());
            let mut parser = Parser::new(lexer);
            let result = parser.parse_program();
            match result {
                Ok(program) => {
                    let Node::Program(program) = program else {
                        panic!("program is not Program. got={:?}", program);
                    };

                    let Node::ExpressionStatement(expr_statement) = &program.statements[0] else {
                        panic!("expected expression. got={:?}", program.statements[0]);
                    };

                    let Node::FunctionLiteral(function) = expr_statement.expression.deref() else {
                        panic!(
                            "expected function literal. got={:?}",
                            expr_statement.expression
                        );
                    };

                    if function.parameters.len() != tt.1.len() {
                        panic!(
                            "length of parameters wrong for test {}. expected {:?}. got={:?}",
                            tt.0,
                            tt.1.len(),
                            function.parameters.len()
                        );
                    }

                    tt.1.iter().enumerate().for_each(|(ix, ident)| {
                        test_literal_identifier(&function.parameters[ix], ident);
                    })
                }
                Err(e) => panic!("Failed to parse program: {:?}", e),
            }
        }
    }

    #[test]
    fn test_call_expression_parsing() {
        let input = "add(1, 2 * 3, 4 + 5);";
        let lexer = Lexer::new(input.to_string());
        let mut parser = Parser::new(lexer);
        let result = parser.parse_program();
        match result {
            Ok(program) => {
                let Node::Program(program) = program else {
                    panic!("program is not Program. got={:?}", program);
                };

                //
                if program.statements.len() != 1 {
                    panic!(
                        "program.statements does not contain 1 statments. got={}",
                        program.statements.len()
                    );
                }

                let Node::ExpressionStatement(expr_statement) = &program.statements[0] else {
                    panic!(
                        "statement ir not ExpressionStatement. got={:?}",
                        program.statements[0]
                    );
                };

                let Node::CallExpression(call_expression) = &expr_statement.expression.deref()
                else {
                    panic!(
                        "expression is not a CallExpression. got={:?}",
                        expr_statement.expression.deref()
                    );
                };

                if !test_identifier(call_expression.function.deref(), "add") {
                    panic!(
                        "CallExpression function is not 'add'. got={:?}",
                        call_expression.function.deref()
                    );
                }

                if call_expression.arguments.len() != 3 {
                    panic!(
                        "wrong number of arguments. got={:?}",
                        call_expression.arguments.len()
                    );
                }

                test_literal_expression(&call_expression.arguments[0], "1");
                test_infix_expression(&call_expression.arguments[1], "2", "*", "3");
                test_infix_expression(&call_expression.arguments[1], "4", "+", "5");
            }
            Err(e) => panic!("Failed to parse program: {:?}", e),
        }
    }

    #[test]
    fn test_string_literal_expression() {
        let input = r#""hello world";"#;

        let lexer = Lexer::new(input.to_string());
        let mut parser = Parser::new(lexer);
        let result = parser.parse_program();
        match result {
            Ok(program) => {
                let Node::Program(program) = program else {
                    panic!("program is not Program. got={:?}", program);
                };

                if program.statements.len() != 1 {
                    panic!(
                        "program.statements does not contain 1 statments. got={}",
                        program.statements.len()
                    );
                }

                let Node::ExpressionStatement(expr_statement) = &program.statements[0] else {
                    panic!(
                        "statement ir not ExpressionStatement. got={:?}",
                        program.statements[0]
                    );
                };

                let Node::StringLiteral(string_literal) = &expr_statement.expression.deref() else {
                    panic!(
                        "expression is not a StringLiteral. got={:?}",
                        expr_statement.expression.deref()
                    );
                };
                if string_literal.value != "hello world" {
                    panic!(
                        "string_literal.value is not 'hello world'. got={:?}",
                        string_literal.value
                    );
                }
            }
            Err(e) => panic!("Failed to parse program: {:?}", e),
        }
    }

    #[test]
    fn test_parsing_array_literals() {
        let input = "[1, 2 * 2, 3 + 3]";
        let lexer = Lexer::new(input.to_string());
        let mut parser = Parser::new(lexer);
        let result = parser.parse_program();
        match result {
            Ok(program) => {
                let Node::Program(program) = program else {
                    panic!("program is not Program. got={:?}", program);
                };
                if program.statements.len() != 1 {
                    panic!(
                        "program.statements does not contain 1 statments. got={}",
                        program.statements.len()
                    );
                }
                let Node::ExpressionStatement(expr_statement) = &program.statements[0] else {
                    panic!(
                        "statement ir not ExpressionStatement. got={:?}",
                        program.statements[0]
                    );
                };
                let Node::ArrayLiteral(array_literal) = &expr_statement.expression.deref() else {
                    panic!(
                        "expression is not a ArrayLiteral. got={:?}",
                        expr_statement.expression.deref()
                    );
                };
                if array_literal.elements.len() != 3 {
                    panic!(
                        "array_literal.elements has not 3 elements. got={}",
                        array_literal.elements.len()
                    );
                }
                test_integer_literal(&array_literal.elements[0], "1");
                test_infix_expression(&array_literal.elements[1], "2", "*", "2");
                test_infix_expression(&array_literal.elements[2], "3", "+", "3");
            }
            Err(e) => {
                panic!("Error parsing program: {:?}", e);
            }
        }
    }

    #[test]
    fn test_parsing_index_expression() {
        let input = "myArray[1 + 1]";
        let lexer = Lexer::new(input.to_string());
        let mut parser = Parser::new(lexer);
        let result = parser.parse_program();
        match result {
            Ok(program) => {
                let Node::Program(program) = program else {
                    panic!("program is not Program. got={:?}", program);
                };
                let Node::ExpressionStatement(expr_statement) = &program.statements[0] else {
                    panic!(
                        "statement ir not ExpressionStatement. got={:?}",
                        program.statements[0]
                    );
                };
                let Node::IndexExpression(index_expr) = &expr_statement.expression.deref() else {
                    panic!(
                        "expression is not a IndexExpression. got={:?}",
                        expr_statement.expression.deref()
                    );
                };
                test_identifier(index_expr.left.deref(), "myArray");
                test_infix_expression(index_expr.index.deref(), "1", "+", "1");
            }
            Err(e) => {
                panic!("Error parsing program: {:?}", e);
            }
        }
    }

    fn test_literal_expression(expr: &Node, expected: &str) -> bool {
        match expr {
            Node::IntegerLiteral(_) => test_integer_literal(expr, expected),
            Node::Identifier(_) => test_identifier(expr, expected),
            Node::BooleanLiteral(_) => {
                test_boolean_literal(expr, expected.parse::<bool>().unwrap())
            }
            _ => {
                println!(
                    "expression not IntegerLiteral. got={:?}",
                    expr.token_literal()
                );
                false
            }
        }
    }

    fn test_boolean_literal(expr: &Node, value: bool) -> bool {
        let Node::BooleanLiteral(boolean_literal) = expr else {
            println!(
                "expression not BooleanLiteral. got={:?}",
                expr.token_literal()
            );
            return false;
        };
        if boolean_literal.value != value {
            println!(
                "boolean_literal.value not {}. got={}",
                value, boolean_literal.value
            );
            return false;
        }
        if boolean_literal.token_literal() != value.to_string() {
            println!(
                "boolean_literal.token_literal not {}. got={}",
                value,
                boolean_literal.token_literal()
            );
            return false;
        }
        true
    }

    fn test_infix_expression(expr: &Node, left: &str, operator: &str, right: &str) -> bool {
        let Node::InfixExpression(infix_expr) = expr else {
            println!(
                "expression not InfixExpression. got={:?}",
                expr.token_literal()
            );
            return false;
        };
        if !test_literal_expression(&infix_expr.left, left) {
            return false;
        }
        if infix_expr.operator != operator {
            println!(
                "infix_expr.operator is not {}. got={}",
                operator, infix_expr.operator
            );
            return false;
        }
        if !test_literal_expression(&infix_expr.right, right) {
            return false;
        }
        true
    }

    fn test_integer_literal(expr: &Node, value: &str) -> bool {
        let Node::IntegerLiteral(integer_literal) = expr else {
            println!(
                "expression not IntegerLiteral. got={:?}",
                expr.token_literal()
            );
            return false;
        };

        if integer_literal.value.to_string() != value {
            println!(
                "integer_literal.value not {}. got={}",
                value, integer_literal.value
            );
            return false;
        }

        if integer_literal.token_literal() != value {
            println!(
                "integer_literal.token_literal not {}. got={}",
                value,
                integer_literal.token_literal()
            );
            return false;
        }

        true
    }

    fn test_identifier(expr: &Node, value: &str) -> bool {
        let Node::Identifier(identifier) = expr else {
            println!("expression not Identifier. got={:?}", expr.token_literal());
            return false;
        };

        test_literal_identifier(identifier, value)
    }

    fn test_literal_identifier(identifier: &Identifier, value: &str) -> bool {
        if identifier.value != value {
            println!("identifier.value not {}. got={}", value, identifier.value);
            return false;
        }

        if identifier.token_literal() != value {
            println!(
                "identifier.token_literal not {}. got={}",
                value,
                identifier.token_literal()
            );
            return false;
        }
        true
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
