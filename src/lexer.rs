use crate::token::{Token, TokenType};

pub struct Lexer {
    input: String,
    position: usize,
    read_position: usize,
    ch: Option<char>,
}

impl Lexer {
    pub fn new(input: String) -> Self {
        let mut lexer = Lexer {
            input,
            position: 0,
            read_position: 0,
            ch: None,
        };

        lexer.read_char();

        lexer
    }

    pub fn next_token(&mut self) -> Token {
        self.skip_whitespace();

        let token = if let Some(tok) = self.ch {
            match tok {
                '=' => {
                    if self.peek_char() == Some('=') {
                        //
                        let current_char = tok;
                        self.read_char();
                        let literal = format!("{}{}", current_char, self.ch.unwrap());
                        self.read_char();
                        Token::new(TokenType::Equal, literal)
                    } else {
                        self.read_char();
                        Token::new(TokenType::Assign, tok.to_string())
                    }
                }
                '+' => {
                    self.read_char();
                    Token::new(TokenType::Plus, tok.to_string())
                }
                '-' => {
                    self.read_char();
                    Token::new(TokenType::Minus, tok.to_string())
                }
                '!' => {
                    if self.peek_char() == Some('=') {
                        //
                        let current_char = tok;
                        self.read_char();
                        let literal = format!("{}{}", current_char, self.ch.unwrap());
                        self.read_char();
                        Token::new(TokenType::NotEqual, literal)
                    } else {
                        self.read_char();
                        Token::new(TokenType::Bang, tok.to_string())
                    }
                }
                '/' => {
                    self.read_char();
                    Token::new(TokenType::Slash, tok.to_string())
                }
                '*' => {
                    self.read_char();
                    Token::new(TokenType::Asterisk, tok.to_string())
                }
                '<' => {
                    self.read_char();
                    Token::new(TokenType::LT, tok.to_string())
                }
                '>' => {
                    self.read_char();
                    Token::new(TokenType::GT, tok.to_string())
                }
                '(' => {
                    self.read_char();
                    Token::new(TokenType::LParen, tok.to_string())
                }
                ')' => {
                    self.read_char();
                    Token::new(TokenType::RParen, tok.to_string())
                }
                '{' => {
                    self.read_char();
                    Token::new(TokenType::LBrace, tok.to_string())
                }
                '}' => {
                    self.read_char();
                    Token::new(TokenType::RBrace, tok.to_string())
                }
                ',' => {
                    self.read_char();
                    Token::new(TokenType::Comma, tok.to_string())
                }
                ';' => {
                    self.read_char();
                    Token::new(TokenType::Semicolon, tok.to_string())
                }

                c if self.is_identifier(c) => {
                    //
                    let literal = self.read_identifier();
                    let token_type = match literal.as_str() {
                        "fn" => TokenType::Function,
                        "let" => TokenType::Let,
                        "true" => TokenType::True,
                        "false" => TokenType::False,
                        "if" => TokenType::If,
                        "else" => TokenType::Else,
                        "return" => TokenType::Return,
                        _ => TokenType::Ident,
                    };

                    Token::new(token_type, literal)
                }
                d if d.is_ascii_digit() => {
                    //
                    Token::new(TokenType::Int, self.read_number())
                }
                _ => Token::new(
                    TokenType::Illegal {
                        ch: self.ch,
                        position: self.position,
                    },
                    tok.to_string(),
                ),
            }
        } else {
            Token::new(TokenType::EOF, "".to_string())
        };

        token
    }

    fn read_char(&mut self) {
        if self.read_position > self.input.len() {
            self.ch = None;
        } else {
            self.ch = self.input.chars().nth(self.read_position);
        }

        self.position = self.read_position;
        self.read_position += 1;
    }

    fn peek_char(&self) -> Option<char> {
        if self.read_position > self.input.len() {
            None
        } else {
            self.input.chars().nth(self.read_position)
        }
    }

    fn read_identifier(&mut self) -> String {
        let position = self.position;
        while let Some(ch) = self.ch {
            if !self.is_identifier(ch) {
                break;
            }
            self.read_char();
        }
        self.input[position..self.position].to_string()
    }

    fn is_identifier(&self, ch: char) -> bool {
        ch.is_ascii_alphabetic() || ch == '_'
    }

    fn read_number(&mut self) -> String {
        let position = self.position;
        while let Some(ch) = self.ch {
            if !ch.is_ascii_digit() {
                break;
            }
            self.read_char();
        }
        self.input[position..self.position].to_string()
    }

    fn skip_whitespace(&mut self) {
        while let Some(ch) = self.ch {
            if !ch.is_ascii_whitespace() {
                break;
            }
            self.read_char();
        }
    }
}

//
#[cfg(test)]
mod tests {

    use super::*;
    #[test]
    fn test_next_token() {
        let input = r#"let five = 5;
let ten = 10;


let add = fn(x, y) {

    x + y;
}

let result = add(five, ten);


!-/*5;

5 < 10 > 5;

if (5 < 10) {
    return true;
} else {
    return false;
}

10 == 10;

10 != 9;

"#;

        let tests = vec![
            (TokenType::Let, "let"),
            (TokenType::Ident, "five"),
            (TokenType::Assign, "="),
            (TokenType::Int, "5"),
            (TokenType::Semicolon, ";"),
            (TokenType::Let, "let"),
            (TokenType::Ident, "ten"),
            (TokenType::Assign, "="),
            (TokenType::Int, "10"),
            (TokenType::Semicolon, ";"),
            (TokenType::Let, "let"),
            (TokenType::Ident, "add"),
            (TokenType::Assign, "="),
            (TokenType::Function, "fn"),
            (TokenType::LParen, "("),
            (TokenType::Ident, "x"),
            (TokenType::Comma, ","),
            (TokenType::Ident, "y"),
            (TokenType::RParen, ")"),
            (TokenType::LBrace, "{"),
            (TokenType::Ident, "x"),
            (TokenType::Plus, "+"),
            (TokenType::Ident, "y"),
            (TokenType::Semicolon, ";"),
            (TokenType::RBrace, "}"),
            (TokenType::Let, "let"),
            (TokenType::Ident, "result"),
            (TokenType::Assign, "="),
            (TokenType::Ident, "add"),
            (TokenType::LParen, "("),
            (TokenType::Ident, "five"),
            (TokenType::Comma, ","),
            (TokenType::Ident, "ten"),
            (TokenType::RParen, ")"),
            (TokenType::Semicolon, ";"),
            (TokenType::Bang, "!"),
            (TokenType::Minus, "-"),
            (TokenType::Slash, "/"),
            (TokenType::Asterisk, "*"),
            (TokenType::Int, "5"),
            (TokenType::Semicolon, ";"),
            (TokenType::Int, "5"),
            (TokenType::LT, "<"),
            (TokenType::Int, "10"),
            (TokenType::GT, ">"),
            (TokenType::Int, "5"),
            (TokenType::Semicolon, ";"),
            (TokenType::If, "if"),
            (TokenType::LParen, "("),
            (TokenType::Int, "5"),
            (TokenType::LT, "<"),
            (TokenType::Int, "10"),
            (TokenType::RParen, ")"),
            (TokenType::LBrace, "{"),
            (TokenType::Return, "return"),
            (TokenType::True, "true"),
            (TokenType::Semicolon, ";"),
            (TokenType::RBrace, "}"),
            (TokenType::Else, "else"),
            (TokenType::LBrace, "{"),
            (TokenType::Return, "return"),
            (TokenType::False, "false"),
            (TokenType::Semicolon, ";"),
            (TokenType::RBrace, "}"),
            (TokenType::Int, "10"),
            (TokenType::Equal, "=="),
            (TokenType::Int, "10"),
            (TokenType::Semicolon, ";"),
            (TokenType::Int, "10"),
            (TokenType::NotEqual, "!="),
            (TokenType::Int, "9"),
            (TokenType::Semicolon, ";"),
            (TokenType::EOF, ""),
        ];

        let mut lexer = Lexer::new(input.to_string());

        tests.iter().enumerate().for_each(|(ix, tt)| {
            let tok = lexer.next_token();
            if tok.token_type != tt.0 {
                panic!("{}: {:?} != {:?}", ix, tok.token_type, tt.0);
            }
            if tok.literal != tt.1 {
                panic!("{}: {:?} != {:?}", ix, tok.literal, tt.1);
            }
        });
    }
}
