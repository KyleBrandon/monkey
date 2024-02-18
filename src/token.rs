#[derive(Debug, Clone)]
pub struct Token {
    pub token_type: TokenType,
    pub literal: String,
}

impl Token {
    pub fn new(token_type: TokenType, literal: String) -> Self {
        Token {
            token_type,
            literal,
        }
    }
}

#[derive(Debug, PartialEq, Copy, Clone, Eq, Hash)]
pub enum TokenType {
    Illegal { ch: Option<char>, position: usize },
    EOF,
    // Identifiers + literals
    Ident,
    Int,
    // Operators
    Assign,
    Plus,
    Minus,
    Bang,
    Slash,
    Asterisk,
    LT,
    GT,
    Equal,
    NotEqual,
    // Delimiters
    Comma,
    Semicolon,
    // Brackets
    LParen,
    RParen,
    LBrace,
    RBrace,
    LBracket,
    RBracket,
    // Keywords
    Function,
    Let,
    True,
    False,
    If,
    Else,
    Return,
    String,
}
