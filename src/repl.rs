use std::io::stdin;

use crate::{lexer::Lexer, token::TokenType};

pub fn start_repl() {
    loop {
        print!(">> ");
        let mut input = String::new();
        if let Ok(_result) = stdin().read_line(&mut input) {
            let mut lexer = Lexer::new(input.clone());
            let mut tok = lexer.next_token();

            while tok.token_type != TokenType::EOF {
                //
                println!("{:?}", tok);
                tok = lexer.next_token();
            }

            //
        } else {
            break;
        }
    }
    println!("Exiting");
}
