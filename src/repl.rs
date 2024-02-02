use std::io::stdin;

use crate::{lexer::Lexer, parser::Parser};

pub fn start_repl() {
    loop {
        print!(">> ");
        let mut input = String::new();
        if let Ok(_result) = stdin().read_line(&mut input) {
            let lexer = Lexer::new(input);
            let mut parser = Parser::new(lexer);
            let result = parser.parse_program();

            if !parser.errors().is_empty() {
                for error in parser.errors() {
                    println!("{}", error);
                }
            }

            match result {
                Ok(program) => {
                    println!("{}", program.string());
                }
                Err(e) => {
                    println!("Error: {:?}", e);
                }
            }

            //
        } else {
            break;
        }
    }
    println!("Exiting");
}
