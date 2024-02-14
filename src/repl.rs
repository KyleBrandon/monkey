use std::io::stdin;

use crate::{environment::Environment, eval, lexer::Lexer, parser::Parser};

pub fn start_repl() {
    let mut env = Environment::new();
    loop {
        print!(">> ");
        let mut input = String::new();
        if let Ok(_result) = stdin().read_line(&mut input) {
            if input.starts_with("::exit") {
                break;
            }

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
                    let Some(evaluated) = eval::eval(&program, &mut env) else {
                        println!("Error evaluating program");
                        continue;
                    };
                    println!("{}", evaluated.inspect());
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
