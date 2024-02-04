use std::ops::Deref;

use crate::{node::Node, object::Object};

pub fn eval(node: &Node) -> Option<Object> {
    match node {
        Node::Program(program) => eval_statements(&program.statements),
        Node::ExpressionStatement(expression) => eval(expression.expression.deref()),
        Node::IntegerLiteral(integer) => Some(Object::Integer(integer.value)),
        _ => None,
    }
}

fn eval_statements(statements: &[Node]) -> Option<Object> {
    let mut result = None;

    for statement in statements {
        result = eval(statement);
    }

    result
}

#[cfg(test)]
mod tests {

    use crate::{lexer::Lexer, object::Object, parser::Parser};

    use super::*;
    #[test]
    fn test_eval_integer_expression() {
        //
        let tests = [("5", 5), ("10", 10)];

        for tt in tests {
            let Some(evaluated) = test_eval(tt.0) else {
                panic!("test_eval returned None");
            };
            test_integer_object(evaluated, tt.1);
        }
    }

    fn test_eval(input: &str) -> Option<Object> {
        let l = Lexer::new(input.to_string());
        let mut p = Parser::new(l);
        let result = p.parse_program();
        match result {
            Ok(program) => eval(&Node::Program(program)),
            Err(e) => panic!("parser error: {:?}", e),
        }
    }

    fn test_integer_object(obj: Object, expected: i64) {
        match obj {
            Object::Integer(i) => {
                assert_eq!(i, expected);
            }
            _ => panic!("object is not Integer. got={:?}", obj),
        }
    }
}
