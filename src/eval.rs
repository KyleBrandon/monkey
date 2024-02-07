use std::ops::Deref;

use tracing::info;

use crate::{
    node::{BlockStatement, IfExpression, Node, Program},
    object::{Object, ObjectType},
};

pub fn eval(node: &Node) -> Option<Object> {
    match node {
        Node::Program(program) => eval_program(program),
        Node::ExpressionStatement(expression) => eval(expression.expression.deref()),
        Node::IntegerLiteral(integer) => Some(Object::Integer(integer.value)),
        Node::BooleanLiteral(boolean) => Some(Object::Boolean(boolean.value)),
        Node::PrefixExpression(expr) => {
            //
            let Some(right) = eval(expr.right.deref()) else {
                return None;
            };

            eval_prefix_expression(&expr.operator, right)
        }
        Node::InfixExpression(expr) => {
            let left = eval(expr.left.deref());
            let right = eval(expr.right.deref());

            eval_infix_expression(&expr.operator, left, right)
        }
        Node::IfExpression(expr) => eval_if_expression(expr),
        Node::ReturnStatement(expr) => {
            let Some(val) = eval(&expr.return_value) else {
                return None;
            };

            Some(Object::ReturnValue(Box::new(val)))
        }
        Node::BlockStatement(expr) => eval_block_statements(expr),
        _ => None,
    }
}

fn eval_program(program: &Program) -> Option<Object> {
    let mut result = None;

    for statement in program.statements.iter() {
        result = eval(statement);

        if let Some(Object::ReturnValue(result)) = result {
            let val = result.deref();
            return Some(val.clone());
        }
    }

    result
}

fn eval_if_expression(expr: &IfExpression) -> Option<Object> {
    info!("eval_if_expression: {:?}", expr);
    let Some(condition) = eval(&expr.condition) else {
        return None;
    };

    if is_truthy(condition) {
        eval(&expr.consequence)
    } else if let Some(alternative) = &expr.alternative {
        eval(alternative)
    } else {
        None
    }
}

fn is_truthy(obj: Object) -> bool {
    match obj {
        Object::Null => false,
        Object::Boolean(b) => b,
        _ => true,
    }
}

fn eval_block_statements(block: &BlockStatement) -> Option<Object> {
    let mut result = None;
    for statement in block.statements.iter() {
        //
        result = eval(statement);
        if let Some(obj) = &result {
            if obj.object_type() == ObjectType::ReturnValue {
                return Some(obj.clone());
            }
        }
    }
    result
}

fn eval_prefix_expression(operator: &str, right: Object) -> Option<Object> {
    match operator {
        "!" => eval_bang_operator_expression(right),
        "-" => eval_minus_operator_expression(right),
        _ => None,
    }
}

fn eval_minus_operator_expression(right: Object) -> Option<Object> {
    match right {
        Object::Integer(i) => Some(Object::Integer(-i)),
        _ => None,
    }
}

fn eval_bang_operator_expression(right: Object) -> Option<Object> {
    match right {
        Object::Boolean(b) => Some(Object::Boolean(!b)),
        Object::Null => Some(Object::Boolean(true)),
        _ => Some(Object::Boolean(false)),
    }
}

fn eval_infix_expression(
    operator: &str,
    left: Option<Object>,
    right: Option<Object>,
) -> Option<Object> {
    match (left, right) {
        (Some(Object::Integer(left)), Some(Object::Integer(right))) => {
            eval_integer_infix_expression(operator, left, right)
        }
        (Some(Object::Boolean(left)), Some(Object::Boolean(right))) => {
            eval_boolean_infix_expression(operator, left, right)
        }
        _ => None,
    }
}

fn eval_integer_infix_expression(operator: &str, left: i64, right: i64) -> Option<Object> {
    match operator {
        "+" => Some(Object::Integer(left + right)),
        "-" => Some(Object::Integer(left - right)),
        "*" => Some(Object::Integer(left * right)),
        "/" => Some(Object::Integer(left / right)),
        "<" => Some(Object::Boolean(left < right)),
        ">" => Some(Object::Boolean(left > right)),
        "==" => Some(Object::Boolean(left == right)),
        "!=" => Some(Object::Boolean(left != right)),

        _ => None,
    }
}

fn eval_boolean_infix_expression(operator: &str, left: bool, right: bool) -> Option<Object> {
    match operator {
        "==" => Some(Object::Boolean(left == right)),
        "!=" => Some(Object::Boolean(left != right)),

        _ => None,
    }
}

#[cfg(test)]
mod tests {

    enum IntOrUnit {
        Int(i64),
        Unit,
    }

    use crate::{lexer::Lexer, object::Object, parser::Parser};

    use super::*;
    #[test]
    fn test_eval_integer_expression() {
        //
        let tests = [
            ("5", 5),
            ("10", 10),
            ("-5", -5),
            ("-10", -10),
            ("5 + 5 + 5 + 5 - 10", 10),
            ("2 * 2 * 2 * 2 * 2", 32),
            ("-50 + 100 + -50", 0),
            ("5 * 2 + 10", 20),
            ("5 + 2 * 10", 25),
            ("20 + 2 * -10", 0),
            ("50 / 2 * 2 + 10", 60),
            ("2 * (5 + 10)", 30),
            ("3 * 3 * 3 + 10", 37),
            ("3 * (3 * 3) + 10", 37),
            ("(5 + 10 * 2 + 15 / 3) * 2 + -10", 50),
        ];

        for tt in tests {
            let Some(evaluated) = test_eval(tt.0) else {
                panic!("test_eval returned None");
            };
            test_integer_object(evaluated, tt.1);
        }
    }

    #[test]
    fn test_eval_boolean_expression() {
        //
        let tests = [
            ("true", true),
            ("false", false),
            ("1 < 2", true),
            ("1 > 2", false),
            ("1 < 1", false),
            ("1 > 1", false),
            ("1 == 1", true),
            ("1 != 1", false),
            ("1 == 2", false),
            ("1 != 2", true),
            ("true == true", true),
            ("false == false", true),
            ("true == false", false),
            ("true != false", true),
            ("false != true", true),
            ("(1 < 2) == true", true),
            ("(1 < 2) == false", false),
            ("(1 > 2) == true", false),
            ("(1 > 2) == false", true),
        ];

        for tt in tests {
            let Some(evaluated) = test_eval(tt.0) else {
                panic!("test_eval returned None");
            };

            test_boolean_object(evaluated, tt.1);
        }
    }

    #[test]
    fn test_bang_operator() {
        let tests = [
            ("!true", false),
            ("!false", true),
            ("!5", false),
            ("!!true", true),
            ("!!false", false),
            ("!!5", true),
        ];

        for tt in tests {
            let Some(evaluated) = test_eval(tt.0) else {
                panic!("test_eval returned None");
            };

            test_boolean_object(evaluated, tt.1);
        }
    }

    #[test]
    fn test_if_else_expressions() {
        tracing_subscriber::fmt().init();
        let tests = [
            ("if (true) { 10 }", IntOrUnit::Int(10)),
            ("if (false) { 10 }", IntOrUnit::Unit),
            ("if (1) { 10 }", IntOrUnit::Int(10)),
            ("if (1 < 2) { 10 }", IntOrUnit::Int(10)),
            ("if (1 > 2) { 10 }", IntOrUnit::Unit),
            ("if (1 > 2) { 10 } else { 20 }", IntOrUnit::Int(20)),
            ("if (1 < 2) { 10 } else { 20 }", IntOrUnit::Int(10)),
        ];

        for tt in tests {
            match test_eval(tt.0) {
                Some(evaluated) => match tt.1 {
                    IntOrUnit::Int(i64) => test_integer_object(evaluated, i64),
                    IntOrUnit::Unit => test_null_object(&evaluated),
                },
                None => match tt.1 {
                    IntOrUnit::Int(_) => panic!("test_eval returned None"),
                    IntOrUnit::Unit => (),
                },
            }
        }
    }

    #[test]
    fn test_return_statement() {
        let tests = [
            ("return 10;", 10),
            ("return 10; 9;", 10),
            ("return 2 * 5; 9;", 10),
            ("9; return 2 * 5; 9;", 10),
            ("if (10 > 1) { return 10; } else { return 1; }", 10),
            ("if (10 < 1) { return 10; } else { return 1; }", 1),
            ("if (10 > 1) { if (10 > 1) { return 10; } return 1; }", 10),
        ];

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
            Ok(program) => eval(&program),
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

    fn test_boolean_object(obj: Object, expected: bool) {
        match obj {
            Object::Boolean(b) => {
                assert_eq!(b, expected);
            }
            _ => panic!("object is not Boolean. got={:?}", obj),
        }
    }

    fn test_null_object(object: &Object) {
        match object {
            Object::Null => (),
            _ => panic!("object is not Null. got={:?}", object),
        }
    }
}
