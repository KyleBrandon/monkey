use std::{ops::Deref, rc::Rc};

use crate::{
    builtins::get_builtin,
    environment::Environment,
    error::EvalError,
    node::{BlockStatement, Identifier, IfExpression, Node, Program},
    object::{Array, Function, Object},
};

pub fn eval(node: &Node, env: &mut Environment) -> Result<Object, EvalError> {
    tracing::info!("evaluating: {:?}", node);
    match node {
        Node::Program(program) => eval_program(program, env),
        Node::ExpressionStatement(expression) => eval(expression.expression.deref(), env),
        Node::IntegerLiteral(integer) => Ok(Object::Integer(integer.value)),
        Node::BooleanLiteral(boolean) => Ok(Object::Boolean(boolean.value)),
        Node::PrefixExpression(expr) => match eval(expr.right.deref(), env) {
            Ok(right) => eval_prefix_expression(&expr.operator, right),
            Err(err) => Err(err),
        },
        Node::InfixExpression(expr) => {
            let result = eval(expr.left.deref(), env);
            let Ok(left) = result else {
                return result;
            };

            let result = eval(expr.right.deref(), env);
            let Ok(right) = result else {
                return result;
            };

            eval_infix_expression(&expr.operator, &left, &right)
        }
        Node::IfExpression(expr) => eval_if_expression(expr, env),
        Node::ReturnStatement(expr) => {
            let result = eval(&expr.return_value, env);
            match result {
                Ok(val) => Ok(Object::ReturnValue(Box::new(val))),
                Err(e) => Err(e),
            }
        }
        Node::BlockStatement(expr) => eval_block_statements(expr, env),
        Node::LetStatement(expr) => {
            let result = eval(&expr.value, env);
            let Ok(ref val) = result else {
                return result;
            };
            env.set(expr.name.value.clone(), val.clone());
            result
        }
        Node::Identifier(ident) => eval_identifier(ident, env),
        Node::FunctionLiteral(func) => Ok(Object::Function(Function {
            parameters: Rc::clone(&func.parameters),
            body: Rc::clone(&func.body),
            env: env.clone(),
        })),
        Node::CallExpression(expr) => {
            let func = expr.function.deref();
            let result = eval(func, env);
            let Ok(function) = result else {
                return result;
            };

            let result = eval_expressions(&expr.arguments, env);
            match result {
                Ok(args) => apply_function(&function, &args),
                Err(e) => Err(e),
            }
        }
        Node::StringLiteral(string) => {
            let s = string.value.clone();
            Ok(Object::String(s))
        }
        Node::ArrayLiteral(array) => {
            let result = eval_expressions(&array.elements, env);
            match result {
                Ok(elements) => Ok(Object::Array(Array { elements })),
                Err(e) => Err(e),
            }
        }
        Node::IndexExpression(expr) => {
            let result = eval(expr.left.deref(), env);
            let Ok(left) = result else {
                return result;
            };

            let result = eval(expr.index.deref(), env);
            let Ok(index) = result else {
                return result;
            };

            eval_index_expression(&left, &index)
        }
        _ => Err(EvalError {
            message: format!("unhandled node: {:?}", node),
        }),
    }
}

fn eval_expressions(exprs: &Vec<Node>, env: &mut Environment) -> Result<Vec<Object>, EvalError> {
    let mut expr_result = vec![];

    for expr in exprs {
        match eval(expr, env) {
            Ok(evaluated) => expr_result.push(evaluated),
            Err(e) => return Err(e),
        }
    }

    Ok(expr_result)
}

fn apply_function(func: &Object, args: &[Object]) -> Result<Object, EvalError> {
    match func {
        Object::Function(function) => {
            //
            let mut extended_env = extended_function_env(function, args);
            let evaluated = eval(function.body.deref(), &mut extended_env);
            unwrap_result_value(evaluated)
        }
        Object::Builtin(function) => function(args),
        _ => Err(EvalError {
            message: format!("not a function: {}", func.object_type()),
        }),
    }
}

fn extended_function_env(function: &Function, args: &[Object]) -> Environment {
    let mut env = Environment::new_enclosed(function.env.clone());
    for (i, param) in function.parameters.iter().enumerate() {
        env.set(param.value.clone(), args[i].clone());
    }

    env
}

fn unwrap_result_value(evaluated: Result<Object, EvalError>) -> Result<Object, EvalError> {
    if let Ok(Object::ReturnValue(val)) = evaluated {
        Ok(*val)
    } else {
        evaluated
    }
}

fn eval_identifier(ident: &Identifier, env: &mut Environment) -> Result<Object, EvalError> {
    if let Some(val) = env.get(&ident.value) {
        Ok(val)
    } else if let Ok(builtin) = get_builtin(ident.value.as_str()) {
        Ok(builtin)
    } else {
        Err(EvalError {
            message: format!("identifier not found: {}", ident.value),
        })
    }
}

fn eval_program(program: &Program, env: &mut Environment) -> Result<Object, EvalError> {
    let mut result: Result<Object, EvalError> = Ok(Object::Null);

    for statement in program.statements.iter() {
        result = eval(statement, env);
        match result {
            Ok(Object::ReturnValue(val)) => {
                return Ok(*val);
            }
            Err(_) => return result,
            _ => (),
        }
    }

    result
}

fn eval_if_expression(expr: &IfExpression, env: &mut Environment) -> Result<Object, EvalError> {
    let result = eval(&expr.condition, env);
    let Ok(condition) = result else {
        return result;
    };

    if is_truthy(condition) {
        eval(&expr.consequence, env)
    } else if let Some(alternative) = &expr.alternative {
        eval(alternative, env)
    } else {
        Err(EvalError {
            message: "Invalid if expression".to_string(),
        })
    }
}

fn is_truthy(obj: Object) -> bool {
    match obj {
        Object::Null => false,
        Object::Boolean(b) => b,
        _ => true,
    }
}

fn eval_block_statements(
    block: &BlockStatement,
    env: &mut Environment,
) -> Result<Object, EvalError> {
    let mut result: Result<Object, EvalError> = Err(EvalError {
        message: "Invalid block statement".to_string(),
    });

    for statement in block.statements.iter() {
        //
        result = eval(statement, env);
        match result {
            Ok(Object::ReturnValue(_)) | Err(_) => {
                return result;
            }
            _ => (),
        }
    }
    result
}

fn eval_prefix_expression(operator: &str, right: Object) -> Result<Object, EvalError> {
    match operator {
        "!" => eval_bang_operator_expression(right),
        "-" => eval_minus_operator_expression(right),
        _ => Err(EvalError {
            message: format!("unknown operator: {}{}", operator, right.object_type()),
        }),
    }
}

fn eval_minus_operator_expression(right: Object) -> Result<Object, EvalError> {
    match right {
        Object::Integer(i) => Ok(Object::Integer(-i)),
        _ => Err(EvalError {
            message: format!("unknown operator: -{}", right.object_type()),
        }),
    }
}

fn eval_bang_operator_expression(right: Object) -> Result<Object, EvalError> {
    match right {
        Object::Boolean(b) => Ok(Object::Boolean(!b)),
        Object::Null => Ok(Object::Boolean(true)),
        _ => Ok(Object::Boolean(false)),
    }
}

fn eval_infix_expression(
    operator: &str,
    left: &Object,
    right: &Object,
) -> Result<Object, EvalError> {
    match (left.clone(), right.clone()) {
        (Object::Integer(left), Object::Integer(right)) => {
            eval_integer_infix_expression(operator, left, right)
        }
        (Object::Boolean(left), Object::Boolean(right)) => {
            eval_boolean_infix_expression(operator, left, right)
        }

        (Object::String(left), Object::String(right)) => {
            eval_string_infix_expression(operator, left, right)
        }
        (left, right) => {
            if left.object_type() != right.object_type() {
                Err(EvalError {
                    message: format!(
                        "type mismatch: {} {} {}",
                        left.object_type(),
                        operator,
                        right.object_type()
                    ),
                })
            } else {
                Err(EvalError {
                    message: format!(
                        "unknown operand: {} {} {}",
                        left.object_type(),
                        operator,
                        right.object_type()
                    ),
                })
            }
        }
    }
}

fn eval_string_infix_expression(
    operator: &str,
    left: String,
    right: String,
) -> Result<Object, EvalError> {
    match operator {
        "+" => Ok(Object::String(left + &right)),
        _ => Err(EvalError {
            message: format!("unknown operator: STRING {} STRING", operator),
        }),
    }
}

fn eval_integer_infix_expression(
    operator: &str,
    left: i64,
    right: i64,
) -> Result<Object, EvalError> {
    match operator {
        "+" => Ok(Object::Integer(left + right)),
        "-" => Ok(Object::Integer(left - right)),
        "*" => Ok(Object::Integer(left * right)),
        "/" => Ok(Object::Integer(left / right)),
        "<" => Ok(Object::Boolean(left < right)),
        ">" => Ok(Object::Boolean(left > right)),
        "==" => Ok(Object::Boolean(left == right)),
        "!=" => Ok(Object::Boolean(left != right)),
        _ => Err(EvalError {
            message: format!("unknown operator: {} {} {}", left, operator, right),
        }),
    }
}

fn eval_boolean_infix_expression(
    operator: &str,
    left: bool,
    right: bool,
) -> Result<Object, EvalError> {
    match operator {
        "==" => Ok(Object::Boolean(left == right)),
        "!=" => Ok(Object::Boolean(left != right)),
        _ => Err(EvalError {
            message: format!("unknown operator: BOOLEAN {} BOOLEAN", operator),
        }),
    }
}

fn eval_index_expression(left: &Object, index: &Object) -> Result<Object, EvalError> {
    match (left.clone(), index.clone()) {
        (Object::Array(array), Object::Integer(i)) => eval_array_index_expression(array, i),
        _ => Err(EvalError {
            message: format!("index operator not supported: {:?}", left),
        }),
    }
}

fn eval_array_index_expression(array: Array, index: i64) -> Result<Object, EvalError> {
    if index < 0 || index >= array.elements.len() as i64 {
        return Ok(Object::Null);
    }
    Ok(array.elements[index as usize].clone())
}

#[cfg(test)]
mod tests {

    enum IntOrUnit {
        Int(i64),
        Array(Vec<i64>),
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
            let Ok(evaluated) = test_eval(tt.0) else {
                panic!("test_eval returned None");
            };
            test_integer_object(&evaluated, tt.1);
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
            let Ok(evaluated) = test_eval(tt.0) else {
                panic!("test_eval returned None");
            };

            test_boolean_object(&evaluated, tt.1);
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
            let Ok(evaluated) = test_eval(tt.0) else {
                panic!("test_eval returned None");
            };

            test_boolean_object(&evaluated, tt.1);
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
                Ok(evaluated) => match tt.1 {
                    IntOrUnit::Int(i64) => test_integer_object(&evaluated, i64),
                    IntOrUnit::Unit => test_null_object(&evaluated),
                    _ => panic!("test_eval returned None"),
                },
                Err(_) => match tt.1 {
                    IntOrUnit::Int(_) => panic!("test_eval returned None"),
                    IntOrUnit::Unit => (),
                    _ => panic!("test_eval returned None"),
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
        tracing_subscriber::fmt().init();

        for tt in tests {
            let Ok(evaluated) = test_eval(tt.0) else {
                panic!("test_eval returned None");
            };
            tracing::info!("evaluated: {:?}", evaluated);

            test_integer_object(&evaluated, tt.1);
        }
    }

    #[test]
    fn test_error_handling() {
        tracing_subscriber::fmt().init();
        let tests = [
            ("5 + true;", "type mismatch: INTEGER + BOOLEAN"),
            ("5 + true; 5;", "type mismatch: INTEGER + BOOLEAN"),
            ("-true", "unknown operator: -BOOLEAN"),
            ("true + false;", "unknown operator: BOOLEAN + BOOLEAN"),
            ("5; true + false; 5", "unknown operator: BOOLEAN + BOOLEAN"),
            (
                "if (10 > 1) { true + false; }",
                "unknown operator: BOOLEAN + BOOLEAN",
            ),
            (
                "if (10 > 1) { if (10 > 1) { return true + false; } return 1; }",
                "unknown operator: BOOLEAN + BOOLEAN",
            ),
            ("foobar", "identifier not found: foobar"),
            ("\"Hello\" - \"World\"", "unknown operator: STRING - STRING"),
        ];

        for tt in tests {
            let result = test_eval(tt.0);
            match result {
                Err(e) => assert_eq!(e.message, tt.1),
                _ => panic!("test_eval returned None"),
            }
        }
    }

    #[test]
    fn test_let_statements() {
        tracing_subscriber::fmt().init();
        let tests = [
            ("let a = 5; a;", 5),
            ("let a = 5 * 5; a;", 25),
            ("let a = 5; let b = a; b;", 5),
            ("let a = 5; let b = a; let c = a + b + 5; c;", 15),
        ];

        for tt in tests {
            let Ok(evaluated) = test_eval(tt.0) else {
                panic!("test_eval returned None");
            };

            test_integer_object(&evaluated, tt.1);
        }
    }

    #[test]
    fn test_function_object() {
        let input = "fn(x) { x + 2; };";
        let Ok(evaluated) = test_eval(input) else {
            panic!("test_eval returned None");
        };
        match evaluated {
            Object::Function(func) => {
                assert_eq!(func.parameters.len(), 1);
                assert_eq!(func.parameters[0].value, "x");
                assert_eq!(func.body.string(), "(x + 2)");
            }
            _ => panic!("object is not Function. got={:?}", evaluated),
        }
    }

    #[test]
    fn test_function_application() {
        let tests = [
            ("let identity = fn(x) { x; }; identity(5);", 5),
            ("let identity = fn(x) { return x; }; identity(5);", 5),
            ("let double = fn(x) { x * 2; }; double(5);", 10),
            ("let add = fn(x, y) { x + y; }; add(5, 5);", 10),
            ("let add = fn(x, y) { x + y; }; add(5 + 5, add(5, 5));", 20),
            ("fn(x) { x; }(5)", 5),
        ];

        for tt in tests {
            let Ok(evaluated) = test_eval(tt.0) else {
                panic!("test_eval returned None");
            };
            test_integer_object(&evaluated, tt.1);
        }
    }

    #[test]
    fn test_closures() {
        let input = r#"
        let newAdder = fn(x) {
            fn(y) { x + y };
        };
        let addTwo = newAdder(2);
        addTwo(3);
        "#;

        test_integer_object(&test_eval(input).unwrap(), 5);
    }

    #[test]
    fn test_string_literal() {
        let input = r#""hello world""#;
        let Ok(evaluated) = test_eval(input) else {
            panic!("test_eval returned None");
        };

        let Object::String(s) = evaluated else {
            panic!("object is not String. got={:?}", &evaluated);
        };

        assert_eq!(s, "hello world".to_string());
    }

    #[test]
    fn test_string_concatenation() {
        let input = r#""hello" + " " + "world""#;

        let Ok(evaluated) = test_eval(input) else {
            panic!("test_eval returned None");
        };

        let Object::String(s) = evaluated else {
            panic!("object is not String. got={:?}", &evaluated);
        };

        assert_eq!(s, "hello world");
    }

    #[test]
    fn test_builtin_function() {
        let tests = vec![
            (r#"len("")"#, Ok(IntOrUnit::Int(0))),
            (r#"len("four")"#, Ok(IntOrUnit::Int(4))),
            (r#"len("hello world")"#, Ok(IntOrUnit::Int(11))),
            (
                r#"len(1)"#,
                Err("argument to `len` not supported, got INTEGER"),
            ),
            (
                r#"len("one", "two")"#,
                Err("wrong number of arguments. got=2, want=1"),
            ),
            (r#"len([1, 2, 3])"#, Ok(IntOrUnit::Int(3))),
            (r#"len([])"#, Ok(IntOrUnit::Int(0))),
            (r#"puts("hello", "world!")"#, Ok(IntOrUnit::Unit)),
            (r#"first([1, 2, 3])"#, Ok(IntOrUnit::Int(1))),
            (r#"first([])"#, Ok(IntOrUnit::Unit)),
            (
                r#"first(1)"#,
                Err("argument to `first` must be ARRAY, got INTEGER"),
            ),
            (r#"last([1, 2, 3])"#, Ok(IntOrUnit::Int(3))),
            (r#"last([])"#, Ok(IntOrUnit::Unit)),
            (
                r#"last(1)"#,
                Err("argument to `last` must be ARRAY, got INTEGER"),
            ),
            (r#"rest([1, 2, 3])"#, Ok(IntOrUnit::Array(vec![2, 3]))),
            (r#"rest([])"#, Ok(IntOrUnit::Unit)),
            (r#"push([], 1)"#, Ok(IntOrUnit::Array(vec![1]))),
            (
                r#"push(1, 1)"#,
                Err("argument to `push` must be ARRAY, got INTEGER"),
            ),
        ];

        for tt in tests {
            let evaluated = test_eval(tt.0);
            match tt.1 {
                Ok(i) => match i {
                    IntOrUnit::Int(i) => {
                        let Ok(Object::Integer(val)) = evaluated else {
                            panic!("object is not Integer. got={:?}", &evaluated);
                        };
                        assert_eq!(val, i);
                    }
                    IntOrUnit::Array(arr) => {
                        let Ok(Object::Array(array)) = evaluated else {
                            panic!("object is not Array. got={:?}", &evaluated);
                        };
                        for (i, el) in arr.iter().enumerate() {
                            test_integer_object(&array.elements[i], *el);
                        }
                    }
                    IntOrUnit::Unit => (),
                },
                Err(s) => {
                    let Err(e) = evaluated else {
                        panic!("object is not Error. got={:?}", &evaluated);
                    };
                    assert_eq!(e.message, s);
                }
            }
        }
    }

    #[test]
    fn test_array_literal() {
        let input = "[1, 2 * 2, 3 + 3]";
        let Ok(evaluated) = test_eval(input) else {
            panic!("test_eval returned None");
        };

        let Object::Array(array) = evaluated else {
            panic!("object is not Array. got={:?}", &evaluated);
        };

        assert_eq!(array.elements.len(), 3);

        test_integer_object(&array.elements[0].clone(), 1);
        test_integer_object(&array.elements[1].clone(), 4);
        test_integer_object(&array.elements[2].clone(), 6);
    }

    #[test]
    fn test_array_index_expressions() {
        let tests = [
            ("[1, 2, 3][0]", IntOrUnit::Int(1)),
            ("[1, 2, 3][1]", IntOrUnit::Int(2)),
            ("[1, 2, 3][2]", IntOrUnit::Int(3)),
            ("let i = 0; [1][i];", IntOrUnit::Int(1)),
            ("[1, 2, 3][1 + 1];", IntOrUnit::Int(3)),
            ("let myArray = [1, 2, 3]; myArray[2];", IntOrUnit::Int(3)),
            (
                "let myArray = [1, 2, 3]; myArray[0] + myArray[1] + myArray[2];",
                IntOrUnit::Int(6),
            ),
            (
                "let myArray = [1, 2, 3]; let i = myArray[0]; myArray[i]",
                IntOrUnit::Int(2),
            ),
            ("[1, 2, 3][3]", IntOrUnit::Unit),
            ("[1, 2, 3][-1]", IntOrUnit::Unit),
        ];

        for tt in tests {
            match test_eval(tt.0) {
                Ok(evaluated) => match tt.1 {
                    IntOrUnit::Int(val) => test_integer_object(&evaluated, val),
                    IntOrUnit::Unit => test_null_object(&evaluated),
                    _ => panic!("test_eval returned None"),
                },
                Err(_) => match tt.1 {
                    IntOrUnit::Unit => (),
                    _ => panic!("test_eval returned None"),
                },
            }
        }
    }

    fn test_eval(input: &str) -> Result<Object, EvalError> {
        let l = Lexer::new(input.to_string());
        let mut p = Parser::new(l);
        let result = p.parse_program();
        let env = &mut Environment::new();
        match result {
            Ok(program) => eval(&program, env),
            Err(e) => Err(EvalError {
                message: e.errors.join(", ").to_string(),
            }),
        }
    }

    fn test_integer_object(obj: &Object, expected: i64) {
        match obj {
            Object::Integer(i) => {
                assert_eq!(*i, expected);
            }
            _ => panic!("object is not Integer. got={:?}", obj),
        }
    }

    fn test_boolean_object(obj: &Object, expected: bool) {
        match obj {
            Object::Boolean(b) => {
                assert_eq!(*b, expected);
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
