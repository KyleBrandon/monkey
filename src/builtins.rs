use crate::{
    error::EvalError,
    object::{Array, Object},
};

pub fn get_builtin(name: &str) -> Result<Object, EvalError> {
    match name {
        "len" => Ok(Object::Builtin(len)),
        "first" => Ok(Object::Builtin(first)),
        "last" => Ok(Object::Builtin(last)),
        "rest" => Ok(Object::Builtin(rest)),
        "push" => Ok(Object::Builtin(push)),
        _ => Err(EvalError {
            message: format!("unknown builtin function: {}", name),
        }),
    }
}

fn first(args: &[Object]) -> Result<Object, EvalError> {
    if args.len() != 1 {
        return Err(EvalError {
            message: format!("wrong number of arguments. got={}, want=1", args.len()),
        });
    }

    match args[0] {
        Object::Array(ref a) => {
            if a.elements.is_empty() {
                return Ok(Object::Null);
            }
            Ok(a.elements.first().cloned().unwrap())
        }
        _ => Err(EvalError {
            message: format!(
                "argument to `first` must be ARRAY, got {}",
                args[0].object_type()
            ),
        }),
    }
}

fn last(args: &[Object]) -> Result<Object, EvalError> {
    if args.len() != 1 {
        return Err(EvalError {
            message: format!("wrong number of arguments. got={}, want=1", args.len()),
        });
    }

    match args[0] {
        Object::Array(ref a) => {
            if a.elements.is_empty() {
                return Ok(Object::Null);
            }
            Ok(a.elements.last().cloned().unwrap())
        }
        _ => Err(EvalError {
            message: format!(
                "argument to `last` must be ARRAY, got {}",
                args[0].object_type()
            ),
        }),
    }
}

fn rest(args: &[Object]) -> Result<Object, EvalError> {
    if args.len() != 1 {
        return Err(EvalError {
            message: format!("wrong number of arguments. got={}, want=1", args.len()),
        });
    }

    match args[0] {
        Object::Array(ref a) => {
            if a.elements.is_empty() {
                return Ok(Object::Null);
            }
            let mut elements = a.elements.clone();
            elements.remove(0);
            Ok(Object::Array(Array { elements }))
        }

        _ => Err(EvalError {
            message: format!(
                "argument to `rest` must be ARRAY, got {}",
                args[0].object_type()
            ),
        }),
    }
}

fn push(args: &[Object]) -> Result<Object, EvalError> {
    if args.len() != 2 {
        return Err(EvalError {
            message: format!("wrong number of arguments. got={}, want=2", args.len()),
        });
    }

    match args[0] {
        Object::Array(ref a) => {
            let mut elements = a.elements.clone();
            elements.push(args[1].clone());
            Ok(Object::Array(Array { elements }))
        }
        _ => Err(EvalError {
            message: format!(
                "argument to `push` must be ARRAY, got {}",
                args[0].object_type()
            ),
        }),
    }
}

fn len(args: &[Object]) -> Result<Object, EvalError> {
    if args.len() != 1 {
        return Err(EvalError {
            message: format!("wrong number of arguments. got={}, want=1", args.len()),
        });
    }

    match args[0] {
        Object::Array(ref a) => Ok(Object::Integer(a.elements.len() as i64)),
        Object::String(ref s) => Ok(Object::Integer(s.len() as i64)),
        _ => Err(EvalError {
            message: format!(
                "argument to `len` not supported, got {}",
                args[0].object_type()
            ),
        }),
    }
}
