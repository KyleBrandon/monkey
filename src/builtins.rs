use crate::object::{Array, Object};

pub fn get_builtin(name: &str) -> Option<Object> {
    match name {
        "len" => Some(Object::Builtin(len)),
        "first" => Some(Object::Builtin(first)),
        "last" => Some(Object::Builtin(last)),
        "rest" => Some(Object::Builtin(rest)),
        "push" => Some(Object::Builtin(push)),
        _ => None,
    }
}

fn first(args: &[Object]) -> Option<Object> {
    if args.len() != 1 {
        return Some(Object::Error(format!(
            "wrong number of arguments. got={}, want=1",
            args.len()
        )));
    }

    match args[0] {
        Object::Array(ref a) => {
            if a.elements.is_empty() {
                return Some(Object::Null);
            }
            a.elements.first().cloned()
        }
        _ => Some(Object::Error(format!(
            "argument to `first` must be ARRAY, got {}",
            args[0].object_type()
        ))),
    }
}

fn last(args: &[Object]) -> Option<Object> {
    if args.len() != 1 {
        return Some(Object::Error(format!(
            "wrong number of arguments. got={}, want=1",
            args.len()
        )));
    }

    match args[0] {
        Object::Array(ref a) => {
            if a.elements.is_empty() {
                return Some(Object::Null);
            }
            a.elements.last().cloned()
        }
        _ => Some(Object::Error(format!(
            "argument to `last` must be ARRAY, got {}",
            args[0].object_type()
        ))),
    }
}

fn rest(args: &[Object]) -> Option<Object> {
    if args.len() != 1 {
        return Some(Object::Error(format!(
            "wrong number of arguments. got={}, want=1",
            args.len()
        )));
    }

    match args[0] {
        Object::Array(ref a) => {
            if a.elements.is_empty() {
                return Some(Object::Null);
            }
            let mut elements = a.elements.clone();
            elements.remove(0);
            Some(Object::Array(Array { elements }))
        }

        _ => Some(Object::Error(format!(
            "argument to `rest` must be ARRAY, got {}",
            args[0].object_type()
        ))),
    }
}

fn push(args: &[Object]) -> Option<Object> {
    if args.len() != 2 {
        return Some(Object::Error(format!(
            "wrong number of arguments. got={}, want=2",
            args.len()
        )));
    }

    match args[0] {
        Object::Array(ref a) => {
            let mut elements = a.elements.clone();
            elements.push(args[1].clone());
            Some(Object::Array(Array { elements }))
        }
        _ => Some(Object::Error(format!(
            "argument to `push` must be ARRAY, got {}",
            args[0].object_type()
        ))),
    }
}

fn len(args: &[Object]) -> Option<Object> {
    if args.len() != 1 {
        return Some(Object::Error(format!(
            "wrong number of arguments. got={}, want=1",
            args.len()
        )));
    }

    match args[0] {
        Object::Array(ref a) => Some(Object::Integer(a.elements.len() as i64)),
        Object::String(ref s) => Some(Object::Integer(s.len() as i64)),
        _ => Some(Object::Error(format!(
            "argument to `len` not supported, got {}",
            args[0].object_type()
        ))),
    }
}
