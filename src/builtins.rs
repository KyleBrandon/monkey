use crate::object::Object;

pub fn get_builtin(name: &str) -> Option<Object> {
    match name {
        "len" => Some(Object::Builtin(len)),
        _ => None,
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
        Object::String(ref s) => Some(Object::Integer(s.len() as i64)),
        _ => Some(Object::Error(format!(
            "argument to `len` not supported, got {}",
            args[0].object_type()
        ))),
    }
}
