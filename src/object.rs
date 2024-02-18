use core::fmt;
use std::{
    fmt::{Display, Formatter},
    rc::Rc,
};

use itertools::Itertools;

use crate::{
    environment::Environment,
    node::{Identifier, Node},
};

type BulitinFunction = fn(&[Object]) -> Option<Object>;

#[derive(Debug, PartialEq)]
pub enum ObjectType {
    Integer,
    Boolean,
    Null,
    ReturnValue,
    Error,
    Function,
    String,
    Builtin,
    Array,
}

impl Display for ObjectType {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        write!(
            f,
            "{}",
            match self {
                ObjectType::Integer => "INTEGER",
                ObjectType::Boolean => "BOOLEAN",
                ObjectType::Null => "NULL",
                ObjectType::ReturnValue => "RETURN_VALUE",
                ObjectType::Error => "ERROR",
                ObjectType::Function => "FUNCTION",
                ObjectType::String => "STRING",
                ObjectType::Builtin => "BULITIN",
                ObjectType::Array => "ARRAY",
            }
        )
    }
}

#[derive(Debug, Clone)]
pub enum Object {
    Integer(i64),
    Boolean(bool),
    Null,
    ReturnValue(Box<Object>),
    Error(String),
    Function(Function),
    String(String),
    Builtin(BulitinFunction),
    Array(Array),
}

impl Object {
    pub fn object_type(&self) -> ObjectType {
        match self {
            Object::Integer(_) => ObjectType::Integer,
            Object::Boolean(_) => ObjectType::Boolean,
            Object::Null => ObjectType::Null,
            Object::ReturnValue(_) => ObjectType::ReturnValue,
            Object::Error(_) => ObjectType::Error,
            Object::Function(_) => ObjectType::Function,
            Object::String(_) => ObjectType::String,
            Object::Builtin(_) => ObjectType::Builtin,
            Object::Array(_) => ObjectType::Array,
        }
    }

    pub fn inspect(&self) -> String {
        match self {
            Object::Integer(i) => i.to_string(),
            Object::Boolean(b) => b.to_string(),
            Object::Null => "null".to_string(),
            Object::ReturnValue(obj) => obj.inspect(),
            Object::Error(e) => e.to_string(),
            Object::Function(f) => {
                let mut buffer = String::new();

                if f.parameters.is_empty() {
                    buffer.push_str("fn() {\n");
                } else {
                    buffer.push_str("fn(");
                    buffer.push_str(&f.parameters.iter().map(|p| p.string()).join(", "));
                    buffer.push_str(") {\n");
                }

                buffer.push_str(&f.body.string());
                buffer.push_str("\n}");

                buffer
            }
            Object::String(s) => s.clone(),
            Object::Builtin(_) => "builtin function".to_string(),
            Object::Array(a) => {
                let mut buffer = String::new();
                buffer.push('[');
                buffer.push_str(&a.elements.iter().map(|e| e.inspect()).join(", "));
                buffer.push(']');
                buffer
            }
        }
    }
}

#[derive(Debug)]
pub struct Function {
    pub parameters: Rc<Vec<Identifier>>,
    pub body: Rc<Node>,
    pub env: Environment,
}

impl Clone for Function {
    fn clone(&self) -> Self {
        Function {
            parameters: Rc::clone(&self.parameters),
            body: Rc::clone(&self.body),
            env: self.env.clone(),
        }
    }
}

#[derive(Debug, Clone)]
pub struct Array {
    pub elements: Vec<Object>,
}
