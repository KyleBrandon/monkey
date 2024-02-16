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
