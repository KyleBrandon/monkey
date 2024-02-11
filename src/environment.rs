use crate::object::Object;

#[derive(Debug, Clone)]
pub struct Environment {
    store: std::collections::HashMap<String, Object>,
    outer: Option<Box<Environment>>,
}

impl Environment {
    pub fn new() -> Self {
        Self {
            store: std::collections::HashMap::new(),
            outer: None,
        }
    }

    pub fn new_enclosed(outer: Environment) -> Self {
        Self {
            store: std::collections::HashMap::new(),
            outer: Some(Box::new(outer)),
        }
    }

    pub fn get(&self, name: &str) -> Option<Object> {
        if let Some(value) = self.store.get(name) {
            return Some(value.clone());
        } else if let Some(outer) = &self.outer {
            return outer.get(name);
        }
        None
    }

    pub fn set(&mut self, name: String, value: Object) -> Option<Object> {
        self.store.insert(name, value)
    }
}

impl Default for Environment {
    fn default() -> Self {
        Environment::new()
    }
}
