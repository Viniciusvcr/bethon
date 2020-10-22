use crate::expr::Value;
use std::collections::HashMap;

#[derive(Default, Debug)]
pub struct Environment {
    values: HashMap<String, Value>,
}

impl Environment {
    pub fn insert(&mut self, identifier: String, value: Value) {
        self.values.insert(identifier, value);
    }

    pub fn get(&mut self, identifier: &str) -> Option<&Value> {
        self.values.get(identifier)
    }
}
