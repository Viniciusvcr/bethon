use std::collections::HashMap;

use super::typings::{types::Type, value::Value};

#[derive(PartialEq, Clone, Default, Debug)]
pub struct Import {
    pub name: String,
    pub t: Type,
    pub v: Value,
}

impl Import {
    pub fn new(name: &str, t: Type, v: Value) -> Self {
        Self {
            name: name.to_string(),
            t,
            v,
        }
    }

    pub fn imports() -> HashMap<String, Vec<Self>> {
        let mut imports = HashMap::default();

        imports.insert(
            "dataclasses".to_string(),
            vec![Import::new("dataclass", Type::Null, Value::PythonNone)],
        );

        imports.insert(
            "enum".to_string(),
            vec![Import::new("Enum", Type::Null, Value::PythonNone)],
        );

        imports
    }
}

pub type Module = HashMap<String, Vec<Import>>;
