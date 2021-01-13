use crate::token::number_type::NumberType;

use super::callable::Callable;

#[derive(Clone, PartialEq, Debug)]
pub enum Value {
    PythonNone,
    Bool(bool),
    Number(NumberType),
    Str(String),
    Fun(Callable),
}

impl std::fmt::Display for Value {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Value::PythonNone => write!(f, "None"),
            Value::Bool(false) => write!(f, "False"),
            Value::Bool(true) => write!(f, "True"),
            Value::Number(value) => write!(f, "{}", value),
            Value::Str(value) => write!(f, "{}", value),
            Value::Fun(callable) => write!(f, "fun <{}>", callable),
        }
    }
}
