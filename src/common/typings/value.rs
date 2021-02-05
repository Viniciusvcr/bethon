use crate::common::grammar::callable::Callable;

use super::{
    number_type::NumberType,
    user_type::{UserInstance, UserType},
};

#[derive(Clone, PartialEq, Debug)]
#[allow(clippy::large_enum_variant)]
pub enum Value {
    PythonNone,
    Bool(bool),
    Number(NumberType),
    Str(String),
    Fun(Callable),
    UserDefined(UserType),
    Instance(UserInstance),
}

impl Default for Value {
    fn default() -> Self {
        Value::PythonNone
    }
}

impl std::fmt::Display for Value {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Value::PythonNone => write!(f, "None"),
            Value::Bool(false) => write!(f, "False"),
            Value::Bool(true) => write!(f, "True"),
            Value::Number(value) => write!(f, "{}", value),
            Value::Str(value) => write!(f, "{}", value),
            Value::Fun(callable) => write!(f, "{}", callable),
            Value::UserDefined(t) => write!(f, "{}", t.name_token.lexeme),
            Value::Instance(instance) => {
                write!(
                    f,
                    "{}({})",
                    instance.type_name.name_token.lexeme,
                    format_attrs(instance)
                )
            }
        }
    }
}

fn format_attrs(instance: &UserInstance) -> String {
    let mut buffer = "".to_string();

    for (attr, _) in &instance.type_name.attrs {
        buffer.push_str(&format!(
            "{}={}, ",
            attr.lexeme,
            instance.attrs.get(&attr.lexeme).unwrap()
        ));
    }

    buffer.pop();
    buffer.pop();

    buffer
}
