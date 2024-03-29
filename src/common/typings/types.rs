use std::collections::HashMap;

use crate::common::{environment::SemanticEnvironment, symbol::token::Token};

use super::{user_type::UserType, var_type::VarType};

#[derive(Debug, PartialEq, Clone)]
pub enum Type {
    Integer,
    Float,
    Boolean,
    Str,
    Null,
    Fun(
        SemanticEnvironment,
        Vec<(Token, VarType)>,
        VarType,
        Vec<String>,
    ),
    UserDefined(UserType),
    Union(Vec<(Type, Token)>),
    Alias(Token, Box<Type>),
    Enum(Token, HashMap<String, Type>),
    Never,
}

impl Type {
    pub fn fmt(x: &Type, y: &Type) -> (Type, Type) {
        (x.to_owned(), y.to_owned())
    }
}

impl Default for Type {
    fn default() -> Self {
        Type::Null
    }
}

impl std::convert::From<&VarType> for Type {
    fn from(var_type: &VarType) -> Self {
        match var_type {
            VarType::Boolean => Type::Boolean,
            VarType::Integer => Type::Integer,
            VarType::Float => Type::Float,
            VarType::Str => Type::Str,
            VarType::PythonNone => Type::Null,
            VarType::Function => Type::Fun(
                SemanticEnvironment::default(),
                Vec::default(),
                VarType::Integer,
                Vec::default(),
            ),
            VarType::Class(x) => Type::UserDefined(UserType::from_var_type(x)),
            VarType::Union(types) => Type::Union(
                types
                    .iter()
                    .map(|(t, token)| (t.into(), token.to_owned()))
                    .collect(),
            ),
        }
    }
}

impl std::fmt::Display for Type {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Type::Null => write!(f, "None"),
            Type::Integer => write!(f, "int"),
            Type::Float => write!(f, "float"),
            Type::Boolean => write!(f, "bool"),
            Type::Str => write!(f, "str"),
            Type::Fun(_, _, ret, _) => write!(f, "<function> -> {}", ret),
            Type::UserDefined(x) => write!(f, "{}", x.name_token.lexeme),
            Type::Union(types) => {
                let mut str = "".to_string();

                types
                    .iter()
                    .for_each(|(t, _)| str.push_str(&format!("{} | ", t)));

                str.pop();
                str.pop();
                str.pop();

                write!(f, "{}", str)
            }
            Type::Alias(id, _) => write!(f, "{}", id.lexeme),
            Type::Enum(id, _) => write!(f, "<enum '{}'>", id.lexeme),
            Type::Never => write!(f, "Never"),
        }
    }
}
