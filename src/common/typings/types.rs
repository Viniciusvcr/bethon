use crate::common::environment::SemanticEnvironment;

use super::{user_type::UserType, var_type::VarType};

#[derive(Debug, PartialEq, Clone)]
pub enum Type {
    Integer,
    Float,
    Boolean,
    Null,
    Str,
    Fun(SemanticEnvironment, Vec<VarType>, VarType, Vec<String>),
    UserDefined(UserType),
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
        }
    }
}
