use crate::common::symbol::token::Token;

use super::types::Type;

#[derive(Debug, Clone, PartialEq)]
pub enum VarType {
    Boolean,
    Integer,
    Float,
    Str,
    PythonNone,
    Function,
    Class(Token),
}

impl Default for VarType {
    fn default() -> Self {
        VarType::PythonNone
    }
}

impl From<Type> for VarType {
    fn from(x: Type) -> Self {
        match x {
            Type::Integer => VarType::Integer,
            Type::Float => VarType::Float,
            Type::Boolean => VarType::Boolean,
            Type::Null => VarType::PythonNone,
            Type::Str => VarType::Str,
            Type::Fun(_, _, _, _) => VarType::Function,
            Type::UserDefined(x) => VarType::Class(x.name_token),
        }
    }
}

impl std::fmt::Display for VarType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            VarType::Boolean => write!(f, "bool"),
            VarType::Integer => write!(f, "int"),
            VarType::Float => write!(f, "float"),
            VarType::Str => write!(f, "str"),
            VarType::PythonNone => write!(f, "None"),
            VarType::Function => write!(f, "function"),
            VarType::Class(token) => write!(f, "{}", token.lexeme),
        }
    }
}
