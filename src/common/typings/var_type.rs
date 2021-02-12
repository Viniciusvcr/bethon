use crate::common::symbol::token::Token;

use super::{literal_type::LiteralType, types::Type};

#[derive(Debug, Clone, PartialEq)]
pub enum VarType {
    Boolean,
    Integer,
    Float,
    Str,
    Literal(LiteralType),
    PythonNone,
    Function,
    Class(Token),
    Union(Vec<(VarType, Token)>),
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
            Type::Literal(x) => VarType::Literal(x),
            Type::Fun(_, _, _, _) => VarType::Function,
            Type::UserDefined(x) => VarType::Class(x.name_token),
            Type::Union(types) => VarType::Union(
                types
                    .iter()
                    .map(|(t, token)| (t.clone().into(), token.to_owned()))
                    .collect(),
            ),
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
            VarType::Literal(x) => write!(f, "{}", x),
            VarType::PythonNone => write!(f, "None"),
            VarType::Function => write!(f, "function"),
            VarType::Class(token) => write!(f, "{}", token.lexeme),
            VarType::Union(types) => {
                let mut str = "".to_string();

                types
                    .iter()
                    .for_each(|(t, _)| str.push_str(&format!("{} | ", t)));

                str.pop();
                str.pop();
                str.pop();

                write!(f, "{}", str)
            }
        }
    }
}
