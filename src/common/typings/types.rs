use crate::common::{environment::SemanticEnvironment, symbol::token::Token};

use super::{literal_type::LiteralType, user_type::UserType, var_type::VarType};

#[derive(Debug, PartialEq, Clone)]
pub enum Type {
    Integer,
    Float,
    Boolean,
    Str,
    Literal(LiteralType),
    Null,
    Fun(
        SemanticEnvironment,
        Vec<(Token, VarType)>,
        VarType,
        Vec<String>,
    ),
    UserDefined(UserType),
}

impl Type {
    pub fn fmt(x: &Type, y: &Type) -> (Type, Type) {
        match (x, y) {
            (Type::Literal(a), Type::Literal(b)) => (a.to_primitive_type(), b.to_primitive_type()),
            (Type::Literal(_), t) => (x.to_owned(), t.to_owned()),
            (t, Type::Literal(b)) => (t.to_owned(), b.to_primitive_type()),
            (a, b) => (a.to_owned(), b.to_owned()),
        }
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
            VarType::Literal(x) => Type::Literal(x.to_owned()),
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
            Type::Literal(x) => write!(f, "{}", x),
            Type::Fun(_, _, ret, _) => write!(f, "<function> -> {}", ret),
            Type::UserDefined(x) => write!(f, "{}", x.name_token.lexeme),
        }
    }
}
