use num_bigint::BigInt;

use super::types::Type;

#[derive(Debug, Clone)]
pub enum LiteralType {
    Boolean(bool),
    Integer(BigInt),
    Float(f64),
    Str(String),
}

impl LiteralType {
    pub fn to_primitive_type(&self) -> Type {
        match self {
            LiteralType::Boolean(_) => Type::Boolean,
            LiteralType::Integer(_) => Type::Integer,
            LiteralType::Float(_) => Type::Float,
            LiteralType::Str(_) => Type::Str,
        }
    }
}

impl std::fmt::Display for LiteralType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            LiteralType::Boolean(true) => write!(f, "True"),
            LiteralType::Boolean(false) => write!(f, "False"),
            LiteralType::Integer(x) => write!(f, "{}", x),
            LiteralType::Float(x) => write!(f, "{}", x),
            LiteralType::Str(x) => write!(f, "\"{}\"", x),
        }
    }
}

impl PartialEq for LiteralType {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (LiteralType::Boolean(a), LiteralType::Boolean(b)) => a == b,
            (LiteralType::Integer(a), LiteralType::Integer(b)) => a == b,
            (LiteralType::Float(a), LiteralType::Float(b)) => a == b,
            (LiteralType::Str(a), LiteralType::Str(b)) => a == b,
            _ => false,
        }
    }
}
