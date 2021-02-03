pub mod number_type;
pub mod token_type;

use token_type::TokenType;

use crate::smntc_analyzer::Type;

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
            Type::UserDefined(_) => VarType::Function, // todo change to VarType::UserDefined
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

#[derive(Debug, Clone, PartialEq, Default)]
// Abstracts the data of the position of a Token on the source code
pub struct Placement {
    pub line: usize,
    pub starts_at: usize,
    pub ends_at: usize,
}

impl Placement {
    pub fn new(line: usize, starts_at: usize, ends_at: usize) -> Self {
        Self {
            line,
            starts_at,
            ends_at,
        }
    }

    pub fn as_tuple(&self) -> (usize, usize, usize) {
        (self.line, self.starts_at, self.ends_at)
    }
}

#[derive(Debug, Clone, PartialEq, Default)]
pub struct Token {
    pub tt: TokenType,
    pub lexeme: String,
    pub placement: Placement,
}

impl Token {
    pub fn tt(&self) -> &TokenType {
        &self.tt
    }

    pub fn placement(&self) -> &Placement {
        &self.placement
    }

    pub fn lexeme(&self) -> String {
        self.lexeme.clone()
    }

    pub fn new(
        tt: TokenType,
        lexeme: String,
        line: usize,
        starts_at: usize,
        ends_at: usize,
    ) -> Self {
        let placement = Placement::new(line, starts_at, ends_at);
        Self {
            tt,
            lexeme,
            placement,
        }
    }
}
