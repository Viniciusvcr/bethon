use crate::common::symbol::token::{Placement, Token};

#[derive(Clone, PartialEq, Debug)]
pub struct OpWithToken<Op> {
    pub op: Op,
    pub token: Token,
}

impl<Op> OpWithToken<Op> {
    pub fn new(op: Op, token: Token) -> Self {
        Self { op, token }
    }

    pub fn get_token_line(&self) -> usize {
        self.token.placement.line
    }

    pub fn get_token_placement(&self) -> &Placement {
        self.token.placement()
    }

    pub fn as_tuple(&self) -> (&Op, &Token) {
        (&self.op, &self.token)
    }
}

#[derive(Clone, PartialEq, Debug, Copy)]
pub enum UnaryOp {
    Minus,
    Plus,
}

impl std::fmt::Display for UnaryOp {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            UnaryOp::Plus => write!(f, "+"),
            UnaryOp::Minus => write!(f, "-"),
        }
    }
}

#[derive(Clone, PartialEq, Debug, Copy)]
pub enum BinaryOp {
    Add,
    Sub,
    Mul,
    Div,
    Mod,
}

impl std::fmt::Display for BinaryOp {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            BinaryOp::Add => write!(f, "+"),
            BinaryOp::Sub => write!(f, "-"),
            BinaryOp::Mul => write!(f, "*"),
            BinaryOp::Div => write!(f, "/"),
            BinaryOp::Mod => write!(f, "%"),
        }
    }
}

#[derive(Clone, PartialEq, Debug, Copy)]
pub enum BinaryCompOp {
    NotEqual,
    Equal,
    LessThan,
    LessEqual,
    GreaterThan,
    GreaterEqual,
}

impl BinaryCompOp {
    pub fn to_word(&self) -> String {
        match self {
            BinaryCompOp::NotEqual => "not equal to".to_string(),
            BinaryCompOp::Equal => "equal to".to_string(),
            BinaryCompOp::LessThan => "less than".to_string(),
            BinaryCompOp::LessEqual => "less equal to".to_string(),
            BinaryCompOp::GreaterThan => "greater than".to_string(),
            BinaryCompOp::GreaterEqual => "greater equal to".to_string(),
        }
    }
}

impl std::fmt::Display for BinaryCompOp {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            BinaryCompOp::NotEqual => write!(f, "!="),
            BinaryCompOp::Equal => write!(f, "=="),
            BinaryCompOp::LessThan => write!(f, "<"),
            BinaryCompOp::LessEqual => write!(f, "<="),
            BinaryCompOp::GreaterThan => write!(f, ">"),
            BinaryCompOp::GreaterEqual => write!(f, ">="),
        }
    }
}

#[derive(Clone, PartialEq, Debug, Copy)]
pub enum BinaryLogicOp {
    And,
    Or,
}

impl std::fmt::Display for BinaryLogicOp {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            BinaryLogicOp::And => write!(f, "and"),
            BinaryLogicOp::Or => write!(f, "or"),
        }
    }
}
