use crate::token::{NumberType, Token};

pub type OpWithToken<Op> = (Op, Token);

#[derive(Clone, PartialEq, Debug)]
pub enum UnaryOp {
    Minus,
    Plus,
}

#[derive(Clone, PartialEq, Debug)]
pub enum BinaryOp {
    Equal,
    LessThan,
    LessEqual,
    GreaterThan,
    GreaterEqual,
    Add,
    Sub,
    Mul,
    Div,
    Mod,
}

#[derive(Clone, PartialEq, Debug)]
pub enum LogicalOp {
    Not,
    And,
    Or,
}

#[derive(Clone, PartialEq, Debug)]
pub enum Value {
    PythonNone,
    Bool(bool),
    Number(NumberType),
    Str(String),
}

impl Value {
    pub fn to_string(&self) -> String {
        use Value::*;

        match self {
            PythonNone => "None".to_string(),
            Bool(false) => format!("False"),
            Bool(true) => format!("True"),
            Number(value) => format!("{}", value),
            Str(value) => value.to_string(),
        }
    }
}

#[derive(Clone, PartialEq, Debug)]
pub enum Expr {
    Binary(Box<Expr>, OpWithToken<BinaryOp>, Box<Expr>),
    Unary(OpWithToken<UnaryOp>, Box<Expr>),
    Grouping(Box<Expr>),
    Literal(OpWithToken<Value>),
}
