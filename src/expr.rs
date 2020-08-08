use crate::token::{NumberType, Token};
use std::hash::{Hash, Hasher};

pub type OpWithToken<Op> = (Op, Token);

#[derive(Clone, PartialEq, Debug)]
pub enum UnaryOp {
    Minus,
    Plus,
}

#[derive(Clone, PartialEq, Debug)]
pub enum BinaryOp {
    Add,
    Sub,
    Mul,
    Div,
    Mod,
}

#[derive(Clone, PartialEq, Debug)]
pub enum BinaryCompOp {
    NotEqual,
    Equal,
    LessThan,
    LessEqual,
    GreaterThan,
    GreaterEqual,
}

#[derive(Clone, PartialEq, Debug)]
pub enum BinaryLogicOp {
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
    BinaryArith(Box<Expr>, OpWithToken<BinaryOp>, Box<Expr>),
    BinaryComp(Box<Expr>, OpWithToken<BinaryCompOp>, Box<Expr>),
    BinaryLogic(Box<Expr>, OpWithToken<BinaryLogicOp>, Box<Expr>),
    LogicNot((Box<Expr>, Token)),
    Unary(OpWithToken<UnaryOp>, Box<Expr>),
    Grouping(Box<Expr>),
    Literal(OpWithToken<Value>),
}

impl Hash for &Expr {
    fn hash<H>(&self, hasher: &mut H)
    where
        H: Hasher,
    {
        std::ptr::hash(self, hasher)
    }
}

impl Eq for &Expr {}
