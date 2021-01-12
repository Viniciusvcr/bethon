use crate::token::{NumberType, Placement, Token};
use std::{
    hash::{Hash, Hasher},
    usize,
};

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

#[derive(Clone, PartialEq, Debug)]
pub enum Value {
    PythonNone,
    Bool(bool),
    Number(NumberType),
    Str(String),
}

impl std::fmt::Display for Value {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Value::PythonNone => write!(f, "None"),
            Value::Bool(false) => write!(f, "False"),
            Value::Bool(true) => write!(f, "True"),
            Value::Number(value) => write!(f, "{}", value),
            Value::Str(value) => write!(f, "{}", value),
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
    Variable(Token, String),
    Call(Box<Expr>, Vec<Expr>),
}

impl Hash for &Expr {
    fn hash<H>(&self, hasher: &mut H)
    where
        H: Hasher,
    {
        std::ptr::hash(self, hasher)
    }
}

impl Expr {
    fn get_token(&self) -> &Token {
        match self {
            Expr::BinaryArith(_, op_and_token, _) => &op_and_token.token,
            Expr::BinaryComp(_, op_and_token, _) => &op_and_token.token,
            Expr::BinaryLogic(_, op_and_token, _) => &op_and_token.token,
            Expr::LogicNot((_, token)) => &token,
            Expr::Unary(op_and_token, _) => &op_and_token.token,
            Expr::Grouping(expr) => expr.get_token(),
            Expr::Literal(op_and_token) => &op_and_token.token,
            Expr::Variable(token, _) => &token,
            Expr::Call(callee, _) => callee.get_token(),
        }
    }

    pub fn get_line(&self) -> usize {
        self.get_token().placement.line
    }
}

impl Eq for &Expr {}

impl std::fmt::Display for Expr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Expr::BinaryArith(left, op_and_token, right) => {
                write!(f, "{} {} {}", left, op_and_token.op, right)
            }
            Expr::BinaryComp(left, op_and_token, right) => {
                write!(f, "{} {} {}", left, op_and_token.op, right)
            }
            Expr::BinaryLogic(left, op_and_token, right) => {
                write!(f, "{} {} {}", left, op_and_token.op, right)
            }
            Expr::LogicNot((expr, _token)) => write!(f, "not {}", expr),
            Expr::Unary(op_and_token, expr) => write!(f, "{} {}", op_and_token.op, expr),
            Expr::Grouping(expr) => write!(f, "({})", expr),
            Expr::Literal(value_and_token) => write!(f, "{}", value_and_token.op),
            Expr::Variable(_token, id) => write!(f, "{}", id),
            Expr::Call(callee, arguments) => {
                write!(f, "{}(", callee)?;
                for arg in arguments {
                    write!(f, "{}", arg)?;
                }
                write!(f, ")")
            }
        }
    }
}
