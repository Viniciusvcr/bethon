pub mod callable;
pub mod operations;
pub mod value;

use crate::token::Token;
use std::{
    hash::{Hash, Hasher},
    usize,
};

use operations::*;
use value::Value;

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
